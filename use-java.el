;;; use-java.el --- Minimal Java + Eglot config -*- lexical-binding: t; -*-

(require 'eglot)

(defconst eglot-java-filename-build-maven "pom.xml" "Maven build file name.")

(defcustom eglot-jdtls-java-home
  (expand-file-name ".." (file-name-directory (file-chase-links (executable-find "javac"))))
  "Default JAVA_HOME location."
  :type 'directory
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-java-program
  (executable-find "java")
  "Default Java executable path location."
  :type 'file
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-dl-snapshots-url
  "https://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz"
  "URL to fetch Eclipse JDT language server packaging information."
  :type 'string
  :risky t
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-dl-metadata-url
  "https://download.eclipse.org/jdtls/snapshots/latest.txt"
  "URL to fetch Eclipse JDT language server packaging information."
  :type 'string
  :risky t
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-install-dir
  (expand-file-name  "share/eclipse.jdt.ls" user-emacs-directory)
  "Location of the Eclipse Java language server installation."
  :type 'directory
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-dl-snapshots-filename
  (expand-file-name "jdt-language-server-latest.tar.gz" eglot-jdtls-install-dir)
  "Location of default jdtls."
  :type 'string
  :risky t
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-dl-metadata-filename
  (expand-file-name "latest.txt" eglot-jdtls-install-dir)
  "Location of the latest.txt version file."
  :type 'string
  :risky t
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-config-dir
  (expand-file-name "config_linux" eglot-jdtls-install-dir)
  "Default to config_linux"
  :type 'string
  :risky t
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-data-dir
  (expand-file-name  "share/workspace" user-emacs-directory)
  "Base directory for JDTLS workspaces, with one subdirectory per project."
  :type 'string
  :risky t
  :group 'eglot-jdtls)

(defun eglot-jdtls--launcher-jars (plugins)
  "Return full paths to Equinox launcher JARs in PLUGINS."
  (directory-files plugins t
                   "\\`org\\.eclipse\\.equinox\\.launcher_.*\\.jar\\'"))

(defun eglot-jdtls--workspace-dir ()
  "Return and create the current project's JDTLS workspace directory."
  (let* ((root (directory-file-name
                (file-truename
                 (project-root (eglot--current-project)))))
         (name (file-name-nondirectory root))
         (workspace (expand-file-name
                     (concat name "-" (md5 root))
                     eglot-jdtls-data-dir)))
    (make-directory workspace t)
    workspace))

(defun eglot-jdtls--contact (_interactive)
  "Build the JDTLS contact using the current installation's launcher."
  (let* ((plugins (expand-file-name "plugins" eglot-jdtls-install-dir))
         (jar (when (file-directory-p plugins)
                (car (eglot-jdtls--launcher-jars plugins)))))
    (unless (and jar (file-readable-p jar))
      (user-error "Cannot find a readable JDTLS launcher in %s" plugins))
    `(eglot-java-eclipse-jdt
      ,eglot-jdtls-java-program
      ;; "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044"
      ;; "-Dlog.protocol=true"
      "-Declipse.application=org.eclipse.jdt.ls.core.id1"
      "-Dosgi.bundles.defaultStartLevel=4"
      "-Declipse.product=org.eclipse.jdt.ls.core.product"
      "-Dlog.level=ALL"
      "-Xmx1G"
      "--add-modules=ALL-SYSTEM"
      "--add-opens" "java.base/java.util=ALL-UNNAMED"
      "--add-opens" "java.base/java.lang=ALL-UNNAMED"
      "-jar" ,jar
      "-configuration" ,eglot-jdtls-config-dir
      "-data" ,(eglot-jdtls--workspace-dir))))

(defun eglot-jdtls--download-file (source-url dest-location)
  "Download a file from a URL at SOURCE-URL and save it to file at DEST-LOCATION."
  (let* ((dest-dir     (file-name-directory dest-location))
         (dest-abspath (expand-file-name dest-location)))
    (unless (file-exists-p dest-dir)
      (make-directory dest-dir t))
    (message "Downloading %s\n to %s." source-url dest-abspath)
    (url-copy-file  source-url dest-abspath t)))

(defun eglot-jdtls--check-jdtls-ver ()
  "Return non-nil if JDTLS is current; missing metadata means unknown."
  (when (file-readable-p eglot-jdtls-dl-metadata-filename)
    (let* ((lines (with-temp-buffer
                    (insert-file-contents eglot-jdtls-dl-metadata-filename)
                    (split-string (buffer-string) "\n" t)))
           (first (car lines))
           (buf (url-retrieve-synchronously eglot-jdtls-dl-metadata-url t t 10))
           (meta (with-current-buffer buf
                   (goto-char (point-min))
                   ;; Headers end at blank line
                   (re-search-forward "\r?\n\r?\n" nil 'noerror)
                   (buffer-substring-no-properties (point) (point-max)))))
      (kill-buffer buf)
      (message "first:%s" first)
      (message "meta:%s" meta)
      (equal (string-trim meta) (string-trim first)))))

(defun eglot-jdtls--upgrade-lsp-server ()
  "Back up JDTLS, upgrade, and restore on failure.
Stop JDTLS before upgrading.  Keep the backup after a successful upgrade."
  (unless (eglot-jdtls--check-jdtls-ver)
    (let* ((install-dir (directory-file-name
                         (expand-file-name eglot-jdtls-install-dir)))
           (backup-dir (make-temp-name (concat install-dir ".backup-"))))
      (rename-file install-dir backup-dir)
      (condition-case err
          (progn
            (make-directory install-dir t)
            (eglot-jdtls--install-lsp-server)
            (message "JDTLS upgraded; backup kept at %s." backup-dir))
        ((error quit)
         (let ((inhibit-quit t))
           (when (file-exists-p install-dir)
             (delete-directory install-dir t))
           (rename-file backup-dir install-dir))
         (signal (car err) (cdr err)))))))

(defun eglot-jdtls--install-lsp-server ()
  "Install the Eclipse JDT LSP server.
DESTINATION-DIR is the directory where the LSP server will be installed."
  (message "Installing Eclipse JDT LSP server, please wait...")
  (eglot-jdtls--download-file eglot-jdtls-dl-snapshots-url eglot-jdtls-dl-snapshots-filename)
  (eglot-jdtls--download-file eglot-jdtls-dl-metadata-url eglot-jdtls-dl-metadata-filename)
  (message "Extracting Eclipse JDT LSP archive, please wait...")
  (unless (eq 0 (call-process "tar" nil "*tar*" t "zxvf"
                              eglot-jdtls-dl-snapshots-filename
                              "-C" eglot-jdtls-install-dir))
    (error "JDTLS extraction failed; see *tar* buffer"))
  (let ((plugins (expand-file-name "plugins" eglot-jdtls-install-dir)))
    (unless (and (file-directory-p plugins)
                 (cl-some #'file-readable-p
                          (eglot-jdtls--launcher-jars plugins))
                 (file-readable-p
                  (expand-file-name "config.ini" eglot-jdtls-config-dir)))
      (error "JDTLS installation is missing its launcher or config.ini")))
  (message "Eclipse JDT LSP server installed in folder \n\"%s\"." eglot-jdtls-install-dir))

(defun eglot-jdtls--ensure ()
  "Install the LSP server as needed and then turn-on eglot."
  (unless (file-exists-p (expand-file-name eglot-jdtls-install-dir))
    (eglot-jdtls--install-lsp-server))
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . eglot-jdtls--contact))
  (eglot-ensure))



(defun eglot-java--find-server ()
  "Find the LSP server of type eglot-java-eclipse-jdt for the
current project. In the strange event that there are multiple,
return the first one."
  (when-let* ((project (project-current))
              (servers (gethash project eglot--servers-by-project)))
    (cl-find-if #'eglot-java-eclipse-jdt-p servers)))

;; (defun java-kit--jdt-uri-handler (operation &rest arguments)
;;   "Handle file OPERATION with ARGUMENTS for JDTLS `jdt://' URIs."
;;   (when (memq operation java-kit--jdt-uri-write-operations)
;;     (signal 'file-error
;;             (list "JDTLS class contents are read-only" (car arguments))))
;;   (let ((local-arguments
;;          (mapcar
;;           (lambda (argument)
;;             (if (and (stringp argument)
;;                      (string-prefix-p "jdt://" argument))
;;                 (java-kit--jdt-uri-local-file argument)
;;               argument))
;;           arguments))
;;         (inhibit-file-name-handlers
;;          (cons #'java-kit--jdt-uri-handler
;;                (and (eq inhibit-file-name-operation operation)
;;                     inhibit-file-name-handlers)))
;;         (inhibit-file-name-operation operation))
;;     (apply operation local-arguments)))
(defun eglot-java--jdt-uri-handler (_operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir (expand-file-name ".eglot-java" (project-root (project-current t))))
         (source-file
          (expand-file-name
           (save-match-data
             (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
               (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))
           cache-dir)))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-java--find-server) :java/classFileContents (list :uri uri)))
            (metadata-file (format "%s.%s.metadata"
                                   (file-name-directory source-file)
                                   (file-name-base source-file))))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert content))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(add-to-list 'file-name-handler-alist '("\\`jdt://" . eglot-java--jdt-uri-handler))

(defclass eglot-java-eclipse-jdt (eglot-lsp-server) ()
  :documentation "Eclipse's Java Development Tools Language Server.")

(cl-defmethod eglot-initialization-options ((server eglot-java-eclipse-jdt))
  "Passes through required jdt initialization options."
  (let ((settings-plist
         `(
           :extendedClientCapabilities (:classFileContentsSupport t)
           :workspaceFolders
           [,@(cl-delete-duplicates
               (mapcar
                #'eglot--path-to-uri
                (let* ((root (project-root (eglot--project server))))
                  (cons
                   root
                   (mapcar
                    #'file-name-directory
                    (append
                     (file-expand-wildcards (concat root "/*/" eglot-java-filename-build-maven))
                     (file-expand-wildcards (concat root "/.project")))))))
               :test #'string=)]
           :settings
           (:java (
                   :home ,eglot-jdtls-java-home
                   :format (:settings (
                                       :url "/home/sbzorro/.emacs.d/share/mimi.xml"
                                       :profile "mimi")))))))
    settings-plist))

;;;###autoload
(define-minor-mode eglot-jdtls-mode
  "Toggle 'eglot-jdtls-mode."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter nil
  (if eglot-jdtls-mode
      (eglot-jdtls--ensure)
    (ignore-errors
      (call-interactively 'eglot-shutdown))))

(provide 'eglot-jdtls)
