;;; init.el --- Minimal Java + Eglot config -*- lexical-binding: t; -*-

(require 'eglot)

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

(defcustom eglot-jdtls-equinox-launcher-jar
  (car (directory-files-recursively eglot-jdtls-install-dir  "^org.eclipse.equinox.launcher_.*.jar$" t))
  "Recursively find eclipse equinox launcher jar form jdtls install dir"
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
  "Default workspace"
  :type 'string
  :risky t
  :group 'eglot-jdtls)

(defcustom eglot-jdtls-server-program
  `(,eglot-jdtls-java-program
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
    "-jar" ,eglot-jdtls-equinox-launcher-jar
    "-configuration" ,eglot-jdtls-config-dir
    "-data" ,eglot-jdtls-data-dir)
  "Eclipse JDT JVM arguments."
  :type '(repeat string)
  :risky t
  :group 'eglot-jdtls)

(defun eglot-jdtls--download-file (source-url dest-location)
  "Download a file from a URL at SOURCE-URL and save it to file at DEST-LOCATION."
  (let*
      (
       (dest-dir     (file-name-directory dest-location))
       (dest-abspath (expand-file-name dest-location)))
    (unless
        (file-exists-p dest-dir)
      (make-directory dest-dir t))
    (message "Downloading %s\n to %s." source-url dest-abspath)
    (url-copy-file  source-url dest-abspath t)))

(defun eglot-jdtls--check-jdtls-ver()
  (let*
      ((lines
        (with-temp-buffer
          (insert-file-contents (expand-file-name "latest.txt" eglot-jdtls-install-dir))
          (split-string (buffer-string) "\n" t)))
       (first (car lines))
       (buf (url-retrieve-synchronously eglot-jdtls-dl-metadata-url t t 10))
       (meta
        (with-current-buffer buf
          (goto-char (point-min))
          ;; Headers end at blank line
          (re-search-forward "\r?\n\r?\n" nil 'noerror)
          (buffer-substring-no-properties (point) (point-max)))))
    (kill-buffer buf)
    (message "first:%s" first)
    (message "meta:%s" meta)
    (equal meta first)))

(defun eglot-jdtls--upgrade-lsp-server ()
  "Upgrade the LSP server installation in a given directory.
INSTALL-DIR is the directory where the LSP server will be upgraded."
  (unless (eglot-jdtls--check-jdtls-ver)
    (progn
      (delete-directory eglot-jdtls-install-dir t)
      (mkdir eglot-jdtls-install-dir t)
      (eglot-jdtls--install-lsp-server))))

(defun eglot-jdtls--install-lsp-server ()
  "Install the Eclipse JDT LSP server.
DESTINATION-DIR is the directory where the LSP server will be installed."
  (message "Installing Eclipse JDT LSP server, please wait...")
  (eglot-jdtls--download-file eglot-jdtls-dl-snapshots-url eglot-jdtls-dl-snapshots-filename)
  (eglot-jdtls--download-file eglot-jdtls-dl-metadata-url eglot-jdtls-dl-metadata-filename)
  (message "Extracting Eclipse JDT LSP archive, please wait...")
  (call-process "tar" nil "*tar*" t "zxvf" eglot-jdtls-dl-snapshots-filename "-C" eglot-jdtls-install-dir)
  (message "Eclipse JDT LSP server installed in folder \n\"%s\"." eglot-jdtls-install-dir))

(defun eglot-jdtls--ensure ()
  "Install the LSP server as needed and then turn-on eglot."
  (unless (file-exists-p (expand-file-name eglot-jdtls-install-dir))
    (eglot-jdtls--install-lsp-server))
  (add-to-list 'eglot-server-programs
               `((java-mode java-ts-mode) . ,eglot-jdtls-server-program))
  (eglot-ensure))



(defun eglot-java--jdt-uri-handler (_operation &rest args)
  "Support Eclipse jdtls `jdt://' uri scheme."
  (let* ((uri (car args))
         (cache-dir (expand-file-name ".eglot-java" (project-root (project-current t))))
         (source-file
          (expand-file-name
           (eglot-java--make-path
            cache-dir
            (save-match-data
              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))))))
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
                     (file-expand-wildcards (concat root "/*" eglot-java-filename-build-maven))
                     (file-expand-wildcards (concat root "/.project")))))))
               :test #'string=)]
           ,@(if-let ()
                 `(:settings (
                              :java (:home ,eglot-jdtls-java-home)
                              :import ()))
               (ignore (eglot--warn "JAVA_HOME environment is not set"))))))
    (if eglot-java-user-init-opts-fn
        (if (fboundp eglot-java-user-init-opts-fn)
            (let ((user-opts (funcall eglot-java-user-init-opts-fn server eglot-java-eclipse-jdt)))
              (eglot-java--plist-merge settings-plist user-opts))
          (warn (format "The LSP settings initialization function is not bound! %s!" eglot-java-user-init-opts-fn)))
      settings-plist)))

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
