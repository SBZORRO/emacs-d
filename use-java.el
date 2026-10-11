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
      ;; Compatibility with legacy AspectJ JARs in this Java 8 project.
      "-Djdk.util.zip.disableZip64ExtraFieldValidation=true"
      "--add-modules=ALL-SYSTEM"
      "--add-opens" "java.base/java.util=ALL-UNNAMED"
      "--add-opens" "java.base/java.lang=ALL-UNNAMED"
      "-javaagent:/home/sbzorro/.emacs.d/share/lombok.jar"
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Handle navigation within JDTLS class sources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JDTLS uses jdt:// URIs for virtual Java documents, typically classes
;; from dependency JARs or the JDK.  A typical URI has this structure:
;;   jdt://contents/<jar>/<package>/<class>?<JDT-element-handle>
;; The query contains the handle JDTLS uses to resolve the Java model
;; element.  Treat the complete URI as opaque and send it back unchanged.
;;
;; This code requests text through java/classFileContents and caches it in
;; a read-only .java file.  Its .metadata sidecar stores the original URI.
;; Eglot uses the local path to open the source and associate its server,
;; then uses the original URI for further navigation into parent classes
;; and constructors.  MD5 only supplies a short, distinct cache filename.
;;
;; The request is documented in the JDTLS protocol extensions wiki:
;; https://github.com/eclipse-jdtls/eclipse.jdt.ls/wiki/Language-Server-Protocol-Extensions
;; The URI structure is defined by JDTUtils.toUri and resolveClassFile:
;; https://github.com/eclipse-jdtls/eclipse.jdt.ls/blob/v1.50.0/org.eclipse.jdt.ls.core/src/org/eclipse/jdt/ls/core/internal/JDTUtils.java
;;
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
;; (defun eglot-java--jdt-uri-handler (_operation &rest args)
;;   "Support Eclipse jdtls `jdt://' uri scheme."
;;   (let* ((uri (car args))
;;          (cache-dir (expand-file-name ".eglot-java" (project-root (project-current t))))
;;          (source-file
;;           (expand-file-name
;;            (save-match-data
;;              (when (string-match "jdt://contents/\\(.*?\\)/\\(.*\\)\.class\\?" uri)
;;                (format "%s.java" (replace-regexp-in-string "/" "." (match-string 2 uri) t t))))
;;            cache-dir)))
;;     (unless (file-readable-p source-file)
;;       (let ((content (jsonrpc-request (eglot-java--find-server) :java/classFileContents (list :uri uri)))
;;             (metadata-file (format "%s.%s.metadata"
;;                                    (file-name-directory source-file)
;;                                    (file-name-base source-file))))
;;         (unless (file-directory-p cache-dir) (make-directory cache-dir t))
;;         (with-temp-file source-file (insert content))
;;         (with-temp-file metadata-file (insert uri))))
;;     source-file))

(defcustom eglot-jdtls-class-cache-dir
  (expand-file-name "share/jdt-sources" user-emacs-directory)
  "Cache for read-only Java class sources returned by JDTLS."
  :type 'directory
  :group 'eglot-jdtls)

(defvar eglot-jdtls--fetching-class-uris nil
  "JDTLS class URIs currently being fetched, to prevent recursive requests.")

(defun eglot-jdtls--class-cache-file (uri)
  "Return a distinct local cache filename for JDTLS URI.
Use the class name and MD5 of the complete URI, including its query,
so classes from different libraries or projects get distinct cache files."
  (let* ((path (car (split-string uri "?")))
         (base (file-name-base (car (last (split-string path "/" t)))))
         (safe-base (replace-regexp-in-string "[^[:alnum:]_$-]" "_" base)))
    (expand-file-name
     (format "%s-%s.java" safe-base (secure-hash 'md5 uri))
     eglot-jdtls-class-cache-dir)))

(defun eglot-jdtls--class-source-file (uri)
  "Fetch source text for URI once and return its read-only local file.
Cache the text in a .java file and preserve the original JDTLS URI in a
.metadata sidecar file for subsequent navigation requests."
  (let ((file (eglot-jdtls--class-cache-file uri)))
    (unless (file-readable-p file)
      (when (member uri eglot-jdtls--fetching-class-uris)
        (error "Recursive JDTLS class source request: %s" uri))
      (let* ((eglot-jdtls--fetching-class-uris
              (cons uri eglot-jdtls--fetching-class-uris))
             (server (or (eglot-current-server) (eglot-java--find-server))))
        (unless server
          (user-error "No JDTLS server is available for this class source"))
        (let ((content (
                        jsonrpc-request
                        server
                        :java/classFileContents (list :uri uri)
                        :timeout 10)))
          (unless (stringp content)
            (error "JDTLS returned no class source for %s" uri))
          (make-directory (file-name-directory file) t)
          (with-temp-file file (insert content))
          (set-file-modes file #o444))))
    ;; Retain the server's document identity when Emacs visits the cache file.
    (let ((metadata (concat file ".metadata")))
      (unless (file-readable-p metadata)
        (with-temp-file metadata (insert uri))))
    file))

(defun eglot-java--jdt-uri-handler (operation &rest arguments)
  "Perform file OPERATION on local read-only sources for `jdt://' URIs.
Make Emacs file operations work with these virtual documents by replacing
URI arguments in ARGUMENTS with cached source paths and calling OPERATION."
  (when (memq operation '(
                          write-region delete-file
                          rename-file add-name-to-file
                          make-symbolic-link set-file-modes set-file-times))
    (signal 'file-error (list "JDTLS class sources are read-only")))
  (cond
   ((eq operation 'file-remote-p) nil)
   ;; Name normalization must not start another synchronous LSP request.
   ((eq operation 'expand-file-name) (car arguments))
   (t
    (let* ((inhibit-file-name-handlers
            (cons #'eglot-java--jdt-uri-handler
                  (and (eq inhibit-file-name-operation operation)
                       inhibit-file-name-handlers)))
           (inhibit-file-name-operation operation)
           (local-arguments
            (mapcar
             (lambda (argument)
               (if (and (stringp argument)
                        (string-prefix-p "jdt://" argument))
                   (eglot-jdtls--class-source-file argument)
                 argument))
             arguments)))
      (apply operation local-arguments)))))

(add-to-list 'file-name-handler-alist '("\\`jdt://" . eglot-java--jdt-uri-handler))

(defun eglot-jdtls--cached-source-uri (path)
  "Read the original JDTLS URI associated with cached source PATH.
Recover the URI from the .metadata sidecar file so requests retain the
class document's identity within JDTLS."
  (when (and (stringp path) (file-name-absolute-p path)
             (string-prefix-p
              (file-name-as-directory (expand-file-name eglot-jdtls-class-cache-dir))
              (expand-file-name path)))
    (let ((metadata (concat (expand-file-name path) ".metadata")))
      (when (file-readable-p metadata)
        (with-temp-buffer
          (insert-file-contents metadata)
          (let ((uri (buffer-string)))
            (when (string-prefix-p "jdt://" uri) uri)))))))

(defun eglot-jdtls--uri-to-path (convert uri)
  "Convert incoming JDTLS URI locations into local cache paths.
This lets Eglot record the server associated with each source file.
Delegate ordinary URI conversion to the original function CONVERT."
  (let ((text (if (keywordp uri) (substring (symbol-name uri) 1) uri)))
    (if (and (stringp text) (string-prefix-p "jdt://" text))
        (eglot-jdtls--class-source-file text)
      (funcall convert uri))))

(defun eglot-jdtls--path-to-uri (convert path &rest options)
  "Convert cached source PATH back into its original JDTLS document URI.
Use that URI for outgoing requests so navigation can continue into parent
classes and constructors.  Delegate other paths and OPTIONS to CONVERT."
  (or (eglot-jdtls--cached-source-uri path)
      (apply convert path options)))

(advice-add 'eglot-uri-to-path :around #'eglot-jdtls--uri-to-path)
(advice-add 'eglot-path-to-uri :around #'eglot-jdtls--path-to-uri)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                   ;; Resolve editor dependencies from the local Maven cache.
                   :import (:maven (:offline (:enabled t)))
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
