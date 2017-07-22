(require 'direx)
(require 'esa-cui)
(require 'json)
(require 'markdown-mode)
(require 'log4e)


(log4e:deflogger "esa-direx" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                       (error . "error")
                                                       (warn  . "warn")
                                                       (info  . "info")
                                                       (debug . "debug")
                                                       (trace . "trace")))
(esa-direx--log-set-level 'trace)


(defgroup esa-direx nil
  ""
  :group 'direx
  :prefix "esa-direx:")

(defclass esa-direx:element (direx:tree)
  ((name :initarg :name :accessor esa-direx:element-name)
   (item :initarg :item :accessor esa-direx:element-item)))

(defclass esa-direx:team (direx:node)
  ((name :initarg :name :accessor esa-direx:team-name)))

(defclass esa-direx:category (esa-direx:element direx:node) ())

(defclass esa-direx:post (esa-direx:element direx:leaf)
  ((number :initarg :number :accessor esa-direx:post-number)))

(defclass esa-direx:team-item (direx:item) ())

(defclass esa-direx:category-item (direx:item) ())

(defclass esa-direx:post-item (direx:item) ())

(defgeneric esa-direx:full-name (element))

(defmethod esa-direx:full-name ((team esa-direx:team))
  "")

(defmethod esa-direx:full-name ((element esa-direx:element))
  (format "%s/%s"
          (or (direx:awhen (direx:item-parent (esa-direx:element-item element))
                (esa-direx:full-name (direx:item-tree it)))
              "")
          (esa-direx:element-name element)))

(defmethod direx:make-item ((team esa-direx:team) parent)
  (let ((item (make-instance 'esa-direx:team-item :tree team)))
    (esa-direx--debug "make team : %s" (esa-direx:element-name team))
    item))

(defmethod direx:make-item ((category esa-direx:category) parent)
  (let ((item (make-instance 'esa-direx:category-item
                             :tree category
                             :parent parent)))
    (setf (esa-direx:element-item category) item)
    (esa-direx--debug "make category : %s" (esa-direx:full-name category))
    item))

(defmethod direx:make-item ((post esa-direx:post) parent)
  (let ((item (make-instance 'esa-direx:post-item
                             :tree post
                             :parent parent)))
    (setf (esa-direx:element-item post) item)
    (esa-direx--debug "make post : %s" (esa-direx:full-name post))
    item))

(defmethod direx:tree-equals ((x esa-direx:post) y)
  (or (eq x y)
      (and (cl-typep y 'esa-direx:post)
           (equal (esa-direx:full-name x) (esa-direx:full-name y)))))

(defmethod direx:node-children ((team esa-direx:team))
  (esa-direx::collect-children "/"))

(defmethod direx:node-children ((category esa-direx:category))
  (esa-direx::collect-children (esa-direx:full-name category)))

(defun esa-direx::collect-children (path)
  (esa-cui:cd path)
  (cl-loop for line in (split-string (esa-cui:ls) "\n")
           if (string-match "/\\'" line)
           collect (esa-direx::make-category
                    (replace-regexp-in-string "/\\'" "" line))
           else if (string-match "\\`\\([0-9]+\\): \\(.+\\)\\'" line)
           collect (esa-direx::make-post
                    path
                    (match-string-no-properties 1 line)
                    (match-string-no-properties 2 line))))

(defun esa-direx::make-team (name)
  (make-instance 'esa-direx:team :name name))

(defun esa-direx::make-category (name)
  (make-instance 'esa-direx:category :name name))

(defun esa-direx::make-post (category-path number name)
  (let* ((json-string (esa-cui:cat (concat category-path "/" number)
                                   :json t
                                   :intent nil))
         (json (json-read-from-string json-string)))
    (make-instance 'esa-direx:post
                   :number number
                   :name name)))

(defmethod direx:generic-find-item ((item esa-direx:category-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-view-item ((item esa-direx:category-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-display-item ((item esa-direx:category-item) not-this-window)
  (direx:toggle-item item))

(defun esa-direx::open-post (post-item)
  (let ((post (direx:item-tree post-item)))
    (run-with-idle-timer 0.2 nil
                         '(lambda (post)
                            (let ((buf (esa-direx::make-post-buffer post)))
                              (switch-to-buffer buf)))
                         post)))

(defmethod direx:generic-find-item ((item esa-direx:post-item) not-this-window)
  (esa-direx::open-post item))

(defmethod direx:generic-view-item ((item esa-direx:post-item) not-this-window)
  (esa-direx::open-post item))

(defmethod direx:generic-display-item ((item esa-direx:post-item) not-this-window)
  (esa-direx::open-post item))

(defvar esa-direx::post nil)

(defun esa-direx::make-post-buffer (post)
  (let* ((post-item (esa-direx:element-item post))
         (category (direx:item-tree (direx:item-parent post-item)))
         (category-full-name (esa-direx:full-name category))
         (number (esa-direx:post-number post))
         (buf-name (format "*esa post %s*" number)))
    (esa-direx--debug "Start make post buffer : category[%s] number[%s]" category-full-name number)
    (or (direx:awhen (get-buffer buf-name)
          (when (buffer-live-p it) it))
        (with-current-buffer (get-buffer-create buf-name)
          (set-buffer-file-coding-system 'utf-8-dos)
          (esa-cui:cd (esa-direx:full-name category))
          (insert (esa-cui:cat number))
          (markdown-mode)
          (local-set-key [remap save-buffer] 'esa-direx:save-post)
          (set (make-local-variable 'esa-direx::post) post)
          (current-buffer)))))

(define-derived-mode esa-direx:mode direx:direx-mode "Direx Esa")

(defun esa-direx::get-buffer (team)
  (let ((bufnm (format "*Direx Esa %s*" (esa-direx:team-name team))))
    (or (direx:awhen (get-buffer bufnm)
          (when (buffer-live-p it) it))
        (with-current-buffer (get-buffer-create bufnm)
          (esa-direx:mode)
          (setq-local revert-buffer-function 'direx:revert-buffer)
          (current-buffer)))))

(defmethod direx:make-buffer ((team esa-direx:team))
  (esa-direx::get-buffer team))

(defun esa-direx:ensure-buffer (team)
  (esa-cui:cd "/")
  (direx:ensure-buffer-for-root (esa-direx::make-team "mf")))


;;;###autoload
(defun esa-direx:open-team (team)
  (interactive
   (completing-read "Team: " (esa-cui:teams) nil t nil '()))
  (switch-to-buffer-other-window (esa-direx:ensure-buffer team)))


(provide 'esa-direx)
