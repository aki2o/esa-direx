(require 'direx)
(require 'esal)
(require 'json)
(require 'log4e)
(require 'dired)


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

(defface esa-direx:team-face
  '((t (:inherit dired-directory)))
  "Face for team."
  :group 'esa-direx)

(defface esa-direx:category-face
  '((t (:inherit dired-directory)))
  "Face for category."
  :group 'esa-direx)

(defface esa-direx:shipped-post-face
  nil
  "Face for shipped post."
  :group 'esa-direx)

(defface esa-direx:wip-post-face
  '((t (:inherit dired-ignored)))
  "Face for wip post."
  :group 'esa-direx)

(defface esa-direx:locked-post-face
  '((t (:inherit dired-warning)))
  "Face for locked post."
  :group 'esa-direx)


(defclass esa-direx:element (direx:tree)
  ((name :initarg :name :accessor esa-direx:element-name)
   (item :initarg :item :accessor esa-direx:element-item)))

(defclass esa-direx:team (esa-direx:element direx:node) ())

(defclass esa-direx:category (esa-direx:element direx:node) ())

(defclass esa-direx:post (esa-direx:element direx:leaf)
  ((number :initarg :number :accessor esa-direx:post-number)
   (wip :initarg :wip :accessor esa-direx:post-wip-p)
   (locked :initarg :locked :accessor esa-direx:post-locked-p)
   (tags :initarg :tags :accessor esa-direx:post-tags)
   (url :initarg :url :accessor esa-direx:post-url)
   (local-path :initarg :local-path :accessor esa-direx:post-local-path)
   (created-by :initarg :created-by :accessor esa-direx:post-created-by)
   (updated-by :initarg :updated-by :accessor esa-direx:post-updated-by)))

(defclass esa-direx:team-item (direx:item) ())

(defclass esa-direx:category-item (direx:item) ())

(defclass esa-direx:post-item (direx:item) ())

(defgeneric esa-direx:team-of (element))

(defmethod esa-direx:team-of ((team esa-direx:team))
  team)

(defmethod esa-direx:team-of ((element esa-direx:element))
  (esa-direx:team-of (direx:item-tree (direx:item-parent (esa-direx:element-item element)))))

(defgeneric esa-direx:full-name (element))

(defmethod esa-direx:full-name ((team esa-direx:team))
  "")

(defmethod esa-direx:full-name ((element esa-direx:element))
  (format "%s/%s"
          (esa-direx:full-name (direx:item-tree (direx:item-parent (esa-direx:element-item element))))
          (esa-direx:element-name element)))

(defmethod direx:make-item ((team esa-direx:team) parent)
  (let ((item (make-instance 'esa-direx:team-item
                             :tree team
                             :face 'esa-direx:team-face)))
    (setf (esa-direx:element-item team) item)
    (esa-direx--debug "make team : %s" (esa-direx:element-name team))
    item))

(defmethod direx:make-item ((category esa-direx:category) parent)
  (let ((item (make-instance 'esa-direx:category-item
                             :tree category
                             :parent parent
                             :face 'esa-direx:category-face)))
    (setf (esa-direx:element-item category) item)
    (esa-direx--debug "make category : %s" (esa-direx:full-name category))
    item))

(defmethod direx:make-item ((post esa-direx:post) parent)
  (let* ((face (cond ((esa-direx:post-locked-p post)
                      'esa-direx:locked-post-face)
                     ((esa-direx:post-wip-p post)
                      'esa-direx:wip-post-face)
                     (t
                      'esa-direx:shipped-post-face)))
         (item (make-instance 'esa-direx:post-item
                             :tree post
                             :parent parent
                             :face face)))
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
  (esal-cd path)
  (cl-loop for line in (split-string (esal-ls) "\n")
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
  (let* ((json-string (esal-cat (concat category-path "/" number)
                                :json t
                                :indent nil))
         (post-stats (json-read-from-string json-string)))
    (make-instance 'esa-direx:post
                   :name name
                   :number number
                   :wip (not (eq (assoc-default 'wip post-stats) json-false))
                   :locked (not (eq (assoc-default 'locked post-stats) json-false))
                   :tags (assoc-default 'tags post-stats)
                   :url (assoc-default 'url post-stats)
                   :local-path (assoc-default 'local_path post-stats)
                   :created-by (assoc-default 'created_by post-stats)
                   :updated-by (assoc-default 'updated_by post-stats))))

(defmethod direx:generic-find-item ((item esa-direx:team-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-view-item ((item esa-direx:team-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-display-item ((item esa-direx:team-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-find-item ((item esa-direx:category-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-view-item ((item esa-direx:category-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-display-item ((item esa-direx:category-item) not-this-window)
  (direx:toggle-item item))

(defmethod direx:generic-find-item ((item esa-direx:post-item) not-this-window)
  (let* ((post (direx:item-tree item))
         (filename (esa-direx:post-local-path post)))
    (esal-lock (esa-direx:post-number post))
    (esa-direx::setup-post-buffer (if not-this-window
                                      (find-file-other-window filename)
                                    (find-file filename))
                                  post)))

(defmethod direx:generic-view-item ((item esa-direx:post-item) not-this-window)
  (let* ((post (direx:item-tree item))
         (filename (esa-direx:post-local-path post)))
    (esal-lock (esa-direx:post-number post))
    (esa-direx::setup-post-buffer (if not-this-window
                                      (view-file-other-window filename)
                                    (view-file filename))
                                  post)))

(defmethod direx:generic-display-item ((item esa-direx:post-item) not-this-window)
  (let* ((post (direx:item-tree item))
         (filename (esa-direx:post-local-path post)))
    (esal-lock (esa-direx:post-number post))
    (esa-direx::setup-post-buffer (display-buffer (find-file-noselect filename))
                                  post)))

(defvar esa-direx::post nil)
(defun esa-direx::setup-post-buffer (buf post)
  (esa-direx--debug "Start setup post buffer : number[%s] buf[%s]"
                    (esa-direx:post-number post) buf)
  (with-current-buffer buf
    (esal-set-active-team (esa-direx:element-name (esa-direx:team-of post)) t)
    (set (make-local-variable 'esa-direx::post) post)
    (setq-local revert-buffer-function 'esa-direx:revert-current-post)
    (local-set-key [remap save-buffer] 'esa-direx:update-current-post)
    (setq-local kill-buffer-hook '(esa-direx:unlock-current-post))
    (current-buffer)))

(define-derived-mode esa-direx:mode direx:direx-mode "Direx Esa")

(defun esa-direx::get-buffer (team)
  (let ((bufnm (format "*Direx Esa %s*" (esa-direx:element-name team))))
    (or (direx:awhen (get-buffer bufnm)
          (when (buffer-live-p it) it))
        (with-current-buffer (get-buffer-create bufnm)
          (esa-direx--debug "Create buffer for %s" (esa-direx:element-name team))
          (esa-direx:mode)
          (esal-set-active-team (esa-direx:element-name team) t)
          (esal-stop-process)
          (setq-local revert-buffer-function 'direx:revert-buffer)
          (current-buffer)))))

(defmethod direx:make-buffer ((team esa-direx:team))
  (esa-direx::get-buffer team))

(defun esa-direx:ensure-buffer (team)
  (direx:ensure-buffer-for-root (esa-direx::make-team "mf")))

(defun esa-direx:revert-current-post (ignore-auto noconfirm)
  (if (not (y-or-n-p "[esa] sync current post?"))
      (message "[esa] quit.")
    ))

(defun esa-direx:update-current-post (&optional arg)
  (interactive "p")
  (save-buffer arg)
  (if (not (y-or-n-p "[esa] update current post?"))
      (message "[esa] quit.")
    (let* ((post esa-direx::post)
           (wip (y-or-n-p "[esa] turn back to wip?"))
           (ship (y-or-n-p "[esa] ship?"))
           (message (read-string "[esa] commit message: ")))
      (esal-update (esa-direx:post-number post)
                      :wip wip
                      :ship ship
                      :message message
                      :lock-keep-p t))))

(defun esa-direx:unlock-current-post ()
  (interactive)
  (let ((post esa-direx::post))
    (esal-unlock (esa-direx:post-number post))))


;;;###autoload
(defun esa-direx:open-team (team)
  (interactive
   (list (completing-read "Team: " (esal-teams) nil t nil '())))
  (switch-to-buffer-other-window (esa-direx:ensure-buffer team)))


(provide 'esa-direx)
