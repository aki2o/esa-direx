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


;;;;;;;;;;;
;; Trees

(defclass esa-direx:element (direx:tree)
  ((name :initarg :name :accessor esa-direx:element-name)
   (item :initarg :item :accessor esa-direx:element-item)))

(defclass esa-direx:team (esa-direx:element direx:node) ())

(defclass esa-direx:category (esa-direx:element direx:node) ())

(defclass esa-direx:post (esa-direx:element direx:leaf)
  ((title :initarg :title :accessor esa-direx:post-title)
   (number :initarg :number :accessor esa-direx:post-number)
   (wip :initarg :wip :accessor esa-direx:post-wip-p)
   (locked :initarg :locked :accessor esa-direx:post-locked-p)
   (tags :initarg :tags :accessor esa-direx:post-tags)
   (url :initarg :url :accessor esa-direx:post-url)
   (local-path :initarg :local-path :accessor esa-direx:post-local-path)
   (created-by :initarg :created-by :accessor esa-direx:post-created-by)
   (updated-by :initarg :updated-by :accessor esa-direx:post-updated-by)))

(defgeneric esa-direx:team-of (element))

(defmethod esa-direx:team-of ((team esa-direx:team))
  team)

(defmethod esa-direx:team-of ((element esa-direx:element))
  (esa-direx:team-of (direx:item-tree (direx:item-parent (esa-direx:element-item element)))))

(defgeneric esa-direx:fullname (element))

(defmethod esa-direx:fullname ((team esa-direx:team))
  "")

(defmethod esa-direx:fullname ((category esa-direx:category))
  (format "%s/%s"
          (esa-direx:fullname (direx:item-tree (direx:item-parent (esa-direx:element-item category))))
          (esa-direx:element-name category)))

(defmethod esa-direx:fullname ((post esa-direx:post))
  (format "%s/%s"
          (esa-direx:fullname (direx:item-tree (direx:item-parent (esa-direx:element-item post))))
          (esa-direx:post-title post)))

(defmethod direx:tree-equals ((x esa-direx:post) y)
  (or (eq x y)
      (and (cl-typep y 'esa-direx:post)
           (equal (esa-direx:fullname x) (esa-direx:fullname y)))))

(defmethod direx:node-children ((team esa-direx:team))
  (esa-direx::collect-children "/"))

(defmethod direx:node-children ((category esa-direx:category))
  (esa-direx::collect-children (esa-direx:fullname category)))

(defun esa-direx::collect-children (path)
  (esal-cd path)
  (cl-loop for line in (split-string (esal-ls nil) "\n")
           if (string-match "/\\'" line)
           collect (esa-direx::make-category
                    (replace-regexp-in-string "/\\'" "" line))
           else if (string-match "\\`\\([0-9]+\\):" line)
           collect (esa-direx::make-post (match-string-no-properties 1 line))))

(defun esa-direx::make-team (name)
  (make-instance 'esa-direx:team :name name))

(defun esa-direx::make-category (name)
  (make-instance 'esa-direx:category :name name))

(defun esa-direx::make-post (number)
  (let* ((json-string (esal-cat number
                                :json t
                                :indent nil))
         (post-stats (json-read-from-string json-string))
         (title (assoc-default 'name post-stats))
         (locked (not (eq (assoc-default 'locked post-stats) json-false)))
         (tags (assoc-default 'tags post-stats))
         (name (format "%s%s: %s%s"
                       (if locked "*" "")
                       number
                       title
                       (if (= (length tags) 0)
                           ""
                         (concat " " (mapconcat (lambda (s) (format "#%s" s)) tags " "))))))
    (make-instance 'esa-direx:post
                   :name name
                   :title title
                   :number number
                   :wip (not (eq (assoc-default 'wip post-stats) json-false))
                   :locked locked
                   :tags tags
                   :url (assoc-default 'url post-stats)
                   :local-path (assoc-default 'local_path post-stats)
                   :created-by (assoc-default 'created_by post-stats)
                   :updated-by (assoc-default 'updated_by post-stats))))


;;;;;;;;;;;;;;;;
;; Tree Items

(defclass esa-direx:team-item (direx:item) ())

(defmethod direx:make-item ((team esa-direx:team) parent)
  (let ((item (make-instance 'esa-direx:team-item
                             :tree team
                             :face 'esa-direx:team-face)))
    (setf (esa-direx:element-item team) item)
    (esa-direx--debug "make team : %s" (esa-direx:element-name team))
    item))

(defclass esa-direx:category-item (direx:item) ())

(defvar esa-direx:category-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'esa-direx:do-move-category)
    map))

(defmethod direx:make-item ((category esa-direx:category) parent)
  (let ((item (make-instance 'esa-direx:category-item
                             :tree category
                             :parent parent
                             :face 'esa-direx:category-face
                             :keymap esa-direx:category-keymap)))
    (setf (esa-direx:element-item category) item)
    (esa-direx--debug "make category : %s" (esa-direx:fullname category))
    item))

(defun esa-direx:do-move-category ()
  (interactive)
  (let* ((item (direx:item-at-point))
         (category (direx:item-tree item))
         (category-fullnames (split-string (esal-ls "/" :recursive t :category-only t) "\n"))
         (category-fullname (completing-read (format "Move %s to " (esa-direx:fullname category)) category-fullnames nil nil nil '())))
    (esal-mv (esa-direx:fullname category) category-fullname)
    (direx:refresh-whole-tree)
    (direx:move-to-item-name-part (direx:item-parent item))))

(defclass esa-direx:post-item (direx:item) ())

(defvar esa-direx:post-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'esa-direx:do-rename-post)
    (define-key map (kbd "S") 'esa-direx:do-ship-post)
    (define-key map (kbd "W") 'esa-direx:do-wip-post)
    (define-key map (kbd "T") 'esa-direx:do-set-tag)
    (define-key map (kbd "B") 'esa-direx:do-browse-post)
    map))

(defmethod direx:make-item ((post esa-direx:post) parent)
  (let* ((face (cond ((esa-direx:post-wip-p post)
                      'esa-direx:wip-post-face)
                     (t
                      'esa-direx:shipped-post-face)))
         (item (make-instance 'esa-direx:post-item
                             :tree post
                             :parent parent
                             :face face
                             :keymap esa-direx:post-keymap)))
    (setf (esa-direx:element-item post) item)
    (esa-direx--debug "make post : %s" (esa-direx:fullname post))
    item))

(defun esa-direx:do-rename-post ()
  (interactive)
  (let* ((item (direx:item-at-point))
         (post (direx:item-tree item))
         (name (read-string (format "Rename %s to " (esa-direx:post-title post)))))
    (esal-update (esa-direx:post-number post) :name name :without-body-p t :lock-keep-p t)
    (direx:item-refresh-parent item)
    (direx:move-to-item-name-part item)))

(defun esa-direx:do-ship-post ()
  (interactive)
  (let* ((item (direx:item-at-point))
         (post (direx:item-tree item)))
    (esal-update (esa-direx:post-number post) :ship t :without-body-p t :lock-keep-p t)
    (direx:item-refresh-parent item)
    (direx:move-to-item-name-part item)))

(defun esa-direx:do-wip-post ()
  (interactive)
  (let* ((item (direx:item-at-point))
         (post (direx:item-tree item)))
    (esal-update (esa-direx:post-number post) :wip t :without-body-p t :lock-keep-p t)
    (direx:item-refresh-parent item)
    (direx:move-to-item-name-part item)))

(defun esa-direx:do-set-tag ()
  (interactive)
  (let* ((item (direx:item-at-point))
         (post (direx:item-tree item))
         (tags '()))
    (esal-update (esa-direx:post-number post) :tags tags :without-body-p t :lock-keep-p t)
    (direx:item-refresh-parent item)
    (direx:move-to-item-name-part item)))

(defun esa-direx:do-browse-post ()
  (interactive)
  (let* ((item (direx:item-at-point))
         (post (direx:item-tree item)))
    (browse-url (esa-direx:post-url post))))

;; (defmethod direx:item-refresh (item esa-direx:category-item)
;;   )

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
  (if (not (y-or-n-p "[esa] Sync current post?"))
      (message "[esa] Quit.")
    (let* ((post esa-direx:post))
      (esal-sync (esa-direx:post-number post) :by-number t))))

(defun esa-direx:update-current-post (&optional arg)
  (interactive "p")
  (save-buffer arg)
  (if (not (y-or-n-p "[esa] Update current post?"))
      (message "[esa] Quit.")
    (let* ((post esa-direx::post)
           (message (read-string "[esa] Commit message: ")))
      (esal-update (esa-direx:post-number post)
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
