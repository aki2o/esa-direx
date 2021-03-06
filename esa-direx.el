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

(defcustom esa-direx:preferred-selection-method 'helm
  "Symbol for a preferred feature used for various selection flow."
  :type '(choice (const helm)
                 (const anything)
                 (const default)
                 (const nil))
  :group 'esa-direx)

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


(defvar esa-direx::post nil)
(defvar esa-direx::tags-hash (make-hash-table :test 'equal))


(defsubst esa-direx::regist-tags (teamnm tags)
  (let ((curr-tags (gethash teamnm esa-direx::tags-hash)))
    (loop for tag in tags
          do (pushnew tag curr-tags :test 'equal))
    (puthash teamnm curr-tags esa-direx::tags-hash)))

(defsubst esa-direx::tags-of (teamnm)
  (gethash teamnm esa-direx::tags-hash))

(defun* esa-direx::select-something (&key description candidates method multiple require default)
  (esa-direx--debug* "start select something. description[%s] method[%s] multiple[%s] require[%s] default[%s]"
                     description method multiple require default)
  (let* ((pref (or method esa-direx:preferred-selection-method))
         (mtd (or (when (and (eq pref 'helm) (featurep 'helm))         'helm)
                  (when (and (eq pref 'anything) (featurep 'anything)) 'anything)
                  (when (eq pref 'default)                             'completing-read)
                  (when (featurep 'helm)                               'helm)
                  (when (featurep 'anything)                           'anything)
                  'completing-read))
         (src `((name . ,description)
                (candidates . ,candidates)
                (candidate-number-limit . 999)
                (action . (lambda (cand)
                            (or (when (not ,multiple)      cand)
                                (when (eq ',mtd 'helm)     (helm-marked-candidates))
                                (when (eq ',mtd 'anything) (anything-marked-candidates))
                                (list cand))))
                (migemo)))
         (args (case mtd
                 (helm            `(:sources ,src))
                 (anything        `(:sources ,src))
                 (completing-read `(,(concat description ": ") ,candidates nil t nil '() ,default))))
         (selected (loop while t
                         for selected = (apply mtd args)
                         if selected return selected
                         if (not require) return selected
                         do (progn (message "[esa] Have to select something")
                                   (sleep-for 2)))))
    (esa-direx--info "finished select something : %s" selected)
    (if (and multiple
             (not (listp selected)))
        (progn (esa-direx--debug "convert result of select something to list")
               (list selected))
      selected)))


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

(defmethod direx:tree-equals ((x esa-direx:team) y)
  (or (eq x y)
      (and (cl-typep y 'esa-direx:team)
           (equal (esa-direx:element-name x) (esa-direx:element-name y)))))

(defmethod direx:tree-equals ((x esa-direx:category) y)
  (or (eq x y)
      (and (cl-typep y 'esa-direx:category)
           (equal (esa-direx:fullname x) (esa-direx:fullname y)))))

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

(defun esa-direx::make-new-post (name)
  (make-instance 'esa-direx:post
                 :name (format "NEW: %s" name)
                 :title name
                 :wip t))


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
    (define-key map (kbd "+") 'esa-direx:do-make-category)
    (define-key map (kbd "N") 'esa-direx:do-new-post)
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
         (element (direx:item-tree item))
         (element-fullname (esa-direx:fullname element))
         (category-fullnames (split-string (esal-ls "/" :recursive t :category-only t) "\n"))
         (category-fullname (esa-direx::select-something :description (format "Move %s to " element-fullname)
                                                         :candidates category-fullnames)))
    (esal-mv element-fullname category-fullname)
    (direx:refresh-whole-tree)
    (direx:move-to-item-name-part (direx:item-parent item))))

(defun esa-direx:do-make-category ()
  (interactive)
  (let* ((parent (direx:item-at-point))
         (name (if (not (esa-direx:category-item-p parent))
                   (error "This node is not category!")
                 (read-string "Category name: ")))
         (category (cond ((= (length name) 0)
                          (error "Require name!"))
                         ((loop for i in (direx:item-children parent)
                                if (string= (direx:item-name i) name) return t
                                finally return nil)
                          (error "Already exists %s!" name))
                         (t
                          (esa-direx::make-category name))))
         (item (direx:make-item category parent)))
    (save-excursion
      (goto-char (overlay-end (direx:item-overlay parent)))
      (direx:item-insert item))
    (direx:move-to-item-name-part item)))

(defclass esa-direx:post-item (direx:item) ())

(defvar esa-direx:post-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'esa-direx:do-move-category)
    (define-key map (kbd "N") 'esa-direx:do-new-post)
    (define-key map (kbd "M") 'esa-direx:do-rename-post)
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
    (esa-direx::regist-tags
     (esa-direx:element-name (esa-direx:team-of post))
     (esa-direx:post-tags post))
    (esa-direx--debug "make post : %s" (esa-direx:fullname post))
    item))

(defun esa-direx:do-new-post ()
  (interactive)
  (let* ((item (direx:item-at-point))
         (parent (if (esa-direx:category-item-p item)
                     item
                   (direx:item-parent item)))
         (name (if (not (esa-direx:category-item-p parent))
                   (error "Not found category for new post!")
                 (read-string "Post name: ")))
         (post (cond ((= (length name) 0)
                      (error "Require name!"))
                     ((loop for i in (direx:item-children parent)
                            if (string= (direx:item-name i) name) return t
                            finally return nil)
                      (error "Already exists %s!" name))
                     (t
                      (esa-direx::make-new-post name))))
         (new-item (direx:make-item post parent)))
    (save-excursion
      (goto-char (overlay-end (direx:item-overlay parent)))
      (direx:item-insert new-item))
    (direx:move-to-item-name-part new-item)
    (esa-direx::ensure-buffer post 'find-file-other-window)))

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
         (team (esa-direx:team-of post))
         (tags (esa-direx::select-something :description "Tags: "
                                            :candidates (esa-direx::tags-of (esa-direx:element-name team))
                                            :multiple t
                                            :default (esa-direx:post-tags post))))
    (esal-update (esa-direx:post-number post) :tags tags :without-body-p t :lock-keep-p t)
    (direx:item-refresh-parent item)
    (direx:move-to-item-name-part item)))

(defun esa-direx:do-browse-post ()
  (interactive)
  (let* ((item (direx:item-at-point))
         (post (direx:item-tree item))
         (url (esa-direx:post-url post)))
    (if (not url)
        (error "This post is not yet uploaded!")
      (browse-url url))))

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
  (esa-direx::ensure-buffer
   (direx:item-tree item)
   (if not-this-window 'find-file-other-window 'find-file)))

(defmethod direx:generic-view-item ((item esa-direx:post-item) not-this-window)
  (esa-direx::ensure-buffer
   (direx:item-tree item)
   (if not-this-window 'view-file-other-window 'view-file)))

(defmethod direx:generic-display-item ((item esa-direx:post-item) not-this-window)
  (display-buffer (esa-direx::ensure-buffer (direx:item-tree item) 'find-file-noselect)))

(defun esa-direx::ensure-buffer (post open-func)
  (let* ((filepath (esa-direx:post-local-path post))
         (number (esa-direx:post-number post))
         (buf (if filepath
                  (funcall open-func filepath)
                (get-buffer-create (esa-direx::buffer-name post)))))
    (esa-direx::setup-post-buffer buf post)
    (when number
      (esal-lock number))))

(defun esa-direx::buffer-name (post)
  (format "[esa] %s: %s"
          (or (esa-direx:post-number post) "NEW")
          (esa-direx:post-title post)))

(defun esa-direx::setup-post-buffer (buf post)
  (esa-direx--debug "Start setup post buffer : number[%s] buf[%s]"
                    (esa-direx:post-number post) buf)
  (with-current-buffer buf
    (esal-set-active-team (esa-direx:element-name (esa-direx:team-of post)) t)
    (set (make-local-variable 'esa-direx::post) post)
    (setq-local revert-buffer-function 'esa-direx:revert-current-post)
    (local-set-key [remap save-buffer] 'esa-direx:update-current-post)
    (setq-local kill-buffer-hook '(esa-direx:unlock-current-post))
    (rename-buffer (esa-direx::buffer-name post))
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
  (direx:ensure-buffer-for-root (esa-direx::make-team team)))

(defun esa-direx:revert-current-post (ignore-auto noconfirm)
  (interactive)
  (let* ((post esa-direx::post)
         (number (esa-direx:post-number post))
         (revert-buffer-function nil))
    (revert-buffer)
    (when post
      (esa-direx::setup-post-buffer (current-buffer) post)
      (when number
        (if (not (y-or-n-p "[esa] Sync current post?"))
            (message "[esa] Quit.")
          (esal-sync (list number) :by-number t))))))

(defun esa-direx:update-current-post (&optional arg)
  (interactive "p")
  (save-buffer arg)
  (when esa-direx::post
    (if (not (y-or-n-p "[esa] Update current post?"))
        (message "[esa] Quit.")
      (let* ((post esa-direx::post)
             (message (read-string "[esa] Commit message: ")))
        (esal-update (esa-direx:post-number post)
                     :message message
                     :lock-keep-p t)))))

(defun esa-direx:unlock-current-post ()
  (interactive)
  (if (not esa-direx::post)
      (message "[esa] Current buffer has not post.")
    (let ((post esa-direx::post))
      (esal-unlock (esa-direx:post-number post)))))

(defun esa-direx:browse-current-post ()
  (interactive)
  (if (not esa-direx::post)
      (message "[esa] Current buffer has not post.")
    (let ((post esa-direx::post))
      (browse-url (esa-direx:post-url post)))))

(defun esa-direx:jump-to-current-category ()
  (interactive)
  (if (not esa-direx::post)
      (message "[esa] Current buffer has not post.")
    (let* ((post esa-direx::post)
           (buf (esa-direx:ensure-buffer (esa-direx:element-name (esa-direx:team-of post)))))
    (with-current-buffer buf
      (direx:goto-item-for-tree-1 post))
    (switch-to-buffer-other-window buf))))


;;;###autoload
(defun esa-direx:open-team (team)
  (interactive
   (list (completing-read "Team: " (esal-teams) nil t nil '())))
  (switch-to-buffer-other-window (esa-direx:ensure-buffer team)))


(defadvice direx:jump-to-directory-other-window (around esa-direx:open-team activate)
  (if esa-direx::post
      (esa-direx:jump-to-current-category)
    ad-do-it))

(defadvice direx-project:jump-to-project-root-other-window (around esa-direx:open-team activate)
  (if esa-direx::post
      (esa-direx:jump-to-current-category)
    ad-do-it))


(provide 'esa-direx)
