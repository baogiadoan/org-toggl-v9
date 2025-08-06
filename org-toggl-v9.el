;;; org-toggl-v9.el — Org‑mode integration with Toggl Track API v9  -*- lexical-binding: t; -*-
(require 'json)
(require 'url)

(defcustom toggl-auth-token ""
  "Your Toggl Track API token."
  :type 'string
  :group 'toggl)

(defvar toggl-workspace-id nil
  "Toggl Workspace ID — required for v9 API endpoints.")

(defvar toggl-current-time-entry nil
  "Current running time entry.")

(defvar toggl-default-project nil
  "Default project ID to use when none specified.")

(defconst toggl-api-base "https://api.track.toggl.com/api/v9/"
  "Base URL for Toggl Track API v9.")

(defun toggl-api-url (endpoint)
  (concat toggl-api-base endpoint))

(defun toggl-auth-header ()
  (cons "Authorization"
        (format "Basic %s" (base64-encode-string (concat toggl-auth-token ":api_token")))))

(defun toggl-request (method endpoint &optional data success error)
  "Make HTTP request using url-retrieve instead of request package."
  (let* ((url (toggl-api-url endpoint))
         (url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(cdr (toggl-auth-header)))
            ,@(when data '(("Content-Type" . "application/json")))))
         (url-request-data (when data (json-encode data))))
    
    (url-retrieve 
     url
     (lambda (status)
       (goto-char (point-min))
       (if (plist-get status :error)
           ;; Error case
           (when error
             (funcall error :error-thrown (plist-get status :error)))
         ;; Success case
         (progn
           ;; Skip HTTP headers
           (re-search-forward "^$" nil 'move)
           (forward-char)
           ;; Parse JSON response
           (condition-case err
               (let* ((json-response (buffer-substring-no-properties (point) (point-max)))
                      (parsed-data (json-read-from-string json-response)))
                 (when success
                   (funcall success :data parsed-data)))
             (error
              (when error
                (funcall error :error-thrown err))))))
       ;; Clean up buffer
       (kill-buffer (current-buffer)))
     nil t)))

(defvar toggl-projects nil)

(defun toggl-fetch-workspaces (&optional callback)
  "Retrieve workspaces from the v9 workspaces endpoint."
  (toggl-request
   "GET" "workspaces"
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      ;; Convert vector to list if needed
      (let ((ws-list (if (vectorp data) (append data nil) data)))
        (if callback
            (funcall callback ws-list)
          (message "Workspaces: %s"
                   (mapcar (lambda (w)
                             (cons (alist-get 'name w)
                                   (alist-get 'id w)))
                           ws-list))))))
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                  (message "Failed to fetch workspaces: %s" error-thrown)))))

(defun toggl-select-workspace ()
  "Prompt to select the workspace to use."
  (interactive)
  (toggl-fetch-workspaces
   (lambda (ws-list)
     (let* ((choices (mapcar (lambda (w)
                               (cons (alist-get 'name w) (alist-get 'id w)))
                             ws-list))
            (sel (completing-read "Workspace: " choices nil t)))
       (setq toggl-workspace-id (assoc-default sel choices))
       (message "Using workspace ID %s (\"%s\")" toggl-workspace-id sel)))))

(defun toggl-get-default-workspace ()
  "Get and set the default workspace ID from user profile."
  (interactive)
  (toggl-request
   "GET" "me"
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let ((default-ws-id (alist-get 'default_workspace_id data)))
        (setq toggl-workspace-id default-ws-id)
        (message "Using default workspace ID: %s" default-ws-id))))
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                  (message "Failed to get default workspace: %s" error-thrown)))))

(defun toggl-fetch-projects (&optional callback)
  "Fetch projects for toggl-workspace-id."
  (unless toggl-workspace-id
    (user-error "Workspace ID not set. Run `toggl-select-workspace` first."))
  (toggl-request
   "GET" (format "workspaces/%s/projects" toggl-workspace-id)
   nil
   (cl-function (lambda (&key data &allow-other-keys)
                  ;; Convert vector to list if needed
                  (let ((projects-list (if (vectorp data) (append data nil) data)))
                    (setq toggl-projects
                          (mapcar (lambda (p)
                                    (cons (alist-get 'name p) (alist-get 'id p)))
                                  projects-list))
                    (when callback (funcall callback toggl-projects))
                    (message "Fetched %d projects" (length toggl-projects)))))
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                  (message "Failed to fetch projects: %s" error-thrown)))))

(defun toggl-get-pid (project-name)
  (cdr (assoc project-name toggl-projects)))

(defun toggl-start-entry (description pid &optional callback)
  "Start a Toggl time entry."
  (unless (and toggl-workspace-id pid)
    (user-error "Workspace ID or project ID missing"))
  (let ((payload `(("description" . ,description)
                   ("start" . ,(format-time-string "%FT%TZ" nil t))
                   ("duration" . -1)
                   ("project_id" . ,pid)
                   ("workspace_id" . ,toggl-workspace-id)
                   ("created_with" . "org-toggl-v9"))))
    (toggl-request
     "POST"
     (format "workspaces/%s/time_entries" toggl-workspace-id)
     payload
     (cl-function (lambda (&key data &allow-other-keys)
                    (setq toggl-current-time-entry data)
                    (when callback (funcall callback data))
                    (message "Started Toggl entry \"%s\"." description)))
     (cl-function (lambda (&key error-thrown &allow-other-keys)
                    (message "Starting entry failed: %s" error-thrown))))))

(defun toggl-stop-entry (&optional callback)
  "Stop the running Toggl time entry."
  (interactive)
  (let ((e toggl-current-time-entry))
    (unless e (user-error "No active time entry"))
    (let ((eid (alist-get 'id e)))
      (toggl-request
       "PATCH"
       (format "workspaces/%s/time_entries/%s/stop"
               toggl-workspace-id eid)
       nil
       (cl-function (lambda (&key data &allow-other-keys)
                      (setq toggl-current-time-entry nil)
                      (when callback (funcall callback data))
                      (message "Stopped entry.")))
       (cl-function (lambda (&key error-thrown &allow-other-keys)
                      (message "Stopping entry failed: %s" error-thrown)))))))

;; Org toggling

(defun org-toggl-clock-in ()
  (let* ((heading (org-get-heading t t t t))
         (proj (org-entry-get (point) "toggl-project" t))
         (pid (or (toggl-get-pid proj) toggl-default-project)))
    (when pid (toggl-start-entry heading pid))))

(defun org-toggl-clock-out ()
  (toggl-stop-entry))

;;;###autoload
(define-minor-mode org-toggl-v9-mode
  "Toggle Org‑mode Toggl integration (v9-compatible)."
  :global t
  :lighter " Toggl9"
  (if org-toggl-v9-mode
      (progn
        (add-hook 'org-clock-in-hook #'org-toggl-clock-in)
        (add-hook 'org-clock-out-hook #'org-toggl-clock-out))
    (remove-hook 'org-clock-in-hook #'org-toggl-clock-in)
    (remove-hook 'org-clock-out-hook #'org-toggl-clock-out)))

(provide 'org-toggl-v9)
