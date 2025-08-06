;;; org-toggl-v9.el — Org‑mode integration with Toggl Track API v9  -*- lexical-binding: t; -*-
(require 'json)
(require 'request)

(defcustom toggl-auth-token ""
  "Your Toggl Track API token."
  :type 'string
  :group 'toggl)

(defvar toggl-workspace-id nil
  "Toggl Workspace ID — required for v9 API endpoints.")

(defconst toggl-api-base "https://api.track.toggl.com/api/v9/"
  "Base URL for Toggl Track API v9.")

(defun toggl-api-url (endpoint)
  (concat toggl-api-base endpoint))

(defun toggl-auth-header ()
  (cons "Authorization"
        (format "Basic %s" (base64-encode-string (concat toggl-auth-token ":api_token")))))

(defun toggl-request (method endpoint &optional data success error)
  (request (togg­l-api-url endpoint)
           :type method
           :data (when data (json-encode data))
           :headers (append (list (togg­l-auth-header))
                            (when data '(("Content-Type" . "application/json"))))
           :parser #'json-read
           :success success
           :error error
           :sync nil))

(defvar toggl-projects nil)

(defun toggl-fetch-workspaces (&optional callback)
  "Retrieve workspaces and optionally CALL CALLBACK with the list."
  (toggl-request
   "GET" "me"
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let ((ws (alist-get 'workspaces data)))
        (if callback
            (funcall callback ws)
          (message "Workspaces: %s"
                   (mapcar (lambda (w)
                             (cons (alist-get 'name w)
                                   (alist-get 'id w)))
                           ws))))))
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

(defun toggl-fetch-projects (&optional callback)
  "Fetch projects for toggl-workspace-id."
  (unless toggl-workspace-id
    (user-error "Workspace ID not set. Run `toggl-select-workspace` first."))
  (toggl-request
   "GET" (format "workspaces/%s/projects" toggl-workspace-id)
   nil
   (cl-function (lambda (&key data &allow-other-keys)
                  (setq toggl-projects
                        (mapcar (lambda (p)
                                  (cons (alist-get 'name p) (alist-get 'id p)))
                                data))
                  (when callback (funcall callback toggl-projects))
                  (message "Fetched %d projects" (length toggl-projects))))
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
