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
  "Prompt to select the workspace to use and automatically fetch projects."
  (interactive)
  (toggl-fetch-workspaces
   (lambda (ws-list)
     (let* ((choices (mapcar (lambda (w)
                               (cons (alist-get 'name w) (alist-get 'id w)))
                             ws-list))
            (sel (completing-read "Workspace: " choices nil t)))
       (setq toggl-workspace-id (assoc-default sel choices))
       (message "Selected workspace: %s" sel)
       ;; Automatically fetch projects after workspace selection
       (message "Fetching projects...")
       (toggl-fetch-projects
        (lambda (projects)
          (message "Ready! Workspace and %d projects loaded." (length projects))))))))

(defun toggl-get-default-workspace ()
  "Get and set the default workspace ID from user profile and fetch projects."
  (interactive)
  (toggl-request
   "GET" "me"
   nil
   (cl-function
    (lambda (&key data &allow-other-keys)
      (let ((default-ws-id (alist-get 'default_workspace_id data)))
        (setq toggl-workspace-id default-ws-id)
        (message "Using default workspace ID: %s" default-ws-id)
        ;; Automatically fetch projects after workspace is set
        (message "Fetching projects...")
        (toggl-fetch-projects
         (lambda (projects)
           (message "Ready! Default workspace and %d projects loaded." (length projects)))))))
   (cl-function (lambda (&key error-thrown &allow-other-keys)
                  (message "Failed to get default workspace: %s" error-thrown)))))

(defun toggl-fetch-projects (&optional callback)
  "Fetch projects for toggl-workspace-id."
  (interactive)
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
  (unless toggl-workspace-id
    (user-error "Workspace ID missing"))
  (let ((payload `(("description" . ,description)
                   ("start" . ,(format-time-string "%FT%TZ" nil t))
                   ("duration" . -1)
                   ,@(when pid `(("project_id" . ,pid)))
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

;; Org toggling with interactive setup
(defun org-toggl-clock-in ()
  (let* ((heading (org-get-heading t t t t))
         (existing-proj (org-entry-get (point) "toggl-project" t))
         (original-buffer (current-buffer))
         (original-point (point))
         (pid nil))
    
    (cond
     ;; Case 1: Task already has a project property
     (existing-proj
      (setq pid (toggl-get-pid existing-proj))
      (if pid
          (toggl-start-entry heading pid)
        (message "Project '%s' not found. Refreshing projects..." existing-proj)
        (toggl-fetch-projects
         (lambda (projects)
           (with-current-buffer original-buffer
             (save-excursion
               (goto-char original-point)
               (setq pid (toggl-get-pid existing-proj))
               (if pid
                   (toggl-start-entry heading pid)
                 (message "Project '%s' still not found. Please check project name." existing-proj))))))))
     
     ;; Case 2: No project property - ask user
     (t
      (if (not toggl-workspace-id)
          ;; No workspace - need to set that up first
          (progn
            (message "Setting up Toggl integration...")
            (call-interactively 'toggl-select-workspace)
            (when toggl-workspace-id
              (org-toggl-clock-in)))
        ;; Have workspace, get project choice
        (if (not toggl-projects)
            ;; Fetch projects first
            (progn
              (message "Fetching projects for selection...")
              (toggl-fetch-projects
               (lambda (projects)
                 (with-current-buffer original-buffer
                   (save-excursion
                     (goto-char original-point)
                     (let* ((choices (append 
                                      '(("(No Project)" . nil))
                                      projects))
                            (selection (completing-read 
                                       (format "Select Toggl project for '%s': " 
                                               (substring heading 0 (min 40 (length heading))))
                                       choices nil t))
                            (selected-project-name (if (string= selection "(No Project)") nil selection))
                            (selected-pid (if selected-project-name 
                                              (assoc-default selection choices) 
                                              nil)))
                       ;; Save the selection as org property
                       (when selected-project-name
                         (org-entry-put (point) "toggl-project" selected-project-name)
                         (message "Saved project '%s' to task properties" selected-project-name))
                       ;; Start the timer
                       (toggl-start-entry heading selected-pid)))))))
          ;; Have projects, show selection immediately
          (let* ((choices (append 
                           '(("(No Project)" . nil))
                           toggl-projects))
                 (selection (completing-read 
                            (format "Select Toggl project for '%s': " 
                                    (substring heading 0 (min 40 (length heading))))
                            choices nil t))
                 (selected-project-name (if (string= selection "(No Project)") nil selection))
                 (selected-pid (if selected-project-name 
                                   (assoc-default selection choices) 
                                   nil)))
            ;; Save the selection as org property
            (when selected-project-name
              (org-entry-put (point) "toggl-project" selected-project-name)
              (message "Saved project '%s' to task properties" selected-project-name))
            ;; Start the timer
            (toggl-start-entry heading selected-pid))))))))

(defun org-toggl-clock-out ()
  (toggl-stop-entry))

(defun toggl-setup-on-startup ()
  "Set up Toggl workspace and projects on Emacs startup."
  (interactive)
  (when (and toggl-auth-token (not (string-empty-p toggl-auth-token)))
    (message "Setting up Toggl integration...")
    (if toggl-workspace-id
        ;; Already have workspace, just fetch projects
        (progn
          (message "Loading projects for existing workspace...")
          (toggl-fetch-projects
           (lambda (projects)
             (message "Toggl ready: %d projects loaded" (length projects)))))
      ;; No workspace set, get default or prompt
      (toggl-request
       "GET" "me"
       nil
       (cl-function
        (lambda (&key data &allow-other-keys)
          (let ((default-ws-id (alist-get 'default_workspace_id data)))
            (if default-ws-id
                (progn
                  (setq toggl-workspace-id default-ws-id)
                  (message "Using default workspace")
                  (toggl-fetch-projects
                   (lambda (projects)
                     (message "Toggl ready: %d projects loaded" (length projects)))))
              ;; No default workspace, need to select
              (message "Multiple workspaces found. Run M-x toggl-select-workspace to choose.")))))
       (cl-function (lambda (&key error-thrown &allow-other-keys)
                      (message "Toggl setup failed: %s" error-thrown)))))))

(defun toggl-auto-setup ()
  "Automatically set up Toggl if auth token is configured."
  (when (and toggl-auth-token 
             (not (string-empty-p toggl-auth-token))
             (not toggl-workspace-id))
    (run-with-timer 2 nil #'toggl-setup-on-startup)))

(defun org-toggl-change-project ()
  "Change the Toggl project for the current task."
  (interactive)
  (unless toggl-workspace-id
    (user-error "No workspace selected. Run M-x toggl-select-workspace first"))
    
  (let* ((original-buffer (current-buffer))
         (original-point (point))
         (heading (org-get-heading t t t t))
         (current-proj (org-entry-get (point) "toggl-project" t)))
    
    (if (not toggl-projects)
        ;; Need to fetch projects first
        (progn
          (message "Fetching projects...")
          (toggl-fetch-projects
           (lambda (projects)
             (with-current-buffer original-buffer
               (save-excursion
                 (goto-char original-point)
                 (let* ((choices (append '(("(No Project)" . nil) ("(Remove Project Property)" . "REMOVE")) 
                                        projects))
                        (selection (completing-read 
                                   (format "Change project for '%s'%s: " 
                                           (substring heading 0 (min 40 (length heading)))
                                           (if current-proj (format " (currently: %s)" current-proj) ""))
                                   choices nil t))
                        (selected-project-name (if (string= selection "(No Project)") nil 
                                                 (if (string= selection "(Remove Project Property)") "REMOVE"
                                                   selection))))
                   (cond
                    ((string= selected-project-name "REMOVE")
                     (org-entry-delete (point) "toggl-project")
                     (message "Removed toggl-project property from task"))
                    (selected-project-name
                     (org-entry-put (point) "toggl-project" selected-project-name)
                     (message "Changed project to '%s'" selected-project-name))
                    (t
                     (org-entry-delete (point) "toggl-project")
                     (message "Set to no project")))))))))
      ;; Have projects, proceed immediately
      (let* ((choices (append '(("(No Project)" . nil) ("(Remove Project Property)" . "REMOVE")) 
                             toggl-projects))
             (selection (completing-read 
                        (format "Change project for '%s'%s: " 
                                (substring heading 0 (min 40 (length heading)))
                                (if current-proj (format " (currently: %s)" current-proj) ""))
                        choices nil t))
             (selected-project-name (if (string= selection "(No Project)") nil 
                                      (if (string= selection "(Remove Project Property)") "REMOVE"
                                        selection))))
        
        (cond
         ((string= selected-project-name "REMOVE")
          (org-entry-delete (point) "toggl-project")
          (message "Removed toggl-project property from task"))
         (selected-project-name
          (org-entry-put (point) "toggl-project" selected-project-name)
          (message "Changed project to '%s'" selected-project-name))
         (t
          (org-entry-delete (point) "toggl-project")
          (message "Set to no project")))))))

;;;###autoload
(define-minor-mode org-toggl-v9-mode
  "Toggle Org‑mode Toggl integration (v9-compatible)."
  :global t
  :lighter " Toggl9"
  (if org-toggl-v9-mode
      (progn
        (add-hook 'org-clock-in-hook #'org-toggl-clock-in)
        (add-hook 'org-clock-out-hook #'org-toggl-clock-out)
        ;; Auto-setup on mode activation
        (toggl-auto-setup))
    (remove-hook 'org-clock-in-hook #'org-toggl-clock-in)
    (remove-hook 'org-clock-out-hook #'org-toggl-clock-out)))

(defun toggl-ensure-setup ()
  "Check current Toggl setup status."
  (interactive)
  (cond
   ((not toggl-auth-token)
    (message "No API token configured. Set toggl-auth-token."))
   ((string-empty-p toggl-auth-token)
    (message "Empty API token. Set toggl-auth-token."))
   ((not toggl-workspace-id)
    (message "No workspace selected. Run M-x toggl-select-workspace"))
   ((not toggl-projects)
    (message "No projects loaded. Run M-x toggl-fetch-projects"))
   (t 
    (message "Toggl ready: workspace %s, %d projects loaded" 
             toggl-workspace-id (length toggl-projects)))))

(provide 'org-toggl-v9)
