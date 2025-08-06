# org-toggl-v9

Seamlessly integrate Org-mode time tracking with Toggl Track API v9. Automatically sync your Org-mode clock-in/clock-out with Toggl time entries.

## Features

- 🔄 **Automatic Sync**: Clock in/out in Org-mode automatically starts/stops Toggl entries
- 🎯 **Project Management**: Assign Toggl projects to specific tasks
- 🤖 **Interactive Setup**: Smart prompts for workspace and project selection on first use
- 💾 **Memory**: Remembers your project choices for each task
- 🛠 **Flexible**: Works with or without projects
- 🔒 **Reliable**: Uses Emacs built-in HTTP (no external dependencies)

## Requirements

- Emacs with Org-mode
- Toggl Track account and API token
- Internet connection

## Installation

### Manual Installation

1. Download `org-toggl-v9.el` to your Emacs configuration directory
2. Add to your Emacs config:

```elisp
(load-path "/path/to/org-toggl-v9.el")
(require 'org-toggl-v9)
```

### Doom Emacs

Add to your `packages.el`:

```elisp
(package! org-toggl-v9 :recipe (:local-repo "path/to/org-toggl-v9"))
```

## Setup

### 1. Get Your Toggl API Token

1. Visit [Toggl Track Profile Settings](https://track.toggl.com/profile)
2. Scroll down to "API Token"
3. Copy your API token

### 2. Configure Your API Token

Add to your Emacs configuration:

```elisp
(setq toggl-auth-token "your-api-token-here")
```

### 3. Enable Integration

```elisp
M-x org-toggl-v9-mode
```

You should see `" Toggl9"` in your mode line when active.

## Usage

### Automatic Mode (Recommended)

Just start using Org-mode clocking normally! The integration will guide you through setup:

1. **Go to any Org heading**:
   ```org
   * TODO Write project documentation
   ```

2. **Clock in**: `C-c C-x C-i`
   - First time: Prompts for workspace and project selection
   - Automatically saves your choice to the task
   - Starts both Org clock and Toggl entry

3. **Clock out**: `C-c C-x C-o`
   - Stops both Org clock and Toggl entry

4. **Next time**: Just clock in/out - no prompts needed!

### Manual Project Assignment

You can also manually assign projects to tasks:

```org
* TODO Fix authentication bug
:PROPERTIES:
:toggl-project: Website Development
:END:

* TODO Client presentation  
:PROPERTIES:
:toggl-project: Client Work
:END:

* TODO Personal learning
```

### Example Workflow

**First time clocking into a task:**
```
Clock in → "Select workspace: My Workspace"
        → "Select project for 'Fix login bug': Website Development" 
        → "Saved project 'Website Development' to task properties"
        → "Started Toggl entry 'Fix login bug'"
```

**Subsequent times:**
```
Clock in → "Started Toggl entry 'Fix login bug'" (instant!)
```

## Available Commands

| Command | Description |
|---------|-------------|
| `toggl-select-workspace` | Choose your Toggl workspace |
| `toggl-fetch-projects` | Refresh project list from Toggl |
| `toggl-get-default-workspace` | Use your default workspace |
| `org-toggl-change-project` | Change project for current task |
| `toggl-stop-entry` | Manually stop current Toggl entry |
| `toggl-ensure-setup` | Check configuration status |
| `org-toggl-v9-mode` | Enable/disable integration |

## Configuration Options

### Default Project

Set a fallback project for tasks without explicit project assignment:

```elisp
(setq toggl-default-project 12345678)  ; Use actual project ID
```

### Workspace Selection

You can set up multiple workspace workflows:

```elisp
;; Automatically use default workspace
M-x toggl-get-default-workspace

;; Or choose from available workspaces  
M-x toggl-select-workspace
```

## Troubleshooting

### "No workspace selected" Error

Run: `M-x toggl-select-workspace`

### "No projects loaded" Message

Run: `M-x toggl-fetch-projects`

### Projects Not Showing Up

1. Check your workspace has projects in Toggl Track web app
2. Run: `M-x toggl-fetch-projects` to refresh

### API Connection Issues

The package uses Emacs built-in HTTP functionality. If you experience connection issues:

1. Verify your API token is correct
2. Check your internet connection
3. Ensure Toggl Track is accessible from your network

### Check Configuration

Run: `M-x toggl-ensure-setup` to see current status

## How It Works

1. **Clock In**: When you clock into an Org task
   - Checks for existing `toggl-project` property
   - If found: Uses that project
   - If not found: Prompts for project selection and saves choice
   - Creates Toggl time entry with task heading as description

2. **Clock Out**: When you clock out
   - Stops the corresponding Toggl time entry

3. **Project Memory**: Saves project selections as Org properties:
   ```org
   * TODO My Task
   :PROPERTIES:
   :toggl-project: My Project Name
   :END:
   ```

## API Compatibility

This package uses Toggl Track API v9. Key endpoints:

- `GET /api/v9/workspaces` - Fetch workspaces
- `GET /api/v9/workspaces/{workspace_id}/projects` - Fetch projects  
- `POST /api/v9/workspaces/{workspace_id}/time_entries` - Start time entry
- `PATCH /api/v9/workspaces/{workspace_id}/time_entries/{entry_id}/stop` - Stop time entry

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

### Development

The package is designed to be simple and reliable:

- Uses only built-in Emacs functionality (no external dependencies)
- Handles API responses as JSON with proper error handling
- Maintains backward compatibility with existing Org workflows

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Changelog

### v1.0.0
- Initial release with Toggl Track API v9 support
- Interactive workspace and project selection
- Automatic Org-mode integration
- Built-in HTTP functionality (no request package dependency)

## Acknowledgments

- Toggl Track for their excellent time tracking service and API
- Org-mode community for the fantastic time tracking foundation
- Emacs community for the powerful extensibility platform

---

**Happy time tracking!** 🕐✨