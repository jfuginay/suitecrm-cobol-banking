#!/bin/bash

# Claude Code Army Setup for SuiteCRM COBOL Project
# Based on: https://steipete.me/posts/command-your-claude-code-army-reloaded

set -e

echo "🤖 Setting up Claude Code Army for SuiteCRM COBOL Project..."

# Check if vibetunnel is installed
if ! command -v vt &> /dev/null; then
    echo "📦 Installing VibeTunnel..."
    curl -s https://api.github.com/repos/steipete/vibetunnel/releases/latest | grep "browser_download_url.*darwin" | cut -d '"' -f 4 | xargs curl -L -o /tmp/vt
    chmod +x /tmp/vt
    sudo mv /tmp/vt /usr/local/bin/vt
    echo "✅ VibeTunnel installed!"
fi

# Create project-specific Claude configurations
CLAUDE_DIR="$HOME/.claude"
mkdir -p "$CLAUDE_DIR"

# Add to global CLAUDE.md if it doesn't exist or append project-specific config
CLAUDE_MD="$CLAUDE_DIR/CLAUDE.md"

cat << 'EOF' >> "$CLAUDE_MD"

# SuiteCRM COBOL Army Configuration

## Project Context
Working on SuiteCRM + COBOL integration for regional banks
- Repository: /Users/jfuginay/Documents/dev/suitecrm-cobol
- Tech Stack: PHP 8.4, SuiteCRM, COBOL, Node.js, Docker
- Goal: Modernizing legacy banking systems with CRM integration

## VibeTunnel Usage
Always use `vt title "descriptive context"` to update session titles:
- When starting new features
- When debugging issues
- When switching between modules
- During long operations

## Common Tasks
1. **Frontend Work**: SuiteCRM PHP modules, dashlets, UI components
2. **Backend Work**: COBOL services, API integration, data processing
3. **Testing**: Unit tests, integration tests, manual testing
4. **DevOps**: Docker setup, deployment, configuration
5. **Documentation**: README updates, architecture docs, guides

EOF

# Create session startup scripts
mkdir -p "$HOME/.claude-sessions"

# Frontend Development Session
cat << 'EOF' > "$HOME/.claude-sessions/frontend.sh"
#!/bin/bash
cd /Users/jfuginay/Documents/dev/suitecrm-cobol
vt title "SuiteCRM Frontend Dev - PHP/Dashlets"
claude code --resume
EOF

# Backend Development Session  
cat << 'EOF' > "$HOME/.claude-sessions/backend.sh"
#!/bin/bash
cd /Users/jfuginay/Documents/dev/suitecrm-cobol
vt title "COBOL Backend Dev - APIs/Services"
claude code --resume
EOF

# Testing Session
cat << 'EOF' > "$HOME/.claude-sessions/testing.sh"
#!/bin/bash
cd /Users/jfuginay/Documents/dev/suitecrm-cobol
vt title "SuiteCRM Testing - Unit/Integration"
claude code --resume
EOF

# DevOps Session
cat << 'EOF' > "$HOME/.claude-sessions/devops.sh"
#!/bin/bash
cd /Users/jfuginay/Documents/dev/suitecrm-cobol
vt title "SuiteCRM DevOps - Docker/Deploy"
claude code --resume
EOF

# Documentation Session
cat << 'EOF' > "$HOME/.claude-sessions/docs.sh"
#!/bin/bash
cd /Users/jfuginay/Documents/dev/suitecrm-cobol
vt title "SuiteCRM Docs - README/Guides"
claude code --resume
EOF

# Make scripts executable
chmod +x "$HOME/.claude-sessions/"*.sh

# Create aliases in shell profile
SHELL_PROFILE=""
if [[ "$SHELL" == *"zsh"* ]]; then
    SHELL_PROFILE="$HOME/.zshrc"
elif [[ "$SHELL" == *"bash"* ]]; then
    SHELL_PROFILE="$HOME/.bashrc"
fi

if [[ -n "$SHELL_PROFILE" ]]; then
    echo "" >> "$SHELL_PROFILE"
    echo "# Claude Code Army Aliases" >> "$SHELL_PROFILE"
    echo "alias claude-frontend='$HOME/.claude-sessions/frontend.sh'" >> "$SHELL_PROFILE"
    echo "alias claude-backend='$HOME/.claude-sessions/backend.sh'" >> "$SHELL_PROFILE"
    echo "alias claude-testing='$HOME/.claude-sessions/testing.sh'" >> "$SHELL_PROFILE"
    echo "alias claude-devops='$HOME/.claude-sessions/devops.sh'" >> "$SHELL_PROFILE"
    echo "alias claude-docs='$HOME/.claude-sessions/docs.sh'" >> "$SHELL_PROFILE"
    echo "alias claude-army='echo \"Available sessions: claude-frontend, claude-backend, claude-testing, claude-devops, claude-docs\"'" >> "$SHELL_PROFILE"
fi

cat << 'EOF'

🎉 Claude Code Army Setup Complete!

## Quick Start Commands:
- `claude-frontend`  - Start frontend development session
- `claude-backend`   - Start backend/COBOL development session  
- `claude-testing`   - Start testing session
- `claude-devops`    - Start DevOps/deployment session
- `claude-docs`      - Start documentation session
- `claude-army`      - Show available sessions

## Usage Tips:
1. Open multiple terminal windows/tabs
2. Run different claude-* commands in each
3. Each Claude will have contextual awareness of its role
4. Use `vt title "what you're working on"` to update session context

## Next Steps:
1. Restart your terminal or run: source ~/.zshrc
2. Open multiple terminal tabs
3. Run different claude-* commands
4. Start parallel development!

Example workflow:
Terminal 1: claude-frontend (work on SuiteCRM UI)
Terminal 2: claude-backend (work on COBOL APIs)  
Terminal 3: claude-testing (run tests)
Terminal 4: claude-devops (manage deployment)

EOF

echo "✅ Setup complete! Restart your terminal to use the new aliases."