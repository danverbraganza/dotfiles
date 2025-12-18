-- ~/.wezterm.lua
local wezterm = require 'wezterm'
local config = {}

-- 1. Colour theme: Ghostty `theme = srcery`
-- WezTerm color scheme name is "Srcery (Gogh)"
-- https://wezterm.org/colorschemes/s/index.html
config.color_scheme = 'Srcery (Gogh)'

-- 2. Font: Ghostty `font-family = "Source Code Pro"`, `font-size = 13`
config.font = wezterm.font 'Source Code Pro'
config.font_size = 13.0

-- 3. Extra cell height: Ghostty `adjust-cell-height = "6%"`
-- 6% taller lines ≈ 1.06 line_height
config.line_height = 1.06

-- 4. macOS Option as Alt/Meta: Ghostty `macos-option-as-alt = true`
-- In WezTerm, this means: don't treat Option as a composing key, just send Alt.
-- (false = raw Alt/meta, true = “compose fancy character”) :contentReference[oaicite:0]{index=0}
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = false

-- Ask before pasting big chunks
local function safe_paste(window, pane)
  -- Grab the system clipboard
  local clipboard = wezterm.gui.get_clipboard()

  clipboard:read_text(function(text)
    if not text or text == '' then
      return
    end

    -- Count lines (newline count + 1)
    local _, newlines = text:gsub('\n', '\n')
    local lines = newlines + 1
    local threshold = 10

    if lines <= threshold then
      -- Small paste, just do it
      pane:paste(text)
      return
    end

    -- Big paste: ask first
    window:perform_action(
      act.Confirmation {
        message = string.format("Paste %d lines from clipboard? (>%d)", lines, threshold),
        action = wezterm.action_callback(function(win, p)
          -- User clicked "Yes"
          p:paste(text)
        end),
        cancel = wezterm.action_callback(function() end),
      },
      pane
    )
  end)
end

-- 5. Working directory inheritance

-- In WezTerm this is the *default* as long as your shell sends OSC 7
-- (see wezterm shell integration docs). :contentReference[oaicite:1]{index=1}
-- No explicit config needed, but you can set default_cwd for the first window:
-- config.default_cwd = wezterm.home_dir

-- 6. Keybindings
local act = wezterm.action

config.keys = {
  -- alt+left / alt+right  ->  ESC b / ESC f
  -- for word-jumps in Emacs, readline, zsh, etc.
  { key = 'LeftArrow',  mods = 'ALT', action = act.SendString('\x1bb') },
  { key = 'RightArrow', mods = 'ALT', action = act.SendString('\x1bf') },

  -- Ctrl+Arrow -> CSI 1;5[A-D]
  -- Emacs and most TUIs read these as C-<arrow>
  { key = 'UpArrow',    mods = 'CTRL', action = act.SendString('\x1b[1;5A') },
  { key = 'DownArrow',  mods = 'CTRL', action = act.SendString('\x1b[1;5B') },
  { key = 'RightArrow', mods = 'CTRL', action = act.SendString('\x1b[1;5C') },
  { key = 'LeftArrow',  mods = 'CTRL', action = act.SendString('\x1b[1;5D') },

  -- C-- : shrink font (WezTerm side)
  { key = '-', mods = 'CTRL', action = act.DecreaseFontSize },
  -- C-_ : send actual C-_ through to Emacs (undo) ASCII 0x1f
  { key = '_', mods = 'CTRL|SHIFT', action = act.SendString('\x1f') },

  -- PageUp and PageDown should scroll by page
  { key = 'PageUp', mods = 'NONE', action = act.ScrollByPage(-1) },
  { key = 'PageDown', mods = 'NONE', action = act.ScrollByPage(1) },

}

return config
