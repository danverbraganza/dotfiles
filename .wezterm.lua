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

-- 5. Working directory inheritance
-- Ghostty:
--   working-directory = inherit
--   window-inherit-working-directory = true
--
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
}

table.insert(config.keys, {
  key = '-',
  mods = 'CTRL',
  action = act.DisableDefaultAssignment,
})
table.insert(config.keys, {
  key = '-',
  mods = 'SUPER',  -- Cmd on macOS
  action = act.DisableDefaultAssignment,
})

return config
