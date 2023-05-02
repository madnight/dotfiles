local wezterm = require("wezterm")

local SOLID_LEFT_ARROW = wezterm.nerdfonts.pl_right_hard_divider
local SOLID_RIGHT_ARROW = wezterm.nerdfonts.pl_left_hard_divider

local config = {
   color_scheme = 'X::Erosion (terminal.sexy)',
   font_size = 14.0,
   use_fancy_tab_bar = false,
   enable_tab_bar = true,
   max_fps = 250,
   animation_fps = 1,
   check_for_updates = false,
   cursor_blink_ease_in = 'Constant',
   cursor_blink_ease_out = 'Constant',
   font = wezterm.font('TamzenForPowerline'),
   window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
   },
   colors = {
     foreground = '#e8e1e1',
     background = '181715'
   },
   leader = { key="a", mods="ALT" },
   disable_default_key_bindings = true,
   keys = {
        { key = 'V', mods = 'CTRL',         action=wezterm.action.PasteFrom 'Clipboard' },
        { key = 'V', mods = 'CTRL',         action=wezterm.action.PasteFrom 'PrimarySelection' },
        { key = "a", mods = "LEADER|CTRL",  action=wezterm.action{SendString="\x01"}},
        { key = "-", mods = "ALT",          action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
        { key = ".", mods = "ALT",          action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
        { key = "f", mods = "ALT",          action="TogglePaneZoomState" },
        { key = "c", mods = "ALT",          action=wezterm.action{SpawnTab="CurrentPaneDomain"}},
        { key = "h", mods = "ALT",          action=wezterm.action{ActivatePaneDirection="Left"}},
        { key = "j", mods = "ALT",          action=wezterm.action{ActivatePaneDirection="Down"}},
        { key = "k", mods = "ALT",          action=wezterm.action{ActivatePaneDirection="Up"}},
        { key = "l", mods = "ALT",          action=wezterm.action{ActivatePaneDirection="Right"}},
        { key = "H", mods = "ALT|SHIFT",    action=wezterm.action{AdjustPaneSize={"Left", 5}}},
        { key = "J", mods = "ALT|SHIFT",    action=wezterm.action{AdjustPaneSize={"Down", 5}}},
        { key = "K", mods = "ALT|SHIFT",    action=wezterm.action{AdjustPaneSize={"Up", 5}}},
        { key = "L", mods = "ALT|SHIFT",    action=wezterm.action{AdjustPaneSize={"Right", 5}}},
        { key = 'n', mods = 'ALT',          action=wezterm.action{ActivateTabRelative=1}},
        { key = 'p', mods = 'ALT',          action=wezterm.action{ActivateTabRelative=-1}},
        { key = "1", mods = "ALT",          action=wezterm.action{ActivateTab=0}},
        { key = "2", mods = "ALT",          action=wezterm.action{ActivateTab=1}},
        { key = "3", mods = "ALT",          action=wezterm.action{ActivateTab=2}},
        { key = "4", mods = "ALT",          action=wezterm.action{ActivateTab=3}},
        { key = "5", mods = "ALT",          action=wezterm.action{ActivateTab=4}},
        { key = "6", mods = "ALT",          action=wezterm.action{ActivateTab=5}},
        { key = "7", mods = "ALT",          action=wezterm.action{ActivateTab=6}},
        { key = "8", mods = "ALT",          action=wezterm.action{ActivateTab=7}},
        { key = "9", mods = "ALT",          action=wezterm.action{ActivateTab=8}},
        { key = "m", mods = "ALT",          action=wezterm.action.ActivateCopyMode},
        { key = "&", mods = "ALT|SHIFT",    action=wezterm.action{CloseCurrentTab={confirm=true}}},
        { key = "x", mods = "ALT",          action=wezterm.action{CloseCurrentPane={confirm=true}}},
        { key = "n", mods="SHIFT|CTRL",     action="ToggleFullScreen" },
    },
}

return config
