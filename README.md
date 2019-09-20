## Arch Linux Setup
<br>

| Software | Choice | Note
| ------------- |:-------------:|-------------|
| Shell | [zsh](https://wiki.archlinux.org/index.php/zsh) | with extensive git [prompt](https://github.com/starcraftman/zsh-git-prompt)
| Window Manager | [bspwm](https://wiki.archlinux.org/index.php/bspwm) | [xmonad](https://github.com/xmonad/xmonad) and [i3-gaps](https://github.com/Airblader/i3) also configured
| Terminal      | [(u)xterm](https://wiki.archlinux.org/index.php/Xterm) | it's the [fastest](https://lwn.net/Articles/751763/); [urxvt](https://wiki.archlinux.org/index.php/Rxvt-unicode) also configured
| Multiplexer | [tmux](https://wiki.archlinux.org/index.php/tmux) | [tmux-resurrect](https://github.com/tmux-plugins/tmux-resurrect) for persistent sessions
| Editor      | [vim](https://wiki.archlinux.org/index.php/vim) | plus many [lazy](https://github.com/junegunn/vim-plug#on-demand-loading-of-plugins) loaded [plugins](https://github.com/madnight/dotfiles/blob/master/vim/plugins.vim)
| Status Bar | [polybar](https://github.com/jaagr/polybar) | used [dzen2](https://wiki.archlinux.org/index.php/Dzen) before
| Launcher | [rofi](https://wiki.archlinux.org/index.php/rofi) | [dmenu](https://wiki.archlinux.org/index.php/dmenu) replacement
| Browser | [chromium](https://wiki.archlinux.org/index.php/chromium) | [firefox](https://wiki.archlinux.org/index.php/Firefox) as alternative; with vim [plugin](https://github.com/1995eaton/chromium-vim)
| Chat | [weechat](https://wiki.archlinux.org/index.php/Weechat) | plus [weeslack](https://github.com/wee-slack/wee-slack) plugin
| E-Mail | [thunderbird](https://wiki.archlinux.org/index.php/thunderbird) | [mutt](https://wiki.archlinux.org/index.php/Mutt) as ncurses alternative
| Compositor | [compton](https://wiki.archlinux.org/index.php/compton) | avoid screen tearing [issues](https://www.reddit.com/r/archlinux/comments/7yhuy3/screen_tearing_issue_in_arch_linux/)
| Video/Music | [vlc](https://wiki.archlinux.org/index.php/VLC_media_player) | [mplayer](https://wiki.archlinux.org/index.php/MPlayer) as alternative
| File Finder | [fzf + ripgrep](https://medium.com/@crashybang/supercharge-vim-with-fzf-and-ripgrep-d4661fc853d2) | [ripgrep](https://github.com/BurntSushi/ripgrep) is the fastest grep
| Screen Locker | [i3lock](https://github.com/i3/i3lock) | [with](https://github.com/madnight/dotfiles/blob/c112cc973ddcb0c754c00ca266eb0bdbc13dcd62/scripts/lock.go) screenshot, pixelation and lock icon


<br>![Alt text](https://raw.github.com/madnight/dotfiles/master/screenshot.png "SCREENSHOT")
