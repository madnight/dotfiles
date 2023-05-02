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
| PDF | [zathura](https://wiki.archlinux.org/title/zathura) | with [vi-styled](https://hleb.dev/post/zathura/) keybindings
| Compositor | [compton](https://wiki.archlinux.org/index.php/compton) | avoid screen tearing [issues](https://www.reddit.com/r/archlinux/comments/7yhuy3/screen_tearing_issue_in_arch_linux/)
| Video/Music | [vlc](https://wiki.archlinux.org/index.php/VLC_media_player) | [mplayer](https://wiki.archlinux.org/index.php/MPlayer) as alternative
| File Manager | [ranger](https://github.com/ranger/ranger) | console file manager with vi key [bindings](https://ranger.github.io/ranger.1.html#KEY-BINDINGS)
| File Finder | [fzf + ripgrep](https://medium.com/@crashybang/supercharge-vim-with-fzf-and-ripgrep-d4661fc853d2) | [ripgrep](https://github.com/BurntSushi/ripgrep) is the fastest grep
| Screen Locker | [i3lock](https://github.com/i3/i3lock) | [with](https://github.com/madnight/dotfiles/blob/c112cc973ddcb0c754c00ca266eb0bdbc13dcd62/scripts/lock.go) screenshot, pixelation and lock icon


<br>![Alt text](https://raw.github.com/madnight/dotfiles/master/screenshot.png "SCREENSHOT")

## Hardware

#### Notebook
I use Notebooks from the Thinkpad T-Series (T430, T450s, T480s, T480), which you can buy of eBay for cheap. If you are interested, then try to pick one with at least one unsoldered RAM slot (upgradeable), SSD, IPS Display with 300 nits or higher and Core i5/i7 (or even better Ryzen CPU), like the [T480](https://www.notebookcheck.net/Lenovo-ThinkPad-T480-Core-i7-8650U-FHD-Laptop-Review.315574.0.html) which is an excellent choice, when in mint condition (A-ware) and bought from a eBay store with a customer friendly refund policy.

#### Monitor
I use a multi-monitor setup with either 3 external monitors or 2 external + 1 internal notebook screens. I use second hand 24 inch monitors for the left and right and one 27 inch monitor in the center. The T-Series Thinkpads supports up to 3 screens (the internal notebook screen also counts as screen). Nevertheless, it's possible to have more monitors than that (e.g. day-trader setup) with an eGPU or DisplayLink USB to HDMI adapter. I've tested a four screen setup with 3 external monitors (one per DisplayLink adapter) + the internal Thinkpad screen and it works.

#### Keyboard
The Thinkpad keyboard is excellent. But for external keyboards you can get even better ones. If you are a fan of flat keyboards like the Thinkpad keyboard with a short key travel, then I recommend the Apple Magic Keyboard (wired, silver, aluminium), which I've used for many years. If that does not satisfy your need, you can dive into the limitless world of mechanical keyboards. I would opt for either a fully custom keyboard or a prebuild with a decent build quality e.g. a brand like Ducky, Leopold, Durgod or Filco and avoid "gaming" brands. I picked up a [Durgod K320](http://web.archive.org/web/20200731171335/https://www.amazon.com/dp/B078HFTTYK/) with TKL ISO-layout, non-RGB, PBT keycaps, lubed stabilizers and Cherry MX brown switches.

#### Chair
I use the [IKEA Markus](http://web.archive.org/web/20200303192021/https://www.ikea.com/us/en/p/markus-office-chair-vissle-dark-gray-90289172/). It has an overall good build quality. It is a quiet popular chair and chances are that you can buy a used one nearby. I would say that the chair is likely to last for at least 5 years and might hold up to 10 years. The lumbar support is in the right spot (I'm 184 cm / 6 feet). I did two hacks to the chair. I increased the height of the backsupport and I did the IKEA arm rest hack, both hacks can be found on reddit. I thought about getting a chair with more adjustments, like 4D adjustable arm rests, adjustable back height, adjustable headrest, ... but those chairs are very pricey (fully configured Hermann Miller starts at over 1000 euro). I think the Markus is fine for most people but your mileage may vary.

#### Table
I went for the [IKEA Thyge](http://web.archive.org/web/20191120195444/https://www.ikea.com/us/en/p/thyge-desk-white-silver-color-s49110931/). The table top is 160x80cm, which is enough width for a triple monitor (24-27") setup. The legs are height adjustable between 60-90cm. I would recommend to adjust it to be between 65-75cm, just so that you have an ergonomic typing position, since many desks are too high for comfortable/ergonomic typing. The Thyge is also a very popular IKEA product and you might get it second hand, as I did.
