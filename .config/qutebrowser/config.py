c.tabs.tabs_are_windows = False
c.tabs.show = "multiple"
c.tabs.last_close = "close"

c.auto_save.session = True
c.scrolling.smooth = True
c.session.lazy_restore = True

# Scale pages and UI better for hidpi
# c.zoom.default = "100"
c.fonts.hints = "bold 12pt monospace"

# Better default fonts
c.fonts.web.family.standard = "Bitstream Vera Sans"
c.fonts.web.family.serif = "Bitstream Vera Serif"
c.fonts.web.family.sans_serif = "Bitstream Vera Sans"
c.fonts.web.family.fixed = "Fira Mono"
c.fonts.statusbar = "12pt Cantarell"

# Use dark mode where possible
c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.policy.images = "never"
c.colors.webpage.bg = "black"

# Automatically turn on insert mode when a loaded page focuses a text field
c.input.insert_mode.auto_load = True

# Edit fields in Emacs with Ctrl+E
c.editor.command = ["emacsclient", "+{line}:{column}", "{file}"]

# Content
c.content.pdfjs = True
c.content.autoplay = False

# Adblocking
# c.content.blocking.enabled = True
# c.content.blocking.method = "adblock"
# c.content.blocking.adblock.lists = [
#     "https://easylist.to/easylist/easylist.txt",
#     "https://easylist.to/easylist/easyprivacy.txt",
#     "https://easylist.to/easylist/fanboy-annoyance.txt",
#     "https://easylist-downloads.adblockplus.org/abp-filters-anti-cv.txt",
#     "https://secure.fanboy.co.nz/fanboy-annoyance.txt",
#     "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt",
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/legacy.txt",
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters.txt",
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances.txt",
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badware.txt",
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt",
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/resource-abuse.txt",
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/unbreak.txt"
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/badlists.txt",
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2020.txt",
#     "https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2021.txt",
#     "https://www.malwaredomainlist.com/hostslist/hosts.txt",
#     "https://www.i-dont-care-about-cookies.eu/abp/",
#     "https://pgl.yoyo.org/adservers/serverlist.php?showintro=0;hostformat=hosts",
#     "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=1&mimetype=plaintext"
# ]

# Rendering issues
c.qt.args = ["disable-seccomp-filter-sandbox"]

# Make Ctrl+g quit everything like in Emacs
config.bind('<Ctrl-g>', 'mode-leave', mode='insert')
config.bind('<Ctrl-g>', 'mode-leave', mode='command')
config.bind('<Ctrl-g>', 'mode-leave', mode='prompt')
config.bind('<Ctrl-g>', 'mode-leave', mode='hint')

# Mpv spawn
config.bind('wm', 'spawn umpv {url}')
config.bind('wM', 'hint links spawn umpv {hint-url}')

# Tweak some keybindings
config.unbind('d') # Don't close tab on lower-case 'd'
config.bind('yy', 'yank')

# Swap J and K for tab switching
config.bind('J', 'tab-prev')
config.bind('K', 'tab-next')

# Vim-style movement keys in command mode
config.bind('<Ctrl-j>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-k>', 'completion-item-focus --history prev', mode='command')

# More binding hints here: https://gitlab.com/Kaligule/qutebrowser-emacs-config/blob/master/config.py

# Load the autoconfig file (quteconfig.py)
config.load_autoconfig()
