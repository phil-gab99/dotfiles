import sys, os

################################################################################
# ALIASES
################################################################################

# Type - dict
c.aliases = {
    'w': 'session-save',
    'q': 'close',
    'qa': 'quit',
    'wq': 'quit --save',
    'wqa': 'quit --save'
}

################################################################################
# TABS AND WINDOW MANAGEMENT
################################################################################

# Open new window for every tab
# Type - Bool
c.tabs.tabs_are_windows = False

# When to show tab bar
# Type - String
# Values - always    -> Always show tab bar
#          never     -> Always hide tab bar
#          multuple  -> Hide tab bar if only one tab open
#          switching -> Show tab bar when switching tabs
c.tabs.show = 'multiple'

# Behaviour on last closed tab
# Type - String
# Values - ignore       -> Do nothing
#          blank        -> Load blank page
#          startpage    -> Load start page
#          default-page -> Load default page
#          close        -> Close window
c.tabs.last_close = 'close'

# Auto save session
# Type - Bool
c.auto_save.session = True

# Enable smooth scrolling
# Type - Bool
c.scrolling.smooth = True

# Load a restored tab as soon as it takes focus.
# Type - Bool
c.session.lazy_restore = True

################################################################################
# FONTS AND THEMES
################################################################################

# Font family for standard fonts
# Type - FontFamily
c.fonts.web.family.standard = 'Bitstream Vera Sans'

# Font family for serif fonts
# Type - FontFamily
c.fonts.web.family.serif = 'Bitstream Vera Serif'

# Font family for sans-serif fonts.
# Type: FontFamily
c.fonts.web.family.sans_serif = 'Bitstream Vera Sans'

# Font family for fixed fonts.
# Type: FontFamily
c.fonts.web.family.fixed = 'Fira Mono'

# Font used for hints
# Type - Font
c.fonts.hints = 'bold 12pt monospace'

# Font used in statusbar
# Type - Font
c.fonts.statusbar = '12pt Cantarell'

# Render web contents using dark theme
# Type - Bool
c.colors.webpage.darkmode.enabled = True

# Which images to apply dark mode to
# Type - String
# Values - always -> Apply dark mode filter to all images
#          never  -> Never apply dark mode filter to any images
#          smart  -> Apply dark mode based on image content
c.colors.webpage.darkmode.policy.images = 'never'

# Background color for webpages
# Type - QtColor
c.colors.webpage.bg = 'black'

################################################################################
# EDITING
################################################################################

# Automatically turn on insert mode when a loaded page focuses a text field
# Type - Bool
c.input.insert_mode.auto_load = True

# Edit fields in Emacs with Ctrl+E
# Type - ShellCommand
c.editor.command = ["emacsclient", "+{line}:{column}", "{file}"]

################################################################################
# CONTENT VIEWING
################################################################################

# Allow pdf.js to view PDF files in browser
# Type - Bool
c.content.pdfjs = False

# Autoplay video elements
# Type - Bool
c.content.autoplay = False

################################################################################
# ADBLOCKING - TODO
################################################################################
sys.path.append(os.path.join(sys.path[0], 'jmatrix'))
config.source("jmatrix/jmatrix/integrations/qutebrowser.py")
# c.content.blocking.enabled = False
# c.content.blocking.method = null
# c.content.blocking.adblock.lists = [
#     'https://easylist.to/easylist/easylist.txt',
#     'https://easylist.to/easylist/easyprivacy.txt',
#     'https://easylist.to/easylist/fanboy-annoyance.txt',
#     'https://easylist-downloads.adblockplus.org/abp-filters-anti-cv.txt',
#     'https://secure.fanboy.co.nz/fanboy-annoyance.txt',
#     'https://secure.fanboy.co.nz/fanboy-cookiemonster.txt',
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/legacy.txt',
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters.txt',
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/annoyances.txt',
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/badware.txt',
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/privacy.txt',
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/resource-abuse.txt',
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/unbreak.txt'
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/badlists.txt',
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2020.txt',
#     'https://github.com/uBlockOrigin/uAssets/raw/master/filters/filters-2021.txt',
#     'https://www.malwaredomainlist.com/hostslist/hosts.txt',
#     'https://www.i-dont-care-about-cookies.eu/abp/',
#     'https://pgl.yoyo.org/adservers/serverlist.php?showintro=0;hostformat=hosts',
#     'https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=1&mimetype=plaintext'
# ]

################################################################################
# KEYBINDINGS
################################################################################

## Normal mode ##

# Swap J and K for tab switching
config.bind('J', 'tab-prev')
config.bind('K', 'tab-next')
config.unbind('d') # Don't close tab on lower-case 'd'
config.bind('yy', 'yank')
config.bind(',m', 'spawn mpv {url}')
config.bind(',M', 'hint links spawn mpv {hint-url}')

## Command mode ##
config.bind('<Ctrl-g>', 'mode-leave', mode='command')
config.bind('<ctrl-j>', 'completion-item-focus --history next', mode='command')
config.bind('<ctrl-k>', 'completion-item-focus --history prev', mode='command')

## Hint mode ##
config.bind('<Ctrl-g>', 'mode-leave', mode='hint')

## Insert mode ##
config.bind('<Ctrl-g>', 'mode-leave', mode='insert')

## Prompt mode ##
config.bind('<Ctrl-g>', 'mode-leave', mode='prompt')
config.bind('<ctrl-j>', 'prompt-item-focus next', mode='prompt')
config.bind('<ctrl-k>', 'prompt-item-focus prev', mode='prompt')
config.unbind('<Tab>', mode='prompt')

################################################################################
# MISC
################################################################################

# Additional arguments to pass to Qt, without leading '--'
# Type - List of String
c.qt.args = ['disable-seccomp-filter-sandbox'] # Fix rendering issue

# Load the autoconfig file (quteconfig.py)
config.load_autoconfig()
