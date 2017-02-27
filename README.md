# javascript-emacs
GNU Emacs configuration for JavaScript development.

Node.js is required for JavaScript syntax checking.

Pylint is required for python syntax checking.

## Features

### Avy
Move your cursor easily around whole screen.
After activating desired mode, insert letter(s) to which you want to jump and press highlighted key(s) to proceed.
![avy](gifs/avy.gif)
- `Ctrl + l` - go to line
- `Ctrl + w` - go to word (one letter)
- `Ctrl + space` - go to word (two letters)

### Multiple cursors
Use more than one cursor. You can combine this mode with variety of others.
![multiple-cursors](gifs/multiple-cursors.gif)
- `Alt + .` - add cursor forward/below
- `Alt + ,` - add cursor backward/up

### Snippets
Create your own templates or use default set.
![snippets](gifs/snippets.gif)
- `f tab` - anonymous function snippet
- `fn tab` - named function snippet
- `fnr tab` - named function snippet with return
- `< tab` - flexible HTML snippet
- `<c tab` - flexible HTML snippet with class

### Recursive grep
Search for code fragments based on string.
![rgrep](gifs/rgrep.gif)
- `Alt + r` - open rgrep

### Undo tree
Easily try different ideas and never lose code by accident.
![undo-tree](gifs/undo-tree.gif)
- `Ctrl + x u` - open undo-tree
- `Ctrl + f` - move to the right branch
- `Ctrl + b` - move to the left branch

### Magit
Manage your git repositories directly in emacs.
![magit](gifs/magit.gif)
- `Alt + m` - open magit
- `h` - open help and show key bindings (in magit)

### Yanking
Copy & paste multiple strings.
![yank](gifs/yank.gif)
- `Ctrl + y` - paste, then by pressing `Alt + y` paste previously copied text

### Expanding region
Use semantic text selection.
![expand-region](gifs/expand-region.gif)
- `Alt + s` - expand selection, then by pressing `s` expand it wider

## Installation
1. Download repository.
2. Symlink or copy `./configs/.emacs` and `./configs/.emacs.d` to your home directory.

## Included packages
- redo+
- [expand-region](https://github.com/magnars/expand-region.el)
- [smex](https://github.com/nonsequitur/smex)
- [ido](https://github.com/DarwinAwardWinner/ido-ubiquitous)
- [yasnippet](http://github.com/capitaomorte/yasnippet)
- [js-doc](https://github.com/mooz/js-doc)
- [js2-mode](https://github.com/mooz/js2-mode/)
- [ac-js2](https://github.com/ScottyB/ac-js2)
- [projectile](https://github.com/bbatsov/projectile)
- [smartparens](https://github.com/Fuco1/smartparens)
- [auto-complete](https://github.com/auto-complete/auto-complete)
- [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
- [magit](https://github.com/magit/magit)
- [highlight-parentheses](https://github.com/tsdh/highlight-parentheses.el)
- [yasnippet](http://github.com/capitaomorte/yasnippet)
- [nyan-mode](https://github.com/TeMPOraL/nyan-mode/)
- [hlinum](https://github.com/tom-tan/hlinum-mode/)
- syntax-subword
- [less-css-mode](https://github.com/purcell/less-css-mode)
- [undo-tree](http://www.dr-qubit.org/emacs.php)
- [helm](https://emacs-helm.github.io/helm/)
- [helm-projectile](https://github.com/bbatsov/helm-projectile)
- [scss-mode](https://github.com/antonj/scss-mode)
- [god-mode](https://github.com/chrisdone/god-mode)
- [use-package](https://github.com/jwiegley/use-package)
- [avy](https://github.com/abo-abo/avy)
- [markdown-mode](http://jblevins.org/projects/markdown-mode/)
- [php-mode](https://github.com/ejmr/php-mode)
- [smart-forward](https://github.com/magnars/smart-forward.el)
- [web-mode](https://github.com/fxbois/web-mode)
- [smooth-scroll](https://www.emacswiki.org/emacs/smooth-scroll.el)
- [csv-mode](https://github.com/emacsmirror/csv-mode)
- [column-enforce-mode](www.github.com/jordonbiondo/column-enforce-mode)
- [yaml-mode](https://github.com/yoshiki/yaml-mode)
- [clojure-mode](https://github.com/clojure-emacs/clojure-mode)
- [xkcd](https://github.com/vibhavp/emacs-xkcd)
- [jedi](https://github.com/tkf/emacs-jedi)
- [anaconda-mode](https://github.com/proofit404/anaconda-mode)
- [parinfer](https://github.com/DogLooksGood/parinfer-mode)
- [flycheck](https://github.com/flycheck/flycheck)
- [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell)

## Key bindings
- Movement
  - `Ctrl + f` - move forward
  - `Ctrl + b` - move backward
  - `Ctrl + n` -  move to next line
  - `Ctrl + p` - move to previous line
  - `Ctrl + a` - move to beginning of the line
  - `Ctrl + e` - move to ending of the line
  - `Alt + f` - move word forward
  - `Alt + b` - move word backward
  - `Alt + right arrow` - move forward (semantic)
  - `Alt + left arrow` - move backward (semantic)
  - `Alt + up arrow` - move up (semantic)
  - `Alt + down arrow` - move down (semantic)
  - `Alt + <` - move to beginning of the file
  - `Alt + >` - move to ending of the file
  - `Alt + v` - scroll down
  - `Ctrl + v` - scroll up
  - `Alt + p` - move line/region up
  - `Alt + n` - move line/region down
  - `up arrow` - jump to line with number

- Selection
  - `Ctrl + space` - enter/exit text selection mode
  - `Ctrl + .` - mark next like this
  - `Ctrl + ,` - mark previous like this
  - `Ctrl + j` - new line for many cursors
  - `Alt + s` - expand selection, then by pressing `s` expand it wider
  - `Ctrl + x + p` - select all

- History and search
  - `Alt + w` - copy
  - `Ctrl + k` - cut
  - `Ctrl + y` - paste, then by pressing `Alt + y` paste previously copied text
  - `Left arrow` - undo
  - `Right arrow` - redo
  - `Ctrl + s` - search (forward)
  - `Ctrl + r` - search (backward)
  - `Alt + g` - recursive grep

- Text removal
  - `Alt + backspace` - remove word backward
  - `Alt + d` - remove word forward
  - `Ctrl + d` - remove character forward

- Files and console
  - `Ctrl + c p f` - search file
  - `Ctrl + l` - search file (helm)
  - `Alt + l` - search buffer (helm)
  - `Alt + c` - search command (helm)
  - `Ctrl + x b` - search through open files
  - `Alt + x` - open console
  - `Ctrl + x Ctrl + f` - browse files
  - `Ctrl + r` - next
  - `Ctrl + s` - previous

- Avy
  - `Ctrl + space` - go to word (two letters)
  - `Ctrl + w` - go to word (one letter)
  - `Ctrl + l` - go to line

- JavaScript
  - `f tab` - anonymous function snippet
  - `fn tab` - named function snippet
  - `i tab` - if snippet
  - `ie tab` - if else snippet
  - `Ctrl + c i` - js function description comment

- Windows
  - `Ctrl + x 3` - split screen vertically
  - `Ctrl + x 2` - split screen horizontally
  - `Ctrl + x o` - move to next window
  - `Ctrl + x 0` - close window
  - `Ctrl + x Ctrl + o` - rotate buffer windows

- Other
  - `Alt + ;` - comment/uncomment line/selection
  - `Ctrl + x u` - open undo-tree buffer
  - `Alt + m` - open magit status buffer
  - `down arrow` - count lines, words and characters

## Credits
Great thanks to Andrzej for showing me emacs, what made me immediately hooked.
