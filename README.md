# hardyn-emacs
My personal configuration of Emacs used for front-end development.

![main](images/main.png)

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
- [scss-mode](https://github.com/antonj/scss-mode)
- [god-mode](https://github.com/chrisdone/god-mode)
- [use-package](https://github.com/jwiegley/use-package)
- [avy](https://github.com/abo-abo/avy)
- [markdown-mode](http://jblevins.org/projects/markdown-mode/)

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

- History, search and selection
    - `Left arrow` - undo
    - `Right arrow` - redo
    - `Ctrl + s` - search (forward)
    - `Ctrl + r` - search (backward)
    - `C-space` - enter/exit mark mode

- Files and console
    - `Ctrl + c p f` - search file
    - `Ctrl + x b` - search through open files
    - `Alt + x` - open console

- Avy
    - `Ctrl + space` - go to word with avy (two letters)
    - `Ctrl + w` - go to word with avy (one letter)
    - `Ctrl + l` - go to line with avy
