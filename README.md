
# Table of Contents

1.  [Overview](#org4dade87)
    1.  [Quick start](#org6b2f3b5)
    2.  [Why a literate configuration](#orgd1a884a)
    3.  [My customizations](#orgc314271)
    4.  [The importance of the `init.el` file](#org32dd112)
2.  [The Configuration](#org2189586)
    1.  [A preamble](#org32d65ac)
    2.  [Speed up loading](#org5c266b1)
    3.  [Speed up line movement](#org2c58e5f)
    4.  [I use Ctrl-Z for personal bindings](#org03fc1b6)
    5.  [Extra org mode key bindings](#org2555333)
    6.  [Theme and mode line](#org58713fb)
    7.  [Packages](#org7171da7)
        1.  [`magit`](#org050f101)
        2.  [`diminish`](#orgb1f6fd6)
        3.  [`which-key`](#org0e41eeb)
        4.  [`projectile`](#orga725ca7)
        5.  [`company-mode`](#org75567d0)
        6.  [`ivy/swiper`](#org162bf35)
        7.  [Use `ivy` and `posframe` together](#org10e63ec)
        8.  [`prescient`](#orge87b490)
        9.  [`yasnippet`](#orgaa8920c)
        10. [`dired`](#orgfc2c009)
        11. [`command-log-mod`](#org1e13ba9)
        12. [Docker](#org21f9f9d)
        13. [`flycheck`](#orge781d49)
        14. [yaml-mode](#org9bb4fdd)
        15. [`bind-key`](#org92a078d)
        16. [`helpful`](#org42cc60c)
        17. [`savehist`](#orgd1afcad)
        18. [Very large files](#orgb41dec2)
        19. [Logs in general](#org31a9ab3)
        20. [Random packages](#orgb0d45bb)
    8.  [`org-mode`](#orgc731f4b)
        1.  [For blogging](#orgb89b80a)
        2.  [Use of babel](#orgd5eab22)
    9.  [Language support](#org0bb35e7)
        1.  [Taking the `eglot` plunge](#orge5fca60)
        2.  [Python](#orgb37e1a8)
    10. [Additional bits-o-configuration](#org79fb906)
        1.  [Limit the length of `which-function`](#orgec2b961)
        2.  [`my-ansi-term`](#org5381f31)
        3.  [Understand file type by shebang](#org6741eee)
        4.  [React to screen width changes for `posframe`](#org8330a97)
    11. [Final (random) bits](#orgfb9d8cf)
3.  [`init.el`, short and sweet](#org2198f29)
    1.  [`org-mode` export hacks for HTML and Markdown](#org0ae5e36)



<a id="org4dade87"></a>

# Overview

This is a literate and <font color=red size=+3><b><u>portable</u></b></font> Emacs initialization.


<a id="org6b2f3b5"></a>

## Quick start

Simply

1.  git clone this repository into `~/.emacs.d`: `git clone https://github.com/Atman50/emacs-config.git =/.emacs.d`

2.  start Emacs

That's it.

Starting Emacs for the first time on a new machine loads all the packages/configuration loads. It takes some time on this first
load since all the packages referenced need to download and compile. On subsequent Emacs invocations startup time is much better.
The ability to simply clone and start is what makes this configuration **portable**. Note that some of the Emacs customization (see
`custom.el`) are system (file system) dependent. I handle this by using git to create a stash of the localized changes for
`custom.el` and then apply it whenever I take updated configurations from the repository.


<a id="orgd1a884a"></a>

## Why a literate configuration

Well mostly I wanted to learn how to do it, but also I was having issues with managing my initialization/configuration. FWIW
this is the approach that I came up with. My  `init.el` file is simple and relies on the fact that customizations are saved to
its own file and that these customizations are read in before the packages are loaded.

What I've gotten out of all this work is a portable and documented configuration that works well for me. Please feel free to
take whatever portion(s) you wish from this and make it your own.

I have tried to make this configuration 100% portable meaning that on a new system (Linux or Windows at this point) with Emacs
installed. I simple git clone this repository to `~/.emacs.d` and then fire up Emacs. Should work every time. 


<a id="orgc314271"></a>

## My customizations

While not many, I use the customization engine to provide any custom definitions. The are put into the `my-configuration` group.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">my/cfg-file</td>
<td class="org-left">"~/.emacs.d/README"</td>
</tr>


<tr>
<td class="org-left">my/which-function-max-width</td>
<td class="org-left">64</td>
</tr>
</tbody>
</table>


<a id="org32dd112"></a>

## The importance of the `init.el` file

There's a detailed explanation on how this configuration works WRT to `init.el` at the end of this document.

TLDR; The real trick with `init.el` is to load the Emacs user customizations first so that packages properly initialize and
everything places nicely with [Emacs customization](https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html). Then make sure `org-mode` is at the proper level and tangle this document
into the configuration to load.


<a id="org2189586"></a>

# The Configuration

Here are the configuration bits. All of the following code snippets that are tangled from this file placed into an `.el` file that gets loaded
from the initialization (`~/emacs.d/init.el`) file.


<a id="org32d65ac"></a>

## A preamble

First make sure that we are doing lexical scoping for speed. See [Some Performance Advantages of Lexical Scope blog](https://nullprogram.com/blog/2016/12/22/).

```emacs-lisp
;;; README.el --- a file generated from README.org - do not edit by hand!!!!
;; -*- lexical-binding: t; -*-
;;; Commentary:
;;;     Org tangled from README.org. Edit the org file to chnage this configuration
;;; Code:
```


<a id="org5c266b1"></a>

## Speed up loading

This is a little piece of code that I picked up that might make things faster when downloading and installing all the packages.
This turns down the garbage collector during the use-package loading when it has to do some compiling. Set it back when done with
init.

```emacs-lisp
(setq gc-cons-threshold 100000000)
;; never set it back, but if you wanted to:
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
```


<a id="org2c58e5f"></a>

## Speed up line movement

I ran into this little tidbit while reading Sacha Chua's posts from Emacs. It is described [here](https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746), but to summarize the
`next-line` defun triggers `line-move-partial` which leads to excessive processing. By setting the variable here, the speed of
using `next-line` gets very cut down.

```emacs-lisp
(setq auto-window-vscroll nil)
```


<a id="org03fc1b6"></a>

## I use Ctrl-Z for personal bindings

Ctrl-C was supposed to be left for personal customization but seems to get used more than it should,
therefore I've started to bind things to Ctrl-Z, which had the annoying and useless minimize functionality.

```emacs-lisp
(bind-keys :map global-map                      ;; get rid of pesky "\C-z" and use for personal bindings
           :prefix-map my-ctrl-z-prefix-map
           :prefix "C-z"
           ("C-d" . dired-jump)
           ("c"   . comment-region)
           ("d"   . docker)
           ("e"   . eglot-rename)
           ("f"   . magit-find-file-other-window)
           ("g"   . magit-status)
           ("h a" . helpful-at-point)
           ("h c" . helpful-command)
           ("h C" . helpful-callable)
           ("h f" . helpful-function)
           ("h k" . helpful-key)
           ("h m" . helpful-macro)
           ("h v" . helpful-variable)
           ("l"   . magit-log-buffer-file)
           ("n"   . linum-mode)
           ("r"   . revert-buffer)
           ("t"   . toggle-truncate-lines)
           ("u"   . uncomment-region))
```


<a id="org2555333"></a>

## Extra org mode key bindings

Seems at least one key binding disappeared on me in org mode.

```emacs-lisp
(org-defkey org-mode-map "\C-c\C-x\C-r" 'org-clock-report)
```


<a id="org58713fb"></a>

## Theme and mode line

My progression of modelines has gone from `powerline` to `moody` and now `doom`. The `doom-modeline` package is pretty good and
not as much fuss as I had with `moody`. All the stuff I need there and makes this configuration much easier. You **must** go
install the fonts from the `all-the-icons` package (which is loaded as a dependency) according to the instructions found on the
[`doom-modeline` website](https://github.com/seagle0128/doom-modeline): Run `M-x all-the-icons-install-fonts` and then, on Windows, install the font ttf file by right clicking
on it and doing install.

```emacs-lisp
(use-package color-theme-sanityinc-tomorrow
  :demand t
  :config
  (load-theme 'leuven t))
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))
```


<a id="org7171da7"></a>

## Packages

Here are the packages I use and I've tried to list them in a relatively logical order (trying to put the more significant
packages earlier in this document).


<a id="org050f101"></a>

### [`magit`](https://github.com/magit/magit)

The **most awesome** git porcelain. Most here are part of magit, [`git-time-machine`](https://github.com/pidu/git-timemachine) is not, but well worth using.

```emacs-lisp
(use-package git-commit
  :custom
  (git-commit-fill-column 78)
  (git-commit-style-convention-checks '(non-empty-second-line overlong-summary-line)))
;; (use-package forge
;;   :after magit)
(use-package magit
  ;; Make the default action a branch checkout, not a branch visit when in branch mode
  :bind (:map magit-branch-section-map
              ([remap magit-visit-thing] . magit-branch-checkout))
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  (magit-repository-directories '(("~/repos" . 1)))
  (magit-submodule-arguments '("--recursive")))
(use-package magit-filenotify)
(use-package magit-find-file)
(use-package git-timemachine)
```


<a id="orgb1f6fd6"></a>

### `diminish`

Handy mode to make the modeline more succinct by allowing a *diminished* mode line string. Sometimes the fact that mode is there
is fine and it doesn't need to be on the mode line (diminish it to ""). Putting diminish first not out of importance, but
because it is used later on.

```emacs-lisp
(use-package diminish :defer t)
```


<a id="org0e41eeb"></a>

### [`which-key`](https://github.com/justbur/emacs-which-key)

Perhaps one of the most useful extensions, this little gem will provide a list in the mini-buffer of the relevant keystrokes and
the functions to which they are bound (or a prefix). Many times I've found unknown features by simply looking at the various
options. This is, IMO, a great way to learn Emacs key-bindings.

```emacs-lisp
(use-package which-key :diminish ""
  :custom
  (which-key-mode t))
(use-package which-key-posframe
  :config
  (set-face-attribute 'which-key-posframe nil :background "wheat1")
  :custom
  (which-key-posframe-mode t)
  (which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-left-corner))
(which-function-mode t)
```


<a id="orga725ca7"></a>

### `projectile`

Perhaps one of the most useful packages - understands `git` repositories by default and makes dealing with project-wide stuff
(like opening files and searching through all project files) much more efficient.

```emacs-lisp
(use-package projectile
  :bind
  (:map projectile-mode-map
        ("C-c p"   . projectile-command-map)        ;; traditional binding
        ("C-z C-p" . projectile-command-map)        ;; my binding
        ("C-z p"   . projectile-command-map))       ;; all paths get to projectile
  :config
  (projectile-mode t))
```


<a id="org75567d0"></a>

### [`company-mode`](http://company-mode.github.io/)

Use the excellent [`company-mode`](http://company-mode.github.io/) modular in-buffer text completion framework.

```emacs-lisp
(use-package company
  :diminish
  :config (global-company-mode 1))
```


<a id="org162bf35"></a>

### [`ivy/swiper`](https://github.com/abo-abo/swiper)

I used to be a `helm` user, but switched to `ivy`. Lots of nice features in `ivy` and very easy to configure comparatively.

```emacs-lisp
(use-package ivy
  :diminish ""
  :bind (:map ivy-minibuffer-map
              ("C-w" . ivy-yank-word) ;; make work like isearch
              ("C-r" . ivy-previous-line))
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil) ;; no regexp by default
  (setq ivy-re-builders-alist         ;; allow input not in order
        '((t . ivy--regex-ignore-order)))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-mode t)
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t))
(use-package counsel
  :bind (("C-z j" . counsel-imenu)))
(use-package counsel-projectile
  :config
  (counsel-projectile-mode t))
(use-package counsel-codesearch)
(use-package ivy-hydra)
(use-package swiper
  :bind (("C-S-s" . isearch-forward)  ;; Keep isearch-forward on Shift-Ctrl-s
         ("C-s" . swiper)             ;; Use swiper for search and reverse search
         ("C-S-r" . isearch-backward) ;; Keep isearch-backward on Shift-Ctrl-r
         ("C-r" . swiper)))
(use-package avy
  :bind (("C-:" . avy-goto-char)))
```


<a id="org10e63ec"></a>

### Use `ivy` and `posframe` together

This makes the ivy completion buffers popup over the modeline instead of in the minibuffer.

```emacs-lisp
(use-package posframe)
(use-package ivy-posframe
  :config
  (set-face-attribute 'ivy-posframe nil :background "wheat1")
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left))))
(ivy-posframe-mode 1)
```

I ran into a nice article that fixes a [problem that I often have with Ivy](http://mbork.pl/2018-06-16_ivy-use-selectable-prompt): using a name that is not in the list of candidates (for
example when trying to write to a buffer to a new file name). To fix this, setting `ivy-use-selectable-prompt` to `t` makes going
back before the first candidate to a "verbatim" prompt.


<a id="orge87b490"></a>

### [`prescient`](https://github.com/raxod502/prescient.el)

[`prescient`](https://github.com/raxod502/prescient.el) provides "simple but effective sorting and filtering for Emacs."

```emacs-lisp
(use-package prescient)
(use-package ivy-prescient)
(use-package company-prescient)
```


<a id="orgaa8920c"></a>

### [`yasnippet`](https://www.emacswiki.org/emacs/Yasnippet)

[`yasnippet`](https://www.emacswiki.org/emacs/Yasnippet) is a truly awesome package. Local modifications should go in `~/.emacs.d/snippets/`.

Just love the [`yasnippet`](https://www.emacswiki.org/emacs/Yasnippet) package. I only wish there were more templates out there. Creating new ones and placing them the
appropriate (mode-named) subdirectory of `~/.emacs.d/snippets/`.

```emacs-lisp
(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :config
  (yas-reload-all)
  (require 'warnings)
  :hook ;; fix tab in term-mode
  (term-mode . (lambda() (yas-minor-mode -1)))
  ;; Fix yas indent issues
  (python-mode . (lambda () (set (make-local-variable 'yas-indent-line) 'fixed))))
(use-package yasnippet-snippets)
(yas-global-mode t)
```

The following code allows the `yasnippet` and `company` to work together. Got this from a fix posted on [github](https://gist.github.com/sebastiencs/a16ea58b2d23e2ea52f62fcce70f4073) which was pointed
to by the [company mode Wiki page](https://www.emacswiki.org/emacs/CompanyMode#toc11).

```emacs-lisp
(defvar my/company-point nil)
(advice-add 'company-complete-common :before (lambda () (setq my/company-point (point))))
(advice-add 'company-complete-common :after (lambda ()
                                              (when (equal my/company-point (point))
                                                (yas-expand))))
```


<a id="orgfc2c009"></a>

### `dired`

Make sure dired is properly configured. Using `:ensure nil` here because the dired package is builtin.

```emacs-lisp
(use-package dired
  :ensure nil
  :config (when (string= system-type "darwin")
            (setq dired-use-ls-dired t
                  insert-directory-program "/usr/local/bin/gls"))
  :custom (dired-listing-switches "-aBhl --group-directories-first"))
```


<a id="org1e13ba9"></a>

### `command-log-mod`

These packages are useful when doing presentations.

```emacs-lisp
(use-package command-log-mode :defer t)
```


<a id="org21f9f9d"></a>

### Docker

I manage a lot of docker stuff. The docker package is quite useful.

```emacs-lisp
(use-package docker)
```


<a id="orge781d49"></a>

### `flycheck`

I've abandoned `flymake` (built-in) with `flycheck` (see [flycheck a flymake replacement](https://www.masteringemacs.org/article/spotlight-flycheck-a-flymake-replacement)).

```emacs-lisp
(use-package flycheck
  :config
  (global-flycheck-mode))
```


<a id="org9bb4fdd"></a>

### yaml-mode

```emacs-lisp
(use-package yaml-mode)
```


<a id="org92a078d"></a>

### [`bind-key`](https://github.com/priyadarshan/bind-key)

Much better binding capabilities (in later versions this is already loaded via `use-package`).

```emacs-lisp
(use-package bind-key :defer t)
```


<a id="org42cc60c"></a>

### [`helpful`](https://github.com/Wilfred/helpful)

[Helpful](https://github.com/Wilfred/helpful) provides contextual help and other features. Here are two blogs that provide good information: [initial Helpful blog](http://www.wilfred.me.uk/blog/2017/08/30/helpful-adding-contextual-help-to-emacs/) and
[Helpful, one year in](http://www.wilfred.me.uk/blog/2018/06/22/helpful-one-year-on/). More in-depth help along with lots of other information like references, edebug capabilities, &#x2026;

```emacs-lisp
(use-package helpful)
```


<a id="orgd1afcad"></a>

### [`savehist`](https://www.emacswiki.org/emacs/SaveHist)

A great built-in that allows us to have a history file. This means certain elements are saved between sessions of Emacs. This
history file is kept in `~/.emacs.d/savehist`. Note that in later versions of Emacs this package is already built-in, so check
the built-ins before issuing the `use-package`. In later versions of Emacs seems the `savehist` package is built-in so ignore
annoying errors.

```emacs-lisp
(unless (package-built-in-p 'savehist)
  (use-package savehist :defer t))
```

Set the following variables to control `savehist` (use customize).

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">savehist-file</td>
<td class="org-left">"~/.emacs.d/savehist"</td>
</tr>


<tr>
<td class="org-left">savehist-additional-variables</td>
<td class="org-left">(kill-ring search-ring regexp-search-ring)</td>
</tr>


<tr>
<td class="org-left">savehist-mode</td>
<td class="org-left">t</td>
</tr>
</tbody>
</table>


<a id="orgb41dec2"></a>

### Very large files

Since I deal with potentially gigantic log files, this package allows the file to be carved up and 'paged' through. Get to the
`vlf` stuff through the default prefix `C-c C-v`.

```emacs-lisp
(use-package vlf
  :defer t
  :pin melpa)
```

I got the `vlf` package from a [really good paper](https://writequit.org/articles/working-with-logs-in-emacs.html) on how to use Emacs to deal with logs. If you currently or are going to deal
with logs in your day to day, then this article is invaluable. I've yet to adopt some of the other features described by the
article but I have no need as of yet. Soon maybe.


<a id="org31a9ab3"></a>

### Logs in general

Here's a [great article](https://writequit.org/articles/working-with-logs-in-emacs.html) on dealing with log files. I've stolen some if it here.

First make all the logs read-only (view) mode.

```emacs-lisp
(use-package view
  :ensure t
  :config
  (defun View-goto-line-last (&optional line)
    "goto last line"
    (interactive "P")
    (goto-line (line-number-at-pos (point-max))))
  (define-key view-mode-map (kbd ">") 'View-goto-line-last))

(use-package log4j-mode
  :ensure t
  :init
  (add-hook #'log4j-mode-hook #'view-mode)
  (add-hook #'log4j-mode-hook #'auto-revert-tail-mode)
  (add-hook #'log4j-mode-hook #'read-only-mode))
```


<a id="orgb0d45bb"></a>

### Random packages

OK, a little tired of documenting each package on it's own. These packages are just generally useful. Some of these packages
have become so useful that they've found their way into the list of Emacs built-in packages. In those cases, the package is
checked here against the list of built-ins to avoid warnings when loading a later version of Emacs.

```emacs-lisp
(use-package groovy-mode
  :custom
  (groovy-indent-offset 2))
(use-package plantuml-mode)
(use-package realgud)           ;; A "better" gud
(use-package ibuffer-projectile :defer t)
(use-package xterm-color :defer t)
(unless (package-built-in-p 'sh-script)
  (use-package sh-script :defer t))
(unless (package-built-in-p 'desktop)
  (use-package desktop))
(set-variable 'desktop-path (cons default-directory desktop-path))
(desktop-save-mode t)
(use-package lispy
  :hook
  (emacs-lisp-mode . (lambda () (lispy-mode 1)))
  (minibuffer-setup . (lambda () (when (eq this-command 'eval-expression) (lispy-mode 1)))))
(use-package default-text-scale                     ;; text-scale on steroids - for all windows C-M-- and C-M-=
  :bind (("C-M--" . default-text-scale-decrease)
         ("C-M-=" . default-text-scale-increase)))
(when (string-match "windows" (symbol-name system-type))
  (use-package powershell))
```

Note that the setting of `desktop-path` allows the multiple `.emacs.desktop` files, each in the directory where `emacs` was
started. Although `desktop-path` is changed outside `custom.el`, I've included it here in the table below so you can see that
the default is augmented with the start-up directory which in this case is `~/.emacs.d`.


<a id="orgc731f4b"></a>

## [`org-mode`](https://orgmode.org/)

I've split out this `org-mode` section because of the customization that was necessary to make exporting this module and the
various customized variable tables to output  nicely.

Always put [`org-mode`](https://orgmode.org/) buffers into [`flyspell-mode`](https://www.emacswiki.org/emacs/FlySpell) for live spell checking.

The `htmlize` package allows the HTML and Markdown exporters to work (underlying code). This also provides language-specific
colorization to be present in the export HTML file.

```emacs-lisp
(use-package org-bullets)
(add-hook 'org-mode-hook  (lambda ()
                            (toggle-truncate-lines -1)
                            (auto-fill-mode 1)
                            (org-bullets-mode)
                            (flyspell-mode 1)))

(use-package org-autolist)
(use-package htmlize)
;; Not using the powerpoint generation right now...
;; (use-package ox-reveal)
;; (require 'ox-reveal)
```

I've started using `ox-reveal` for generating presentations from `org-mode`. Here's a [good article](https://opensource.com/article/18/2/how-create-slides-emacs-org-mode-and-revealjs) on getting started. I've set
the `org-reveal-root` to point to <http://cdn.jsdelivr.net/reveal.js/3.0.0/> so that you do not need to install it on your system.
If you want to use your own customized theme, see the instructions at <https://github.com/hakimel/reveal.js/>. NB: I have removed
`ox-reveal` from the normal package load because it has a dependency on the `org` package, but we already install
`org-plus-contrib` which `ox-reveal`, I guess, doesn't recognize. Leaving the code here to make it easy to bring in if you are
working with reveal.js and presentations.

Customized variables for org-mode:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">org-catch-invisible-edits</td>
<td class="org-left">show</td>
</tr>


<tr>
<td class="org-left">org-html-postamble</td>
<td class="org-left">t</td>
</tr>


<tr>
<td class="org-left">org-html-postamble-format</td>
<td class="org-left">(("en" "&lt;p class=\"author\"&gt;Author: %a (%e)&lt;/p&gt;\n&lt;p class=\"date\"&gt;Date: %T&lt;/p&gt;\n&lt;p class=\"creator\"&gt;%c&lt;/p&gt;"))</td>
</tr>


<tr>
<td class="org-left">org-log-done</td>
<td class="org-left">time</td>
</tr>


<tr>
<td class="org-left">org-log-into-drawer</td>
<td class="org-left">t</td>
</tr>
</tbody>
</table>


<a id="orgb89b80a"></a>

### For blogging

I'm using [org2blog](https://github.com/org2blog/org2blog) to blog from org mode. This is a pretty comprehensive package and I'm using the `~/.netrc` file to provide
credentials so there is the setup of `org2blog/wp-blog-alist` here (according to the instructions on GitHub).

```emacs-lisp
(use-package org2blog)
(require 'auth-source)

(let* ((credentials (auth-source-user-and-password "myblog"))
       (username (nth 0 credentials))
       (password (nth 1 credentials))
       (config `(("myblog"
                 :url "http://adamhitstheroad.com/xmlrpc.php"
                 :username ,username
                 :password ,password))))
  (setq org2blog/wp-blog-alist config))
```


<a id="orgd5eab22"></a>

### Use of babel

To do literate programming you need to include the languages to "tangle". Here I've added more than just the standard
`emacs-lisp` value. Added Python, [PlantUML](http://plantuml.com/), and shell.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">org-babel-load-languages</td>
<td class="org-left">((shell . t)<br>&nbsp;(plantuml . t)<br>&nbsp;(python . t)<br>&nbsp;(emacs-lisp . t))</td>
</tr>
</tbody>
</table>


<a id="org0bb35e7"></a>

## Language support

This section covers the various language support features of this configuration.


<a id="orge5fca60"></a>

### Taking the `eglot` plunge

The [`eglot`](https://github.com/joaotavora/eglot) package is a [language server protocol](https://microsoft.github.io/language-server-protocol/) (LSP) client for Emacs that supports many languages out-of-the-box. After
spending time with `elpy` and other Emacs implementations of LSP clients, it turns out `eglot` is very multipurpose with minimal
configuration.

```emacs-lisp
(use-package eglot
  :pin melpa
  :config
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1))))
```


<a id="orgb37e1a8"></a>

### Python

Now with `eglot` this is pretty straight forward configuration.

```emacs-lisp
(use-package pylint)
(use-package python-docstring
  :config
  (python-docstring-install))
(use-package python
  :bind (:map python-mode-map
              ("C-c C-p" .  flycheck-previous-error)
              ("C-c C-n" . flycheck-next-error))
  :hook
  (python-mode . (lambda ()
                   (eglot-ensure)
                   (company-mode)))
  :custom
  (python-flymake-command '("flake8" "-"))
  (python-indent-trigger-commands '(yas-expand))
  (python-shell-completion-native-disabled-interpreters '("pypy" "ipython" "jupyter"))
  (python-shell-interpreter "jupyter")
  (python-shell-interpreter-args "console --simple-prompt")
  (python-shell-prompt-detect-failure-warning nil)
  (python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
  (python-shell-prompt-regexp "In \\[[0-9]+\\]: "))
```


<a id="org79fb906"></a>

## Additional bits-o-configuration


<a id="orgec2b961"></a>

### Limit the length of [`which-function`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Which-Function.html)

[`which-function`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Which-Function.html) which is used on the mode-line has no maximum method/function signature. This handy adviser limits the name to
64 characters.

```emacs-lisp
(defcustom  my/which-function-max-width 64
  "The maximum width of the which-function string."
  :group 'my-configuration
  :type 'integer)
(advice-add #'which-function :filter-return
            (lambda (s) (when (stringp s)
                          (if (< (string-width s) my/which-function-max-width) s
                            (concat (truncate-string-to-width s (- my/which-function-max-width 3)) "...")))))
```


<a id="org5381f31"></a>

### `my-ansi-term`

Allows me to name my ANSI terms. Was very useful when I used more ANSI shells (so that tabs were interpreted by the shell). Some
other modes and shells make this less useful these days.

```emacs-lisp
(cl-defun my/ansi-term (term-name cmd)
  "Create an ansi term with a name - other than *ansi-term* given TERM-NAME and CMD."
  (interactive "sName for terminal: \nsCommand to run [/bin/bash]: ")
  (ansi-term (if (= 0 (length cmd)) "/bin/zsh" cmd))
  (rename-buffer term-name))
```


<a id="org6741eee"></a>

### Understand file type by shebang

When a file is opened and it is determined there is no mode (fundamental-mode) this code reads the first line of the file looking
for an appropriate shebang for either python or bash and sets the mode for the file.

```emacs-lisp
(cl-defun my-find-file-hook ()
  "If `fundamental-mode', look for script type so the mode gets properly set.
Script-type is read from #!/... at top of file."
  (if (eq major-mode 'fundamental-mode)
      (ignore-errors
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^#!\s*/.*/\\(python\\|bash\\|sh\\).*$")
            (if (string= (match-string 1) "python")
                (python-mode)
              (sh-mode))))))
(add-hook 'find-file-hook #'my-find-file-hook)
```


<a id="org8330a97"></a>

### React to screen width changes for `posframe`

Because I use `posframe` quite a bit now (so that the mini-buffer doesn't continue to change sizes, which I find a little
distracting), this code reacts to the width changes and will set the custom variables accordingly.

```emacs-lisp
(cl-defun my/window-size-change (&optional _)
  "My very own resize defun for modifying the posframe size."
  (unless (= (window-pixel-width-before-size-change) (window-pixel-width))
    (let ((body-width (window-body-width)))
      (set-variable 'ivy-posframe-width body-width)
      (set-variable 'ivy-posframe-min-width body-width)
      (set-variable 'which-key-posframe-width body-width)
      (set-variable 'which-key-posframe-min-width body-width))))
(add-hook 'window-size-change-functions 'my/window-size-change)
```


<a id="orgfb9d8cf"></a>

## Final (random) bits

Setup `eldoc` mode, use `y-or-n-p` instead of `yes-or-no-p`. Key bindings&#x2026;

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)   ;; Run elisp with eldoc-mode
(diminish 'eldoc-mode "Doc")                    ;; Diminish eldoc-mode
(fset #'list-buffers #'ibuffer)                 ;; prefer ibuffer over list-buffers
(fset #'yes-or-no-p #'y-or-n-p)                 ;; for lazy people use y/n instead of yes/no
;; Some key bindings
(bind-key "C-x p" #'pop-to-mark-command)
(bind-key "C-h c" #'customize-group)
(bind-key "C-=" #'text-scale-increase)          ;; because it's the + key too and agrees with default-text-scale
(bind-key "C--" #'text-scale-decrease)
(bind-key "<up>" #'enlarge-window ctl-x-map)    ;; note: C-x
(bind-key "<down>" #'shrink-window ctl-x-map)   ;; note: C-x
(bind-key "C-z" 'nil ctl-x-map)                 ;; get rid of annoying minimize "\C-x\C-z"
(setq-default ediff-ignore-similar-regions t)   ;; Not a variable but controls ediff
;; Enable some stuff that's normally disabled
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
;; add pom file to xml type
 (add-to-list 'auto-mode-alist '("\\.pom\\'" . xml-mode))
```


<a id="org2198f29"></a>

# `init.el`, short and sweet

Everything above here is loaded by `init.el`

```emacs-lisp
11  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
12  (load custom-file t)
13  (prefer-coding-system 'utf-8)
14  (package-initialize)
15  (unless (assoc 'use-package package-archive-contents)
16    (package-refresh-contents)
17    (package-install (elt (cdr (assoc 'org-plus-contrib package-archive-contents)) 0))
18    (package-install (elt (cdr (assoc 'use-package package-archive-contents)) 0)))
19  (assoc-delete-all 'org package--builtins)
20  (setq use-package-enable-imenu-support t)
21  (require 'use-package)
22  (use-package org)
23  (defcustom my/cfg-file (concat user-emacs-directory "README")
24    "The base name for the .org file to use for Emacs initialization."
25    :group 'my-configuration
26    :type 'string)
27  (when (file-newer-than-file-p (concat my/cfg-file ".org") (concat my/cfg-file ".el"))
28    (org-babel-tangle-file (concat my/cfg-file ".org")))
29  (load my/cfg-file)
```

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-right" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-right">Line</th>
<th scope="col" class="org-left">Explained</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-right">11</td>
<td class="org-left">Breaking the custom setting out into its own file allows it to be loaded in the next line</td>
</tr>


<tr>
<td class="org-right">12</td>
<td class="org-left">Load the custom file. All custom settings will now be honored by packages when loaded.</td>
</tr>


<tr>
<td class="org-right">13</td>
<td class="org-left">Just a fix for ELPA packages with (certain?) Unicode characters in them.</td>
</tr>


<tr>
<td class="org-right">14</td>
<td class="org-left">Initialize the package system.Reloading this doesn't seem to have a downside (once loaded this seems to not matter).</td>
</tr>


<tr>
<td class="org-right">15</td>
<td class="org-left">If we've not load loaded the definition for the <code>use-package</code> ELPA package, then</td>
</tr>


<tr>
<td class="org-right">16</td>
<td class="org-left">Refresh (read) the package archives. Note: <code>'package-archives</code> from <code>custom.el</code> <b>key</b> here</td>
</tr>


<tr>
<td class="org-right">17</td>
<td class="org-left">Load up org-mode. The pre-packaged org-mode does not have Babel!</td>
</tr>


<tr>
<td class="org-right">18</td>
<td class="org-left">Load up use-package. Used for all package loading</td>
</tr>


<tr>
<td class="org-right">19</td>
<td class="org-left">We'll use org from the proper sources, thank you very much (not the builtin version if any)</td>
</tr>


<tr>
<td class="org-right">20</td>
<td class="org-left">Set the variable to allow <code>use-package</code> to use counsel for imenus</td>
</tr>


<tr>
<td class="org-right">21</td>
<td class="org-left">Require <code>use-package</code></td>
</tr>


<tr>
<td class="org-right">22</td>
<td class="org-left">Make sure <code>use-package</code> is available</td>
</tr>


<tr>
<td class="org-right">23</td>
<td class="org-left">Make sure <code>org</code> is available</td>
</tr>


<tr>
<td class="org-right">24</td>
<td class="org-left">Create customizable config variable</td>
</tr>


<tr>
<td class="org-right">25</td>
<td class="org-left">&#x2026; it's nice having a customizable</td>
</tr>


<tr>
<td class="org-right">26</td>
<td class="org-left">&#x2026;    group for personal configuration settings</td>
</tr>


<tr>
<td class="org-right">27</td>
<td class="org-left">If the .el file doesn't exist or is older than this file then&#x2026;</td>
</tr>


<tr>
<td class="org-right">28</td>
<td class="org-left">&#x2026; create the tangled output of this file</td>
</tr>


<tr>
<td class="org-right">29</td>
<td class="org-left">Load the tangled output of this file</td>
</tr>
</tbody>
</table>

One of the nicest things about Emacs is the extensive and quite useful customization engine. You can customize variables and
faces with ease and make the settings work for you.

Loading the customized variables before the package (using `use-package` of course) means that you can now use the customization
facility in Emacs to modify the variables and have them stick between Emacs invocations. I see lots of configurations with the
`:config` section of a `use-package` invocation performing variable setting. The problem with this is that if you want to change
it using Emacs, the loading of the customizations first causes your new customizations getting overwritten on the next start of
Emacs. You can do it in the reverse order; `use-package` first, then customize, but then you couldn't customize the variable
using the customization system.

Therefore trying to minimize `use-package` customizations and mostly using the `custom.el` file play much nicer with the
[Emacs customization package](https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html).

The most important custom variable for this configuration is `package-archives`, which is used by the loading of the various
extension packages used by this configuration.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">package-archives</td>
<td class="org-left">(("gnu" . "<a href="https://elpa.gnu.org/packages/">https://elpa.gnu.org/packages/</a>")<br>&nbsp;("melpa" . "<a href="https://melpa.org/packages/">https://melpa.org/packages/</a>")<br>&nbsp;("org" . "<a href="https://orgmode.org/elpa/">https://orgmode.org/elpa/</a>"))</td>
</tr>


<tr>
<td class="org-left">use-package-enable-imenu-support</td>
<td class="org-left">t</td>
</tr>
</tbody>
</table>


<a id="org0ae5e36"></a>

## [`org-mode`](https://orgmode.org/) export hacks for HTML and Markdown

I export into markdown for github. I do not use the `ox-gfm` package because when I tried it, it modified the source file because
of this file's use of the `#+CALL` construct (each call adds the table to the source file). So I use the built in `ox-md`
exporter. However, it just indents the code blocks rather put the `` ```emacs-lisp `` code snippet prefix and `` ``` `` postfix but
rather just indents. First we load the library so it turns up in the export menu (`C-x C-e`). Then we override the output method
for the code.

```emacs-lisp
(load-library "ox-md")

(cl-defun org-md-example-block (example-block _contents info)
  "My modified: Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (concat "```emacs-lisp\n"
          (org-remove-indentation
           (org-export-format-code-default example-block info))
          "```\n"))
```

To support the using of dynamic custom vars table using the library of Babel, the export text for Markdown and HTML goes through
`orgtbl-to-orgtbl` which turns the list returned in the an org-mode table. After `orgtbl-to-orgtbl`, the `htmlize` package turns
it into a HTML table. The adviser changes all the spaces after a `<br>` into `&nbsp;` entities and surrounds them with inline
HTML. This is necessary because `orgtbl-to-orgtbl` strips text between the `@@` used to inline HTML. The adviser also protects
any underscores in the table with inline HTML.

```emacs-lisp
(cl-defun my-md-export-hack(text)
  "Fix up md export on writing my README.org file.

Converts a <br> followed by zero or more spaces into inline html format.
For example: an in put of \"hello<br>there<br> my<br>  friend<br>\" becomes
\"hello@@html:<br>@@there@@html:<br>&nbsp;\
  @@my@@html:<br>&nbsp;&nbsp;@@friend@@html:<br>@@\"
This function also adds inline HTML around '_' in the TEXT."
  (when (stringp text)
    (let ((result text)
          (replacements '(("<br>\[[:space:]\]*" (lambda (match)
                                                  (concat "@@html:<br>"
                                                          (apply 'concat (make-list (- (length match) 4) "&nbsp;"))
                                                          "@@")))
                          ("\"\\(https?:\[^\"\]*\\)" "\"@@html:<a href=\"\\1\">\\1</a>@@")
                          ("_" "@@html:_@@")
                          ("<\\(p.*?\\)>" "@@html:&lt;\\1&gt;@@")
                          ("</p>" "@@html:&lt;/p&gt;@@"))))
      (cl-loop for rep in replacements do
               (setq result (replace-regexp-in-string (nth 0 rep) (nth 1 rep) result)))
      result)))

(advice-add #'orgtbl-to-orgtbl :filter-return #'my-md-export-hack)
```

A post-amble to make the tangled `.el` file has no errors/warnings.

```emacs-lisp
;;; README.el ends here
```

