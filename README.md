
# Table of Contents

1.  [Overview](#orgac24169)
    1.  [Quick start](#org350a7da)
    2.  [Why a literate configuration](#org07798fc)
    3.  [`init.el`, short and sweet](#org43a081f)
        1.  [Loading the custom file first](#org0f13cb9)
        2.  [Customizing the configuration](#orgff23e64)
2.  [My Configuration](#org8bec792)
    1.  [A preamble](#org60275be)
    2.  [General packages](#org95fa3c4)
        1.  [Speed up line movement](#org3b9db48)
        2.  [Use Ctrl-Z for personal bindings](#orgdddc460)
        3.  [`diminish`](#orga70b4e0)
        4.  [`bind-key`](#org5928d36)
        5.  [`helpful`](#orgc6492d0)
        6.  [`savehist`](#org3115ce2)
        7.  [Themes and mode line](#orge19888b)
        8.  [For demonstrations](#orgb7b3229)
        9.  [Trying `codesearch`](#org31c1464)
        10. [`which-key`](#org203859c)
        11. [Other useful packages](#orgccf0d16)
        12. [A little nXML configuration](#orgfa47563)
3.  [`company-mode` Configuration](#org70a3730)
4.  [`ivy/swiper` Configuration](#org08300bf)
5.  [`prescient` Configuration](#orga602dc4)
6.  [`yasnippet` Configuration](#org34c00e2)
7.  [Working with C#](#orgf27960f)
8.  [`magit`/git configuration](#org50b5dbe)
9.  [`org-mode` Configuration](#orgd1cd7dd)
    1.  [`org-mode` export hacks for HTML and Markdown](#org8e26a04)
    2.  [Use of babel](#orga679208)
10. [python configuration](#org37ce64a)
    1.  [The tale of two IDEs](#org3005428)
        1.  [`elpy` IDE](#org960a030)
        2.  [`lsp-mode` IDE](#org22d9d80)
    2.  [Python IDE-agnostic configuration](#orgc74d1b0)
11. [Additional bits-o-configuration](#org4693983)
    1.  [Limit the length of `which-function`](#org8a22dfc)
    2.  [`my-ansi-term`](#org9ecd610)
    3.  [Understand file type by shebang](#orgafce9ba)
    4.  [Additional Configuration](#org32ed824)



<a id="orgac24169"></a>

# Overview

This is my literate and <font color=red size=+3><b><u>portable</u></b></font> Emacs initialization.


<a id="org350a7da"></a>

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


<a id="org07798fc"></a>

## Why a literate configuration

Well mostly I wanted to learn how to do it, but also I was having issues with managing my initialization/configuration. FWIW
this is the approach that I came up with. My  `init.el` file is simple and relies on the fact that customizations are saved to
its own file and that these customizations are read in before the packages are loaded.

What I've gotten out of all this work is a portable and documented configuration that works well for me. Please feel free to
take whatever portion(s) you wish from this and make it your own.

I have tried to make this configuration 100% portable meaning that on a new system (Linux or Windows at this point) with Emacs
installed. I simple git clone this repository to `~/.emacs.d` and then fire up Emacs. Should work every time. 


<a id="org43a081f"></a>

## `init.el`, short and sweet

```emacs-lisp
11  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
12  (load custom-file t)
13  (prefer-coding-system 'utf-8)
14  (unless (boundp 'package-user-dir)
15    (unless (boundp 'package-archive-contents)
16      (package-initialize))
17    (unless (assoc 'use-package package-archive-contents)
18      (package-refresh-contents)
19      (package-install (elt (cdr (assoc 'org-plus-contrib package-archive-contents)) 0))
20      (package-install (elt (cdr (assoc 'use-package package-archive-contents)) 0))))
21  (load "use-package")
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
<td class="org-left">If there's no `'package-user-dir` defined; `package.el` isn't loaded</td>
</tr>


<tr>
<td class="org-right">15</td>
<td class="org-left">If there's no `'package-archive-contents` defined; package archives have not been read</td>
</tr>


<tr>
<td class="org-right">16</td>
<td class="org-left">Initialize the package system</td>
</tr>


<tr>
<td class="org-right">17</td>
<td class="org-left">If we've not load loaded the definition for the `use-package` ELPA package, then</td>
</tr>


<tr>
<td class="org-right">18</td>
<td class="org-left">Refresh (read) the package archives. Note: `'package-archives` from `custom.el` **key** here</td>
</tr>


<tr>
<td class="org-right">19</td>
<td class="org-left">Load up the org-mode. The pre-packaged org-mode does not have Babel!</td>
</tr>


<tr>
<td class="org-right">20</td>
<td class="org-left">Load up `use-package`</td>
</tr>


<tr>
<td class="org-right">21</td>
<td class="org-left">Make sure `use-package` is available</td>
</tr>


<tr>
<td class="org-right">22</td>
<td class="org-left">Make sure `org-plus-contrib` is available</td>
</tr>


<tr>
<td class="org-right">23</td>
<td class="org-left">Create customizable config variable</td>
</tr>


<tr>
<td class="org-right">24</td>
<td class="org-left">&#x2026;</td>
</tr>


<tr>
<td class="org-right">25</td>
<td class="org-left">&#x2026; it's nice having a customizable group for personal configuration settings</td>
</tr>


<tr>
<td class="org-right">26</td>
<td class="org-left">&#x2026;</td>
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


<a id="org0f13cb9"></a>

### Loading the custom file first

One of the nicest things about Emacs is the extensive and quite useful customization engine. You can customize variables and
faces with ease and make the settings work for you.

Loading the customized variables before the package (using `use-package` of course) means that you can now use the customization
facility in Emacs to modify the variables and have them stick between Emacs invocations. I see lots of configurations with the
`:config` section of a `use-package` invocation performing variable setting. The problem with this is that if you want to change
it using Emacs, the loading of the customizations first causes your new customizations getting overwritten on the next start of
Emacs. You can do it in the reverse order; `use-package` first, then customize, but then you couldn't customize the variable
using the customization system.

Therefore I try to minimize `use-package` customizations and mostly use the `custom.el` file.

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
</tbody>
</table>


<a id="orgff23e64"></a>

### Customizing the configuration

I've started to use the `defcustom` function to describe those "variables that impact initialization" and are all placed into
the `'my-configuration` group. Each of the configuration variables can be accessed using `M-x customize-group my-configuration`.
This allows me to select features to turn on or off selectively and make them sticky if so desired.

Another reason to load the customization file first.


<a id="org8bec792"></a>

# My Configuration

Here are my configuration bits. All of the following code snippets are tangled from this file into an `.el` file that gets loaded
from the initialization file. Feel free to take as little or as much as you like from here.


<a id="org60275be"></a>

## A preamble

This is a little piece of code that I picked up that might make things faster when downloading and installing all the packages.
This turns down the garbage collector during the use-package loading when it has to do some compiling. Set it back when done with
init.

```emacs-lisp
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
```


<a id="org95fa3c4"></a>

## General packages

Here are some general packages I use


<a id="org3b9db48"></a>

### Speed up line movement

I ran into this little tidbit while reading Sacha Chua's posts from Emacs. It is described [here](https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746), but to summarize the
`next-line` defun triggers `line-move-partial` which leads to excessive processing. By setting the variable here, the speed of
using `next-line` gets very cut down.

```emacs-lisp
(setq auto-window-vscroll nil)
```


<a id="orgdddc460"></a>

### Use Ctrl-Z for personal bindings

Ctrl-c was supposed to be left for personal customization but seems to get used more than it should,
therefore I've started to bind things to Ctrl-Z, which had the annoying and useless minimize functionality.

```emacs-lisp
(bind-keys :map global-map                      ;; get rid of pesky "\C-z" and use for personal bindings
           :prefix-map my-ctrl-z-prefix-map
           :prefix "C-z"
           ("C-d" . dired-jump)
           ("n"   . linum-mode)
           ("g"   . magit-status)
           ("r"   . revert-buffer)
           ("t"   . toggle-truncate-lines)
           ("c"   . comment-region)
           ("u"   . uncomment-region))
```


<a id="orga70b4e0"></a>

### [`diminish`](https://github.com/myrjola/diminish.el)

Handy mode to make the modeline more succinct by allowing a *diminished* mode line string. Sometimes the fact that mode is there
is fine and it doesn't need to be on the mode line (diminish it to "").

```emacs-lisp
(use-package diminish)
```


<a id="org5928d36"></a>

### [`bind-key`](https://github.com/priyadarshan/bind-key)

Much better binding capabilities (in later versions this is already loaded via `use-package`).

```emacs-lisp
(use-package bind-key)
```


<a id="orgc6492d0"></a>

### [`helpful`](https://github.com/Wilfred/helpful)

[Helpful](https://github.com/Wilfred/helpful) provides contextual help and other features. Here are two blogs that provide good information: [initial Helpful blog](http://www.wilfred.me.uk/blog/2017/08/30/helpful-adding-contextual-help-to-emacs/) and
[Helpful, one year in](http://www.wilfred.me.uk/blog/2018/06/22/helpful-one-year-on/). More in-depth help along with lots of other information like references, edebug capabilities, &#x2026;

```emacs-lisp
(use-package helpful
  :bind (("C-z h a" . helpful-at-point)
         ("C-z h c" . helpful-command)
         ("C-z h C" . helpful-callable)
         ("C-z h f" . helpful-function)
         ("C-z h k" . helpful-key)
         ("C-z h m" . helpful-macro)
         ("C-z h v" . helpful-variable)))
```


<a id="org3115ce2"></a>

### [`savehist`](https://www.emacswiki.org/emacs/SaveHist)

A great built-in that allows us to have a history file. This means certain elements are saved between sessions of Emacs. This
history file is kept in `~/.emacs.d/savehist`.

```emacs-lisp
(use-package savehist)
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


<a id="orge19888b"></a>

### Themes and mode line

Recently switched from `powerline` to `moody`. The `moody` interface gives a nice tabbed mode line. I considered making my own
version of the `leuven` template to avoid all this `set-face-attribute` stuff below, but this is much more compact than mucking
around with the themes. Perhaps someday.

```emacs-lisp
(use-package leuven-theme
  :config
  (load-theme 'leuven t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line nil
                        :overline line
                        :box nil
                        :foreground "deep sky blue")
    (set-face-attribute 'mode-line-inactive nil
                        :overline line
                        :underline line
                        :box nil
                        :foreground "#335EA8"
                        :background "gray64")
    (set-face-attribute 'mode-line-buffer-id nil
                        :foreground "firebrick3"
                        :overline line)
    (set-face-attribute 'mode-line-highlight nil
                        :foreground "gold")
    (set-face-attribute 'mode-line-emphasis nil
                        :foreground "gold")
    (set-face-attribute 'compilation-error nil        ;; red on blue is hard to read
                        :background "gray64")

    (set-face-attribute 'which-func nil
                        :foreground "light goldenrod")))
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))
```


<a id="orgb7b3229"></a>

### For demonstrations

These packages are useful when doing presentations.

```emacs-lisp
(use-package command-log-mode :defer t)
```


<a id="org31c1464"></a>

### Trying [`codesearch`](https://github.com/abingham/emacs-codesearch)

`Codesearch` is Google tool written in Go. You'll need to [install Go](https://golang.org/doc/install) on your system. Install [`codesearch`](https://github.com/abingham/emacs-codesearch) by issuing the command
`go get github.com/google/codesearch/cmd/...`.

```emacs-lisp
(use-package codesearch :defer t)
```


<a id="org203859c"></a>

### [`which-key`](https://github.com/justbur/emacs-which-key)

Perhaps one of the most useful extensions, this little gem will provide a list in the mini-buffer of the relevant keystrokes and
the functions to which they are bound (or a prefix). Many times I've found unknown features by simply looking at the various
options. This is, IMO, a great way to learn Emacs key-bindings.

```emacs-lisp
(use-package which-key :diminish "")
```


<a id="orgccf0d16"></a>

### Other useful packages

OK, a little tired of documenting each package on it's own. These packages are just generally useful.

```emacs-lisp
(use-package realgud)           ;; A "better" gud
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-z C-p") 'projectile-command-map)
  (projectile-mode t))
(use-package ibuffer-projectile :defer t)
(use-package xterm-color :defer t)
(use-package sh-script :defer t)
(use-package desktop
  :config
  (set-variable 'desktop-path (cons default-directory desktop-path)))
(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook (lambda () (when (eq this-command 'eval-expression) (lispy-mode 1)))))

(use-package powershell
  :if mswindows-p)
```

Note that the setting of `desktop-path` allows the multiple `.emacs.desktop` files, each in the directory where `emacs` was
started. Although `desktop-path` is changed outside `custom.el`, I've included it here in the table below so you can see that
the default is augmented with the start-up directory which in this case is `~/.emacs.d`.

Customized variables of interest here:

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
<td class="org-left">desktop-path</td>
<td class="org-left">("`/repos/rally/src/" "~/.emacs.d/" "`")</td>
</tr>


<tr>
<td class="org-left">desktop-save-mode</td>
<td class="org-left">t</td>
</tr>
</tbody>
</table>


<a id="orgfa47563"></a>

### A little nXML configuration

It ends up that the XML I've been editing for work is indented with tabs. This little tidbit should set it up correctly.

```emacs-lisp
(add-hook 'nxml-mode-hook (lambda () (setq nxml-child-indent 8 indent-tabs-mode t)))
```


<a id="org70a3730"></a>

# [`company-mode`](http://company-mode.github.io/) Configuration

Use the excellent [`company-mode`](http://company-mode.github.io/) modular in-buffer text completion framework. In particular this is used for [`elpy`](https://github.com/jorgenschaefer/elpy) (python) mode and
[`omnisharp`](https://github.com/OmniSharp/omnisharp-emacs) (C#) mode, although it is used elsewhere.

```emacs-lisp
(use-package company
  :diminish)
```


<a id="org08300bf"></a>

# [`ivy/swiper`](https://github.com/abo-abo/swiper) Configuration

I used to be a `helm` user, but switched to `ivy`. Lots of nice features in `ivy` and very easy to configure comparatively.

```emacs-lisp
(use-package ivy
  :diminish ""
  :bind (:map ivy-minibuffer-map
              ("C-w" . ivy-yank-word)           ;; make work like isearch
              ("C-r" . ivy-previous-line))
  :config
  (ivy-mode 1)
  (setq ivy-initial-inputs-alist nil)           ;; no regexp by default
  (setq ivy-re-builders-alist                   ;; allow input not in order
        '((t . ivy--regex-ignore-order))))
(use-package counsel
  :bind (("C-z j" . counsel-imenu)))
(use-package counsel-projectile
  :config
  (counsel-projectile-mode t))
(use-package counsel-codesearch)
(use-package ivy-hydra)
(use-package swiper
  :bind (("C-S-s" . isearch-forward)            ;; Keep isearch-forward on Shift-Ctrl-s
         ("C-s" . swiper)                       ;; Use swiper for search and reverse search
         ("C-S-r" . isearch-backward)           ;; Keep isearch-backward on Shift-Ctrl-r
         ("C-r" . swiper)))
(use-package avy
  :bind (("C-:" . avy-goto-char))) 
(use-package ivy-posframe
  :if (>= emacs-major-version 26)
  :config (setq ivy-display-function #'ivy-posframe-display))
```

I ran into a nice article that fixes a [problem that I often have with Ivy](http://mbork.pl/2018-06-16_ivy-use-selectable-prompt): using a name that is not in the list of candidates (for
example when trying to write to a buffer to a new file name). To fix this, setting `ivy-use-selectable-prompt` to `t` makes going
back before the first candidate to a "verbatim" prompt.

Customized variables:

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
<td class="org-left">ivy-count-format</td>
<td class="org-left">"(%d/%d) "</td>
</tr>


<tr>
<td class="org-left">ivy-height</td>
<td class="org-left">10</td>
</tr>


<tr>
<td class="org-left">ivy-mode</td>
<td class="org-left">t</td>
</tr>


<tr>
<td class="org-left">ivy-use-selectable-prompt</td>
<td class="org-left">t</td>
</tr>


<tr>
<td class="org-left">ivy-use-virtual-buffers</td>
<td class="org-left">t</td>
</tr>
</tbody>
</table>


<a id="orga602dc4"></a>

# [`prescient`](https://github.com/raxod502/prescient.el) Configuration

[`prescient`](https://github.com/raxod502/prescient.el) provides "simple but effective sorting and filtering for Emacs."

```emacs-lisp
(use-package prescient)
(use-package ivy-prescient)
(use-package company-prescient)
```


<a id="org34c00e2"></a>

# [`yasnippet`](https://www.emacswiki.org/emacs/Yasnippet) Configuration

[`yasnippet`](https://www.emacswiki.org/emacs/Yasnippet) is a truly awesome package. Local modifications should go in `~/.emacs.d/snippets/`.

Just love the [`yasnippet`](https://www.emacswiki.org/emacs/Yasnippet) package. I only wish there were more templates out there. Creating new ones and placing them the
appropriate (mode-named) subdirectory of `~/.emacs.d/snippets/`.

```emacs-lisp
(use-package warnings)
(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :config
  (yas-reload-all)
  ;; fix tab in term-mode
  (add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))
  ;; Fix yas indent issues
  (add-hook 'python-mode-hook (lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
  ;; Setup to allow for yasnippets to use code to expand
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))
(use-package yasnippet-snippets)
```

The following code allows the `yasnippet` and `company` to work together. Got this from a fix posted on [github](https://gist.github.com/sebastiencs/a16ea58b2d23e2ea52f62fcce70f4073) which was pointed
to by the [company mode Wiki page](https://www.emacswiki.org/emacs/CompanyMode#toc11).

```emacs-lisp
(advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
(advice-add 'company-complete-common :after (lambda ()
                                              (when (equal my-company-point (point))
                                                (yas-expand))))
```

Customizations of interest:

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
<td class="org-left">yas-global-mode</td>
<td class="org-left">t</td>
</tr>
</tbody>
</table>


<a id="orgf27960f"></a>

# Working with C#

I'm a C# developer and pretty much dislike big edits using Visual Studio. I've spent some amount of time coming
up with a good C# configuration. This works spectacularly well and takes only minutes to setup.

There are comprehensive directions at [`omnisharp-emacs`](https://github.com/OmniSharp/omnisharp-emacs.git) for using omnisharp.

```emacs-lisp
(defcustom my/use-omnisharp t
  "Control whether or not to load omnisharp"
  :group 'my-configuration
  :type 'boolean)

(use-package omnisharp
  :if my/use-omnisharp
  :diminish " \u221e" ;; infinity symbol
  :demand t
  :bind (:map omnisharp-mode-map
              ("C-z o" . omnisharp-start-omnisharp-server)
              ("C-z d" . omnisharp-go-to-definition-other-window)
              ("C-x C-j" . counsel-imenu))
  :config
  (add-to-list 'company-backends #'company-omnisharp))
(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook (lambda() (setq tab-width 4)))
  (when my/use-omnisharp
    (add-hook 'csharp-mode-hook #'omnisharp-mode)
    (add-hook 'csharp-mode-hook #'company-mode)))
```


<a id="org50b5dbe"></a>

# [`magit`](https://github.com/magit/magit)/git configuration

The **most awesome** git porcelain. Most here are part of magit, [`git-time-machine`](https://github.com/pidu/git-timemachine) is not, but well worth using.

```emacs-lisp
(use-package git-commit)
(use-package magit
  :bind (("C-z f" . magit-find-file-other-window)
         ("C-z g" . magit-status)
         ("C-z l" . magit-log-buffer-file))
  ;; Make the default action a branch checkout, not a branch visit when in branch mode
  :bind (:map magit-branch-section-map
              ([remap magit-visit-thing] . magit-branch-checkout)))
(use-package magit-filenotify)
(use-package magit-find-file)
(use-package git-timemachine)
```

Customized variables:

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
<td class="org-left">git-commit-fill-column</td>
<td class="org-left">78</td>
</tr>


<tr>
<td class="org-left">magit-completing-read-function</td>
<td class="org-left">ivy-completing-read</td>
</tr>


<tr>
<td class="org-left">magit-pull-arguments</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">nil</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">magit-repository-directories</td>
<td class="org-left">(("~/repos" . 1))</td>
</tr>
</tbody>
</table>


<a id="orgd1cd7dd"></a>

# [`org-mode`](https://orgmode.org/) Configuration

I use [`org-bullets`](https://github.com/emacsorphanage/org-bullets) which used to be part of the `org-plus-contrib` package but seems to no longer be included . Always throw
[`org-mode`](https://orgmode.org/) buffers into [`flyspell-mode`](https://www.emacswiki.org/emacs/FlySpell) for live spell checking.

The `htmlize` package allows the HTML and Markdown exporters to work (underlying code).

```emacs-lisp
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (toggle-truncate-lines -1)
                             (auto-fill-mode 1)
                             (org-bullets-mode))))
(use-package org-autolist)
(use-package htmlize)
(add-hook 'org-mode-hook #'flyspell-mode)
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


<a id="org8e26a04"></a>

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
        \"hello@@html:<br>@@there@@html:<br>&nbsp;@@my@@html:<br>&nbsp;&nbsp;@@friend@@html:<br>@@\"
        This function also adds inline HTML around '_' in the text."
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


<a id="orga679208"></a>

## Use of babel

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


<a id="org37ce64a"></a>

# python configuration

At one point I was using anaconda but have switched back to elpy. I really like `eply-config` that tells you if everything is
working properly. I've been using a `virtualenv` for my python development and couldn't be happier. Perhaps the only thing that
bothers me is that when an object is returned, PyCharm will give you list and dictionary methods while `eply=/=company` does not.
Seems to be the only real issue at this point.


<a id="org3005428"></a>

## The tale of two IDEs

I've decided to take the [Language Server Protocol](https://langserver.org/) out for a spin. Unfortunately it might be a while before I decide to switch
since there are some things I find a little annoying, like initial startup speed of loading a large Python file into Emacs,
presumably because `lsp-mode` is initializing. Either way, the Python IDE is selected using the.

To switch between the two IDEs might take a bit of futzing - I've had to go remove `elpy` entries from `~/.emacs.d/custom.el` to
switch to the `lsp-mode`.

```emacs-lisp
(defcustom my/use-elpy t
  "Setting to t uses elpy as the Python IDE. Set to nil to use lsp."
  :group 'my-configuration
  :type '(choice
          (const :tag "lsp" nil)
          (const :tag "elpy" t)))
```


<a id="org960a030"></a>

### [`elpy`](https://github.com/jorgenschaefer/elpy) IDE

The tried and true [`elpy`](https://github.com/jorgenschaefer/elpy) Python IDE.

```emacs-lisp
(when my/use-elpy
  (use-package elpy
    :demand t
    :bind (:map elpy-mode-map
                ("C-z ." . elpy-goto-definition))
    :config
    (elpy-enable)
    (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
    (use-package company-jedi
      :config
      (push 'company-jedi company-backends))))
```


<a id="org22d9d80"></a>

### [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode) IDE

This is a newer mode based on the [Language Server Protocol](https://langserver.org/). Used along with this is the [`lsp-ui`](https://github.com/emacs-lsp/lsp-ui) goodies.

```emacs-lisp
(unless my/use-elpy
    (use-package lsp-mode
      :demand t
      :config
      (use-package lsp-python)
      (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)  
      (lsp-define-stdio-client lsp-python "python" #'projectile-project-root '("pyls"))
      (use-package lsp-ui
        :config
        (require 'lsp-imenu)
        (add-hook 'lsp-mode-hook 'lsp-ui-mode))
      (use-package company-lsp
        :config
        (push 'company-lsp company-backends))
      (add-hook 'lsp-after-initialize-hook (lambda ()
                                             (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
                                               (lsp--set-configuration lsp-cfg))))))

```


<a id="orgc74d1b0"></a>

## Python IDE-agnostic configuration

The `remove-hook` call in the `:config` section of the `flymake` package install is there because of the persistent, and annoying,
output to the `*Flymake Log*` buffer:

> Warning [flymake create\_and\_activate\_device.py]: Disabling backend flymake-proc-legacy-flymake because (error Can’t find a suitable init function)

The `remove-hook` relieves this issue. NB: this may be for Emacs version 27.0.50 only.

```emacs-lisp
(use-package pylint)
(use-package python-docstring
  :config
  (python-docstring-install))
(use-package flymake
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  (when my/use-elpy (add-hook 'flymake-mode-hook (lambda ()
                                                   (setq flymake-start-syntax-check-on-newline t
                                                         flymake-no-changes-timeout 0.5)))))
(use-package python
  ;; :bind (:map python-mode-map
  ;;             ("C-c n" . flymake-goto-next-error)
  ;;             ("C-c p" . flymake-goto-prev-error))
  :config
  (add-hook 'inferior-python-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'python-mode-hook (lambda ()
                                (unless my/use-elpy (lsp-python-enable))
                                (flymake-mode)
                                (company-mode))))
```

Customized variables used in this python configuration:

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
<td class="org-left">python-check-command</td>
<td class="org-left">"/usr/local/bin/flake8"</td>
</tr>


<tr>
<td class="org-left">python-shell-interpreter</td>
<td class="org-left">"jupyter"</td>
</tr>


<tr>
<td class="org-left">python-shell-interpreter-args</td>
<td class="org-left">"console &#x2013;simple-prompt"</td>
</tr>


<tr>
<td class="org-left">python-shell-prompt-detect-failure-warning</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">python-shell-prompt-output-regexp</td>
<td class="org-left">"Out\\\[[0-9]+\\]: "</td>
</tr>


<tr>
<td class="org-left">python-shell-prompt-regexp</td>
<td class="org-left">"In \\\[[0-9]+\\]: "</td>
</tr>
</tbody>
</table>


<a id="org4693983"></a>

# Additional bits-o-configuration


<a id="org8a22dfc"></a>

## Limit the length of [`which-function`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Which-Function.html)

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


<a id="org9ecd610"></a>

## `my-ansi-term`

Allows me to name my ANSI terms. Was very useful when I used more ANSI shells (so that tabs were interpreted by the shell). Some
other modes and shells make this less useful these days.

```emacs-lisp
(cl-defun my-ansi-term (term-name cmd)
  "Create an ansi term with a name - other than *ansi-term* given TERM-NAME and CMD."
  (interactive "sName for terminal: \nsCommand to run [/bin/bash]: ")
  (ansi-term (if (= 0 (length cmd)) "/bin/bash" cmd))
  (rename-buffer term-name))
```


<a id="orgafce9ba"></a>

## Understand file type by shebang

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
            (re-search-forward "^#!\s*/.*/\\(python\\|bash\\).*$")
            (if (string= (match-string 1) "python")
                (python-mode)
              (sh-mode))))))

(add-hook 'find-file-hook 'my-find-file-hook)
```


<a id="org32ed824"></a>

## Additional Configuration

Setup `eldoc` mode, use `y-or-n-p` instead of `yes-or-no-p`. Key bindings&#x2026;

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)   ;; Run elisp with eldoc-mode
(diminish 'eldoc-mode "Doc")                    ;; Diminish eldoc-mode

(fset #'list-buffers #'ibuffer)                 ;; prefer ibuffer over list-buffers
(fset #'yes-or-no-p #'y-or-n-p)                 ;; for lazy people use y/n instead of yes/no

;; Some key bindings
(bind-key "C-x p" #'pop-to-mark-command)
(bind-key "C-h c" #'customize-group)
(bind-key "C-+" #'text-scale-increase)
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
```

