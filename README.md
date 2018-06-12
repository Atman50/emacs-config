
# Table of Contents

1.  [Overview](#org4a7f32c)
    1.  [Quick start](#orgadee97e)
    2.  [Why a literate configuration](#org7fc2eee)
    3.  [`init.el` in a few small sections](#orgfbd34a0)
        1.  [Load the custom file](#orga088776)
        2.  [Random setting](#orgc6915da)
        3.  [Initialize the package](#org5ebbfe5)
        4.  [Finally load up this file](#org0acab12)
2.  [Configuration](#org25c537a)
    1.  [Just a little preamble](#orgd63cb70)
    2.  [General packages](#orgb693e16)
        1.  [Speed up line movement](#org2eeabc4)
        2.  [diminish](#org7e41a70)
        3.  [bind-key](#org4b8df6b)
        4.  [savehist](#orgfa65f50)
        5.  [Themes and mode line](#org3ba517e)
        6.  [For demonstrations](#org8ce854a)
        7.  [Trying `codesearch`](#org4a20e17)
        8.  [Other useful packages](#orgc74d4dd)
3.  [`company` Configuration](#org39548e4)
4.  [`ivy` Configuration](#orgc43751f)
5.  [`prescient` Configuration](#orgc31c891)
6.  [`yasnippet` Configuration](#org758d630)
7.  [Working with C#](#orgb2492a8)
8.  [`magit`/git configuration](#orgbd464ee)
9.  [`org-mode` Configuration](#org4d0d23a)
    1.  [`org-mode` export hacks for HTML and Markdown](#orgd5ecf78)
10. [python configuration](#orgd1d6db3)
11. [Additional bits-o-configuration](#org6d7821b)
    1.  [Limit the length of `which-function`](#org2c85a26)
    2.  [`my-ansi-term`](#org9ad346e)
    3.  [Understand file type by shebang](#org28519df)
    4.  [Additional Configuration](#orgd3313fa)



<a id="org4a7f32c"></a>

# Overview

This is my literate and <font color=red size=+3><b><u>portable</u></b></font> Emacs initialization.


<a id="orgadee97e"></a>

## Quick start

Simply

1.  git clone this repository into `~/.emacs.d`: `git clone https://github.com/Atman50/emacs-config.git =/.emacs.d`

2.  start Emacs

That's it.

Starting Emacs for the first time on a new machine loads all the packages/configuration loads. It takes some time on this first
load since all the packages referenced need to download and compile. On subsequent Emacs invocations startup time is much better.
The ability to simply clone and start is what makes this configuration **highly portable**. Note that some of the Emacs
customization (see `custom.el`) are system (file system) dependent. I handle this by using git to create a stash of the
localized changes for `custom.el` and then apply it whenever I take updated configurations from the repository.


<a id="org7fc2eee"></a>

## Why a literate configuration

Well mostly I wanted to learn how to do it, but also I was having issues with managing my initialization/configuration. FWIW
this is the approach that I came up with. A simple 7 part `init.el` file with customization saved to its own file (that's read
first).

What I've gotten out of all this work is a truly portable, documented configuration that works well for me. Please feel free to
take whatever portion(s) you wish from this and make it your very own.

I have tried to make this configuration 100% portable meaning that on a new system (Linux or Windows at this point) with Emacs on
it, I simple git clone this repository to =~/.emacs.d/~ and then fire up Emacs. Works every time for me.


<a id="orgfbd34a0"></a>

## `init.el` in a few small sections

To get started with a literate configuration, I use this simple `init.el` file. Currently the `init.el` file used here contains
only 16 lines of actual code.

Here are the pieces of the `init.el` file explained. The line numbers are the line numbers from the `init.el` file from this
repository.


<a id="orga088776"></a>

### Load the custom file

An Emacs user recently said "I don't use the Emacs customization" facility. I think that's just crazy. One of the nicest things
about Emacs is the extensive and quite useful customization engine. You can customize variables and faces with ease and make the
settings work for you.

Loading this file first, even before package stuff, is important to get things working. In a subsequent section of this
`init.el` the `package-refresh-contents` uses the variable `package-archives` for importing archive information (and eventually
packages).

```emacs-lisp
14  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
15  (load custom-file t)
```

I've pointed the customization at my own file `custom.el` and loaded it here. Customization will now be written to this file
from the Emacs customization system.

The most important custom variable at this point in the configuration is `package-archives` which should now be set as follows:

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


<a id="orgc6915da"></a>

### Random setting

```emacs-lisp
18  (prefer-coding-system 'utf-8)
```

This was necessary because some packages in ELPA had Unicode characters in them that my Windows system didn't like. Not a bad
idea to set this somewhere and I needed it before the `package-refresh-contents` below.


<a id="org5ebbfe5"></a>

### Initialize the package

This code is meant to insure that both `org` mode (the non-built most recent `org` version available in the org repository) and
`use-package` are installed. We need the `org` package to tangle (babel) this file itself and the rest of the configuration
heavily relies on the most excellent `use-package` extension.

The call to `package-refresh-contents` heavily depends on the value of `package-archives` to be correct. In the `init.el` file,
the custom file is loaded before this code so it is set via normal Emacs customization.

I had to change the following logic so that it worked under both 26.0.91 and 27.0.50 (there is some difference to the
initialization process that got in the way of my old logic). First time in should package-install both org mode and use-package. 
Then only on 26.0.91 package-initialize is called (27.0.50 calls package-initialize for you? - something to do with an early
initialize capability&#x2026;).

```emacs-lisp
23  (unless (boundp 'package-user-dir)
24    (unless (boundp 'package-archive-contents)
25      (package-initialize))
26    (unless (assoc 'use-package package-archive-contents)
27      (package-refresh-contents)
28      (package-install (elt (cdr (assoc 'org-plus-contrib package-archive-contents)) 0))
29      (package-install (elt (cdr (assoc 'use-package package-archive-contents)) 0))))
30  (require 'use-package)
31  (require 'org)
```

This code makes it so the actual package contents of the repositories is **not** refreshed every time Emacs loads, mainly for
speed of startup purposes. You can always refresh the list by using `M-x list-packages`. This is recommended on occasion as the
extension packages used should be updated.

NOTE: I could not make `use-package` ignore the built-in `org` package in favor of the `org` package from the org repository.
Many people suggested use the `:ensure`  and `:demand` keywords to control `use-package`, but to no avail. There's a nice
discussion of <https://github.com/jwiegley/use-package/issues/319>.


<a id="org0acab12"></a>

### Finally load up this file

Simply use this file (I default it to `README`) and Babel tangle the configuration (`README.org`) into a file that gets loaded
(`README.el`). The remainder of the initialization follows in this file.

```emacs-lisp
34  (defvar my-cfg (concat user-emacs-directory "README"))
35  (when (file-newer-than-file-p (concat my-cfg ".org") (concat my-cfg ".el"))
36    (org-babel-tangle-file (concat my-cfg ".org")))
37  (load my-cfg)
```


<a id="org25c537a"></a>

# Configuration

Here are my configuration bits. All of the following code snippets are tangled from this file into an `.el` file that gets loaded
from the initialization file. Feel free to take as little or as much as you like from here.


<a id="orgd63cb70"></a>

## Just a little preamble

This is a little piece of code that I picked up that might make things faster when downloading and installing all the packages.
This turns down the garbage collector during the use-package loading when it has to do some compiling. Set it back when done with
init.

```emacs-lisp
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
```

Also create a handy variable to know if we are Windows - used later on here.

```emacs-lisp
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
```


<a id="orgb693e16"></a>

## General packages

Here are some general packages I use


<a id="org2eeabc4"></a>

### Speed up line movement

I ran into this little tidbit while reading Sacha Chua's posts from Emacs. It is described [here](https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746), but to summarize the
`next-line` defun triggers `line-move-partial` which leads to excessive processing. By setting the variable here, the speed of
using `next-line` gets very cut down.

```emacs-lisp
(setq auto-window-vscroll nil)
```


<a id="org7e41a70"></a>

### [diminish](https://github.com/myrjola/diminish.el)

Handy mode to make the modeline more succinct by allowing a *diminished* mode line string. Sometimes the fact that mode is there
is fine and it doesn't need to be on the mode line (diminish it to "").

```emacs-lisp
(use-package diminish)
```


<a id="org4b8df6b"></a>

### [bind-key](https://github.com/priyadarshan/bind-key)

Much better binding capabilities

```emacs-lisp
(use-package bind-key)
```


<a id="orgfa65f50"></a>

### savehist

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


<a id="org3ba517e"></a>

### Themes and mode line

Recently switched from `powerline` to `moody`. The `moody` interface gives a nice tabbed mode line.

```emacs-lisp
(use-package leuven-theme
  :config
  (load-theme 'leuven t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line nil
                        :overline line
                        :box nil
                        :foreground "dark turquoise")
    (set-face-attribute 'mode-line-inactive nil
                        :overline line
                        :underline line
                        :box nil
                        :foreground "#335EA8"
                        :background "#c2c2c2")
    (set-face-attribute 'mode-line-buffer-id nil
                        :foreground "#335EA8"
                        :overline line)
    (set-face-attribute 'mode-line-highlight nil
                        :foreground "DarkOrange2")
    (set-face-attribute 'mode-line-emphasis nil
                        :foreground "#335EA8")
    (set-face-attribute 'which-func nil
                        :foreground "ivory2")))

(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))
```


<a id="org8ce854a"></a>

### For demonstrations

These packages are useful when doing presentations.

```emacs-lisp
(use-package command-log-mode :defer t)
```


<a id="org4a20e17"></a>

### Trying `codesearch`

`Codesearch` is Google tool written in Go. You'll need to [install Go](https://golang.org/doc/install) on your system. The install `codesearch` by issuing the
command `go get github.com/google/codesearch/cmd/...`.

```emacs-lisp
(use-package codesearch :defer t)
```


<a id="orgc74d4dd"></a>

### Other useful packages

OK, a little tired of documenting each package on it's own. These packages are just generally useful.

`which-key` very helpful for finding way around.

```emacs-lisp
(use-package realgud)           ;; A "better" gud
(use-package projectile
  :config
  (projectile-mode t))
(use-package ibuffer-projectile :defer t)
(use-package xterm-color :defer t)
(use-package which-key :diminish "")
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
<td class="org-left">("`/repos/Midmarket/" "~/.emacs.d/" "`")</td>
</tr>


<tr>
<td class="org-left">desktop-save-mode</td>
<td class="org-left">t</td>
</tr>
</tbody>
</table>


<a id="org39548e4"></a>

# `company` Configuration

Use the excellent `company` modular in-buffer text completion framework. In particular this is used for `elpy` (python) mode and
`omnisharp` (C#) mode, although it is used elsewhere.

```emacs-lisp
(use-package company
  :diminish "Co")
```


<a id="orgc43751f"></a>

# `ivy` Configuration

Was a `helm` user, but switched to `ivy`. Lots of nice features in `ivy`

```emacs-lisp
(use-package ivy
  :diminish ""
  :bind (:map ivy-minibuffer-map
              ("C-w" . ivy-yank-word)           ;; make work like isearch
              ("C-r" . ivy-previous-line))
  :config
  (setq ivy-initial-inputs-alist nil)           ;; no regexp by default
  (setq ivy-re-builders-alist                   ;; allow input not in order
        '((t . ivy--regex-ignore-order))))
(use-package counsel
  :bind (("C-c j" . counsel-imenu)))
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
<td class="org-left">ivy-use-virtual-buffers</td>
<td class="org-left">t</td>
</tr>
</tbody>
</table>


<a id="orgc31c891"></a>

# `prescient` Configuration

`prescient` provides "simple but effective sorting and filtering for Emacs."

```emacs-lisp
(use-package prescient)
(use-package ivy-prescient)
(use-package company-prescient)
```


<a id="org758d630"></a>

# `yasnippet` Configuration

`yasnippet` is a truly awesome package. Local modifications should go in `~/.emacs.d/snippets/`.

Just love `yasnippet` package.

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


<a id="orgb2492a8"></a>

# Working with C#

I'm a C# developer and pretty much dislike big edits using Visual Studio. I've spent some amount of time coming
up with a good C# configuration. This works spectacularly well and takes only minutes to setup.

There are comprehensive directions at [omnisharp-emacs](https://github.com/OmniSharp/omnisharp-emacs.git) for using omnisharp.

```emacs-lisp
(defvar config/use-omnisharp t)

(use-package omnisharp
  :diminish "\u221e" ;; infinity symbol
  :if config/use-omnisharp
  :bind (:map omnisharp-mode-map
              ("C-c o" . omnisharp-start-omnisharp-server)
              ("C-c d" . omnisharp-go-to-definition-other-window)
              ("C-x C-j" . counsel-imenu))
  :config
  (add-to-list 'company-backends #'company-omnisharp))
(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook (lambda() (setq tab-width 4)))
  (when config/use-omnisharp
    (add-hook 'csharp-mode-hook #'omnisharp-mode)
    (add-hook 'csharp-mode-hook #'company-mode)))
```


<a id="orgbd464ee"></a>

# [`magit`](https://github.com/magit/magit)/git configuration

The **most awesome** git porcelain. Most here are part of magit, `[[https://github.com/pidu/git-timemachine][git-time-machine]]` is not, but well worth using.

```emacs-lisp
(use-package git-commit)
(use-package magit
  :bind (("C-c f" . magit-find-file-other-window)
         ("C-c g" . magit-status)
         ("C-c l" . magit-log-buffer-file))
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


<a id="org4d0d23a"></a>

# `org-mode` Configuration

Org-mode configurations. `org-bullets` used to be part of org but is now outside. Always throw `org-mode` buffers into
`flyspell-mode`.

The `htmlize` package allows the HTML and Markdown exporters to work (underlying code).

```emacs-lisp
(use-package org-bullets
   :config (add-hook 'org-mode-hook (lambda ()
                                      (toggle-truncate-lines -1)
                                      (auto-fill-mode 1)
                                      (org-bullets-mode))))
(use-package org-autolist)
(use-package htmlize)
(add-hook 'org-mode-hook #'flyspell-mode)
```

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


<a id="orgd5ecf78"></a>

## `org-mode` export hacks for HTML and Markdown

I export into markdown for github. I do not use the `ox-gfm` package because when I tried it, it modified the source file because
of this file's use of the `#+CALL` construct (each call adds the table to the source file). So I use the built in `ox-md`
exporter. However, it just indents the code blocks rather put the `` ```emacs-lisp `` code snippet prefix and `` ``` `` postfix but
rather just indents. First we load the library so it turns up in the export menu (`C-x C-e`). Then we override the output method
for the code.

```emacs-lisp
(load-library "ox-md")

(defun org-md-example-block (example-block _contents info)
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
(defun my-md-export-hack(text)
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


<a id="orgd1d6db3"></a>

# python configuration

At one point I was using anaconda but have switched back to elpy. I really like `eply-config` that tells you if everything is
working properly. I've been using a `virtualenv` for my python development and couldn't be happier. Perhaps the only thing that
bothers me is that when an object is returned, PyCharm will give you list and dictionary methods while `eply=/=company` does not.
Seems to be the only real issue at this point.

The `remove-hook` call in the `:config` section of the `flymake` package install is there because of the persistent, and annoying,
output to the `*Flymake Log*` buffer:

> Warning [flymake create<sub>and</sub><sub>activate</sub><sub>device.py</sub>]: Disabling backend flymake-proc-legacy-flymake because (error Can’t find a suitable init function)

The `remove-hook` relieves this issue. NB: this may be for Emacs version 27.0.50 only.

```emacs-lisp
(use-package company-jedi)
(use-package elpy
  :bind (:map elpy-mode-map
              ("C-c ." . elpy-goto-definition))
  :config
  (elpy-enable))
(use-package pylint)
(use-package python-docstring
  :config
  (python-docstring-install))
(use-package flymake
  :config
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake))
(use-package python
  :config
  (add-hook 'inferior-python-mode-hook (lambda () (setq tab-width 4)))
  (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends #'company-jedi)))
  (add-hook 'python-mode-hook #'flymake-mode)
  (add-hook 'python-mode-hook #'company-mode))
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
<td class="org-left">elpy-modules</td>
<td class="org-left">(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)</td>
</tr>


<tr>
<td class="org-left">python-shell-interpreter</td>
<td class="org-left">"ipython"</td>
</tr>


<tr>
<td class="org-left">python-shell-interpreter-args</td>
<td class="org-left">"-i &#x2013;simple-prompt"</td>
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


<a id="org6d7821b"></a>

# Additional bits-o-configuration


<a id="org2c85a26"></a>

## Limit the length of `which-function`

`which-function` which is used by `powerline` has no maximum method/function signature. This handy adviser limits the name to 64
characters.

```emacs-lisp
(defvar  which-function-max-width 64 "The maximum width of the which-function string.")
(advice-add #'which-function :filter-return
            (lambda (s) (when (stringp s)
                          (if (< (string-width s) which-function-max-width) s
                            (concat (truncate-string-to-width s (- which-function-max-width 3)) "...")))))
```


<a id="org9ad346e"></a>

## `my-ansi-term`

Allows me to name my ANSI terms. Was very useful when I used more ANSI shells (so that tabs were interpreted by the shell). Some
other modes and shells make this less useful these days.

```emacs-lisp
(defun my-ansi-term (term-name cmd)
  "Create an ansi term with a name - other than *ansi-term* given TERM-NAME and CMD."
  (interactive "sName for terminal: \nsCommand to run [/bin/bash]: ")
  (ansi-term (if (= 0 (length cmd)) "/bin/bash" cmd))
  (rename-buffer term-name))
```


<a id="org28519df"></a>

## Understand file type by shebang

When a file is opened and it is determined there is no mode (fundamental-mode) this code reads the first line of the file looking
for an appropriate shebang for either python or bash and sets the mode for the file.

```emacs-lisp
(defun my-find-file-hook ()
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


<a id="orgd3313fa"></a>

## Additional Configuration

Setup `eldoc` mode, use `y-or-n-p` instead of `yes-or-no-p`. Key bindings&#x2026;

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)   ;; Run elisp with eldoc-mode
(fset #'list-buffers #'ibuffer)                 ;; prefer ibuffer over list-buffers
(fset #'yes-or-no-p #'y-or-n-p)                 ;; for lazy people use y/n instead of yes/no
(diminish 'eldoc-mode "Doc")                    ;; Diminish eldoc-mode

;; Some key bindings
(bind-key "C-x p" #'pop-to-mark-command)
(bind-key "C-h c" #'customize-group)
(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)
(bind-key "C-z" 'nil)                           ;; get rid of pesky "\C-z"
(bind-key "C-z" 'nil ctl-x-map)                 ;;    and "\C-x\C-z" annoying minimize
(bind-key "C-c C-d" #'dired-jump)
(bind-key "C-c r" #'revert-buffer)
(bind-key "C-c t" #'toggle-truncate-lines)
(bind-key "C-c c" #'comment-region)
(bind-key "C-c u" #'uncomment-region)
(bind-key "<up>" #'enlarge-window ctl-x-map)     ;; note: C-x
(bind-key "<down>" #'shrink-window ctl-x-map)    ;; note: C-x

(setq-default ediff-ignore-similar-regions t)   ;; Not a variable but controls ediff

;; Enable some stuff that's normally disabled
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
```

