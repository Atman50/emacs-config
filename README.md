
# Table of Contents

1.  [Overview](#org5188a9e)
    1.  [Quick start](#org9d5c496)
    2.  [Why a literate configuration](#org431af22)
    3.  [`init.el` in a few small sections](#orge1ead9b)
        1.  [Load the custom file](#org37d8e75)
        2.  [Initialize the package](#orgd74f696)
        3.  [Random setting](#org518d096)
        4.  [Refresh the package archives](#org3fe445c)
        5.  [Load up `use-package`](#org3a5c804)
        6.  [Make sure we have a recent `org` package](#org09e15ee)
        7.  [Finally, load up this file](#orgf72eb96)
2.  [Configuration](#org0f7361b)
    1.  [Just a little preamble](#org2e61a4f)
    2.  [General packages](#org12c328f)
        1.  [diminish](#org49edc88)
        2.  [bind-key](#orgd7cc4b1)
        3.  [savehist](#org27ecc5b)
        4.  [ag](#org8fd9947)
        5.  [powershell](#org4d00325)
        6.  [themes and modeline](#orgdebae0e)
        7.  [aspx editing](#org3f1f367)
        8.  [Other useful packages](#orgaa843af)
3.  [Working with C#](#org0d9e7fa)
4.  [`magit`/git configuration](#org26c7da7)
5.  [`org-mode` Configuration](#orge7cb2f9)
    1.  [`org-mode` export hacks for HTML and Markdown](#org1694e26)
6.  [python configuration](#orgc488fb2)
7.  [`ivy` Configuration](#org2b73119)
8.  [`yasnippet` Configuration](#org10fe7de)
9.  [Additional bits-o-configuration](#org9d432d8)
    1.  [Limit the length of `which-function`](#org1556283)
    2.  [`my-ansi-term`](#org3c9c631)
    3.  [Understand file type by shebang](#orgfe3f62c)
    4.  [Additional Configuration](#org5df2b61)



<a id="org5188a9e"></a>

# Overview

This is my literate and <font color=red size=+3><b><u>portable</u></b></font> Emacs initialization.


<a id="org9d5c496"></a>

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


<a id="org431af22"></a>

## Why a literate configuration

Well mostly I wanted to learn how to do it, but also I was having issues with managing my initialization/configuration. FWIW
this is the approach that I came up with. A simple 7 part `init.el` file with customization saved to its own file (that's read
first).

What I've gotten out of all this work is a truly portable, documented configuration that works well for me. Please feel free to
take whatever portion(s) you wish from this and make it your very own.

I have tried to make this configuration 100% portable meaning that on a new system (Linux or Windows at this point) with Emacs on
it, I simple git clone this repository to =~/.emacs.d/~ and then fire up Emacs. Works every time for me.


<a id="orge1ead9b"></a>

## `init.el` in a few small sections

To get started with a literate configuration, I use this simple `init.el` file. Here is the entire [`init.el`](https://github.com/Atman50/emacs-config/blob/master/init.el) file.

Here are the pieces of the `init.el` file explained.


<a id="org37d8e75"></a>

### Load the custom file

An Emacs user recently said "I don't use the Emacs customization" facility. I think that's just crazy. One of the nicest things
about Emacs is the extensive and quite useful customization engine. You can customize variables and faces with ease and make the
settings work for you.

Loading this file first, even before package stuff, is important to get things working. In a subsequent section of this
`init.el` the `package-refresh-contents` uses the variable `package-archives` for importing archive information (and eventually
packages). 

```emacs-lisp
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)
```

I've pointed the customization at my own file `custom.el` and loaded it here. Customization will now be written to this file
from the Emacs customization system.


<a id="orgd74f696"></a>

### Initialize the package

```emacs-lisp
(package-initialize)
```

Says it all; Initializes the package system.


<a id="org518d096"></a>

### Random setting

```emacs-lisp
(prefer-coding-system 'utf-8)
```

This was necessary because some packages in ELPA had Unicode characters in them that my Windows system didn't like. Not a bad
idea to set this somewhere and I needed it before the `package-refresh-contents` below.


<a id="org3fe445c"></a>

### Refresh the package archives

But only if necessary. If we don't see the `use-package` package in the package manager, then we'll assume that this is probably
a new load and we need to go fetch (for the first time) the archive content directories.

```emacs-lisp
(unless (assoc 'use-package package-archive-contents)
  (package-refresh-contents))
```

If you wished to get fresh contents everytime Emacs starts, just get rid of the first line unless statement.

The `package-refresh-contents` uses the following:

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


<a id="org3a5c804"></a>

### Load up `use-package`

This configuration relies heavily upon John Wiegley's most excellent `use-package` package. Although no longer used in the
initialization file itself (because of issues loading `org-mode` - see below), this is here to power the rest of this literate
configuration.

```emacs-lisp
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
```


<a id="org09e15ee"></a>

### Make sure we have a recent `org` package

I could not make `use-package` ignore the built-in `org` package in favor of the `org` package from the org repository. Many
people suggested use the `:ensure`  and `:demand` keywords to control `use-package`, but to no avail. There's a nice discussion
of <https://github.com/jwiegley/use-package/issues/319>.

Here's my code that guarantees that an `org` gets loaded from a repository ignoring the built in version. This first line checks
to see if `org` is already loaded by interrogating the packages directory for an installed `org` package. Granted, this is a
little "hacky" as it depends on the `org` package's version being numeric (at least the first two characters being numbers).
The second line actually installs the package using the package definition in `package-archive-contents`. The contents are
assured above by `package-refresh-contents`, which should only fire the first time this initialization is run on a new
`~/.emacs.d` directory. The require at the end loads up `org` so the final piece of the configuration works.

```emacs-lisp
(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9][0-9]*"))
  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))
(require 'org)
```


<a id="orgf72eb96"></a>

### Finally, load up this file

Simply use this file (I default it to `README`) and Babel tangle the configuration (`README.org`) into a file that gets loaded
(`README.el`). The remainder of the initialization follows in this file.

```emacs-lisp
(defvar my-cfg (concat user-emacs-directory "README"))
(when (file-newer-than-file-p (concat my-cfg ".org") (concat my-cfg ".el"))
  (org-babel-tangle-file (concat my-cfg ".org")))
(load my-cfg)
```


<a id="org0f7361b"></a>

# Configuration

Here are my configuration bits. All of the following code snippets are tangled from this file into an `.el` file that gets loaded
from the initialization file. Feel free to take as little or as much as you like from here.


<a id="org2e61a4f"></a>

## Just a little preamble

This is a little piece of code that I picked up that might make things faster when downloading and installing all the packages.
This turns down the garbage collector during the use-package loading when it has to do some compiling. Set it back when done with
init.

```emacs-lisp
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))
```

Also create a handy variable to know if we are Windows - used later on here.

```emacs-lisp
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
```


<a id="org12c328f"></a>

## General packages

Here are some general packages


<a id="org49edc88"></a>

### [diminish](https://github.com/myrjola/diminish.el)

Handy mode to make the modeline nicer. I also use to set mode to special characters (for example, see `flycheck-mode`)

```emacs-lisp
(use-package diminish)
```


<a id="orgd7cc4b1"></a>

### [bind-key](https://github.com/priyadarshan/bind-key)

Much better binding capabilities

```emacs-lisp
(use-package bind-key)
```


<a id="org27ecc5b"></a>

### savehist

A great built-in that allows us to have a history file. This means certain elements are saved between sessions of Emacs. Set the
following variables to control `savehist` (use customize).

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

```emacs-lisp
(use-package savehist :demand t)                ;; Nice history in ~/.emacs.d/savehist
```


<a id="org8fd9947"></a>

### [ag](https://github.com/Wilfred/ag.el)

AKA `silversearcher`. Simple interface to excellent tool. I have it installed in my `cygwin64` area and it seems to play well in my
Windows environment.

NB: doesn't seem to work so well under Windows.

```emacs-lisp
(use-package ag)
```


<a id="org4d00325"></a>

### [powershell](http://github.com/jschaf/powershell.el)

Excellent too to run powershell in Emacs

```emacs-lisp
(use-package powershell
  :if mswindows-p)
```


<a id="orgdebae0e"></a>

### themes and modeline

```emacs-lisp
(load-theme 'leuven t)                          ;; Theme: works better before powerline
(use-package powerline
  :demand t
  :config (powerline-default-theme))
```


<a id="org3f1f367"></a>

### aspx editing

Make aspx editing more palatable using `html-mode`.

```emacs-lisp
(add-to-list 'auto-mode-alist
             '("\\.aspx\\'" . html-mode)
             '("\\.aspcx\\'" . html-mode))
```


<a id="orgaa843af"></a>

### Other useful packages

OK, a little tired of documenting each package on it's own. These packages are just generally useful.

`which-key` very helpful for finding way around.

```emacs-lisp
(use-package realgud :demand t)
(use-package projectile :demand t :config (projectile-mode t))
(use-package ibuffer-projectile)
(use-package xterm-color)
(use-package which-key :demand t :diminish "")
(use-package sh-script)
(use-package desktop
  :config
  ;; put desktop in Emacs start directory
  (set-variable 'desktop-path (cons default-directory desktop-path)))
(use-package paredit
  :demand t
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))
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
<td class="org-left">("`/.emacs.d/" "~/.emacs.d/" "`")</td>
</tr>


<tr>
<td class="org-left">desktop-save-mode</td>
<td class="org-left">t</td>
</tr>
</tbody>
</table>


<a id="org0d9e7fa"></a>

# Working with C#

I'm a C# developer and pretty much dislike big edits using Visual Studio. I've spent some amount of time coming
up with a good C# configuration. This works spectacularly well and takes only minutes to setup.

To use Omnisharp follow these directions:

1.  Load up local Omnisharp (Roslyn flavor) from [Omnisharp-Roslyn releases](https://github.com/OmniSharp/omnisharp-roslyn/releases)
2.  Customize the variable `omnisharp-server-executable-path` to point to your Omnisharp Roslyn. For example
    "c:/omnisharp-roslyn-v1.27.2/OmniSharp.exe".

There are comprehensive directions at [omnisharp-emacs](https://github.com/OmniSharp/omnisharp-emacs.git).

```emacs-lisp
(defvar config/use-omnisharp nil)
(let ((omnisharp (car (get 'omnisharp-server-executable-path 'saved-value))))
  (unless (null omnisharp)
    (setq config/use-omnisharp (file-exists-p omnisharp))))

(use-package omnisharp
  :diminish "\u221e"                            ;; infinity symbol
  :if config/use-omnisharp
  :bind (:map omnisharp-mode-map
              ("C-c o" . omnisharp-start-omnisharp-server)
              ("C-c d" . omnisharp-go-to-definition-other-window)
              ("C-x C-j" . counsel-imenu)))
(use-package csharp-mode
  :config
  (when config/use-omnisharp
    (add-hook 'csharp-mode-hook 'company-mode)
    (add-hook 'csharp-mode-hook 'omnisharp-mode)))
```


<a id="org26c7da7"></a>

# [`magit`](https://github.com/magit/magit)/git configuration

The **most awesome** git porcelain. Most here are part of magit, `[[https://github.com/pidu/git-timemachine][git-time-machine]]` is not, but well worth using.

```emacs-lisp
(use-package git-commit)
(use-package magit
  :demand t
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


<a id="orge7cb2f9"></a>

# `org-mode` Configuration

Org-mode configurations. `org-bullets` used to be part of org but is now outside.

The `htmlize` package allows the HTML and Markdown exporters to work (underlying code).

```emacs-lisp
(use-package org-bullets
   :demand t
   :config (add-hook 'org-mode-hook (lambda () 
                                      (toggle-truncate-lines -1)
                                      (auto-fill-mode 1)
                                      (org-bullets-mode))))
(use-package org-autolist :demand t)
(use-package htmlize :demand t)
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


<a id="org1694e26"></a>

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

(advice-add 'orgtbl-to-orgtbl :filter-return 'my-md-export-hack)
```


<a id="orgc488fb2"></a>

# python configuration

At one point I was using anaconda but have switched back to elpy. I really like `eply-config` that tells you if everything is
working properly. I've been using a `virtualenv` for my python development and couldn't be happier. Perhaps the only thing that
bothers me is that when an object is returned, PyCharm will give you list and dictionary methods while `eply=/=company` does not.
Seems to be the only real issue at this point.

```emacs-lisp
(use-package company
  :diminish "Co"
  :config
  (when config/use-omnisharp
    (add-to-list 'company-backends 'company-omnisharp)))
(use-package company-jedi
  :demand t)
(use-package elpy
  :demand t
  :config
  (elpy-enable))
(use-package flycheck
  :diminish  "\u2714"           ;; heavy checkmark
  :config
  (global-flycheck-mode))
(use-package pylint)
(use-package python-docstring
  :config
  (python-docstring-install))
(use-package python
  :config
  (progn
    (add-hook 'python-mode-hook '(lambda () (add-to-list 'company-backends 'company-jedi)))
    (add-hook 'python-mode-hook 'flycheck-mode)
    (add-hook 'python-mode-hook 'company-mode)))
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
<td class="org-left">python-indent-trigger-commands</td>
<td class="org-left">(yas-expand yas/expand)</td>
</tr>


<tr>
<td class="org-left">python-shell-completion-setup-code</td>
<td class="org-left">"from IPython.core.completerlib import module_completion"</td>
</tr>


<tr>
<td class="org-left">python-shell-completion-string-code</td>
<td class="org-left">"';'.join(get_ipython().Completer.all_completions('''%s'''))\n"</td>
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


<a id="org2b73119"></a>

# `ivy` Configuration

Was a `helm` user, but switched to `ivy`. Lots of nice features in `ivy`

```emacs-lisp
(use-package ivy
  :demand t
  :diminish ""
  :bind (:map ivy-minibuffer-map
              ("C-w" . ivy-yank-word)           ;; make work like isearch
              ("C-r" . ivy-previous-line))
  :config
  (progn
    (setq ivy-initial-inputs-alist nil)         ;; no regexp by default
    (setq ivy-re-builders-alist                 ;; allow input not in order
          '((t . ivy--regex-ignore-order)))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x g" . counsel-git)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-j" . counsel-imenu))
  :bind (:map help-map
              ("f" . counsel-describe-function)
              ("v" . counsel-describe-variable)
              ("b" . counsel-descbinds)))
(use-package counsel-projectile
  :demand t
  :config
  (counsel-projectile-mode t))
(use-package counsel-etags)
(use-package ivy-hydra)
(use-package swiper
  :bind (("C-S-s" . isearch-forward)
         ("C-s" . swiper)
         ("C-S-r" . isearch-backward)
         ("C-r" . swiper)))
(use-package avy)

(use-package ivy-posframe
  :demand t
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
<td class="org-left">32</td>
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


<a id="org10fe7de"></a>

# `yasnippet` Configuration

`yasnippet` is a truly awesome package. Local modifications should go in `~/.emacs.d/snippets/`.

This also takes care of hooking up `company` completion with `yasnippet` expansion.

```emacs-lisp
(use-package warnings :demand t)
(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :config
  (progn
    (yas-reload-all)
    ;; fix tab in term-mode
    (add-hook 'term-mode-hook (lambda() (yas-minor-mode -1)))
    ;; Fix yas indent issues
    (add-hook 'python-mode-hook '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
    ;; Setup to allow for yasnippets to use code to expand
    (add-to-list 'warning-suppress-types '(yasnippet backquote-change))))
(use-package yasnippet-snippets :demand t)

(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  "Add in the company-yasnippet BACKEND."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
```

Configured variables of interest:

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


<a id="org9d432d8"></a>

# Additional bits-o-configuration


<a id="org1556283"></a>

## Limit the length of `which-function`

`which-function` which is used by `powerline` has no maximum method/function signature. This handy adviser limits the name to 64
characters.

```emacs-lisp
(defvar  which-function-max-width 64 "The maximum width of the which-function string.")
(advice-add 'which-function :filter-return
            (lambda (s) (when (stringp s)
                          (if (< (string-width s) which-function-max-width) s
                            (concat (truncate-string-to-width s (- which-function-max-width 3)) "...")))))
```


<a id="org3c9c631"></a>

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


<a id="orgfe3f62c"></a>

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


<a id="org5df2b61"></a>

## Additional Configuration

Setup `eldoc` mode, use `y-or-n-p` instead of `yes-or-no-p`. Key bindings&#x2026;

```emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)    ;; Run elisp with eldoc-mode
(fset 'list-buffers 'ibuffer)                   ;; prefer ibuffer over list-buffers
(fset 'yes-or-no-p 'y-or-n-p)                   ;; for lazy people use y/n instead of yes/no
(diminish 'eldoc-mode "Doc")                    ;; Diminish eldoc-mode

;; Some key bindings
(bind-key "C-x p" 'pop-to-mark-command)
(bind-key "C-h c" 'customize-group)
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)
(bind-key "C-z" 'nil)                           ;; get rid of pesky "\C-z"
(bind-key "C-z" 'nil ctl-x-map)                 ;;    and "\C-x\C-z" annoying minimize
(bind-key "C-c C-d" 'dired-jump)
(bind-key "C-c r" 'revert-buffer)
(bind-key "C-c t" 'toggle-truncate-lines)
(bind-key "C-c c" 'comment-region)
(bind-key "C-c u" 'uncomment-region)
(bind-key "<up>" 'enlarge-window ctl-x-map)     ;; note: C-x
(bind-key "<down>" 'shrink-window ctl-x-map)    ;; note: C-x

(setq-default ediff-ignore-similar-regions t)   ;; Not a variable but controls ediff

;; Enable some stuff that's normally disabled
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
```

