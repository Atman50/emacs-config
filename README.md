
# Table of Contents

1.  [Overview](#orgb3a106a)
    1.  [Quick start](#org5e853d2)
    2.  [`init.el` explained](#org6bd8c31)
2.  [Configuration](#org271f174)
    1.  [Just a little preamble](#orge516b67)
    2.  [General packages](#org9897998)
        1.  [diminish](#org1759342)
        2.  [bind-key](#org90c265e)
        3.  [savehist](#orgf800f21)
        4.  [ag](#org6f47882)
        5.  [powershell](#org4950fe1)
        6.  [themes and modeline](#org394500e)
        7.  [aspx editing](#orga560982)
        8.  [Other useful packages](#org79bc876)
3.  [Working with C#](#orgc364991)
4.  [magit configuration](#org07cb61e)
5.  [org-mode configuration](#org1c76808)
6.  [python configuration](#orgf7fe5e7)
7.  [ivy configuration](#org9ec16ff)
8.  [yasnippet configuration](#orgbb3fd86)
9.  [Additional bits-o-configuration](#org8184240)
    1.  [Limit the length of `which-function`](#org19a619a)
    2.  [`my-ansi-term`](#org0d170f4)
    3.  [Understand file type by shebang](#org6d91935)
    4.  [Additional configs](#org6af6a16)



<a id="orgb3a106a"></a>

# Overview

This is my literate and **portable** Emacs initialization "system."


<a id="org5e853d2"></a>

## Quick start

First git clone this repository into `~/.emacs.d`: `git clone https://github.com/Atman50/emacs-config.git =/.emacs.d`

Now simply start Emacs and all the packages/configuration loads. It takes some time on the first load since all the packages referenced need to download and compile. On subsequent Emacs invocations startup time is better.

The ability to simply clone and start makes this configuration **highly portable**. One issue is that some of the customization are file system dependent. I handle this by using git to create a stash of the localized changes for `custom.el` and then apply it whenever I take updated configurations from the repository.

A minor warning is that Emacs load times can be somewhat slow. Startup continues to get slower as the size of the desktop file increases (the more files that need to be opened at the start of Emacs). Since I tend to stay in Emacs for quite some time, this doesn't get in my way.


<a id="org6bd8c31"></a>

## `init.el` explained

To get started with a literate configuration, I use this simple `init.el` file.

Following this code block is the explanation.

```emacs-lisp
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))      ;; 1
(load custom-file t)

(package-initialize)                                                        ;; 2

(prefer-coding-system 'utf-8)                                               ;; 3

(unless (assoc 'use-package package-archive-contents)                       ;; 4
  (package-refresh-contents))

(unless (package-installed-p 'use-package)                                  ;; 5
  (package-install 'use-package))
(require 'use-package)

(unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))     ;; 6
  (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))
(require 'org)

(defvar my-cfg (concat user-emacs-directory "README"))                      ;; 7
(when (file-newer-than-file-p (concat my-cfg ".org") (concat my-cfg ".el"))
  (org-babel-tangle-file (concat my-cfg ".org")))
(load my-cfg)
```

1.  Provide for a separate `custom.el` file. Keeping the customizations separate and first allows for standard emacs customization of **most** of the package variables. When package variables setup outside of the customization file, then `M-x describe-variable` says it was "changed outside of customization".

2.  Initializes the package system. Put this after the customizations so that the variable `package-load-list` can be customized (I use [Gnu](https://gnu.org/packages), [Melpa](https://melpa.org/packages), and [Org](https://orgmode.org/packages) - see `custom.el`).

3.  Sets the preferred coding-system. Since I work on Windows sometimes and some Melpa packages are lacking the proper [byte order mark](https://en.wikipedia.org/wiki/Byte_order_mark) at the beginning of the file, this needs to happen before `package-refresh-contents` so that it finishes without issue. It's not a bad idea to just do this ubiquitously, so I do it here.

4.  Gets the package contents. This uses `package-archives` from the `custom.el` file to load up the repository contents and is only called if use-package is not found in the `package-archives-contents`, meaning the archives not read.

5.  Assures use-package is loaded; `use-package` is used to perform the remainder of this configuration.

6.  Installs org. It ends up that the built-in org-mode is rather old and use-package seems to have issues forcing the installation. This little tibit, looks in the elpa directory

of the one from the org repository, although you can install from hand from the `M-x list-packages` buffer. To automate this, the code here works but might be a little fragile. For exmaple, if the version number isn't just a single number. This not only loads the org package from the org repository, it also makes sure it is up-to-date. The code could be guarded by finding any org-# directory under elpa and not installing.

1.  Does the deed and loads this file. If the file has already been "babel-ed" then just load the results, otherwise do the "babel-ing".

That's it. Used to be simpler, but had to account for overriding the built-in org-mode package.

The package-refresh-contents in the above code depends upon:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Current Value</th>
<th scope="col" class="org-left">Default Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">package-archives</td>
<td class="org-left">(("gnu" . "<a href="https://elpa.gnu.org/packages/">https://elpa.gnu.org/packages/</a>")<br>&nbsp;("melpa" . "<a href="https://melpa.org/packages/">https://melpa.org/packages/</a>")<br>&nbsp;("org" . "<a href="https://orgmode.org/elpa/">https://orgmode.org/elpa/</a>"))</td>
<td class="org-left">(("gnu" . "<a href="http://elpa.gnu.org/packages/">http://elpa.gnu.org/packages/</a>"))</td>
</tr>
</tbody>
</table>


<a id="org271f174"></a>

# Configuration


<a id="orge516b67"></a>

## Just a little preamble

This is a little piece of code that I picked up that might make things faster when downloading and installing all the packages. This turns down the garbage collector during the use-package loading when it has to do some compiling. Set it back when done with init.

```emacs-lisp
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))
```

Also create a handy variable to know if we are Windows - used later on here.

```emacs-lisp
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
```


<a id="org9897998"></a>

## General packages

Here are some general packages


<a id="org1759342"></a>

### [diminish](https://github.com/myrjola/diminish.el)

Handy mode to make the modeline nicer. I also use to set mode to special characters (for example, see flycheck-mode)

```emacs-lisp
(use-package diminish)
```


<a id="org90c265e"></a>

### [bind-key](https://github.com/priyadarshan/bind-key)

Much better binding capabilities

```emacs-lisp
(use-package bind-key)
```


<a id="orgf800f21"></a>

### savehist

A great builtin that allows us to have a history file. This means certain elements are saved between sessions of emacs. Set the following variables to control `savehist` (use customize).

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Current Value</th>
<th scope="col" class="org-left">Default Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">savehist-file</td>
<td class="org-left">"~/.emacs.d/savehist"</td>
<td class="org-left">"~/.emacs.d/history"</td>
</tr>


<tr>
<td class="org-left">savehist-additional-variables</td>
<td class="org-left">(kill-ring search-ring regexp-search-ring)</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">savehist-mode</td>
<td class="org-left">t</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>

```emacs-lisp
(use-package savehist :demand t)                ;; Nice history in ~/.emacs.d/savehist
```


<a id="org6f47882"></a>

### [ag](https://github.com/Wilfred/ag.el)

AKA silversearcher. Simple interface to excellent tool. I have it installed in my cygwin64 area and it seems to play well in my Windows environment.

NB: doesn't seem to work so well under Windows.

```emacs-lisp
(use-package ag)
```


<a id="org4950fe1"></a>

### [powershell](http://github.com/jschaf/powershell.el)

Excellent too to run powershell in Emacs

```emacs-lisp
(use-package powershell
  :if mswindows-p)
```


<a id="org394500e"></a>

### themes and modeline

```emacs-lisp
(load-theme 'leuven t)                          ;; Theme: works better before powerline
(use-package powerline
  :demand t
  :config (powerline-default-theme))
```


<a id="orga560982"></a>

### aspx editing

Make aspx editing more palatable using html mode

```emacs-lisp
(add-to-list 'auto-mode-alist
             '("\\.aspx\\'" . html-mode)
             '("\\.aspcx\\'" . html-mode))
```


<a id="org79bc876"></a>

### Other useful packages

Ok, a little tired of documenting each package on it's own. These packages are just generally useful.

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

Note that the setting of `desktop-path` allows the multiple `.emacs.desktop` files, each in the directory where `emacs` was started. Although `desktop-path` is changed outside `custom.el`, I've included it here in the table below so you can see that the default is augmented with the startup directory which in this case is `~/.emacs.d`.

Customized variables of interest here:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Current Value</th>
<th scope="col" class="org-left">Default Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">desktop-path</td>
<td class="org-left">("`/.emacs.d/" "~/.emacs.d/" "`")</td>
<td class="org-left">("`/.emacs.d/" "`")</td>
</tr>


<tr>
<td class="org-left">desktop-save-mode</td>
<td class="org-left">t</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>


<a id="orgc364991"></a>

# Working with C#

Because I'm a C# developer and pretty much dislike a lot of the GUI issues in Visual Studio, I've spent some amount of time coming up with a good C# configuration. This works spectularly well and takes only minutes to setup.

To use omnisharp follow these directions:

1.  Load up local omnisharp (roslyn flavor) from [Omnisharp-Roslyn releases](https://github.com/OmniSharp/omnisharp-roslyn/releases)
2.  Customize the variable `omnisharp-server-executable-path` to point to your omnisharp roslyn. For example "c:/omnisharp-roslyn-v1.27.2/OmniSharp.exe".

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


<a id="org07cb61e"></a>

# [magit](https://github.com/magit/magit) configuration

The most awesome git porcelain. Most here are part of magit, `[[https://github.com/pidu/git-timemachine][git-time-machine]]` is not, but well worth using.

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

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Current Value</th>
<th scope="col" class="org-left">Default Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">magit-completing-read-function</td>
<td class="org-left">ivy-completing-read</td>
<td class="org-left">magit-builtin-completing-read</td>
</tr>


<tr>
<td class="org-left">magit-pull-arguments</td>
<td class="org-left">nil</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">nil</td>
<td class="org-left">nil</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">magit-repository-directories</td>
<td class="org-left">(("~/repos" . 1))</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>


<a id="org1c76808"></a>

# org-mode configuration

Org-mode configurations. `org-bullets` used to be part of org but is now outside.

The `htmlize` package allows the html and markdown exporters to work (underlying code).

```emacs-lisp
(use-package org-bullets
   :demand t
   :config (add-hook 'org-mode-hook 'org-bullets-mode))
(use-package org-autolist :demand t)
(use-package htmlize :demand t)
```

I export into markdown for github. I do not use the `ox-gfm` package because when I tried it, it modified the source file because of this file's use of the `#+CALL` construct (each call adds the table to the source file). So I use the built in `ox-md` exporter. However, it just indents the code blocks rather put the `` ```emacs-lisp `` code snippet prefix and `` ``` `` postfix but rather just indents. First we load the library so it turns up in the export menu (`C-x C-e`). Then we override the output method for the code.

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

To support the using of dynamic custom vars table using the library of Babel, the export text for markdown and html goes through `orgtbl-to-orgtbl` which turns the list returned in the an org-mode table. After `orgtbl-to-orgtbl`, the `htmlize` package turns it into a HTML table. The advisor changes all the spaces after a `<br>` into `&nbsp;` entities and surrounds them with inline HTML. This is necessary because `orgtbl-to-orgtbl` strips text between the `@@` used to inline HTML. The advisor also protects any underscores in the table with inline HTML.

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

Customized variables for org-mode:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Current Value</th>
<th scope="col" class="org-left">Default Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">org-catch-invisible-edits</td>
<td class="org-left">show</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">org-html-postamble</td>
<td class="org-left">t</td>
<td class="org-left">auto</td>
</tr>


<tr>
<td class="org-left">org-html-postamble-format</td>
<td class="org-left">(("en" "&lt;p class=\"author\"&gt;Author: %a (%e)&lt;/p&gt;<br>&lt;p class=\"date\"&gt;Date: %T&lt;/p&gt;<br>&lt;p class=\"creator\"&gt;%c&lt;/p&gt;"))</td>
<td class="org-left">(("en" "&lt;p class=\"author\"&gt;Author: %a (%e)&lt;/p&gt;<br>&lt;p class=\"date\"&gt;Date: %d&lt;/p&gt;<br>&lt;p class=\"creator\"&gt;%c&lt;/p&gt;<br>&lt;p class=\"validation\"&gt;%v&lt;/p&gt;"))</td>
</tr>


<tr>
<td class="org-left">org-log-done</td>
<td class="org-left">time</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">org-log-into-drawer</td>
<td class="org-left">t</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>


<a id="orgf7fe5e7"></a>

# python configuration

At one point I was using anaconda but have switched back to elpy. I really like `eply-config` that tells you if everything is working properly. I've been using a `virtualenv` for my python development and couldn't be happier. Perhaps ethe only thing that bothers me is that when an object is returned, pycharm will give you list and dictionary methods while eply/company does not. Seems to be the only real issue at this point.

The variables that might be setup for python (look in [custom.el](custom.el) for them): `python-indent-trigger-commands`, `python-shell-completion-setup-code`, `python-shell-completion-string-code`, `python-shell-interpreter`, `python-shell-interpreter-args`, `python-shell-prompt-output-regexp`, and `python-shell-prompt-regexp`.

```emacs-lisp
(use-package company
  :diminish "Co"
  :config
  (when config/use-omnisharp
    (add-to-list 'company-backends 'company-omnisharp)))
(use-package company-jedi)
(use-package elpy
  :demand t
  :config
  (progn
    (elpy-enable)
    (add-hook 'elpy-mode-hook
              '(lambda ()
                 (progn
                   (setq-local flymake-start-syntax-check-on-newline t)
                   (setq-local flymake-no-changes-timeout 0.5))))))
(use-package flycheck
  :diminish  "\u2714"           ;; heavy checkmark
  :config
  (global-flycheck-mode))
(use-package flycheck-pyflakes) ;; flycheck uses flake8!
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

Customized variables for python:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Current Value</th>
<th scope="col" class="org-left">Default Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">python-indent-trigger-commands</td>
<td class="org-left">(yas-expand yas/expand)</td>
<td class="org-left">(indent-for-tab-command yas-expand yas/expand)</td>
</tr>


<tr>
<td class="org-left">python-shell-completion-setup-code</td>
<td class="org-left">"from IPython.core.completerlib import module_completion"</td>
<td class="org-left">"<br>def __PYTHON_EL_get_completions(text):<br>&nbsp;&nbsp;&nbsp;&nbsp;completions = []<br>&nbsp;&nbsp;&nbsp;&nbsp;completer = None<br><br>&nbsp;&nbsp;&nbsp;&nbsp;try:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;import readline<br><br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;try:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;import __builtin__<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;except ImportError:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Python 3<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;import builtins as __builtin__<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;builtins = dir(__builtin__)<br><br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;is_ipython = ('__IPYTHON__' in builtins or<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'__IPYTHON__active' in builtins)<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;splits = text.split()<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;is_module = splits and splits[0] in ('from', 'import')<br><br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;if is_ipython and is_module:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;from IPython.core.completerlib import module_completion<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;completions = module_completion(text.strip())<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;elif is_ipython and '__IP' in builtins:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;completions = __IP.complete(text)<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;elif is_ipython and 'get_ipython' in builtins:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;completions = get_ipython().Completer.all_completions(text)<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;else:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# Try to reuse current completer.<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;completer = readline.get_completer()<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;if not completer:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# importing rlcompleter sets the completer, use it as a<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;# last resort to avoid breaking customizations.<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;import rlcompleter<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;completer = readline.get_completer()<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;if getattr(completer, 'PYTHON_EL_WRAPPED', False):<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;completer.print_mode = False<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;i = 0<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;while True:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;completion = completer(text, i)<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;if not completion:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;break<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;i += 1<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;completions.append(completion)<br>&nbsp;&nbsp;&nbsp;&nbsp;except:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;pass<br>&nbsp;&nbsp;&nbsp;&nbsp;finally:<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;if getattr(completer, 'PYTHON_EL_WRAPPED', False):<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;completer.print_mode = True<br>&nbsp;&nbsp;&nbsp;&nbsp;return completions"</td>
</tr>


<tr>
<td class="org-left">python-shell-completion-string-code</td>
<td class="org-left">"';'.join(get_ipython().Completer.all_completions('''%s'''))<br>"</td>
<td class="org-left">"';'.join(__PYTHON_EL_get_completions('''%s'''))"</td>
</tr>


<tr>
<td class="org-left">python-shell-interpreter</td>
<td class="org-left">"ipython"</td>
<td class="org-left">"python"</td>
</tr>


<tr>
<td class="org-left">python-shell-interpreter-args</td>
<td class="org-left">"-i &#x2013;simple-prompt"</td>
<td class="org-left">"-i"</td>
</tr>


<tr>
<td class="org-left">python-shell-prompt-output-regexp</td>
<td class="org-left">"Out\\\[[0-9]+\\]: "</td>
<td class="org-left">""</td>
</tr>


<tr>
<td class="org-left">python-shell-prompt-regexp</td>
<td class="org-left">"In \\\[[0-9]+\\]: "</td>
<td class="org-left">">>> "</td>
</tr>
</tbody>
</table>


<a id="org9ec16ff"></a>

# ivy configuration

Was a help user, but switched to ivy. Lots of nice features in ivy

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
```

Customized variables:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Current Value</th>
<th scope="col" class="org-left">Default Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">ivy-count-format</td>
<td class="org-left">"(%d/%d) "</td>
<td class="org-left">"%-4d "</td>
</tr>


<tr>
<td class="org-left">ivy-height</td>
<td class="org-left">16</td>
<td class="org-left">10</td>
</tr>


<tr>
<td class="org-left">ivy-mode</td>
<td class="org-left">t</td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">ivy-use-virtual-buffers</td>
<td class="org-left">t</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>


<a id="orgbb3fd86"></a>

# yasnippet configuration

yasnippet is a truly awesome package. Local modifications should go in `~/.emacs.d/snippets/`.

This also takes care of hooking up company completion with yasnippet expansion.

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
(use-package yasnippet-snippets :demand t)      ;; Don't forget the snippets

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

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Symbol Name</th>
<th scope="col" class="org-left">Current Value</th>
<th scope="col" class="org-left">Default Value</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left">yas-global-mode</td>
<td class="org-left">t</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>


<a id="org8184240"></a>

# Additional bits-o-configuration


<a id="org19a619a"></a>

## Limit the length of `which-function`

`which-function` which is used by `powerline` has no maximum method/function signature. This handy advisor limits the name to 64 characters.

```emacs-lisp
(defvar  which-function-max-width 64 "The maximum width of the which-function string.")
(advice-add 'which-function :filter-return
            (lambda (s) (when (stringp s)
                          (if (< (string-width s) which-function-max-width) s
                            (concat (truncate-string-to-width s (- which-function-max-width 3)) "...")))))
```


<a id="org0d170f4"></a>

## `my-ansi-term`

Allows me to name my ANSI terms. Was very useful when I used more ANSI shells (so that tabs were interpretted by the shell). Some other modes and shells make this less useful these days.

```emacs-lisp
(defun my-ansi-term (term-name cmd)
  "Create an ansi term with a name - other than *ansi-term* given TERM-NAME and CMD."
  (interactive "sName for terminal: \nsCommand to run [/bin/bash]: ")
  (ansi-term (if (= 0 (length cmd)) "/bin/bash" cmd))
  (rename-buffer term-name))
```


<a id="org6d91935"></a>

## Understand file type by shebang

When a file is opened and it is determined there is no mode (fundamental-mode) this code reads the first line of the file looking for an appropriate shebang for either python or bash and sets the mode for the file.

```emacs-lisp
(defun my-find-file-hook ()
  "If `fundamental-mode', look for script type so the mode gets properly set.
Script-type is read from #!/... at top of file."
  (if (eq major-mode 'fundamental-mode)
      (condition-case nil
          (save-excursion
            (goto-char (point-min))
            (re-search-forward "^#!\s*/.*/\\(python\\|bash\\).*$")
            (if (string= (match-string 1) "python")
                (python-mode)
              (sh-mode)))
        (error nil))))
(add-hook 'find-file-hook 'my-find-file-hook)
```


<a id="org6af6a16"></a>

## Additional configs

Setup `eldoc` mode, use y-or-n (instead of yes and no). Key bindings&#x2026;

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

;; Turn on some stuff that's normally set off
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
```

