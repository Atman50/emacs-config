
# Table of Contents

1.  [Overview](#org2114404)
    1.  [Quick start](#orgb1a64c8)
    2.  [`init.el` explained](#org1aaa393)
2.  [Configuration](#org1f51163)
    1.  [Just a little preamble](#org45e727b)
    2.  [General packages](#orgdb136dc)
        1.  [auto-compile](#orga797d97)
        2.  [diminish](#org3a631c9)
        3.  [bind-key](#org55b0119)
        4.  [savehist](#org833d2f8)
        5.  [ag](#org44f3190)
        6.  [powershell](#org8d339b2)
        7.  [themes and modeline](#orgffd2353)
        8.  [aspx editing](#org4324ac6)
        9.  [Other useful packages](#org34921ed)
3.  [Working with C#](#org42639be)
4.  [magit configuration](#orgad994fa)
5.  [org-mode configuration](#org73c5634)
6.  [python configuration](#org3d4f730)
7.  [ivy configuration](#org448f201)
8.  [yasnippet configuration](#org6e1c6e7)
9.  [Additional bits-o-configuration](#orgf516ee6)
    1.  [Limit the length of `which-function`](#orga25a234)
    2.  [`my-ansi-term`](#org550ae29)
    3.  [Understand file type by shebang](#org08ed305)
    4.  [Additional configs](#org26ee26c)



<a id="org2114404"></a>

# Overview

This is my literate and **portable** Emacs initialization "system."


<a id="orgb1a64c8"></a>

## Quick start

First git clone this repository into `~/.emacs.d`: `git clone https://github.com/Atman50/emacs-config.git =/.emacs.d`

Now simply start Emacs and all the packages/configuration loads. It takes some time on the first load since all the packages referenced need to download and compile. On subsequent Emacs invocations startup time is better.

The ability to simply clone and start makes this configuration **highly portable**. One issue is that some of the customization are file system dependent. I handle this by using git to create a stash of the localized changes for `custom.el` and then apply it whenever I take updated configurations from the repository.

A minor warning is that Emacs load times can be somewhat slow. Startup continues to get slower as the size of the desktop file increases (the more files that need to be opened at the start of Emacs). Since I tend to stay in Emacs for quite some time, this doesn't get in my way.


<a id="org1aaa393"></a>

## `init.el` explained

To get started with a literate configuration, I use this simple `init.el` file.

Following this code block is the explanation.

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

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


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
<td class="org-left">(("gnu" . "https://elpa.gnu.org/packages/")<br>&nbsp;("melpa" . "https://melpa.org/packages/")<br>&nbsp;("org" . "https://orgmode.org/elpa/"))<br></td>
<td class="org-left">(("gnu" . "http://elpa.gnu.org/packages/"))<br></td>
</tr>
</tbody>
</table>


<a id="org1f51163"></a>

# Configuration


<a id="org45e727b"></a>

## Just a little preamble

This is a little piece of code that I picked up that might make things faster when downloading and installing all the packages. This turns down the garbage collector during the use-package loading when it has to do some compiling. Set it back when done with init.

    (setq gc-cons-threshold 64000000)
    (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

Also create a handy variable to know if we are Windows - used later on here.

    (defvar mswindows-p (string-match "windows" (symbol-name system-type)))


<a id="orgdb136dc"></a>

## General packages

Here are some general packages


<a id="orga797d97"></a>

### [auto-compile](https://github.com/emacscollective/auto-compile)

This package provides a guarantee that compiled byte code files are never outdated by mistake.

    (use-package auto-compile
      :config
      (progn
        (auto-compile-on-load-mode)
        (auto-compile-on-save-mode)))

Customized variables:

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


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
<td class="org-left">load-prefer-newer</td>
<td class="org-left">t</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>


<a id="org3a631c9"></a>

### [diminish](https://github.com/myrjola/diminish.el)

Handy mode to make the modeline nicer. I also use to set mode to special characters (for example, see flycheck-mode)

    (use-package diminish)


<a id="org55b0119"></a>

### [bind-key](https://github.com/priyadarshan/bind-key)

Much better binding capabilities

    (use-package bind-key)


<a id="org833d2f8"></a>

### savehist

A great builtin that allows us to have a history file. This means certain elements are saved between sessions of emacs. Set the following variables to control `savehist` (use customize).

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


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
<td class="org-left">(kill-ring search-ring regexp-search-ring)<br></td>
<td class="org-left">nil</td>
</tr>


<tr>
<td class="org-left">savehist-mode</td>
<td class="org-left">t</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>

    (use-package savehist :demand t)                ;; Nice history in ~/.emacs.d/savehist


<a id="org44f3190"></a>

### [ag](https://github.com/Wilfred/ag.el)

AKA silversearcher. Simple interface to excellent tool. I have it installed in my cygwin64 area and it seems to play well in my Windows environment.

NB: doesn't seem to work so well under Windows.

    (use-package ag)


<a id="org8d339b2"></a>

### [powershell](http://github.com/jschaf/powershell.el)

Excellent too to run powershell in Emacs

    (use-package powershell
      :if mswindows-p)


<a id="orgffd2353"></a>

### themes and modeline

    (load-theme 'leuven t)                          ;; Theme: works better before powerline
    (use-package powerline
      :demand t
      :config (powerline-default-theme))


<a id="org4324ac6"></a>

### aspx editing

Make aspx editing more palatable using html mode

    (add-to-list 'auto-mode-alist
                 '("\\.aspx\\'" . html-mode)
                 '("\\.aspcx\\'" . html-mode))


<a id="org34921ed"></a>

### Other useful packages

Ok, a little tired of documenting each package on it's own. These packages are just generally useful.

`which-key` very helpful for finding way around.

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

Note that the setting of `desktop-path` allows the multiple `.emacs.desktop` files, each in the directory where `emacs` was started. Although `desktop-path` is changed outside `custom.el`, I've included it here in the table below so you can see that the default is augmented with the startup directory which in this case is `~/.emacs.d`.

Customized variables of interest here:

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


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
<td class="org-left">("`/.emacs.d/" "~/.emacs.d/" "`")<br></td>
<td class="org-left">("`/.emacs.d/" "`")<br></td>
</tr>


<tr>
<td class="org-left">desktop-save-mode</td>
<td class="org-left">t</td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>


<a id="org42639be"></a>

# Working with C#

Because I'm a C# developer and pretty much dislike a lot of the GUI issues in Visual Studio, I've spent some amount of time coming up with a good C# configuration. This works spectularly well and takes only minutes to setup.

To use omnisharp follow these directions:

1.  Load up local omnisharp (roslyn flavor) from [Omnisharp-Roslyn releases](https://github.com/OmniSharp/omnisharp-roslyn/releases)
2.  Customize the variable `omnisharp-server-executable-path` to point to your omnisharp roslyn. For example "c:/omnisharp-roslyn-v1.27.2/OmniSharp.exe".

There are comprehensive directions at [omnisharp-emacs](https://github.com/OmniSharp/omnisharp-emacs.git).

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


<a id="orgad994fa"></a>

# [magit](https://github.com/magit/magit) configuration

The most awesome git porcelain. Most here are part of magit, `[[https://github.com/pidu/git-timemachine][git-time-machine]]` is not, but well worth using.

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

Customized variables:

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


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
<td class="org-left">(("~/repos" . 1))<br></td>
<td class="org-left">nil</td>
</tr>
</tbody>
</table>


<a id="org73c5634"></a>

# org-mode configuration

Org-mode configurations. `org-bullets` used to be part of org but is now outside.

The `htmlize` package allows for `org-html-export-to-html` to operate.

    (use-package org-bullets
       :demand t
       :config (add-hook 'org-mode-hook 'org-bullets-mode))
    (use-package org-autolist :demand t)
    (use-package htmlize :demand t)
    (load-library "ox-md")      ;; Turn on MD exports

Customized variables for org-mode:

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


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
<td class="org-left">(("en" "<p class=\\"author\\">Author: %a (%e)</p><br><p class=\\"date\\">Date: %T</p><br><p class=\\"creator\\">%c</p>"))<br></td>
<td class="org-left">(("en" "<p class=\\"author\\">Author: %a (%e)</p><br><p class=\\"date\\">Date: %d</p><br><p class=\\"creator\\">%c</p><br><p class=\\"validation\\">%v</p>"))<br></td>
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


<a id="org3d4f730"></a>

# python configuration

At one point I was using anaconda but have switched back to elpy. I really like `eply-config` that tells you if everything is working properly. I've been using a `virtualenv` for my python development and couldn't be happier. Perhaps ethe only thing that bothers me is that when an object is returned, pycharm will give you list and dictionary methods while eply/company does not. Seems to be the only real issue at this point.

The variables that might be setup for python (look in [custom.el](custom.el) for them): `python-indent-trigger-commands`, `python-shell-completion-setup-code`, `python-shell-completion-string-code`, `python-shell-interpreter`, `python-shell-interpreter-args`, `python-shell-prompt-output-regexp`, and `python-shell-prompt-regexp`.

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

Customized variables for python:

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


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
<td class="org-left">(yas-expand yas/expand)<br></td>
<td class="org-left">(indent-for-tab-command yas-expand yas/expand)<br></td>
</tr>


<tr>
<td class="org-left">python-shell-completion-setup-code</td>
<td class="org-left">"from IPython.core.completerlib import modulecompletion</sub>"</td>
<td class="org-left">"<br>def PYTHONEL</sub>get</sub>completions</sub>(text):<br>    completions = []<br>    completer = None<br><br>    try:<br>        import readline<br><br>        try:<br>            import \_builtin</sub><br>        except ImportError:<br>            # Python 3<br>            import builtins as builtin<br>        builtins = dir(builtin)<br><br>        isipython</sub> = ('IPYTHON' in builtins or<br>                      '\_IPYTHON</sub>\_active</sub>' in builtins)<br>        splits = text.split()<br>        ismodule</sub> = splits and splits[0] in ('from', 'import')<br><br>        if isipython</sub> and ismodule</sub>:<br>            from IPython.core.completerlib import modulecompletion<br></sub>            completions = modulecompletion</sub>(text.strip())<br>        elif isipython</sub> and '\_IP</sub>' in builtins:<br>            completions = \_IP.complete</sub>(text)<br>        elif isipython</sub> and 'getipython</sub>' in builtins:<br>            completions = getipython</sub>().Completer.allcompletions</sub>(text)<br>        else:<br>            # Try to reuse current completer.<br>            completer = readline.getcompleter</sub>()<br>            if not completer:<br>                # importing rlcompleter sets the completer, use it as a<br>                # last resort to avoid breaking customizations.<br>                import rlcompleter<br>                completer = readline.getcompleter</sub>()<br>            if getattr(completer, 'PYTHONEL</sub>WRAPPED</sub>', False):<br>                completer.printmode</sub> = False<br>            i = 0<br>            while True:<br>                completion = completer(text, i)<br>                if not completion:<br>                    break<br>                i += 1<br>                completions.append(completion)<br>    except:<br>        pass<br>    finally:<br>        if getattr(completer, 'PYTHONEL</sub>WRAPPED</sub>', False):<br>            completer.printmode</sub> = True<br>    return completions"</td>
</tr>


<tr>
<td class="org-left">python-shell-completion-string-code</td>
<td class="org-left">"';'.join(getipython</sub>().Completer.allcompletions</sub>('''%s'''))<br>"</td>
<td class="org-left">"';'.join(\_PYTHON</sub>EL</sub>get</sub>completions</sub>('''%s'''))"</td>
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


<a id="org448f201"></a>

# ivy configuration

Was a help user, but switched to ivy. Lots of nice features in ivy

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

Customized variables:

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


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


<a id="org6e1c6e7"></a>

# yasnippet configuration

yasnippet is a truly awesome package. Local modifications should go in `~/.emacs.d/snippets/`.

This also takes care of hooking up company completion with yasnippet expansion.

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

Configured variables of interest:

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


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


<a id="orgf516ee6"></a>

# Additional bits-o-configuration


<a id="orga25a234"></a>

## Limit the length of `which-function`

`which-function` which is used by `powerline` has no maximum method/function signature. This handy advisor limits the name to 64 characters.

    (defvar  which-function-max-width 64 "The maximum width of the which-function string.")
    (advice-add 'which-function :filter-return
                (lambda (s) (when (stringp s)
                              (if (< (string-width s) which-function-max-width) s
                                (concat (truncate-string-to-width s (- which-function-max-width 3)) "...")))))


<a id="org550ae29"></a>

## `my-ansi-term`

Allows me to name my ANSI terms. Was very useful when I used more ANSI shells (so that tabs were interpretted by the shell). Some other modes and shells make this less useful these days.

    (defun my-ansi-term (term-name cmd)
      "Create an ansi term with a name - other than *ansi-term* given TERM-NAME and CMD."
      (interactive "sName for terminal: <br>sCommand to run [/bin/bash]: ")
      (ansi-term (if (= 0 (length cmd)) "/bin/bash" cmd))
      (rename-buffer term-name))


<a id="org08ed305"></a>

## Understand file type by shebang

When a file is opened and it is determined there is no mode (fundamental-mode) this code reads the first line of the file looking for an appropriate shebang for either python or bash and sets the mode for the file.

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


<a id="org26ee26c"></a>

## Additional configs

Setup `eldoc` mode, use y-or-n (instead of yes and no). Key bindings&#x2026;

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

