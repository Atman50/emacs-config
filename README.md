<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Overview</a>
<ul>
<li><a href="#sec-1-1">1.1. Quick start</a></li>
<li><a href="#sec-1-2">1.2. <code>init.el</code> explained</a></li>
</ul>
</li>
<li><a href="#sec-2">2. Configuration</a>
<ul>
<li><a href="#sec-2-1">2.1. Just a little preamble</a></li>
<li><a href="#sec-2-2">2.2. General packages</a>
<ul>
<li><a href="#sec-2-2-1">2.2.1. auto-compile</a></li>
<li><a href="#sec-2-2-2">2.2.2. diminish</a></li>
<li><a href="#sec-2-2-3">2.2.3. bind-key</a></li>
<li><a href="#sec-2-2-4">2.2.4. savehist</a></li>
<li><a href="#sec-2-2-5">2.2.5. ag</a></li>
<li><a href="#sec-2-2-6">2.2.6. powershell</a></li>
<li><a href="#sec-2-2-7">2.2.7. themes and modeline</a></li>
<li><a href="#sec-2-2-8">2.2.8. aspx editing</a></li>
<li><a href="#sec-2-2-9">2.2.9. Other useful packages</a></li>
</ul>
</li>
</ul>
</li>
<li><a href="#sec-3">3. Working with C#</a></li>
<li><a href="#sec-4">4. magit configuration</a></li>
<li><a href="#sec-5">5. org-mode configuration</a></li>
<li><a href="#sec-6">6. python configuration</a></li>
<li><a href="#sec-7">7. ivy configuration</a></li>
<li><a href="#sec-8">8. yasnippet configuration</a></li>
<li><a href="#sec-9">9. Additional bits-o-configuration</a>
<ul>
<li><a href="#sec-9-1">9.1. Limit the length of <code>which-function</code></a></li>
<li><a href="#sec-9-2">9.2. <code>my-ansi-term</code></a></li>
<li><a href="#sec-9-3">9.3. Understand file type by shebang</a></li>
<li><a href="#sec-9-4">9.4. Additional configs</a></li>
</ul>
</li>
</ul>
</div>
</div>


# Overview<a id="sec-1" name="sec-1"></a>

This is my literate and **portable** Emacs initialization "system."

## Quick start<a id="sec-1-1" name="sec-1-1"></a>

First git clone this repository into `<home>/.emacs.d`: `git clone https://github.com/Atman50/emacs-config.git ~/.emacs.d`

Now simply start Emacs and all the packages/configuration loads. It takes some time on the first load since all the packages referenced need to download and compile. On subsequent Emacs invocations startup time is better.

The ability to simply clone and start makes this configuration **highly portable**. One issue is that some of the customization are file system dependent. I handle this by using git to create a stash of the localized changes for `custom.el` and then apply it whenever I take updated configurations from the repository.

A minor warning is that Emacs load times can be somewhat slow. Startup continues to get slower as the size of the desktop file increases (the more files that need to be opened at the start of Emacs). Since I tend to stay in Emacs for quite some time, this doesn't get in my way.

## `init.el` explained<a id="sec-1-2" name="sec-1-2"></a>

To get started with a literate configuration, I use this simple `init.el` file.

Following this code block is the explanation.

    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))      ;; 1
    (load custom-file t)
    
    (package-initialize)                                                        ;; 2
    
    (prefer-coding-system 'utf-8)                                               ;; 3
    
    (package-refresh-contents)                                                  ;; 4
    
    (unless (package-installed-p 'use-package)                                  ;; 5
      (package-install 'use-package))
    (require 'use-package)
    
    (use-package org                                                            ;; 6
      :demand t
      :pin "org")
    
    (defvar my-cfg (concat user-emacs-directory "README"))                      ;; 7
    (when (file-newer-than-file-p (concat my-cfg ".org") (concat my-cfg ".el"))
      (org-babel-tangle-file (concat my-cfg ".org")))
    (load my-cfg)

1.  Provide for a separate `custom.el` file. Keeping the customizations separate and first allows for standard emacs customization of **most** of the package variables. When package variables setup outside of the customization file, then `M-x describe-variable` says it was "changed outside of customization".

2.  Initializes the package system. Put this after the customizations so that the variable `package-load-list` can be customized (I use [Gnu](https://gnu.org/packages), [Melpa](https://melpa.org/packages), and [Org](https://orgmode.org/packages) - see `custom.el`).

3.  Sets the preferred coding-system. Since I work on Windows sometimes and some Melpa packages are lacking the proper [byte order mark](https://en.wikipedia.org/wiki/Byte_order_mark) at the beginning of the file, this needs to happen before `package-refresh-contents` so that it finishes without issue. It's not a bad idea to just do this ubiquitously, so I do it here.

4.  Gets the package contents. This uses `package-archives` from the `custom.el` file to load up the repository contents.

5.  Assures use-package is loaded; `use-package` is used to perform the remainder of this configuration.

6.  Installs org. Org-babel is used to load the configuration documented here. This code makes sure it's loaded and that it comes from the "org" repository.

7.  Does the deed and loads this file. If the file has already been "babel-ed" then just load the results, otherwise do the "babel-ing".

That's it. It really is that simple.

The package-initialize in the above code depends upon:

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<tbody>
<tr>
<td class="left">package-archives</td>
<td class="left">(("gnu" . "https://elpa.gnu.org/packages/") ("melpa" . "https://melpa.org/packages/") ("org" . "https://orgmode.org/elpa/"))</td>
<td class="left">(("gnu" . "http://elpa.gnu.org/packages/"))</td>
</tr>
</tbody>
</table>

# Configuration<a id="sec-2" name="sec-2"></a>

## Just a little preamble<a id="sec-2-1" name="sec-2-1"></a>

This is a little piece of code that I picked up that might make things faster when downloading and installing all the packages. This turns down the garbage collector during the use-package loading when it has to do some compiling. Set it back when done with init.

    (setq gc-cons-threshold 64000000)
    (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

Also create a handy variable to know if we are Windows - used later on here.

    (defvar mswindows-p (string-match "windows" (symbol-name system-type)))

## General packages<a id="sec-2-2" name="sec-2-2"></a>

Here are some general packages

### [auto-compile](https://github.com/emacscollective/auto-compile)<a id="sec-2-2-1" name="sec-2-2-1"></a>

This package provides a guarantee that compiled byte code files are never outdated by mistake. You'll want to modify the variable `load-prefer-newer`.

    (use-package auto-compile
      :config
      (progn
        (auto-compile-on-load-mode)
        (auto-compile-on-save-mode)))

### [diminish](https://github.com/myrjola/diminish.el)<a id="sec-2-2-2" name="sec-2-2-2"></a>

Handy mode to make the modeline nicer. I also use to set mode to special characters (for example, see flycheck-mode)

    (use-package diminish)

### [bind-key](https://github.com/priyadarshan/bind-key)<a id="sec-2-2-3" name="sec-2-2-3"></a>

Much better binding capabilities

    (use-package bind-key)

### savehist<a id="sec-2-2-4" name="sec-2-2-4"></a>

A great builtin that allows us to have a history file. This means certain elements are saved between sessions of emacs. Set the following variables to control `savehist` (use customize).

<table border="2" cellspacing="0" cellpadding="6" rules="all" frame="border">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="left" />
</colgroup>
<tbody>
<tr>
<td class="left">savehist-file</td>
<td class="left">"~/.emacs.d/savehist"</td>
<td class="left">"~/.emacs.d/history"</td>
</tr>


<tr>
<td class="left">savehist-additional-variables</td>
<td class="left">(kill-ring search-ring regexp-search-ring)</td>
<td class="left">nil</td>
</tr>


<tr>
<td class="left">savehist-mode</td>
<td class="left">t</td>
<td class="left">nil</td>
</tr>
</tbody>
</table>

    (use-package savehist :demand t)                ;; Nice history in ~/.emacs.d/savehist

### [ag](https://github.com/Wilfred/ag.el)<a id="sec-2-2-5" name="sec-2-2-5"></a>

AKA silversearcher. Simple interface to excellent tool. I have it installed in my cygwin64 area and it seems to play well in my Windows environment.

NB: doesn't seem to work so well under Windows.

    (use-package ag)

### [powershell](http://github.com/jschaf/powershell.el)<a id="sec-2-2-6" name="sec-2-2-6"></a>

Excellent too to run powershell in Emacs

    (use-package powershell
      :if mswindows-p)

### themes and modeline<a id="sec-2-2-7" name="sec-2-2-7"></a>

    (load-theme 'leuven t)                          ;; Theme: works better before powerline
    (use-package powerline
      :demand t
      :config (powerline-default-theme))

### aspx editing<a id="sec-2-2-8" name="sec-2-2-8"></a>

Make aspx editing more palatable using html mode

    (add-to-list 'auto-mode-alist
                 '("\\.aspx\\'" . html-mode)
                 '("\\.aspcx\\'" . html-mode))

### Other useful packages<a id="sec-2-2-9" name="sec-2-2-9"></a>

Ok, a little tired of documenting each package on it's own. These packages are just generally useful.

`which-key` very helpful for finding way around.

The `desktop` package allows for saved desktops in the emacs start directory (`desktop-path` gets set here). Modify `desktop-save-mode` to t to turn on desktop saving.

Make sure to customize `projectile-completion-system` to "ivy".

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

# Working with C#<a id="sec-3" name="sec-3"></a>

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

# [magit](https://github.com/magit/magit) configuration<a id="sec-4" name="sec-4"></a>

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

# org-mode configuration<a id="sec-5" name="sec-5"></a>

Org mode configurations. `org-bullets` used to be part of org but is now outside.

The `htmlize` package allows for `org-html-export-to-html` to operate.

    (use-package org-bullets
       :demand t
       :config (add-hook 'org-mode-hook 'org-bullets-mode))
    (use-package org-autolist :demand t)
    (use-package htmlize :demand t)

# python configuration<a id="sec-6" name="sec-6"></a>

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

# ivy configuration<a id="sec-7" name="sec-7"></a>

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

# yasnippet configuration<a id="sec-8" name="sec-8"></a>

yasnippet is a truly awesome package. Local modifications should go in "~/.emacs.d/snippets/".

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

# Additional bits-o-configuration<a id="sec-9" name="sec-9"></a>

## Limit the length of `which-function`<a id="sec-9-1" name="sec-9-1"></a>

`which-function` which is used by `powerline` has no maximum method/function signature. This handy advisor limits the name to 64 characters.

    (defvar  which-function-max-width 64 "The maximum width of the which-function string.")
    (advice-add 'which-function :filter-return
                (lambda (s) (when (stringp s)
                              (if (< (string-width s) which-function-max-width) s
                                (concat (truncate-string-to-width s (- which-function-max-width 3)) "...")))))

## `my-ansi-term`<a id="sec-9-2" name="sec-9-2"></a>

Allows me to name my ANSI terms. Was very useful when I used more ANSI shells (so that tabs were interpretted by the shell). Some other modes and shells make this less useful these days.

    (defun my-ansi-term (term-name cmd)
      "Create an ansi term with a name - other than *ansi-term* given TERM-NAME and CMD."
      (interactive "sName for terminal: \nsCommand to run [/bin/bash]: ")
      (ansi-term (if (= 0 (length cmd)) "/bin/bash" cmd))
      (rename-buffer term-name))

## Understand file type by shebang<a id="sec-9-3" name="sec-9-3"></a>

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

## Additional configs<a id="sec-9-4" name="sec-9-4"></a>

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