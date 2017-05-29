
(load-file "~/.emacs.d/sensible-defaults.el/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(add-to-list 'load-path "~/.emacs.d/resources/")
(require 'mbsync)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)

(package-initialize)
(elpy-enable)

(eval-after-load "erc"
  '(progn
     (load "~/.erc/.erc-auth")
     ;; Set personal information
     (setq erc-nick "talfouzan")
     (setq erc-user-full-name "Thunayan Al Fouzan")

     ;;Mirc colors
     (setq erc-interpret-mirc-color t)

     ;; Set autojoin channels
     (setq erc-autojoin-channels-alist
           '(("freenode.net" "#emacs" "#emacs-beginners" "#erc")))))

;; Set autoconnect networks
(defun my-erc ()
  "Connect to my default ERC servers."

  (interactive)
  (erc-tls :server "irc.freenode.net" :port 7000))

; add the source shipped with mu to load-path
(add-to-list 'load-path (expand-file-name "/usr/local/Cellar/mu/0.9.18_1/share/emacs/site-lisp/mu/mu4e"))

; make sure emacs finds applications in /usr/local/bin
(setq exec-path (cons "/usr/local/bin" exec-path))

; require mu4e
(require 'mu4e)

; tell mu4e where my Maildir is
(setq mu4e-maildir "/Users/talfouzan/Mail")
; tell mu4e how to sync email
(setq mu4e-get-mail-command "/usr/local/bin/mbsync -a")
; tell mu4e to use w3m for html rendering
(setq mu4e-html2text-command "/usr/local/bin/w3m -T text/html")

; taken from mu4e page to define bookmarks
(add-to-list 'mu4e-bookmarks
            '("size:5M..500M"       "Big messages"     ?b))

; mu4e requires to specify drafts, sent, and trash dirs
; a smarter configuration allows to select directories according to the account (see mu4e page)
(setq mu4e-drafts-folder "/work/drafts")
(setq mu4e-sent-folder "/work/sent")
(setq mu4e-trash-folder "/work/trash")
;; Choose the style you prefer for desktop notifications
;; If you are on Linux you can use
;; 1. notifications - Emacs lisp implementation of the Desktop Notifications API
;; 2. libnotify     - Notifications using the `notify-send' program, requires `notify-send' to be in PATH
;;
;; On Mac OSX you can set style to
;; 1. notifier      - Notifications using the `terminal-notifier' program, requires `terminal-notifier' to be in PATH
;; 1. growl         - Notifications using the `growl' program, requires `growlnotify' to be in PATH
(mu4e-alert-set-default-style 'osx-notifier)
(alert-add-rule :category "mu4e-alert" :style 'fringe :predicate (lambda (_) (string-match-p "^mu4e-" (symbol-name major-mode))) :continue t)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
(setq mu4e-alert-email-notification-types '(count))
;;store org-mode links to messages
(require 'org-mu4e)
;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

; use msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/local/bin/msmtp")
; tell msmtp to choose the SMTP server according to the from field in the outgoing email
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)
(evil-leader/set-leader ";")

(evil-mode 1)

(global-evil-surround-mode 1)

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system
  (scroll-bar-mode -1))

(global-prettify-symbols-mode t)

(when window-system
  (setq solarized-use-variable-pitch nil)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-dark t))

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(setq hrs/default-font "Inconsolata")
(setq hrs/default-font-size 14)
(setq hrs/current-font-size hrs/default-font-size)

(setq hrs/font-change-increment 1.1)

(defun hrs/set-font-size ()
  "Set the font to `hrs/default-font' at `hrs/current-font-size'."
  (set-frame-font
   (concat hrs/default-font "-" (number-to-string hrs/current-font-size))))

(defun hrs/reset-font-size ()
  "Change font size back to `hrs/default-font-size'."
  (interactive)
  (setq hrs/current-font-size hrs/default-font-size)
  (hrs/set-font-size))

(defun hrs/increase-font-size ()
  "Increase current font size by a factor of `hrs/font-change-increment'."
  (interactive)
  (setq hrs/current-font-size
        (ceiling (* hrs/current-font-size hrs/font-change-increment)))
  (hrs/set-font-size))

(defun hrs/decrease-font-size ()
  "Decrease current font size by a factor of `hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-font-size
        (max 1
             (floor (/ hrs/current-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(define-key global-map (kbd "C-)") 'hrs/reset-font-size)
(define-key global-map (kbd "C-+") 'hrs/increase-font-size)
(define-key global-map (kbd "C-=") 'hrs/increase-font-size)
(define-key global-map (kbd "C-_") 'hrs/decrease-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

(hrs/reset-font-size)

(when window-system
  (global-hl-line-mode))

(defmacro diminish-minor-mode (filename mode &optional abbrev)
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(diminish-minor-mode 'abbrev 'abbrev-mode)
(diminish-minor-mode 'simple 'auto-fill-function)
(diminish-minor-mode 'company 'company-mode)
(diminish-minor-mode 'eldoc 'eldoc-mode)
(diminish-minor-mode 'flycheck 'flycheck-mode)
(diminish-minor-mode 'flyspell 'flyspell-mode)
(diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
(diminish-minor-mode 'projectile 'projectile-mode)
(diminish-minor-mode 'ruby-end 'ruby-end-mode)
(diminish-minor-mode 'subword 'subword-mode)
(diminish-minor-mode 'undo-tree 'undo-tree-mode)
(diminish-minor-mode 'yard-mode 'yard-mode)
(diminish-minor-mode 'yasnippet 'yas-minor-mode)
(diminish-minor-mode 'wrap-region 'wrap-region-mode)

(diminish-minor-mode 'paredit 'paredit-mode " π")

(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'haskell-mode-hook "λ=")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'python-mode-hook "Py")

(require 'diff-hl)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(defun hrs/view-buffer-name ()
    "Display the filename of the current buffer."
    (interactive)
    (message (buffer-file-name)))

  (defun hrs/generate-scratch-buffer ()
    "Create and switch to a temporary scratch buffer with a random
       name."
    (interactive)
    (switch-to-buffer (make-temp-name "scratch-")))

  (defun hrs/beautify-json ()
    "Pretty-print the JSON in the marked region. Currently shells
       out to `jsonpp'--be sure that's installed!"
    (interactive)
    (save-excursion
      (shell-command-on-region (mark) (point) "jsonpp" (buffer-name) t)))

  (defun hrs/unfill-paragraph ()
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive)
    (let ((fill-column (point-max)))
      (fill-paragraph nil)))

  (defun hrs/kill-current-buffer ()
    "Kill the current buffer without prompting."
    (interactive)
    (kill-buffer (current-buffer)))

  (defun hrs/visit-last-dired-file ()
    "Open the last file in an open dired buffer."
    (end-of-buffer)
    (previous-line)
    (dired-find-file))

  (defun hrs/visit-last-migration ()
    "Open the last file in 'db/migrate/'. Relies on projectile. Pretty sloppy."
    (interactive)
    (dired (expand-file-name "db/migrate" (projectile-project-root)))
    (hrs/visit-last-dired-file)
    (kill-buffer "migrate"))

  (defun hrs/add-auto-mode (mode &rest patterns)
    "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
    (dolist (pattern patterns)
      (add-to-list 'auto-mode-alist (cons pattern mode))))

  (defun hrs/find-file-as-sudo ()
    (interactive)
    (let ((file-name (buffer-file-name)))
      (when file-name
        (find-alternate-file (concat "/sudo::" file-name)))))

  (defun hrs/insert-random-string (len)
    "Insert a random alphanumeric string of length len."
    (interactive)
    (let ((mycharset "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstyvwxyz"))
      (dotimes (i len)
        (insert (elt mycharset (random (length mycharset)))))))

  (defun hrs/generate-password ()
    "Insert a good alphanumeric password of length 30."
    (interactive)
    (hrs/insert-random-string 30))

  (defun reload-settings ()
   (interactive)
   (org-babel-load-file "~/.emacs.d/settings.org"))

  (defun settings ()
   (interactive)
   (find-file "~/.emacs.d/settings.org"))

(defun hrs/de-unicode ()
  "Tidy up a buffer by replacing all special Unicode characters
     (smart quotes, etc.) with their more sane cousins"
  (interactive)
  (let ((unicode-map '(("[\u2018\|\u2019\|\u201A\|\uFFFD]" . "'")
                       ("[\u201c\|\u201d\|\u201e]" . "\"")
                       ("\u2013" . "--")
                       ("\u2014" . "---")
                       ("\u2026" . "...")
                       ("\u00A9" . "(c)")
                       ("\u00AE" . "(r)")
                       ("\u2122" . "TM")
                       ("[\u02DC\|\u00A0]" . " "))))
    (save-excursion
      (loop for (key . value) in unicode-map
            do
            (goto-char (point-min))
            (replace-regexp key value)))))

(setq-default tab-width 2)

(global-subword-mode 1)

(setq compilation-scroll-output t)

(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(hrs/add-auto-mode 'scheme-mode "\\.blu$")

(global-set-key (kbd "C-x g") 'magit-status)

(setq magit-push-always-verify nil)

(add-hook 'with-editor-mode-hook 'evil-insert-state)

(defun hrs/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(global-set-key (kbd "C-c v") 'projectile-ag)
(global-set-key (kbd "C-c C-v") 'hrs/search-project-for-symbol-at-point)

(setq projectile-switch-project-action 'projectile-dired)

(setq python-indent 2)

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(add-hook 'web-mode-hook
          (lambda ()
            (rainbow-mode)
            (rspec-mode)
            (setq web-mode-markup-indent-offset 2)))

(hrs/add-auto-mode
 'web-mode
 "\\.erb$"
 "\\.html$"
 "\\.php$"
 "\\.rhtml$")

(global-set-key (kbd "C-c s") 'multi-term)

(setq multi-term-program-switches "--login")

(evil-set-initial-state 'term-mode 'emacs)

(defun hrs/term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(add-hook 'term-mode-hook
          (lambda ()
            (goto-address-mode)
            (define-key term-raw-map (kbd "C-y") 'hrs/term-paste)
            (define-key term-raw-map (kbd "<mouse-2>") 'hrs/term-paste)
            (define-key term-raw-map (kbd "M-o") 'other-window)
            (setq yas-dont-activate t)))

(require 'dired-x)
(require 'dired+)
(require 'dired-open)

(setq dired-open-extensions
      '(("pdf" . "evince")
        ("mkv" . "vlc")
        ("mp4" . "vlc")
        ("avi" . "vlc")))

(setq-default dired-listing-switches "-lhvA")

(evil-define-key 'normal dired-mode-map (kbd "j") 'dired-next-line)
(evil-define-key 'normal dired-mode-map (kbd "k") 'dired-previous-line)

(setq dired-clean-up-buffers-too t)

(setq dired-recursive-copies 'always)

(setq dired-recursive-deletes 'top)

(setq server-socket-dir (expand-file-name "server" user-emacs-directory))
(server-start)

(global-set-key (kbd "C-x k") 'hrs/kill-current-buffer)

(setq exec-path (append exec-path '("/usr/local/bin")))

(add-hook 'after-init-hook 'global-company-mode)

(setq-default indent-tabs-mode nil)

(setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
(yas-global-mode 1)

(setq yas/indent-line nil)
;; function to return first name of email recipients
;; used by yasnippet
;; inspired by
;;http://blog.binchen.org/posts/how-to-use-yasnippets-to-produce-email-templates-in-emacs.html
(defun bjm/mu4e-get-names-for-yasnippet ()
  "Return comma separated string of names for an email"
  (interactive)
  (let ((email-name "") str email-string email-list email-name2 tmpname)
    (save-excursion
      (goto-char (point-min))
      ;; first line in email could be some hidden line containing NO to field
      (setq str (buffer-substring-no-properties (point-min) (point-max))))
    ;; take name from TO field - match series of names
    (when (string-match "^To: \"?\\(.+\\)" str)
      (setq email-string (match-string 1 str)))
    ;;split to list by comma
    (setq email-list (split-string email-string " *, *"))
    ;;loop over emails
    (dolist (tmpstr email-list)
      ;;get first word of email string
      (setq tmpname (car (split-string tmpstr " ")))
      ;;remove whitespace or ""
      (setq tmpname (replace-regexp-in-string "[ \"]" "" tmpname))
      ;;join to string
      (setq email-name
            (concat email-name ", " tmpname)))
    ;;remove initial comma
    (setq email-name (replace-regexp-in-string "^, " "" email-name))

    ;;see if we want to use the name in the FROM field
    ;;get name in FROM field if available, but only if there is only
    ;;one name in TO field
    (if (< (length email-list) 2)
        (when (string-match "^\\([^ ,\n]+\\).+writes:$" str)
          (progn (setq email-name2 (match-string 1 str))
                 ;;prefer name in FROM field if TO field has "@"
                 (when (string-match "@" email-name)
                   (setq email-name email-name2))
                 )))
    email-name))

(define-abbrev-table 'global-abbrev-table
  '((";name" "Thunayan Al Fouzan")
    (";email" "talfouzan@me.com")
    (";tb" "")
    (";site" "")))

(setq-default abbrev-mode t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(ido-ubiquitous)
(flx-ido-mode 1) ; better/faster matching
(setq ido-create-new-buffer 'always) ; don't confirm to create new buffers
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-hook 'gfm-mode-hook 'flyspell-mode)

(hrs/add-auto-mode 'gfm-mode "\\.md$")

(setq markdown-command "pandoc --standalone --mathjax --from=markdown")

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'gfm-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(global-set-key (kbd "C-c q") 'auto-fill-mode)

(require 'flycheck)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(add-to-list 'flycheck-checkers 'proselint)

(add-hook 'markdown-mode-hook #'flycheck-mode)
(add-hook 'gfm-mode-hook #'flycheck-mode)
(add-hook 'text-mode-hook #'flycheck-mode)
(add-hook 'org-mode-hook #'flycheck-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(eval-after-load 'wgrep
  '(define-key grep-mode-map
    (kbd "C-c C-c") 'wgrep-finish-edit))

(setq wgrep-auto-save-buffer t)

(wrap-region-global-mode t)
(wrap-region-add-wrapper "/" "/" nil 'ruby-mode)
(wrap-region-add-wrapper "`" "`" nil '(markdown-mode ruby-mode))

(defun hrs/split-horizontally-for-temp-buffers ()
  (when (one-window-p t)
    (split-window-horizontally)))

(add-hook 'temp-buffer-window-setup-hook
          'hrs/split-horizontally-for-temp-buffers)

(projectile-global-mode)

(require 'engine-mode)

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine wiktionary
  "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

(engine-mode t)

(defvar backup-dir (expand-file-name "~/.emacs.d/emacs_backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory autosave-dir)

(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

(global-set-key (kbd "<f7>") 'gud-cont)
(global-set-key (kbd "<f6>") 'gud-step)
(global-set-key (kbd "<f5>") 'gud-next)
(global-set-key (kbd "<f8>") 'gud-finish)

(require 'whitespace)
(setq-default show-trailing-whitespace t)

(defun no-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(add-hook 'minibuffer-setup-hook
          'no-trailing-whitespace)
(add-hook 'eww-mode-hook
          'no-trailing-whitespace)
(add-hook 'ielm-mode-hook
          'no-trailing-whitespace)
(add-hook 'gdb-mode-hook
          'no-trailing-whitespace)
(add-hook 'help-mode-hook
          'no-trailing-whitespace)

(require 'linum)
(set-face-attribute 'linum nil
                    :background (face-attribute 'default :background)
                    :foreground (face-attribute 'font-lock-comment-face :foreground))
(defface linum-current-line-face
  `((t :background "gray30" :foreground "gold"))
  "Face for the currently active Line number")
(defvar my-linum-current-line-number 0)
(defun get-linum-format-string ()
  (setq-local my-linum-format-string
              (let ((w (length (number-to-string
                                (count-lines (point-min) (point-max))))))
                (concat " %" (number-to-string w) "d "))))
(add-hook 'linum-before-numbering-hook 'get-linum-format-string)
(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'linum-current-line-face
                'linum)))
(setq linum-format 'my-linum-format)
(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))
(ad-activate 'linum-update)

(require 'relative-line-numbers)
(set-face-attribute 'relative-line-numbers-current-line nil
                    :background "gray30" :foreground "gold")
(setq relative-line-numbers-motion-function 'forward-visible-line)
(setq relative-line-numbers-format
      '(lambda (offset)
         (concat " " (number-to-string (abs offset)) " ")))

(defun num ()
  (interactive)
  (if (bound-and-true-p relative-line-numbers-mode)
      (relative-line-numbers-mode 'toggle))
  (linum-mode 'toggle))
(defun rnum ()
  (interactive)
  (if (bound-and-true-p linum-mode)
      (linum-mode 'toggle))
  (relative-line-numbers-mode 'toggle))

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-o") 'other-window)

(define-key input-decode-map "\e[1;2A" [S-up])

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(setq TeX-parse-self t)

(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode)
            (setq TeX-master t)))

(require 'ox-md)
(require 'ox-beamer)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)
   (dot . t)
   (gnuplot . t)))

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-export-with-smart-quotes t)

(setq org-directory "~/org")
(require 'org-protocol)
(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file "~/Dropbox/inbox.org")
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(defun hrs/copy-tasks-from-inbox ()
  (when (file-exists-p org-inbox-file)
    (save-excursion
      (find-file org-index-file)
      (goto-char (point-max))
      (insert-file-contents org-inbox-file)
      (delete-file org-inbox-file))))

(setq org-agenda-files (list org-index-file))

(defun hrs/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)

(setq org-log-done 'time)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (hrs/copy-tasks-from-inbox)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'open-index-file)

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)
(add-hook 'gfm-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))
(add-hook 'haskell-mode-hook
          (lambda () (local-set-key (kbd "M-n") 'org-capture-todo)))

(setq org-capture-templates
        '(("w" "Default template"
           entry
           (file+headline "~/org/blog-ideas.org" "Reading List")
           "* %:description\n%u\n\n%c\n\n%i"
           :empty-lines 1)

          ("g" "Groceries"
           checkitem
           (file (org-file-path "groceries.org")))

          ("l" "Today I Learned..."
           entry
           (file+datetree (org-file-path "til.org"))
           "* %?\n")

          ("r" "Reading"
           entry
           (file (org-file-path "to-read.org")))

          ("t" "Todo"
           entry
           (file+headline org-index-file "Inbox")
           "* TODO %?\n")

           ("o" "org-todo"
            entry
            (file+headline org-index-file "todo.org" "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))
(setq mu4e-update-interval 600)
(setq mu4e-headers-fields
      '( (:human-date    .  25)    ;; alternatively, use :human-date
         (:flags         .   6)
         (:from          .  22)
         (:subject       .  nil)))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(setq user-full-name "Thunayan Al Fouzan"
         user-mail-address "talfouzan@me.com")

(require 'osx-location)
  (eval-after-load 'osx-location
   '(when (eq system-type 'darwin)
      (add-hook 'osx-location-changed-hook
                (lambda ()
                  (setq calendar-latitude osx-location-latitude
                        calendar-longitude osx-location-longitude
                        calendar-location-name (format "%s, %s" osx-location-latitude osx-location-longitude))))))
