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

(setq org-directory "~/git/org")
(require 'org-protocol)
(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file "~/Dropbox/inbox.org")
(setq org-index-file (org-file-path "refile.org"))
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

;; The following setting is different from the document so that you
;; can override the document org-agenda-files by setting your
;;;; org-habit.el --- The habit tracking code for Org-mode

;; Copyright (C) 2009-2012 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw at gnu dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the habit tracking code for Org-mode

;;; Code:

(require 'org)
(require 'org-agenda)

(eval-when-compile
  (require 'cl))

(defgroup org-habit nil
  "Options concerning habit tracking in Org-mode."
  :tag "Org Habit"
  :group 'org-progress)

(defcustom org-habit-graph-column 40
  "The absolute column at which to insert habit consistency graphs.
Note that consistency graphs will overwrite anything else in the buffer."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-preceding-days 21
  "Number of days before today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-following-days 7
  "Number of days after today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-show-habits t
  "If non-nil, show habits in agenda buffers."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-habits-only-for-today t
  "If non-nil, only show habits on today's agenda, and not for future days.
Note that even when shown for future days, the graph is always
relative to the current effective date."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-all-today nil
  "If non-nil, will show the consistency graph of all habits on
today's agenda, even if they are not scheduled."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-today-glyph ?!
  "Glyph character used to identify today."
  :group 'org-habit
  :version "24.1"
  :type 'character)

(defcustom org-habit-completed-glyph ?*
  "Glyph character used to show completed days on which a task was done."
  :group 'org-habit
  :version "24.1"
  :type 'character)

(defface org-habit-clear-face
  '((((background light)) (:background "#8270f9"))
    (((background dark)) (:background "blue")))
  "Face for days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-clear-future-face
  '((((background light)) (:background "#d6e4fc"))
    (((background dark)) (:background "midnightblue")))
  "Face for future days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-ready-face
  '((((background light)) (:background "#4df946"))
    (((background dark)) (:background "forestgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-ready-future-face
  '((((background light)) (:background "#acfca9"))
    (((background dark)) (:background "darkgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-alert-face
  '((((background light)) (:background "#f5f946"))
    (((background dark)) (:background "gold")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-alert-future-face
  '((((background light)) (:background "#fafca9"))
    (((background dark)) (:background "darkgoldenrod")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-overdue-face
  '((((background light)) (:background "#f9372d"))
    (((background dark)) (:background "firebrick")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-overdue-future-face
  '((((background light)) (:background "#fc9590"))
    (((background dark)) (:background "darkred")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)

(defun org-habit-duration-to-days (ts)
  (if (string-match "\\([0-9]+\\)\\([dwmy]\\)" ts)
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25))))))
    (error "Invalid duration string: %s" ts)))

(defun org-is-habit-p (&optional pom)
  "Is the task at POM or point a habit?"
  (string= "habit" (org-entry-get (or pom (point)) "STYLE")))

(defun org-habit-parse-todo (&optional pom)
  "Parse the TODO surrounding point for its habit-related data.
Returns a list with the following elements:

  0: Scheduled date for the habit (may be in the past)
  1: \".+\"-style repeater for the schedule, in days
  2: Optional deadline (nil if not present)
  3: If deadline, the repeater for the deadline, otherwise nil
  4: A list of all the past dates this todo was mark closed

This list represents a \"habit\" for the rest of this module."
  (save-excursion
    (if pom (goto-char pom))
    (assert (org-is-habit-p (point)))
    (let* ((scheduled (org-get-scheduled-time (point)))
	   (scheduled-repeat (org-get-repeat org-scheduled-string))
	   (end (org-entry-end-position))
	   (habit-entry (org-no-properties (nth 4 (org-heading-components))))
	   closed-dates deadline dr-days sr-days)
      (if scheduled
	  (setq scheduled (time-to-days scheduled))
	(error "Habit %s has no scheduled date" habit-entry))
      (unless scheduled-repeat
	(error
	 "Habit '%s' has no scheduled repeat period or has an incorrect one"
	 habit-entry))
      (setq sr-days (org-habit-duration-to-days scheduled-repeat))
      (unless (> sr-days 0)
	(error "Habit %s scheduled repeat period is less than 1d" habit-entry))
      (when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
	(setq dr-days (org-habit-duration-to-days
		       (match-string-no-properties 1 scheduled-repeat)))
	(if (<= dr-days sr-days)
	    (error "Habit %s deadline repeat period is less than or equal to scheduled (%s)"
		   habit-entry scheduled-repeat))
	(setq deadline (+ scheduled (- dr-days sr-days))))
      (org-back-to-heading t)
      (let* ((maxdays (+ org-habit-preceding-days org-habit-following-days))
	     (reversed org-log-states-order-reversed)
	     (search (if reversed 're-search-forward 're-search-backward))
	     (limit (if reversed end (point)))
	     (count 0))
	(unless reversed (goto-char end))
	(while (and (< count maxdays)
		    (funcall search "- State \"DONE\".*\\[\\([^]]+\\)\\]" limit t))
	  (push (time-to-days
		 (org-time-string-to-time (match-string-no-properties 1)))
		closed-dates)
	  (setq count (1+ count))))
      (list scheduled sr-days deadline dr-days closed-dates))))

(defsubst org-habit-scheduled (habit)
  (nth 0 habit))
(defsubst org-habit-scheduled-repeat (habit)
  (nth 1 habit))
(defsubst org-habit-deadline (habit)
  (let ((deadline (nth 2 habit)))
    (or deadline
	(if (nth 3 habit)
	    (+ (org-habit-scheduled habit)
	       (1- (org-habit-scheduled-repeat habit)))
	  (org-habit-scheduled habit)))))
(defsubst org-habit-deadline-repeat (habit)
  (or (nth 3 habit)
      (org-habit-scheduled-repeat habit)))
(defsubst org-habit-done-dates (habit)
  (nth 4 habit))

(defsubst org-habit-get-priority (habit &optional moment)
  "Determine the relative priority of a habit.
This must take into account not just urgency, but consistency as well."
  (let ((pri 1000)
	(now (if moment (time-to-days moment) (org-today)))
	(scheduled (org-habit-scheduled habit))
	(deadline (org-habit-deadline habit)))
    ;; add 10 for every day past the scheduled date, and subtract for every
    ;; day before it
    (setq pri (+ pri (* (- now scheduled) 10)))
    ;; add 50 if the deadline is today
    (if (and (/= scheduled deadline)
	     (= now deadline))
	(setq pri (+ pri 50)))
    ;; add 100 for every day beyond the deadline date, and subtract 10 for
    ;; every day before it
    (let ((slip (- now (1- deadline))))
      (if (> slip 0)
	  (setq pri (+ pri (* slip 100)))
	(setq pri (+ pri (* slip 10)))))
    pri))

(defun org-habit-get-faces (habit &optional now-days scheduled-days donep)
  "Return faces for HABIT relative to NOW-DAYS and SCHEDULED-DAYS.
NOW-DAYS defaults to the current time's days-past-the-epoch if nil.
SCHEDULED-DAYS defaults to the habit's actual scheduled days if nil.

Habits are assigned colors on the following basis:
  Blue      Task is before the scheduled date.
  Green     Task is on or after scheduled date, but before the
	    end of the schedule's repeat period.
  Yellow    If the task has a deadline, then it is after schedule's
	    repeat period, but before the deadline.
  Orange    The task has reached the deadline day, or if there is
	    no deadline, the end of the schedule's repeat period.
  Red       The task has gone beyond the deadline day or the
	    schedule's repeat period."
  (let* ((scheduled (or scheduled-days (org-habit-scheduled habit)))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (scheduled-end (+ scheduled (1- s-repeat)))
	 (d-repeat (org-habit-deadline-repeat habit))
	 (deadline (if scheduled-days
		       (+ scheduled-days (- d-repeat s-repeat))
		     (org-habit-deadline habit)))
	 (m-days (or now-days (time-to-days (current-time)))))
    (cond
     ((< m-days scheduled)
      '(org-habit-clear-face . org-habit-clear-future-face))
     ((< m-days deadline)
      '(org-habit-ready-face . org-habit-ready-future-face))
     ((= m-days deadline)
      (if donep
	  '(org-habit-ready-face . org-habit-ready-future-face)
	'(org-habit-alert-face . org-habit-alert-future-face)))
     (t
      '(org-habit-overdue-face . org-habit-overdue-future-face)))))

(defun org-habit-build-graph (habit starting current ending)
  "Build a graph for the given HABIT, from STARTING to ENDING.
CURRENT gives the current time between STARTING and ENDING, for
the purpose of drawing the graph.  It need not be the actual
current time."
  (let* ((done-dates (sort (org-habit-done-dates habit) '<))
	 (scheduled (org-habit-scheduled habit))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (start (time-to-days starting))
	 (now (time-to-days current))
	 (end (time-to-days ending))
	 (graph (make-string (1+ (- end start)) ?\ ))
	 (index 0)
	 last-done-date)
    (while (and done-dates (< (car done-dates) start))
      (setq last-done-date (car done-dates)
	    done-dates (cdr done-dates)))
    (while (< start end)
      (let* ((in-the-past-p (< start now))
	     (todayp (= start now))
	     (donep (and done-dates
			 (= start (car done-dates))))
	     (faces (if (and in-the-past-p
			     (not last-done-date)
			     (not (< scheduled now)))
			'(org-habit-clear-face . org-habit-clear-future-face)
		      (org-habit-get-faces
		       habit start (and in-the-past-p
					(if last-done-date
					    (+ last-done-date s-repeat)
					  scheduled))
		       donep)))
	     markedp face)
	(if donep
	    (let ((done-time (time-add
			      starting
			      (days-to-time
			       (- start (time-to-days starting))))))

	      (aset graph index org-habit-completed-glyph)
	      (setq markedp t)
	      (put-text-property
	       index (1+ index) 'help-echo
	       (format-time-string (org-time-stamp-format) done-time) graph)
	      (while (and done-dates
			  (= start (car done-dates)))
		(setq last-done-date (car done-dates)
		      done-dates (cdr done-dates))))
	  (if todayp
	      (aset graph index org-habit-today-glyph)))
	(setq face (if (or in-the-past-p todayp)
		       (car faces)
		     (cdr faces)))
	(if (and in-the-past-p
		 (not (eq face 'org-habit-overdue-face))
		 (not markedp))
	    (setq face (cdr faces)))
	(put-text-property index (1+ index) 'face face graph))
      (setq start (1+ start)
	    index (1+ index)))
    graph))

(defun org-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t) l c
	(buffer-invisibility-spec '(org-link))
	(moment (time-subtract (current-time)
			       (list 0 (* 3600 org-extend-today-until) 0)))
	disabled-overlays)
    ;; Disable filters; this helps with alignment if there are links.
    (mapc (lambda (ol)
	    (when (overlay-get ol 'invisible)
	      (overlay-put ol 'invisible nil)
	      (setq disabled-overlays (cons ol disabled-overlays))))
	  (overlays-in (point-min) (point-max)))
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (not (eobp))
	(let ((habit (get-text-property (point) 'org-habit-p)))
	  (when habit
	    (move-to-column org-habit-graph-column t)
	    (delete-char (min (+ 1 org-habit-preceding-days
				 org-habit-following-days)
			      (- (line-end-position) (point))))
	    (insert-before-markers
	     (org-habit-build-graph
	      habit
	      (time-subtract moment (days-to-time org-habit-preceding-days))
	      moment
	      (time-add moment (days-to-time org-habit-following-days))))))
	(forward-line)))
    (mapc (lambda (ol) (overlay-put ol 'invisible t))
	  disabled-overlays)))

(defun org-habit-toggle-habits ()
  "Toggle display of habits in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-habit-show-habits (not org-habit-show-habits))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Habits turned %s"
	   (if org-habit-show-habits "on" "off")))

(org-defkey org-agenda-mode-map "K" 'org-habit-toggle-habits)

(provide 'org-habit)

;;; org-habit.el ends here;;; org-habit.el --- The habit tracking code for Org-mode

;; Copyright (C) 2009-2012 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw at gnu dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the habit tracking code for Org-mode

;;; Code:

(require 'org)
(require 'org-agenda)

(eval-when-compile
  (require 'cl))

(defgroup org-habit nil
  "Options concerning habit tracking in Org-mode."
  :tag "Org Habit"
  :group 'org-progress)

(defcustom org-habit-graph-column 40
  "The absolute column at which to insert habit consistency graphs.
Note that consistency graphs will overwrite anything else in the buffer."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-preceding-days 21
  "Number of days before today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-following-days 7
  "Number of days after today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-show-habits t
  "If non-nil, show habits in agenda buffers."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-habits-only-for-today t
  "If non-nil, only show habits on today's agenda, and not for future days.
Note that even when shown for future days, the graph is always
relative to the current effective date."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-all-today nil
  "If non-nil, will show the consistency graph of all habits on
today's agenda, even if they are not scheduled."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-today-glyph ?!
  "Glyph character used to identify today."
  :group 'org-habit
  :version "24.1"
  :type 'character)

(defcustom org-habit-completed-glyph ?*
  "Glyph character used to show completed days on which a task was done."
  :group 'org-habit
  :version "24.1"
  :type 'character)

(defface org-habit-clear-face
  '((((background light)) (:background "#8270f9"))
    (((background dark)) (:background "blue")))
  "Face for days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-clear-future-face
  '((((background light)) (:background "#d6e4fc"))
    (((background dark)) (:background "midnightblue")))
  "Face for future days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-ready-face
  '((((background light)) (:background "#4df946"))
    (((background dark)) (:background "forestgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-ready-future-face
  '((((background light)) (:background "#acfca9"))
    (((background dark)) (:background "darkgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-alert-face
  '((((background light)) (:background "#f5f946"))
    (((background dark)) (:background "gold")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-alert-future-face
  '((((background light)) (:background "#fafca9"))
    (((background dark)) (:background "darkgoldenrod")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-overdue-face
  '((((background light)) (:background "#f9372d"))
    (((background dark)) (:background "firebrick")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-overdue-future-face
  '((((background light)) (:background "#fc9590"))
    (((background dark)) (:background "darkred")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)

(defun org-habit-duration-to-days (ts)
  (if (string-match "\\([0-9]+\\)\\([dwmy]\\)" ts)
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25))))))
    (error "Invalid duration string: %s" ts)))

(defun org-is-habit-p (&optional pom)
  "Is the task at POM or point a habit?"
  (string= "habit" (org-entry-get (or pom (point)) "STYLE")))

(defun org-habit-parse-todo (&optional pom)
  "Parse the TODO surrounding point for its habit-related data.
Returns a list with the following elements:

  0: Scheduled date for the habit (may be in the past)
  1: \".+\"-style repeater for the schedule, in days
  2: Optional deadline (nil if not present)
  3: If deadline, the repeater for the deadline, otherwise nil
  4: A list of all the past dates this todo was mark closed

This list represents a \"habit\" for the rest of this module."
  (save-excursion
    (if pom (goto-char pom))
    (assert (org-is-habit-p (point)))
    (let* ((scheduled (org-get-scheduled-time (point)))
	   (scheduled-repeat (org-get-repeat org-scheduled-string))
	   (end (org-entry-end-position))
	   (habit-entry (org-no-properties (nth 4 (org-heading-components))))
	   closed-dates deadline dr-days sr-days)
      (if scheduled
	  (setq scheduled (time-to-days scheduled))
	(error "Habit %s has no scheduled date" habit-entry))
      (unless scheduled-repeat
	(error
	 "Habit '%s' has no scheduled repeat period or has an incorrect one"
	 habit-entry))
      (setq sr-days (org-habit-duration-to-days scheduled-repeat))
      (unless (> sr-days 0)
	(error "Habit %s scheduled repeat period is less than 1d" habit-entry))
      (when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
	(setq dr-days (org-habit-duration-to-days
		       (match-string-no-properties 1 scheduled-repeat)))
	(if (<= dr-days sr-days)
	    (error "Habit %s deadline repeat period is less than or equal to scheduled (%s)"
		   habit-entry scheduled-repeat))
	(setq deadline (+ scheduled (- dr-days sr-days))))
      (org-back-to-heading t)
      (let* ((maxdays (+ org-habit-preceding-days org-habit-following-days))
	     (reversed org-log-states-order-reversed)
	     (search (if reversed 're-search-forward 're-search-backward))
	     (limit (if reversed end (point)))
	     (count 0))
	(unless reversed (goto-char end))
	(while (and (< count maxdays)
		    (funcall search "- State \"DONE\".*\\[\\([^]]+\\)\\]" limit t))
	  (push (time-to-days
		 (org-time-string-to-time (match-string-no-properties 1)))
		closed-dates)
	  (setq count (1+ count))))
      (list scheduled sr-days deadline dr-days closed-dates))))

(defsubst org-habit-scheduled (habit)
  (nth 0 habit))
(defsubst org-habit-scheduled-repeat (habit)
  (nth 1 habit))
(defsubst org-habit-deadline (habit)
  (let ((deadline (nth 2 habit)))
    (or deadline
	(if (nth 3 habit)
	    (+ (org-habit-scheduled habit)
	       (1- (org-habit-scheduled-repeat habit)))
	  (org-habit-scheduled habit)))))
(defsubst org-habit-deadline-repeat (habit)
  (or (nth 3 habit)
      (org-habit-scheduled-repeat habit)))
(defsubst org-habit-done-dates (habit)
  (nth 4 habit))

(defsubst org-habit-get-priority (habit &optional moment)
  "Determine the relative priority of a habit.
This must take into account not just urgency, but consistency as well."
  (let ((pri 1000)
	(now (if moment (time-to-days moment) (org-today)))
	(scheduled (org-habit-scheduled habit))
	(deadline (org-habit-deadline habit)))
    ;; add 10 for every day past the scheduled date, and subtract for every
    ;; day before it
    (setq pri (+ pri (* (- now scheduled) 10)))
    ;; add 50 if the deadline is today
    (if (and (/= scheduled deadline)
	     (= now deadline))
	(setq pri (+ pri 50)))
    ;; add 100 for every day beyond the deadline date, and subtract 10 for
    ;; every day before it
    (let ((slip (- now (1- deadline))))
      (if (> slip 0)
	  (setq pri (+ pri (* slip 100)))
	(setq pri (+ pri (* slip 10)))))
    pri))

(defun org-habit-get-faces (habit &optional now-days scheduled-days donep)
  "Return faces for HABIT relative to NOW-DAYS and SCHEDULED-DAYS.
NOW-DAYS defaults to the current time's days-past-the-epoch if nil.
SCHEDULED-DAYS defaults to the habit's actual scheduled days if nil.

Habits are assigned colors on the following basis:
  Blue      Task is before the scheduled date.
  Green     Task is on or after scheduled date, but before the
	    end of the schedule's repeat period.
  Yellow    If the task has a deadline, then it is after schedule's
	    repeat period, but before the deadline.
  Orange    The task has reached the deadline day, or if there is
	    no deadline, the end of the schedule's repeat period.
  Red       The task has gone beyond the deadline day or the
	    schedule's repeat period."
  (let* ((scheduled (or scheduled-days (org-habit-scheduled habit)))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (scheduled-end (+ scheduled (1- s-repeat)))
	 (d-repeat (org-habit-deadline-repeat habit))
	 (deadline (if scheduled-days
		       (+ scheduled-days (- d-repeat s-repeat))
		     (org-habit-deadline habit)))
	 (m-days (or now-days (time-to-days (current-time)))))
    (cond
     ((< m-days scheduled)
      '(org-habit-clear-face . org-habit-clear-future-face))
     ((< m-days deadline)
      '(org-habit-ready-face . org-habit-ready-future-face))
     ((= m-days deadline)
      (if donep
	  '(org-habit-ready-face . org-habit-ready-future-face)
	'(org-habit-alert-face . org-habit-alert-future-face)))
     (t
      '(org-habit-overdue-face . org-habit-overdue-future-face)))))

(defun org-habit-build-graph (habit starting current ending)
  "Build a graph for the given HABIT, from STARTING to ENDING.
CURRENT gives the current time between STARTING and ENDING, for
the purpose of drawing the graph.  It need not be the actual
current time."
  (let* ((done-dates (sort (org-habit-done-dates habit) '<))
	 (scheduled (org-habit-scheduled habit))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (start (time-to-days starting))
	 (now (time-to-days current))
	 (end (time-to-days ending))
	 (graph (make-string (1+ (- end start)) ?\ ))
	 (index 0)
	 last-done-date)
    (while (and done-dates (< (car done-dates) start))
      (setq last-done-date (car done-dates)
	    done-dates (cdr done-dates)))
    (while (< start end)
      (let* ((in-the-past-p (< start now))
	     (todayp (= start now))
	     (donep (and done-dates
			 (= start (car done-dates))))
	     (faces (if (and in-the-past-p
			     (not last-done-date)
			     (not (< scheduled now)))
			'(org-habit-clear-face . org-habit-clear-future-face)
		      (org-habit-get-faces
		       habit start (and in-the-past-p
					(if last-done-date
					    (+ last-done-date s-repeat)
					  scheduled))
		       donep)))
	     markedp face)
	(if donep
	    (let ((done-time (time-add
			      starting
			      (days-to-time
			       (- start (time-to-days starting))))))

	      (aset graph index org-habit-completed-glyph)
	      (setq markedp t)
	      (put-text-property
	       index (1+ index) 'help-echo
	       (format-time-string (org-time-stamp-format) done-time) graph)
	      (while (and done-dates
			  (= start (car done-dates)))
		(setq last-done-date (car done-dates)
		      done-dates (cdr done-dates))))
	  (if todayp
	      (aset graph index org-habit-today-glyph)))
	(setq face (if (or in-the-past-p todayp)
		       (car faces)
		     (cdr faces)))
	(if (and in-the-past-p
		 (not (eq face 'org-habit-overdue-face))
		 (not markedp))
	    (setq face (cdr faces)))
	(put-text-property index (1+ index) 'face face graph))
      (setq start (1+ start)
	    index (1+ index)))
    graph))

(defun org-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t) l c
	(buffer-invisibility-spec '(org-link))
	(moment (time-subtract (current-time)
			       (list 0 (* 3600 org-extend-today-until) 0)))
	disabled-overlays)
    ;; Disable filters; this helps with alignment if there are links.
    (mapc (lambda (ol)
	    (when (overlay-get ol 'invisible)
	      (overlay-put ol 'invisible nil)
	      (setq disabled-overlays (cons ol disabled-overlays))))
	  (overlays-in (point-min) (point-max)))
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (not (eobp))
	(let ((habit (get-text-property (point) 'org-habit-p)))
	  (when habit
	    (move-to-column org-habit-graph-column t)
	    (delete-char (min (+ 1 org-habit-preceding-days
				 org-habit-following-days)
			      (- (line-end-position) (point))))
	    (insert-before-markers
	     (org-habit-build-graph
	      habit
	      (time-subtract moment (days-to-time org-habit-preceding-days))
	      moment
	      (time-add moment (days-to-time org-habit-following-days))))))
	(forward-line)))
    (mapc (lambda (ol) (overlay-put ol 'invisible t))
	  disabled-overlays)))

(defun org-habit-toggle-habits ()
  "Toggle display of habits in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-habit-show-habits (not org-habit-show-habits))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Habits turned %s"
	   (if org-habit-show-habits "on" "off")))

(org-defkey org-agenda-mode-map "K" 'org-habit-toggle-habits)

(provide 'org-habit)

;;; org-habit.el ends here;;; org-habit.el --- The habit tracking code for Org-mode

;; Copyright (C) 2009-2012 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw at gnu dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the habit tracking code for Org-mode

;;; Code:

(require 'org)
(require 'org-agenda)

(eval-when-compile
  (require 'cl))

(defgroup org-habit nil
  "Options concerning habit tracking in Org-mode."
  :tag "Org Habit"
  :group 'org-progress)

(defcustom org-habit-graph-column 40
  "The absolute column at which to insert habit consistency graphs.
Note that consistency graphs will overwrite anything else in the buffer."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-preceding-days 21
  "Number of days before today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-following-days 7
  "Number of days after today to appear in consistency graphs."
  :group 'org-habit
  :type 'integer)

(defcustom org-habit-show-habits t
  "If non-nil, show habits in agenda buffers."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-habits-only-for-today t
  "If non-nil, only show habits on today's agenda, and not for future days.
Note that even when shown for future days, the graph is always
relative to the current effective date."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-show-all-today nil
  "If non-nil, will show the consistency graph of all habits on
today's agenda, even if they are not scheduled."
  :group 'org-habit
  :type 'boolean)

(defcustom org-habit-today-glyph ?!
  "Glyph character used to identify today."
  :group 'org-habit
  :version "24.1"
  :type 'character)

(defcustom org-habit-completed-glyph ?*
  "Glyph character used to show completed days on which a task was done."
  :group 'org-habit
  :version "24.1"
  :type 'character)

(defface org-habit-clear-face
  '((((background light)) (:background "#8270f9"))
    (((background dark)) (:background "blue")))
  "Face for days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-clear-future-face
  '((((background light)) (:background "#d6e4fc"))
    (((background dark)) (:background "midnightblue")))
  "Face for future days on which a task shouldn't be done yet."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-ready-face
  '((((background light)) (:background "#4df946"))
    (((background dark)) (:background "forestgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-ready-future-face
  '((((background light)) (:background "#acfca9"))
    (((background dark)) (:background "darkgreen")))
  "Face for days on which a task should start to be done."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-alert-face
  '((((background light)) (:background "#f5f946"))
    (((background dark)) (:background "gold")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-alert-future-face
  '((((background light)) (:background "#fafca9"))
    (((background dark)) (:background "darkgoldenrod")))
  "Face for days on which a task is due."
  :group 'org-habit
  :group 'org-faces)

(defface org-habit-overdue-face
  '((((background light)) (:background "#f9372d"))
    (((background dark)) (:background "firebrick")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)
(defface org-habit-overdue-future-face
  '((((background light)) (:background "#fc9590"))
    (((background dark)) (:background "darkred")))
  "Face for days on which a task is overdue."
  :group 'org-habit
  :group 'org-faces)

(defun org-habit-duration-to-days (ts)
  (if (string-match "\\([0-9]+\\)\\([dwmy]\\)" ts)
      ;; lead time is specified.
      (floor (* (string-to-number (match-string 1 ts))
		(cdr (assoc (match-string 2 ts)
			    '(("d" . 1)    ("w" . 7)
			      ("m" . 30.4) ("y" . 365.25))))))
    (error "Invalid duration string: %s" ts)))

(defun org-is-habit-p (&optional pom)
  "Is the task at POM or point a habit?"
  (string= "habit" (org-entry-get (or pom (point)) "STYLE")))

(defun org-habit-parse-todo (&optional pom)
  "Parse the TODO surrounding point for its habit-related data.
Returns a list with the following elements:

  0: Scheduled date for the habit (may be in the past)
  1: \".+\"-style repeater for the schedule, in days
  2: Optional deadline (nil if not present)
  3: If deadline, the repeater for the deadline, otherwise nil
  4: A list of all the past dates this todo was mark closed

This list represents a \"habit\" for the rest of this module."
  (save-excursion
    (if pom (goto-char pom))
    (assert (org-is-habit-p (point)))
    (let* ((scheduled (org-get-scheduled-time (point)))
	   (scheduled-repeat (org-get-repeat org-scheduled-string))
	   (end (org-entry-end-position))
	   (habit-entry (org-no-properties (nth 4 (org-heading-components))))
	   closed-dates deadline dr-days sr-days)
      (if scheduled
	  (setq scheduled (time-to-days scheduled))
	(error "Habit %s has no scheduled date" habit-entry))
      (unless scheduled-repeat
	(error
	 "Habit '%s' has no scheduled repeat period or has an incorrect one"
	 habit-entry))
      (setq sr-days (org-habit-duration-to-days scheduled-repeat))
      (unless (> sr-days 0)
	(error "Habit %s scheduled repeat period is less than 1d" habit-entry))
      (when (string-match "/\\([0-9]+[dwmy]\\)" scheduled-repeat)
	(setq dr-days (org-habit-duration-to-days
		       (match-string-no-properties 1 scheduled-repeat)))
	(if (<= dr-days sr-days)
	    (error "Habit %s deadline repeat period is less than or equal to scheduled (%s)"
		   habit-entry scheduled-repeat))
	(setq deadline (+ scheduled (- dr-days sr-days))))
      (org-back-to-heading t)
      (let* ((maxdays (+ org-habit-preceding-days org-habit-following-days))
	     (reversed org-log-states-order-reversed)
	     (search (if reversed 're-search-forward 're-search-backward))
	     (limit (if reversed end (point)))
	     (count 0))
	(unless reversed (goto-char end))
	(while (and (< count maxdays)
		    (funcall search "- State \"DONE\".*\\[\\([^]]+\\)\\]" limit t))
	  (push (time-to-days
		 (org-time-string-to-time (match-string-no-properties 1)))
		closed-dates)
	  (setq count (1+ count))))
      (list scheduled sr-days deadline dr-days closed-dates))))

(defsubst org-habit-scheduled (habit)
  (nth 0 habit))
(defsubst org-habit-scheduled-repeat (habit)
  (nth 1 habit))
(defsubst org-habit-deadline (habit)
  (let ((deadline (nth 2 habit)))
    (or deadline
	(if (nth 3 habit)
	    (+ (org-habit-scheduled habit)
	       (1- (org-habit-scheduled-repeat habit)))
	  (org-habit-scheduled habit)))))
(defsubst org-habit-deadline-repeat (habit)
  (or (nth 3 habit)
      (org-habit-scheduled-repeat habit)))
(defsubst org-habit-done-dates (habit)
  (nth 4 habit))

(defsubst org-habit-get-priority (habit &optional moment)
  "Determine the relative priority of a habit.
This must take into account not just urgency, but consistency as well."
  (let ((pri 1000)
	(now (if moment (time-to-days moment) (org-today)))
	(scheduled (org-habit-scheduled habit))
	(deadline (org-habit-deadline habit)))
    ;; add 10 for every day past the scheduled date, and subtract for every
    ;; day before it
    (setq pri (+ pri (* (- now scheduled) 10)))
    ;; add 50 if the deadline is today
    (if (and (/= scheduled deadline)
	     (= now deadline))
	(setq pri (+ pri 50)))
    ;; add 100 for every day beyond the deadline date, and subtract 10 for
    ;; every day before it
    (let ((slip (- now (1- deadline))))
      (if (> slip 0)
	  (setq pri (+ pri (* slip 100)))
	(setq pri (+ pri (* slip 10)))))
    pri))

(defun org-habit-get-faces (habit &optional now-days scheduled-days donep)
  "Return faces for HABIT relative to NOW-DAYS and SCHEDULED-DAYS.
NOW-DAYS defaults to the current time's days-past-the-epoch if nil.
SCHEDULED-DAYS defaults to the habit's actual scheduled days if nil.

Habits are assigned colors on the following basis:
  Blue      Task is before the scheduled date.
  Green     Task is on or after scheduled date, but before the
	    end of the schedule's repeat period.
  Yellow    If the task has a deadline, then it is after schedule's
	    repeat period, but before the deadline.
  Orange    The task has reached the deadline day, or if there is
	    no deadline, the end of the schedule's repeat period.
  Red       The task has gone beyond the deadline day or the
	    schedule's repeat period."
  (let* ((scheduled (or scheduled-days (org-habit-scheduled habit)))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (scheduled-end (+ scheduled (1- s-repeat)))
	 (d-repeat (org-habit-deadline-repeat habit))
	 (deadline (if scheduled-days
		       (+ scheduled-days (- d-repeat s-repeat))
		     (org-habit-deadline habit)))
	 (m-days (or now-days (time-to-days (current-time)))))
    (cond
     ((< m-days scheduled)
      '(org-habit-clear-face . org-habit-clear-future-face))
     ((< m-days deadline)
      '(org-habit-ready-face . org-habit-ready-future-face))
     ((= m-days deadline)
      (if donep
	  '(org-habit-ready-face . org-habit-ready-future-face)
	'(org-habit-alert-face . org-habit-alert-future-face)))
     (t
      '(org-habit-overdue-face . org-habit-overdue-future-face)))))

(defun org-habit-build-graph (habit starting current ending)
  "Build a graph for the given HABIT, from STARTING to ENDING.
CURRENT gives the current time between STARTING and ENDING, for
the purpose of drawing the graph.  It need not be the actual
current time."
  (let* ((done-dates (sort (org-habit-done-dates habit) '<))
	 (scheduled (org-habit-scheduled habit))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (start (time-to-days starting))
	 (now (time-to-days current))
	 (end (time-to-days ending))
	 (graph (make-string (1+ (- end start)) ?\ ))
	 (index 0)
	 last-done-date)
    (while (and done-dates (< (car done-dates) start))
      (setq last-done-date (car done-dates)
	    done-dates (cdr done-dates)))
    (while (< start end)
      (let* ((in-the-past-p (< start now))
	     (todayp (= start now))
	     (donep (and done-dates
			 (= start (car done-dates))))
	     (faces (if (and in-the-past-p
			     (not last-done-date)
			     (not (< scheduled now)))
			'(org-habit-clear-face . org-habit-clear-future-face)
		      (org-habit-get-faces
		       habit start (and in-the-past-p
					(if last-done-date
					    (+ last-done-date s-repeat)
					  scheduled))
		       donep)))
	     markedp face)
	(if donep
	    (let ((done-time (time-add
			      starting
			      (days-to-time
			       (- start (time-to-days starting))))))

	      (aset graph index org-habit-completed-glyph)
	      (setq markedp t)
	      (put-text-property
	       index (1+ index) 'help-echo
	       (format-time-string (org-time-stamp-format) done-time) graph)
	      (while (and done-dates
			  (= start (car done-dates)))
		(setq last-done-date (car done-dates)
		      done-dates (cdr done-dates))))
	  (if todayp
	      (aset graph index org-habit-today-glyph)))
	(setq face (if (or in-the-past-p todayp)
		       (car faces)
		     (cdr faces)))
	(if (and in-the-past-p
		 (not (eq face 'org-habit-overdue-face))
		 (not markedp))
	    (setq face (cdr faces)))
	(put-text-property index (1+ index) 'face face graph))
      (setq start (1+ start)
	    index (1+ index)))
    graph))

(defun org-habit-insert-consistency-graphs (&optional line)
  "Insert consistency graph for any habitual tasks."
  (let ((inhibit-read-only t) l c
	(buffer-invisibility-spec '(org-link))
	(moment (time-subtract (current-time)
			       (list 0 (* 3600 org-extend-today-until) 0)))
	disabled-overlays)
    ;; Disable filters; this helps with alignment if there are links.
    (mapc (lambda (ol)
	    (when (overlay-get ol 'invisible)
	      (overlay-put ol 'invisible nil)
	      (setq disabled-overlays (cons ol disabled-overlays))))
	  (overlays-in (point-min) (point-max)))
    (save-excursion
      (goto-char (if line (point-at-bol) (point-min)))
      (while (not (eobp))
	(let ((habit (get-text-property (point) 'org-habit-p)))
	  (when habit
	    (move-to-column org-habit-graph-column t)
	    (delete-char (min (+ 1 org-habit-preceding-days
				 org-habit-following-days)
			      (- (line-end-position) (point))))
	    (insert-before-markers
	     (org-habit-build-graph
	      habit
	      (time-subtract moment (days-to-time org-habit-preceding-days))
	      moment
	      (time-add moment (days-to-time org-habit-following-days))))))
	(forward-line)))
    (mapc (lambda (ol) (overlay-put ol 'invisible t))
	  disabled-overlays)))

(defun org-habit-toggle-habits ()
  "Toggle display of habits in an agenda buffer."
  (interactive)
  (org-agenda-check-type t 'agenda)
  (setq org-habit-show-habits (not org-habit-show-habits))
  (org-agenda-redo)
  (org-agenda-set-mode-name)
  (message "Habits turned %s"
	   (if org-habit-show-habits "on" "off")))

(org-defkey org-agenda-mode-map "K" 'org-habit-toggle-habits)

(provide 'org-habit)

;;; org-habit.el ends here; org-agenda-files in the variable org-user-agenda-files
;;
(if (boundp 'org-user-agenda-files)
    (setq org-agenda-files org-user-agenda-files)
  (setq org-agenda-files (quote ("~/git/org"
                               ))))

;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'bh/org-todo)
(global-set-key (kbd "<S-f5>") 'bh/widen)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)

(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-c c") 'org-capture)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)

(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(setq org-directory "~/git/org")
(setq org-default-notes-file "~/git/org/refile.org")

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/git/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/git/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/git/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/git/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/git/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/git/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/git/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
                            :min-duration 0
                            :max-gap 0
                            :gap-ok-around ("4:00"))))

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

; global Effort estimate values
                                        ; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

;; Agenda log mode items to display (closed and state changes by default)
(setq org-agenda-log-mode-items (quote (closed state)))

; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@funco" . ?f)
                            (:endgroup)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("FUNCO" . ?F)
                            ("ORG" . ?O)
                            ("NORANG" . ?N)
                            ("crypt" . ?E)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(require 'bbdb )
(require 'bbdb-com )

(global-set-key (kbd "<f9> p") 'bh/phone-call)

;;
;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
(defun bh/phone-call ()
  "Return name and company info for caller from bbdb lookup"
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
                               (bbdb-hashtable)
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
      ; Something was supplied - look it up in bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)))
                name)))

    ; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (company (bbdb-record-company rec)))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when company
                                    (concat " - " company)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))

(setq org-agenda-span 'day)

(setq org-stuck-projects (quote ("" nil nil "")))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(setq org-alphabetical-lists t)

;; Explicitly load required exporters
(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)

(setq org-ditaa-jar-path "~/git/org-mode/contrib/scripts/ditaa.jar")
(setq org-plantuml-jar-path "~/java/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

;; Don't enable this because it breaks access to emacs from my Android phone
(setq org-startup-with-inline-images nil)

; experimenting with docbook exports - not finished
(setq org-export-docbook-xsl-fo-proc-command "fop %s %s")
(setq org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")
                                        ;
                                        ; Inline images in HTML instead of producting links to the image
(setq org-html-inline-images t)
                                        ; Do not use sub or superscripts - I currently don't need this functionality in my documents
(setq org-export-with-sub-superscripts nil)
                                        ; Use org.css from the norang website for export document stylesheets
(setq org-html-head-extra "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />")
(setq org-html-head-include-default-style nil)
                                        ; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))
                                        ; Export with LaTeX fragments
(setq org-export-with-LaTeX-fragments t)
                                        ; Increase default number of headings to export
(setq org-export-headline-levels 6)

                                        ; List of projects
                                        ; norang       - http://www.norang.ca/
                                        ; doc          - http://doc.norang.ca/
                                        ; org-mode-doc - http://doc.norang.ca/org-mode.html and associated files
                                        ; org          - miscellaneous todo lists for publishing
(setq org-publish-project-alist
                                        ;
                                        ; http://www.norang.ca/  (norang website)
                                        ; norang-org are the org-files that generate the content
                                        ; norang-extra are images and css files that need to be included
                                        ; norang is the top-level project that gets published
      (quote (("norang-org"
               :base-directory "~/git/www.norang.ca"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
               :recursive t
               :table-of-contents nil
               :base-extension "org"
               :publishing-function org-html-publish-to-html
               :style-include-default nil
               :section-numbers nil
               :table-of-contents nil
               :html-head "<link rel=\"stylesheet\" href=\"norang.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("norang-extra"
               :base-directory "~/git/www.norang.ca/"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("norang"
               :components ("norang-org" "norang-extra"))
                                        ;
                                        ; http://doc.norang.ca/  (norang website)
                                        ; doc-org are the org-files that generate the content
                                        ; doc-extra are images and css files that need to be included
                                        ; doc is the top-level project that gets published
              ("doc-org"
               :base-directory "~/git/doc.norang.ca/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :recursive nil
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-html-publish-to-html org-org-publish-to-org)
               :style-include-default nil
               :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("doc-extra"
               :base-directory "~/git/doc.norang.ca/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive nil
               :author nil)
              ("doc"
               :components ("doc-org" "doc-extra"))
              ("doc-private-org"
               :base-directory "~/git/doc.norang.ca/private"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
               :recursive nil
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-html-publish-to-html org-org-publish-to-org)
               :style-include-default nil
               :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :auto-sitemap t
               :sitemap-filename "index.html"
               :sitemap-title "Norang Private Documents"
               :sitemap-style "tree"
               :author-info nil
               :creator-info nil)
              ("doc-private-extra"
               :base-directory "~/git/doc.norang.ca/private"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs/private"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive nil
               :author nil)
              ("doc-private"
               :components ("doc-private-org" "doc-private-extra"))
                                        ;
                                        ; Miscellaneous pages for other websites
                                        ; org are the org-files that generate the content
              ("org-org"
               :base-directory "~/git/org/"
               :publishing-directory "/ssh:www-data@www:~/org"
               :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function org-html-publish-to-html
               :style-include-default nil
               :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
                                        ;
                                        ; http://doc.norang.ca/  (norang website)
                                        ; org-mode-doc-org this document
                                        ; org-mode-doc-extra are images and css files that need to be included
                                        ; org-mode-doc is the top-level project that gets published
                                        ; This uses the same target directory as the 'doc' project
              ("org-mode-doc-org"
               :base-directory "~/git/org-mode-doc/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-html-publish-to-html)
               :plain-source t
               :htmlized-source t
               :style-include-default nil
               :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
               :author-info nil
               :creator-info nil)
              ("org-mode-doc-extra"
               :base-directory "~/git/org-mode-doc/"
               :publishing-directory "/ssh:www-data@www:~/doc.norang.ca/htdocs"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|org"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("org-mode-doc"
               :components ("org-mode-doc-org" "org-mode-doc-extra"))
                                        ;
                                        ; http://doc.norang.ca/  (norang website)
                                        ; org-mode-doc-org this document
                                        ; org-mode-doc-extra are images and css files that need to be included
                                        ; org-mode-doc is the top-level project that gets published
                                        ; This uses the same target directory as the 'doc' project
              ("tmp-org"
               :base-directory "/tmp/publish/"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
               :recursive t
               :section-numbers nil
               :table-of-contents nil
               :base-extension "org"
               :publishing-function (org-html-publish-to-html org-org-publish-to-org)
               :html-head "<link rel=\"stylesheet\" href=\"http://doc.norang.ca/org.css\" type=\"text/css\" />"
               :plain-source t
               :htmlized-source t
               :style-include-default nil
               :auto-sitemap t
               :sitemap-filename "index.html"
               :sitemap-title "Test Publishing Area"
               :sitemap-style "tree"
               :author-info t
               :creator-info t)
              ("tmp-extra"
               :base-directory "/tmp/publish/"
               :publishing-directory "/ssh:www-data@www:~/www.norang.ca/htdocs/tmp"
               :base-extension "css\\|pdf\\|png\\|jpg\\|gif"
               :publishing-function org-publish-attachment
               :recursive t
               :author nil)
              ("tmp"
               :components ("tmp-org" "tmp-extra")))))

                                        ; I'm lazy and don't want to remember the name of the project to publish when I modify
                                        ; a file that is part of a project.  So this function saves the file, and publishes
                                        ; the project that includes this file
                                        ;
                                        ; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
(defun bh/save-then-publish (&optional force)
  (interactive "P")
  (save-buffer)
  (org-save-all-org-buffers)
  (let ((org-html-head-extra)
        (org-html-validation-link "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"))
    (org-publish-current-project force)))

(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

(setq org-latex-listings t)

(setq org-html-xml-declaration (quote (("html" . "")
                                       ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                                       ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))))

(setq org-export-allow-BIND t)

; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

                                        ; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

                                        ; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

                                        ; Activate appointments so we get notifications
(appt-activate t)

                                        ; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;; Skeletons
;;
;; sblk - Generic block #+begin_FOO .. #+end_FOO
(define-skeleton skel-org-block
  "Insert an org block, querying for type."
  "Type: "
  "#+begin_" str "\n"
  _ - \n
  "#+end_" str "\n")

(define-abbrev org-mode-abbrev-table "sblk" "" 'skel-org-block)

;; splantuml - PlantUML Source block
(define-skeleton skel-org-block-plantuml
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "splantuml" "" 'skel-org-block-plantuml)

(define-skeleton skel-org-block-plantuml-activity
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str "-act.png :cache yes :tangle " str "-act.txt\n"
  (bh/plantuml-reset-counters)
  "@startuml\n"
  "skinparam activity {\n"
  "BackgroundColor<<New>> Cyan\n"
  "}\n\n"
  "title " str " - \n"
  "note left: " str "\n"
  "(*) --> \"" str "\"\n"
  "--> (*)\n"
  _ - \n
  "@enduml\n"
  "#+end_src\n")

(defvar bh/plantuml-if-count 0)

(defun bh/plantuml-if ()
  (incf bh/plantuml-if-count)
  (number-to-string bh/plantuml-if-count))

(defvar bh/plantuml-loop-count 0)

(defun bh/plantuml-loop ()
  (incf bh/plantuml-loop-count)
  (number-to-string bh/plantuml-loop-count))

(defun bh/plantuml-reset-counters ()
  (setq bh/plantuml-if-count 0
        bh/plantuml-loop-count 0)
  "")

(define-abbrev org-mode-abbrev-table "sact" "" 'skel-org-block-plantuml-activity)

(define-skeleton skel-org-block-plantuml-activity-if
  "Insert a org plantuml block activity if statement"
  ""
  "if \"\" then\n"
  "  -> [condition] ==IF" (setq ifn (bh/plantuml-if)) "==\n"
  "  --> ==IF" ifn "M1==\n"
  "  -left-> ==IF" ifn "M2==\n"
  "else\n"
  "end if\n"
  "--> ==IF" ifn "M2==")

(define-abbrev org-mode-abbrev-table "sif" "" 'skel-org-block-plantuml-activity-if)

(define-skeleton skel-org-block-plantuml-activity-for
  "Insert a org plantuml block activity for statement"
  "Loop for each: "
  "--> ==LOOP" (setq loopn (bh/plantuml-loop)) "==\n"
  "note left: Loop" loopn ": For each " str "\n"
  "--> ==ENDLOOP" loopn "==\n"
  "note left: Loop" loopn ": End for each " str "\n" )

(define-abbrev org-mode-abbrev-table "sfor" "" 'skel-org-block-plantuml-activity-for)

(define-skeleton skel-org-block-plantuml-sequence
  "Insert a org plantuml activity diagram block, querying for filename."
  "File appends (no extension): "
  "#+begin_src plantuml :file " str "-seq.png :cache yes :tangle " str "-seq.txt\n"
  "@startuml\n"
  "title " str " - \n"
  "actor CSR as \"Customer Service Representative\"\n"
  "participant CSMO as \"CSM Online\"\n"
  "participant CSMU as \"CSM Unix\"\n"
  "participant NRIS\n"
  "actor Customer"
  _ - \n
  "@enduml\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sseq" "" 'skel-org-block-plantuml-sequence)

;; sdot - Graphviz DOT block
(define-skeleton skel-org-block-dot
  "Insert a org graphviz dot block, querying for filename."
  "File (no extension): "
  "#+begin_src dot :file " str ".png :cache yes :cmdline -Kdot -Tpng\n"
  "graph G {\n"
  _ - \n
  "}\n"
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sdot" "" 'skel-org-block-dot)

;; sditaa - Ditaa source block
(define-skeleton skel-org-block-ditaa
  "Insert a org ditaa block, querying for filename."
  "File (no extension): "
  "#+begin_src ditaa :file " str ".png :cache yes\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "sditaa" "" 'skel-org-block-ditaa)

;; selisp - Emacs Lisp source block
(define-skeleton skel-org-block-elisp
  "Insert a org emacs-lisp block"
  ""
  "#+begin_src emacs-lisp\n"
  _ - \n
  "#+end_src\n")

(define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

(global-set-key (kbd "<f5>") 'bh/org-todo)

(defun bh/org-todo (arg)
  (interactive "p")
  (if (equal arg 4)
      (save-restriction
        (bh/narrow-to-org-subtree)
        (org-show-todo-tree nil))
    (bh/narrow-to-org-subtree)
    (org-show-todo-tree nil)))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (beginning-of-buffer)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (beginning-of-buffer))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/project-list nil)

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
                                        ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

                                        ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

                                        ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
                                        ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (beginning-of-buffer)
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (beginning-of-buffer)
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

(setq org-show-entry-below (quote ((default))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
          'append)

(defun bh/set-agenda-restriction-lock (arg)
  "Set restriction lock to current task subtree or file if prefix is specified"
  (interactive "p")
  (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
         (tags (org-with-point-at pom (org-get-tags-at))))
    (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
      (save-restriction
        (cond
         ((and (equal major-mode 'org-agenda-mode) pom)
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))
          (org-agenda-redo))
         ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
          (org-agenda-set-restriction-lock 'file))
         (pom
          (org-with-point-at pom
            (org-agenda-set-restriction-lock restriction-type))))))))

;; Limit restriction lock highlighting to the headline only
(setq org-agenda-restriction-lock-highlight-subtree nil)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)

;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)

;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)

;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file "~/git/org/diary.org")

(setq org-agenda-insert-diary-extract-time t)

;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Enable display of the time grid so we can see the marker for the current time
(setq org-agenda-time-grid (quote ((daily today remove-match)
                                   #("----------------" 0 16 (org-heading t))
                                   (0900 1100 1300 1500 1700))))

;; Display tags farther right
(setq org-agenda-tags-column -102)

;;
;; Agenda sorting functions
;;
(setq org-agenda-cmp-user-defined 'bh/agenda-sort)

(defun bh/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
                                        ; time specific items are already sorted first by org-agenda-sorting-strategy

                                        ; non-deadline and non-scheduled items next
     ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

                                        ; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

                                        ; late deadlines next
     ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

                                        ; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

                                        ; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

                                        ; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

                                        ; finally default to unsorted
     (t (setq result nil)))
    result))

(defmacro bh/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(cond
                                        ; if both match leave them unsorted
    ((and (apply ,fn (list ,a))
          (apply ,fn (list ,b)))
     (setq result nil))
                                        ; if a matches put a first
    ((apply ,fn (list ,a))
     (setq result -1))
                                        ; otherwise if b matches put b first
    ((apply ,fn (list ,b))
     (setq result 1))
                                        ; if none match leave them unsorted
    (t nil)))

(defmacro bh/agenda-sort-test-num (fn compfn a b)
  `(cond
    ((apply ,fn (list ,a))
     (setq num-a (string-to-number (match-string 1 ,a)))
     (if (apply ,fn (list ,b))
         (progn
           (setq num-b (string-to-number (match-string 1 ,b)))
           (setq result (if (apply ,compfn (list num-a num-b))
                            -1
                          1)))
       (setq result -1)))
    ((apply ,fn (list ,b))
     (setq result 1))
    (t nil)))

(defun bh/is-not-scheduled-or-deadline (date-str)
  (and (not (bh/is-deadline date-str))
       (not (bh/is-scheduled date-str))))

(defun bh/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun bh/is-late-deadline (date-str)
  (string-match "\\([0-9]*\\) d\. ago:" date-str))

(defun bh/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\.:" date-str))

(defun bh/is-deadline (date-str)
  (or (bh/is-due-deadline date-str)
      (bh/is-late-deadline date-str)
      (bh/is-pending-deadline date-str)))

(defun bh/is-scheduled (date-str)
  (or (bh/is-scheduled-today date-str)
      (bh/is-scheduled-late date-str)))

(defun bh/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun bh/is-scheduled-late (date-str)
  (string-match "Sched\.\\(.*\\)x:" date-str))

;; Use sticky agenda's so they persist
(setq org-agenda-sticky t)

;; The following setting is different from the document so that you
;; can override the document path by setting your path in the variable
;; org-mode-user-contrib-lisp-path
;;
(if (boundp 'org-mode-user-contrib-lisp-path)
    (add-to-list 'load-path org-mode-user-contrib-lisp-path)
  (add-to-list 'load-path (expand-file-name "~/git/org-mode/contrib/lisp")))

(require 'org-checklist)
(require 'org-habit)

(setq org-enforce-todo-dependencies t)

(setq org-hide-leading-stars nil)

(setq org-startup-indented t)

(setq org-cycle-separator-lines 0)

(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item . auto))))

(setq org-insert-heading-respect-content nil)

(setq org-reverse-note-order nil)

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings (quote ((default))))

(setq org-special-ctrl-a/e t)
(setq org-special-ctrl-k t)
(setq org-yank-adjusted-subtrees t)

(setq org-id-method (quote uuidgen))

(setq org-deadline-warning-days 30)

(setq org-table-export-default-format "orgtbl-to-csv")

(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file))))

                                        ; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

(setq org-log-done (quote time))
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(setq org-clock-sound "/usr/local/lib/tngchime.wav")

; Enable habit tracking (and a bunch of other modules)
(setq org-modules (quote (org-bbdb
                          org-bibtex
                          org-crypt
                          org-gnus
                          org-id
                          org-info
                          org-jsinfo
                          org-habit
                          org-inlinetask
                          org-irc
                          org-mew
                          org-mhe
                          org-protocol
                          org-rmail
                          org-vm
                          org-wl
                          org-w3m)))

                                        ; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 50)

(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(global-auto-revert-mode t)

(require 'org-crypt)
                                        ; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
                                        ; GPG key to use for encryption
(setq org-crypt-key "F0B66B40")

(setq org-crypt-disable-auto-save nil)

(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . ignore)
                                      ("1" . ignore)
                                      ("2" . ignore)
                                      ("3" . ignore)
                                      ("4" . ignore)
                                      ("5" . ignore)
                                      ("6" . ignore)
                                      ("7" . ignore)
                                      ("8" . ignore)
                                      ("9" . ignore)

                                      ("a" . ignore)
                                      ("d" . ignore)
                                      ("h" . bh/hide-other)
                                      ("i" progn
                                       (forward-char 1)
                                       (call-interactively 'org-insert-heading-respect-content))
                                      ("k" . org-kill-note-or-show-branches)
                                      ("l" . ignore)
                                      ("m" . ignore)
                                      ("q" . bh/show-org-agenda)
                                      ("r" . ignore)
                                      ("s" . org-save-all-org-buffers)
                                      ("w" . org-refile)
                                      ("x" . ignore)
                                      ("y" . ignore)
                                      ("z" . org-add-note)

                                      ("A" . ignore)
                                      ("B" . ignore)
                                      ("E" . ignore)
                                      ("F" . bh/restrict-to-file-or-follow)
                                      ("G" . ignore)
                                      ("H" . ignore)
                                      ("J" . org-clock-goto)
                                      ("K" . ignore)
                                      ("L" . ignore)
                                      ("M" . ignore)
                                      ("N" . bh/narrow-to-org-subtree)
                                      ("P" . bh/narrow-to-org-project)
                                      ("Q" . ignore)
                                      ("R" . ignore)
                                      ("S" . ignore)
                                      ("T" . bh/org-todo)
                                      ("U" . bh/narrow-up-one-org-level)
                                      ("V" . ignore)
                                      ("W" . bh/widen)
                                      ("X" . ignore)
                                      ("Y" . ignore)
                                      ("Z" . ignore))))

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

(require 'org-protocol)

(setq require-final-newline t)

(defvar bh/insert-inactive-timestamp t)

(defun bh/toggle-insert-inactive-timestamp ()
  (interactive)
  (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

(defun bh/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun bh/insert-heading-inactive-timestamp ()
  (save-excursion
    (when bh/insert-inactive-timestamp
      (org-return)
      (org-cycle)
      (bh/insert-inactive-timestamp))))

(add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

(setq org-export-with-timestamps nil)

(setq org-return-follows-link t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:foreground "red" :box (:line-width -1 :style released-button)))) t))

(defun bh/prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

(setq org-remove-highlights-with-change t)

(add-to-list 'Info-default-directory-list "~/git/org-mode/doc")

(setq org-read-date-prefer-future 'time)

(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-")
                                            ("A)" . "-")
                                            ("B)" . "-")
                                            ("a)" . "-")
                                            ("b)" . "-")
                                            ("A." . "-")
                                            ("B." . "-")
                                            ("a." . "-")
                                            ("b." . "-"))))

(setq org-tags-match-list-sublevels t)

(setq org-agenda-persistent-filter t)

(setq org-link-mailto-program (quote (compose-mail "%a" "%s")))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/smex-items"))
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Bookmark handling
;;
(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

(require 'org-mime)

(setq org-agenda-skip-additional-timestamps-same-entry t)

(setq org-table-use-standard-references (quote from))

(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.mm\\'" . system)
                            ("\\.x?html?\\'" . system)
                            ("\\.pdf\\'" . system))))

; Overwrite the current window with the agenda
(setq org-agenda-window-setup 'current-window)

(setq org-clone-delete-id t)

(setq org-cycle-include-plain-lists t)

(setq org-src-fontify-natively t)

(setq org-structure-template-alist
      (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
              ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
              ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
              ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
              ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
              ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
              ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
              ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
              ("H" "#+html: " "<literal style=\"html\">?</literal>")
              ("a" "#+begin_ascii\n?\n#+end_ascii")
              ("A" "#+ascii: ")
              ("i" "#+index: ?" "#+index: ?")
              ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)
(add-hook 'org-clock-in-hook 'bh/mark-next-parent-tasks-todo 'append)

(setq org-startup-folded t)

(add-hook 'message-mode-hook 'orgstruct++-mode 'append)
(add-hook 'message-mode-hook 'turn-on-auto-fill 'append)
(add-hook 'message-mode-hook 'bbdb-define-all-aliases 'append)
(add-hook 'message-mode-hook 'orgtbl-mode 'append)
(add-hook 'message-mode-hook 'turn-on-flyspell 'append)
(add-hook 'message-mode-hook
          '(lambda () (setq fill-column 72))
          'append)

;; flyspell mode for spell checking everywhere
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Disable keys in org-mode
;;    C-c [
;;    C-c ]
;;    C-c ;
;;    C-c C-x C-q  cancelling the clock (we never want this)
(add-hook 'org-mode-hook
          '(lambda ()
             ;; Undefine C-c [ and C-c ] since this breaks my
             ;; org-agenda files when directories are include It
             ;; expands the files in the directories individually
             (org-defkey org-mode-map "\C-c[" 'undefined)
             (org-defkey org-mode-map "\C-c]" 'undefined)
             (org-defkey org-mode-map "\C-c;" 'undefined)
             (org-defkey org-mode-map "\C-c\C-x\C-q" 'undefined))
          'append)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c M-o") 'bh/mail-subtree))
          'append)

(defun bh/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

(setq org-catch-invisible-edits 'error)

(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)
