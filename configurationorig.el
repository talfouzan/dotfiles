
(load-file "~/.emacs.d/sensible-defaults.el/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)

(add-to-list 'load-path "~/.emacs.d/resources/")

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)

(evil-mode 1)

(global-evil-surround-mode 1)

(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(defun hrs/view-buffer-name ()
  "Display the filename of the current buffer."
  (interactive)
  (message (buffer-file-name)))

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
     name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

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

(setq-default tab-width 2)

(global-subword-mode 1)

(setq compilation-scroll-output t)

(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode)
            (setq css-indent-offset 2)))

(add-hook 'scss-mode-hook 'rainbow-mode)

(setq scss-compile-at-save nil)

(setq exec-path (append exec-path (list "~/.cabal/bin")))

(add-hook 'haskell-mode-hook
          (lambda ()
            (haskell-doc-mode)
            (turn-on-haskell-indent)
            (ghc-init)))

(setq js-indent-level 2)

(add-hook 'coffee-mode-hook
          (lambda ()
            (yas-minor-mode 1)
            (setq coffee-tab-width 2)))

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

(hrs/add-auto-mode 'prolog-mode "\\.pl$")

(defun hrs/search-project-for-symbol-at-point ()
  "Use `projectile-ag' to search the current project for `symbol-at-point'."
  (interactive)
  (projectile-ag (projectile-symbol-at-point)))

(global-set-key (kbd "C-c v") 'projectile-ag)
(global-set-key (kbd "C-c C-v") 'hrs/search-project-for-symbol-at-point)

(setq projectile-switch-project-action 'projectile-dired)

(setq python-indent 2)

(chruby "ruby-2.4.0")

(setq xmpfilter-command-name
      "ruby -S xmpfilter --no-warnings --dev --fork --detect-rbtest")
(require 'rcodetools)

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-insert-encoding-magic-comment nil)
            (yas-minor-mode)
            (rspec-mode)
            (yard-mode)
            (flycheck-mode)
            (local-set-key "\r" 'newline-and-indent)
            (setq rspec-command-options "--color --order random")
            (define-key ruby-mode-map (kbd "C-c C-c") 'xmp)
            (projectile-rails-mode)))

(hrs/add-auto-mode
 'ruby-mode
 "\\Gemfile$"
 "\\.rake$"
 "\\.gemspec$"
 "\\Guardfile$"
 "\\Rakefile$"
 "\\Vagrantfile$"
 "\\Vagrantfile.local$")

(add-hook 'rspec-compilation-mode-hook
          (lambda ()
            (make-local-variable 'compilation-scroll-output)
            (setq compilation-scroll-output 'first-error)))

(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2
                  sh-indentation 2)))

(add-hook 'slim-mode-hook 'rspec-mode)

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

(add-hook 'yaml-mode-hook 'rspec-mode)

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

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(setq org-directory "~/org")

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

(setq org-capture-templates
      '(("b" "Blog idea"
         entry
         (file (org-file-path "blog-ideas.org"))
         "* TODO %?\n")

        ("g" "Groceries"
         checkitem
         (file (org-file-path "groceries.org")))

        ("l" "Today I Learned..."
         entry
         (file+datetree (org-file-path "til.org"))
         "* %?\n")

        ("r" "Reading"
         checkitem
         (file (org-file-path "to-read.org")))

        ("t" "Todo"
         entry
         (file+headline org-index-file "Inbox")
         "* TODO %?\n")))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

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

(setq org-html-postamble nil)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq TeX-parse-self t)

(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode)
            (setq TeX-master t)))

(defvar hrs/jekyll-posts-directory "~/documents/blog/_posts/")
(defvar hrs/jekyll-post-extension ".md")

(defun hrs/replace-unusual-characters (title)
  "Replace characters that aren't alphanumeric with hyphens."
  (replace-regexp-in-string " " "-"
                            (downcase (replace-regexp-in-string "[^A-Za-z0-9 ]" " " title))))

(defun hrs/slug-for (title)
  "Given a blog post title, return a convenient URL slug.
  Downcase letters and remove special characters."
  (let ((slug (hrs/replace-unusual-characters title)))
    (while (string-match "--" slug)
      (setq slug (replace-regexp-in-string "--" "-" slug)))
    slug))

(defun hrs/timestamped-slug-for (title)
  "Turn a string into a slug with a timestamp and title."
  (concat (format-time-string "%Y-%m-%d")
          "-"
          (hrs/slug-for title)))

(defun hrs/jekyll-yaml-template (title)
  "Return the YAML header information appropriate for a blog
  post. Include the title, the current date, the post layout, and
  an empty list of tags."
  (concat
   "---\n"
   "title: " title "\n"
   "date: " (format-time-string "%Y-%m-%d") "\n"
   "layout: post\n"
   "tags: []\n"
   "---\n\n"))

(defun hrs/new-blog-post (title)
  "Create a new blog post in Jekyll."
  (interactive "sPost title: ")
  (let ((post (concat hrs/jekyll-posts-directory
                      (hrs/timestamped-slug-for title)
                      hrs/jekyll-post-extension)))
    (if (file-exists-p post)
        (find-file post)
      (find-file post)
      (insert (hrs/jekyll-yaml-template title)))))

(setq hrs/checklist-template "~/documents/daily-checklist.org.erb")

(defun hrs/today-checklist-filename ()
  "The filename of today's checklist."
  (concat "/tmp/daily-checklist-" (format-time-string "%Y-%m-%d") ".org"))

(defun hrs/today ()
  "Take a look at today's checklist."
  (interactive)
  (let ((filename (hrs/today-checklist-filename)))
    (if (file-exists-p filename)
        (find-file filename)
      (progn
        (shell-command (concat "erb " hrs/checklist-template " > " filename))
        (find-file filename)))))

(global-set-key (kbd "C-c t") 'hrs/today)

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

(global-set-key (kbd "C-x k") 'hrs/kill-current-buffer)

(setq exec-path (append exec-path '("/usr/local/bin")))

(add-hook 'after-init-hook 'global-company-mode)

(setq-default indent-tabs-mode nil)

(setq yas-snippet-dirs '("~/.emacs.d/snippets/text-mode"))
(yas-global-mode 1)

(setq yas/indent-line nil)

(define-abbrev-table 'global-abbrev-table
  '((";name" "Harry R. Schwartz")
    (";email" "hello@harryrschwartz.com")
    (";tb" "harry@thoughtbot.com")
    (";site" "http://harryrschwartz.com")))

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

(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-o") 'other-window)

(define-key input-decode-map "\e[1;2A" [S-up])
