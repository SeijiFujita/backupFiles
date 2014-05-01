;;; package --- .emacs / .init.el
;;; Commentary: 
;;;###autoload
;;; Code:
;;http://emacs-jp.github.io/packages/package-management/package-el.html
;; Usage
;; M-x package-list-packages ;; show package list
;; M-x package-install
;; 
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;;
;;
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
;; for windows
(setq default-file-name-coding-system 'shift_jis)
;;
;;-- http://memememomo.hatenablog.com/entry/2013/03/17/182626
;; command for byte compile
;; $ emacs -batch -f batch-byte-compile .emacs.d/packages/auto-install.el 
;;
;; 最終行に必ず一行挿入する
(setq require-final-newline t)
;; バッファの最後でnewlineで新規行を追加するのを禁止する
(setq next-line-add-newlines nil)
;; バックアップファイルを作らない
(setq make-backup-files nil)
;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
;; ediffを1ウィンドウで実行
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; diffのオプション
(setq diff-switches '("-u" "-p" "-N"))
;; grep
(require 'grep)
(setq grep-command-before-query "grep -nH -r -e  *")
(defun grep-default-command ()
(if current-prefix-arg
(let ((grep-command-before-target
(concat grep-command-before-query
(shell-quote-argument (grep-tag-default)))))
(cons (if buffer-file-name
(concat grep-command-before-target
" *."
(file-name-extension buffer-file-name))
(concat grep-command-before-target " ."))
(+ (length grep-command-before-target) 1)))
(car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
(+ (length grep-command-before-query) 1)))
;;
;;-- http://weboo-returns.com/blog/emacs-shows-double-space-and-tab/
;; Show Zenkaku Char 
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")
;;
;; - See more at: http://yohshiy.blog.fc2.com/blog-entry-171.html#sthash.Gn2t1Aip.dpuf
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; tab size
(setq tab-width 4)
;;(setq default-tab-width 4)

;; back space key(^H)
(global-set-key (kbd "C-h") 'delete-backward-char)
;; help for help
(global-set-key (kbd "M-?") 'help-for-help)

;; line number
;;(require 'linum)
;;(global-linum-mode t)

;; tool bar
;;(setq tool-bar-style 'text)
;;(setq tool-bar-style 'both-horiz)
(tool-bar-mode -1)
;;
;;(menu-bar-mode -1)
;;
(scroll-bar-mode -1)
;; 折り返し記号を目立たなくする
(set-face-foreground 'fringe "#404F58")
;;
;; カーソルの位置を覚えておく
(require 'saveplace)
(setq-default save-place t)
;; カーソル位置の表示
(column-number-mode 1)
;; blink off
(blink-cursor-mode 0)
;; スタート画面を表示しない
(setq inhibit-startup-screen t)
;; scratchバッファのメッセージを表示しない
(setq initial-scratch-message "")
;; カーソル位置から行頭まで削除する
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))
;; C-S-kに設定
(global-set-key (kbd "C-c k") 'backward-kill-line);; メモをするための関数を定義

;; sadfafsaf sfdafafasfsa
;; インスタントメモ
(defun memo ()
  (interactive)
  (let ((path "~/memo/") (filename (format-time-string "%H.%M.%S")))
    (if (file-directory-p path)
	(switch-to-buffer (find-file (concat path "m-" filename ".md")))
      (message (concat "No such directory: " path)))))
(global-set-key (kbd "C-c m") 'memo)


;; -- color
;; http://qiita.com/kubosho_/items/17464754663936cb7895
(require 'color-theme)
(color-theme-initialize)
;;(color-theme-molokai)
;;(color-theme-tango)
(color-theme-tangotango)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [基本] 背景色・透過
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 背景色設定
;;(custom-set-faces
;; '(default ((t (:background "#000022" :foreground "#EEEEEE"))))
;; '(cursor (
;;           (((class color) (background dark )) (:background "#00AA00"))
;;           (((class color) (background light)) (:background "#999999"))
;;           (t ())
;;           )))
;; フレーム透過設定
(add-to-list 'default-frame-alist '(alpha . (0.95 0.95)))
;; dlang
;; http://qiita.com/tm_tn/items/1d01c4500e1ca7632140
;;;
;
(require 'compile)
(add-to-list
 'compilation-error-regexp-alist
 '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
   1 2 nil (3 . 4)))
;
;flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  )

;; dlang
;;(add-to-list 'exec-path (expand-file-name "C:/D/dmd.2.065.0/windows/bin"))
(require 'flycheck-d-unittest)
(setup-flycheck-d-unittest)
(add-hook 'd-mode-hook 'flycheck-dmd-dub-set-include-path)
(add-hook 'd-mode-hook
	  (lambda ()
	    (local-set-key  (kbd "C-c C-p") 'flycheck-previous-error)
	    (local-set-key  (kbd "C-c C-n") 'flycheck-next-error)))
;;
;; --http://seesaawiki.jp/whiteflare503/d/Emacs%20%A5%A4%A5%F3%A5%C7%A5%F3%A5%C8
;;
(setq-default c-basic-offset 4     ;;基本インデント量4
              tab-width 4          ;;タブ幅4
              indent-tabs-mode nil)  ;;インデントをタブでするかスペースでするか
(defun set-c-mode-common-conf ()
;; (欲張り削除 + electric-mode)バックスペースなどの削除するキーを押すと
;; スペースを一気に消す欲張り削除機能とelecetic-modeをを有功にする
;;(c-toggle-hungry-state 1)
;; (自動インデント) 改行をしたら次の行を自動でインデントしてくれる
(c-toggle-auto-newline 1)
(c-set-style "stroustrup")                  ;; スタイルはストラウストラップ
(flyspell-prog-mode)                        ;; flyspell-prog-mode(自動ispell機能)
(show-paren-mode t)                         ;; カッコを強調表示する
;; 他のエディタなどがファイルを書き換えたらすぐにそれを反映する
;; auto-revert-modeを有効にする
 (auto-revert-mode)
)
;; C言語系のモード全てに適用されるフック
;; c-mode-common-hookにフックを設定する
(add-hook 'c-mode-common-hook 'set-c-mode-common-conf)
;;
;; IME の設定
(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-state-indicator "[--]") ;; おこのみで
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]")) ;; おこのみで
(w32-ime-initialize)

;;; IME ON/OFF 時にカーソル色を変える。
(setq ime-activate-cursor-color "#00a000")
(setq ime-inactivate-cursor-color "#000000")
(set-cursor-color ime-inactivate-cursor-color)
;; ※input-method-activate-hook, input-method-inactivate-hook じゃない方がいい感じになる。
(add-hook 'w32-ime-on-hook
          (function (lambda ()
                      (set-cursor-color ime-activate-cursor-color))))
(add-hook 'w32-ime-off-hook
          (function (lambda ()
                      (set-cursor-color ime-inactivate-cursor-color))))

;;http://www.shido.info/lisp/scheme1.html
(show-paren-mode t)  ; 対応する括弧を表示する
; run-scheme で動かす処理系
;;(setq scheme-program-name "C:/Lisp/Program Files/MzScheme/MzScheme.exe")
;;(setq scheme-program-name "C:/Lisp/racket/Racket.exe")
;;(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; C-c s で emacs 上で MzScheme が走るようにする
;;(define-key global-map "\C-cs" 'run-scheme)
;; Emacs を scheme 専用に使う時には以下を追加すると便利
;(setq inhibit-startup-message t)  ; 起動画面を表示しない
;(run-scheme scheme-program-name)  ; 起動時にいきなり scheme 処理系を走らせる


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(initial-frame-alist (quote ((top . 10) (left . 10) (width . 100) (height . 40))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "outline" :slant normal :weight normal :height 120 :width normal))))
 )

;;; init.el
