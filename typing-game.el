(defgroup typing-game nil
  "打字游戏")
(defcustom typing-game-height 10
  "打字游戏界面的行数")
(defcustom typing-game-width 10
  "打字游戏界面的列数")
(defcustom typing-game-letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "字母候选列表")
(defcustom letter-number-one-row 2
  "每行产生的新字母数")

(defun typing-game//random-letter ()
  "产生随机字母"
  (elt typing-game-letters (random (length typing-game-letters))))

(defun typing-game//generate-letters ()
  "返回新产生的字幕"
  (let ((str (make-string typing-game-width ?\ )))
    (dotimes (var letter-number-one-row)
      (setf (elt str (random typing-game-width)) (typing-game//random-letter)))
    str))

(defun typing-game/down (buffer)
  "字幕下滚一行"
  (with-current-buffer buffer
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (insert (typing-game//generate-letters))
        (newline)
        ;; 跳转到第N行
        (goto-char (point-min))
        (forward-line (- typing-game-height 1))
        ;; 删除后面的内容
        (end-of-line)
        (delete-region (point) (point-max))))))

(defvar typing-game-timer nil)

(defcustom typing-game-buffer "*typing-game*"
  "buffer name of typing-game")

(defun typing-game/make-gui ()
  (switch-to-buffer (get-buffer-create typing-game-buffer))
  (typing-game-mode)
  ;; (let ((height-delta (- typing-game-height (window-body-height)))
  ;;       (width-delta (- typing-game-width (window-body-width))))
  ;;   (window-resize nil width-delta nil t)
  ;;   (window-resize nil height-delta t t))
  ;; (setq window-size-fixed t)
  )
(defun typing-game/start-game-at-speed (speed)
  ""
  (interactive "P")
  (setq speed (or speed 1))
  (typing-game/make-gui)
  (typing-game/stop-game)
  (setq typing-game-timer (run-with-timer (/ 1.0 speed) (/ 5 speed) #'typing-game/down (current-buffer))))

(defun typing-game/stop-game ()
  (interactive)
  (when typing-game-timer
    (cancel-timer typing-game-timer)))

(defun typing-game/erase ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (query-replace-skip-read-only t)
          (this-command-keys (this-command-keys)))
      (goto-char (point-min))
      (while (search-forward this-command-keys nil t)
        (replace-match " " nil t))
      (replace-string (this-command-keys)" "))))


(define-derived-mode typing-game-mode text-mode "typing-game"
  "Major mode for running typing-game"
  (local-set-key  [remap self-insert-command] 'typing-game/erase))
