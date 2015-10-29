(defgroup typing-game nil
  "打字游戏")
(defcustom typing-game-height 10
  "打字游戏界面的行数")
(defcustom typing-game-width 10
  "打字游戏界面的列数")
(defcustom typing-game-letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "字母候选列表")
(defcustom typing-game-letter-number-per-row 2
  "每行产生的新字母数")
(defvar typing-game-total-scores 0)
(defcustom typing-game-scores-per-escaped-letter 10
  "how many scores will be reduced when one letter escaped")
(defcustom typing-game-scores-per-erased-letter 1
  "how many scores will be increased when one letter erased ")
(defcustom typing-game-escaped-letters ""
  "letters that escaped in the game")

(defun typing-game//random-letter ()
  "产生随机字母"
  (elt typing-game-letters (random (length typing-game-letters))))

(defun typing-game//generate-letters ()
  "返回新产生的字幕"
  (let ((str (make-string typing-game-width ?\ )))
    (dotimes (var typing-game-letter-number-per-row)
      (setf (elt str (random typing-game-width)) (typing-game//random-letter)))
    str))

(defun typing-game/scroll-down (buffer)
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
        (end-of-line)
        ;; 删除后面的内容
        (let* ((escaped-letters (replace-regexp-in-string "[ \r\n]" "" (buffer-substring-no-properties (point) (point-max))))
               (escaped-letter-number (length escaped-letters)))
          (setq typing-game-escaped-letters (concat escaped-letters typing-game-escaped-letters))
          (setq typing-game-total-scores (- typing-game-total-scores (* typing-game-scores-per-escaped-letter escaped-letter-number))))
        (delete-region (point) (point-max))))))

(defun typing-game/erase ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (case-fold-search nil)
          (query-replace-skip-read-only t)
          (this-command-keys (this-command-keys)))
      (goto-char (point-min))
      (while (search-forward this-command-keys nil t)
        (setq typing-game-total-scores (+ typing-game-total-scores  typing-game-scores-per-erased-letter ))
        (replace-match " " nil t)))))


(define-derived-mode typing-game-mode text-mode "typing-game"
  "Major mode for running typing-game"
  (local-set-key  [remap self-insert-command] 'typing-game/erase))

(defvar typing-game-timer nil)

(defcustom typing-game-buffer "*typing-game*"
  "buffer name of typing-game")

(defun typing-game/make-gui ()
  (switch-to-buffer (get-buffer-create typing-game-buffer))
  (typing-game-mode)
  (read-only-mode)
  ;; (let ((height-delta (- typing-game-height (window-body-height)))
  ;;       (width-delta (- typing-game-width (window-body-width))))
  ;;   (window-resize nil width-delta nil t)
  ;;   (window-resize nil height-delta t t))
  ;; (setq window-size-fixed t)
  )
(defun typing-game/start-game-at-speed (speed)
  "start the typing game. `speed' determied how fast the letters failing. "
  (interactive "P")
  (setq speed (or speed 1))
  (typing-game/make-gui)
  (typing-game/stop-game)
  (setq typing-game-timer (run-with-timer 0 (/ 5 speed) #'typing-game/scroll-down (current-buffer))))

(defun typing-game/stop-game ()
  (interactive)
  (when typing-game-timer
    (cancel-timer typing-game-timer)))
