(defgroup typing-game nil
  "typing game")
(defcustom typing-game-letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "字母候选列表")
(defcustom typing-game-letters-per-row 2
  "how many letters will be generated one row")
(defvar typing-game-total-scores 0)
(defcustom typing-game-scores-per-escaped-letter 10
  "how many scores will be reduced when one letter escaped")
(defcustom typing-game-scores-per-erased-letter 1
  "how many scores will be increased when one letter erased ")
(defcustom typing-game-escaped-letters ""
  "letters that escaped in the game")

(defun typing-game//random-letter ()
  "generate a random letter"
  (elt typing-game-letters (random (length typing-game-letters))))

(defun typing-game//generate-letters (letter-number)
  "返回新产生的字幕"
  (let* ((typing-game-width (window-body-width))
         (str (make-string typing-game-width ?\ )))
    (dotimes (var letter-number)
      (setf (elt str (random typing-game-width)) (typing-game//random-letter)))
    str))

(defun typing-game/scroll-down (buffer)
  "字幕下滚一行"
  (with-current-buffer buffer
    (save-excursion
      (let ((inhibit-read-only t)
            (typing-game-height (window-body-height)))
        (goto-char (point-min))
        (insert (typing-game//generate-letters typing-game-letters-per-row))
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
        (delete-region (point) (point-max)))))
  (force-mode-line-update))

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
        (replace-match " " nil t))
      (force-mode-line-update))))


(defcustom typing-game-format
  `((:eval (format "[%s]" typing-game-total-scores)))
  "format for displaying the total scores in the mode line"
  :group 'typing-game)

(setq typing-game-format
  `((:eval (format "[%s]" typing-game-total-scores))))

(define-derived-mode typing-game-mode text-mode "typing-game"
  "Major mode for running typing-game"
  (local-set-key  [remap self-insert-command] 'typing-game/erase)
  (make-local-variable 'global-mode-string)
  (setq global-mode-string (append global-mode-string `(,typing-game-format))))

(defvar typing-game-timer nil)

(defcustom typing-game-buffer "*typing-game*"
  "buffer name of typing-game")

(defun typing-game/make-gui ()
  (switch-to-buffer (get-buffer-create typing-game-buffer))
  (typing-game-mode)
  (read-only-mode))

(defun typing-game/start-game-at-speed (speed)
  "start the typing game. `speed' determied how fast the letters failing. "
  (interactive "P")
  (let ((speed (or speed 1)))
    (typing-game/make-gui)
    (typing-game/stop-game 'DO-NOT-SHOW-RESULT)
    (setq typing-game-timer (run-with-timer 0 (/ 5 speed) #'typing-game/scroll-down (current-buffer)))))

(defun typing-game/stop-game (&optional not-show-result)
  (interactive)
  (when (timerp  typing-game-timer)
    (cancel-timer typing-game-timer)
    (setq typing-game-timer nil))
  (unless not-show-result
    (typing-game/show-result)))

(defun typing-game/show-result ()
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create typing-game-buffer)
      (erase-buffer)
      (insert (user-login-name) ":")
      (newline)
      (insert (format "Total Scores: %d"  typing-game-total-scores))
      (newline)
      (insert "Those letters are escaped: \n" typing-game-escaped-letters)
      (newline))))
