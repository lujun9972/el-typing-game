(defgroup type-game nil
  "打字游戏")
(defcustom type-game-height 10
  "打字游戏界面的行数")
(defcustom type-game-width 10
  "打字游戏界面的列数")
(defcustom type-game-letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "字母候选列表")
(defcustom letter-number-on-row 2
  "每行产生的新字母数")

(defun type-game//random-letter ()
  ""
  (elt type-game-letters (random (length type-game-letters))))

(defun type-game//generate-letters ()
  "返回新产生的字母"
  (let ((str (make-string type-game-width ?\ )))
    (dotimes (var letter-number-on-row)
      (setf (elt str (random type-game-width)) (type-game//random-letter)))
    str))

(defun type-game/down (buffer)
  "字符下滚一行"
  (with-current-buffer buffer
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (insert (type-game//generate-letters))
        (newline)
        ;; 跳转到第N行
        (goto-char (point-min))
        (forward-line (- type-game-height 1))
        ;; 删除后面的内容
        (end-of-line)
        (delete-region (point) (point-max))))))

(defvar type-game-timer nil)

(defun type-game/start-game-at-speed (speed)
  ""
  (interactive "N")
  ;; (type-game-mode 1)
  ;; (let ((height-delta (- type-game-height (window-body-height)))
  ;;       (width-delta (- type-game-width (window-body-width))))
  ;;   (window-resize nil width-delta nil t)
  ;;   (window-resize nil height-delta t t))
  ;; (setq window-size-fixed t)
  (type-game/stop-game)
  (setq type-game-timer (run-with-timer (/ 1.0 speed) (/ 5 speed) #'type-game/down (current-buffer))))

(defun type-game/stop-game ()
  (interactive)
  (when type-game-timer
    (cancel-timer type-game-timer)))

(defun type-game/erase ()
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


(define-derived-mode type-game-mode text-mode "type-game"
  "Major mode for running type-game"
  (local-set-key  [remap self-insert-command] 'type-game/erase))
