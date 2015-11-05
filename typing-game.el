;;; typing-game.el --- a simple typing game in Emacs

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2015-11-03
;; Version: 0.1
;; Keywords: lisp, game 

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; typing-game's code can be found here:
;;   http://github.com/lujun9972/el-typing-game

;;; Commentary:

;; typing-game is a simple typing game in Emacs. You can customzie which characters to practice, how fast the characters failing and something else.

;;; Code:

(defgroup typing-game nil
  "a typing game in Emacs"
  :version "24.4"
  :group 'typing-game)

(defcustom typing-game-characters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  "characters to be practice")
(defcustom typing-game-characters-per-row 2
  "how many characters will be generated one row")
(defvar typing-game-total-scores 0)
(defcustom typing-game-scores-per-escaped-character -10
  "how many scores will be reduced when one character escaped")
(defcustom typing-game-scores-per-erased-character 1
  "how many scores will be increased when one character erased ")
(defcustom typing-game-default-level 3
  "the default level of typing game")
(defcustom typing-game-buffer "*typing-game*"
  "buffer name of typing-game")

(defvar typing-game-escaped-characters ""
  "characters that escaped in the game")

(defun typing-game//random-character ()
  "generate a random character"
  (elt typing-game-characters (random (length typing-game-characters))))

(defun typing-game//generate-characters (character-number)
  "返回新产生的字幕"
  (let* ((typing-game-width (window-body-width))
         (str (make-string typing-game-width ?\ )))
    (dotimes (var character-number)
      (setf (elt str (random typing-game-width)) (typing-game//random-character)))
    str))

(defun typing-game//change-scores (change)
  "change and refresh the total scores"
  (setq typing-game-total-scores (+ typing-game-total-scores change))
  (force-mode-line-update))

(defun typing-game//fix-screen ()
  "In window mode, keep screen from jumping by keeping window started at the beginning of buffer"
  (set-window-start (selected-window) (point-min))
  (goto-char (point-min)))

(defun typing-game/scroll-down ()
  "字幕下滚一行"
  (when (eq major-mode 'typing-game-mode) ;这样光标离开游戏窗口后,便不再继续
    (save-excursion
      (let ((inhibit-read-only t)
            (typing-game-height (window-body-height)))
        (goto-char (point-min))
        (insert (typing-game//generate-characters typing-game-characters-per-row))
        (newline)
        ;; 跳转到第N行
        (goto-char (point-min))
        (forward-line (- typing-game-height 1))
        (end-of-line)
        ;; 删除后面的内容
        (let* ((escaped-characters (replace-regexp-in-string "[ \r\n]" "" (buffer-substring-no-properties (point) (point-max))))
               (escaped-character-number (length escaped-characters)))
          (setq typing-game-escaped-characters (concat escaped-characters typing-game-escaped-characters))
          (typing-game//change-scores (* typing-game-scores-per-escaped-character escaped-character-number)))
        (delete-region (point) (point-max))))
    (typing-game//fix-screen)))

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
        (typing-game//change-scores  typing-game-scores-per-erased-character)
        (replace-match " " nil t))))
  (typing-game//fix-screen))


(defcustom typing-game-format
  `((:eval (format "[SCORES:%s]" typing-game-total-scores)))
  "format for displaying the total scores in the mode line"
  :group 'typing-game)


(define-derived-mode typing-game-mode text-mode "typing-game"
  "Major mode for running typing-game"
  (local-set-key  [remap self-insert-command] 'typing-game/erase)
  (make-local-variable 'global-mode-string)
  (setq global-mode-string (append global-mode-string `(,typing-game-format))))

(defvar typing-game-start-time (float-time)
  "when this game started")

(defun typing-game/init-game ()
  (switch-to-buffer (get-buffer-create typing-game-buffer))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (typing-game-mode)
  (read-only-mode)
  ;; (setq window-size-fixed t)
  (setq typing-game-total-scores 0)
  (setq typing-game-escaped-characters "")
  (setq typing-game-start-time (float-time)))

(defvar typing-game-timer nil)

(defun typing-game/start-game-at-speed (speed)
  "start the typing game. `speed' determied how fast the characters failing. "
  (interactive "P")
  (let ((speed (or speed typing-game-default-level)))
    (typing-game/init-game)
    (typing-game/cancel-game)
    (setq typing-game-timer (run-with-timer 0 (/ 5 speed) #'typing-game/scroll-down ))))

(defun typing-game/cancel-game ()
  (interactive)
  (when (timerp  typing-game-timer)
    (cancel-timer typing-game-timer)
    (setq typing-game-timer nil)))

(defun typing-game/stop-game ()
  (interactive)
  (typing-game/cancel-game)
  (typing-game/show-result))

(defun typing-game/show-result ()
  (let ((inhibit-read-only t))
    (with-current-buffer (get-buffer-create typing-game-buffer)
      (erase-buffer)
      (insert (user-login-name) ":")
      (newline)
      (insert (format "Total Scores: %d"  typing-game-total-scores))
      (newline)
      (insert (format "Escaped time (seconds): %d"  (- (float-time) typing-game-start-time)))
      (newline)
      (insert "Those characters are escaped: \n" typing-game-escaped-characters)
      (newline))))

(provide 'typing-game)

;;; typing-game.el ends here
