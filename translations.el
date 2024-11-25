;; Translate text in emacs
;; Copyright 2024 Matvii Jarosh
;;
;; This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
;; 
;; You should have received a copy of the GNU Lesser General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;; Matvii Jarosh <matviijarosh@gmail.com>

(require 'json)
(require 'url)

(defun translate-text (from_lang to_lang str)
  "Function from translate text"
  (interactive)
  (let* ((url-request-method "GET")
	 ;; IF SERVER NOT WORK CHANGE https://lingva-translate-eta.vercel.app TO OTHER LINGVA SERVER
         (url (format "https://lingva-translate-eta.vercel.app/api/v1/%s/%s/%s"
                      from_lang
                      to_lang
                      (url-hexify-string (replace-regexp-in-string " " "+" str)))) 
         (buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json (json-read-from-string (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8)))
             (translation (gethash "translation" json)))
        (kill-buffer buffer)
	;; change '+' to spaces
        (replace-regexp-in-string "+" " " translation)))))

(defun get-text-between-symboles (from-lan to-lan)
  "get text from comments"
  (save-excursion
    ;; get block comments
    (let* ((start-comment-str "/*")
           (end-comment-str "*/")
           (start-comment 0)
           (end-comment 0)
           (comment-str ""))
      (goto-char (point-min))
      (catch 'loop
        (while t
          (unless (search-forward start-comment-str nil t)
            (throw 'loop nil))
          (setq start-comment (point))                    
          (search-forward end-comment-str) 
          (setq end-comment (- (point) (length end-comment-str)))
          (unless (setq comment-str (delete-and-extract-region start-comment end-comment))
            (throw 'loop nil))                    
          (goto-char start-comment)
          (insert (translate-text from-lan to-lan comment-str)))))
    ;; get line comments
    (let* ((start-comment-str (list "//" "##" ";;"))
           (current-start-comment-str "")
           (start-comment 0)
           (end-comment 0)
           (comment-str "")
           (i 0))       
      (while (< i (length start-comment-str))
        (setq current-start-comment-str (nth i start-comment-str))
        (message current-start-comment-str)
        (goto-char (point-min))
        (catch 'loop
          (while t
            (unless (search-forward current-start-comment-str nil t)
              (throw 'loop nil))
            (setq start-comment (point))   
            (setq end-comment (line-end-position))
            (unless (setq comment-str (delete-and-extract-region start-comment end-comment))
              (throw 'loop nil))                        
            (goto-char start-comment)
            (insert (translate-text from-lan to-lan comment-str))))
        (setq i (+ i 1))))))

(defun tr-lan2lan (from-lan to-lan)
  (let ((source-line (read-string "Enter: " nil))
        (tr-line ""))
    (message (translate-text from-lan to-lan source-line))))

;;(defun tr-en2ru () (interactive) (tr-lan2lan "en" "ru"))
;;(defun tr-f-en2ru () (interactive) (get-text-between-symboles "en" "ru"))

;; translate text from user
(defun tr-en2ru () (interactive) (tr-lan2lan "en" "ru"))
(defun tr-en2fr () (interactive) (tr-lan2lan "en" "fr"))
(defun tr-en2de () (interactive) (tr-lan2lan "en" "de"))
(defun tr-en2uk () (interactive) (tr-lan2lan "en" "uk"))
(defun tr-en2es () (interactive) (tr-lan2lan "en" "es"))
(defun tr-en2it () (interactive) (tr-lan2lan "en" "it"))
(defun tr-en2pt () (interactive) (tr-lan2lan "en" "pt"))

(defun tr-ru2en () (interactive) (tr-lan2lan "ru" "en"))
(defun tr-ru2fr () (interactive) (tr-lan2lan "ru" "fr"))
(defun tr-ru2de () (interactive) (tr-lan2lan "ru" "de"))
(defun tr-ru2uk () (interactive) (tr-lan2lan "ru" "uk"))
(defun tr-ru2es () (interactive) (tr-lan2lan "ru" "es"))
(defun tr-ru2it () (interactive) (tr-lan2lan "ru" "it"))
(defun tr-ru2pt () (interactive) (tr-lan2lan "ru" "pt"))

(defun tr-fr2en () (interactive) (tr-lan2lan "fr" "en"))
(defun tr-fr2ru () (interactive) (tr-lan2lan "fr" "ru"))
(defun tr-fr2de () (interactive) (tr-lan2lan "fr" "de"))
(defun tr-fr2uk () (interactive) (tr-lan2lan "fr" "uk"))
(defun tr-fr2es () (interactive) (tr-lan2lan "fr" "es"))
(defun tr-fr2it () (interactive) (tr-lan2lan "fr" "it"))
(defun tr-fr2pt () (interactive) (tr-lan2lan "fr" "pt"))

(defun tr-de2en () (interactive) (tr-lan2lan "de" "en"))
(defun tr-de2ru () (interactive) (tr-lan2lan "de" "ru"))
(defun tr-de2fr () (interactive) (tr-lan2lan "de" "fr"))
(defun tr-de2uk () (interactive) (tr-lan2lan "de" "uk"))
(defun tr-de2es () (interactive) (tr-lan2lan "de" "es"))
(defun tr-de2it () (interactive) (tr-lan2lan "de" "it"))
(defun tr-de2pt () (interactive) (tr-lan2lan "de" "pt"))

(defun tr-uk2en () (interactive) (tr-lan2lan "uk" "en"))
(defun tr-uk2ru () (interactive) (tr-lan2lan "uk" "ru"))
(defun tr-uk2fr () (interactive) (tr-lan2lan "uk" "fr"))
(defun tr-uk2de () (interactive) (tr-lan2lan "uk" "de"))
(defun tr-uk2es () (interactive) (tr-lan2lan "uk" "es"))
(defun tr-uk2it () (interactive) (tr-lan2lan "uk" "it"))
(defun tr-uk2pt () (interactive) (tr-lan2lan "uk" "pt"))

(defun tr-es2en () (interactive) (tr-lan2lan "es" "en"))
(defun tr-es2ru () (interactive) (tr-lan2lan "es" "ru"))
(defun tr-es2fr () (interactive) (tr-lan2lan "es" "fr"))
(defun tr-es2de () (interactive) (tr-lan2lan "es" "de"))
(defun tr-es2uk () (interactive) (tr-lan2lan "es" "uk"))
(defun tr-es2it () (interactive) (tr-lan2lan "es" "it"))
(defun tr-es2pt () (interactive) (tr-lan2lan "es" "pt"))

(defun tr-it2en () (interactive) (tr-lan2lan "it" "en"))
(defun tr-it2ru () (interactive) (tr-lan2lan "it" "ru"))
(defun tr-it2fr () (interactive) (tr-lan2lan "it" "fr"))
(defun tr-it2de () (interactive) (tr-lan2lan "it" "de"))
(defun tr-it2uk () (interactive) (tr-lan2lan "it" "uk"))
(defun tr-it2es () (interactive) (tr-lan2lan "it" "es"))
(defun tr-it2pt () (interactive) (tr-lan2lan "it" "pt"))

(defun tr-pt2en () (interactive) (tr-lan2lan "pt" "en"))
(defun tr-pt2ru () (interactive) (tr-lan2lan "pt" "ru"))
(defun tr-pt2fr () (interactive) (tr-lan2lan "pt" "fr"))
(defun tr-pt2de () (interactive) (tr-lan2lan "pt" "de"))
(defun tr-pt2uk () (interactive) (tr-lan2lan "pt" "uk"))
(defun tr-pt2es () (interactive) (tr-lan2lan "pt" "es"))
(defun tr-pt2it () (interactive) (tr-lan2lan "pt" "it"))

;; translate files
(defun tr-f-en2ru () (interactive) (get-text-between-symboles "en" "ru"))
(defun tr-f-en2fr () (interactive) (get-text-between-symboles "en" "fr"))
(defun tr-f-en2de () (interactive) (get-text-between-symboles "en" "de"))
(defun tr-f-en2uk () (interactive) (get-text-between-symboles "en" "uk"))
(defun tr-f-en2es () (interactive) (get-text-between-symboles "en" "es"))
(defun tr-f-en2it () (interactive) (get-text-between-symboles "en" "it"))
(defun tr-f-en2pt () (interactive) (get-text-between-symboles "en" "pt"))

(defun tr-f-ru2en () (interactive) (get-text-between-symboles "ru" "en"))
(defun tr-f-ru2fr () (interactive) (get-text-between-symboles "ru" "fr"))
(defun tr-f-ru2de () (interactive) (get-text-between-symboles "ru" "de"))
(defun tr-f-ru2uk () (interactive) (get-text-between-symboles "ru" "uk"))
(defun tr-f-ru2es () (interactive) (get-text-between-symboles "ru" "es"))
(defun tr-f-ru2it () (interactive) (get-text-between-symboles "ru" "it"))
(defun tr-f-ru2pt () (interactive) (get-text-between-symboles "ru" "pt"))

(defun tr-f-fr2en () (interactive) (get-text-between-symboles "fr" "en"))
(defun tr-f-fr2ru () (interactive) (get-text-between-symboles "fr" "ru"))
(defun tr-f-fr2de () (interactive) (get-text-between-symboles "fr" "de"))
(defun tr-f-fr2uk () (interactive) (get-text-between-symboles "fr" "uk"))
(defun tr-f-fr2es () (interactive) (get-text-between-symboles "fr" "es"))
(defun tr-f-fr2it () (interactive) (get-text-between-symboles "fr" "it"))
(defun tr-f-fr2pt () (interactive) (get-text-between-symboles "fr" "pt"))

(defun tr-f-de2en () (interactive) (get-text-between-symboles "de" "en"))
(defun tr-f-de2ru () (interactive) (get-text-between-symboles "de" "ru"))
(defun tr-f-de2fr () (interactive) (get-text-between-symboles "de" "fr"))
(defun tr-f-de2uk () (interactive) (get-text-between-symboles "de" "uk"))
(defun tr-f-de2es () (interactive) (get-text-between-symboles "de" "es"))
(defun tr-f-de2it () (interactive) (get-text-between-symboles "de" "it"))
(defun tr-f-de2pt () (interactive) (get-text-between-symboles "de" "pt"))

(defun tr-f-uk2en () (interactive) (get-text-between-symboles "uk" "en"))
(defun tr-f-uk2ru () (interactive) (get-text-between-symboles "uk" "ru"))
(defun tr-f-uk2fr () (interactive) (get-text-between-symboles "uk" "fr"))
(defun tr-f-uk2de () (interactive) (get-text-between-symboles "uk" "de"))
(defun tr-f-uk2es () (interactive) (get-text-between-symboles "uk" "es"))
(defun tr-f-uk2it () (interactive) (get-text-between-symboles "uk" "it"))
(defun tr-f-uk2pt () (interactive) (get-text-between-symboles "uk" "pt"))

(defun tr-f-es2en () (interactive) (get-text-between-symboles "es" "en"))
(defun tr-f-es2ru () (interactive) (get-text-between-symboles "es" "ru"))
(defun tr-f-es2fr () (interactive) (get-text-between-symboles "es" "fr"))
(defun tr-f-es2de () (interactive) (get-text-between-symboles "es" "de"))
(defun tr-f-es2uk () (interactive) (get-text-between-symboles "es" "uk"))
(defun tr-f-es2it () (interactive) (get-text-between-symboles "es" "it"))
(defun tr-f-es2pt () (interactive) (get-text-between-symboles "es" "pt"))

(defun tr-f-it2en () (interactive) (get-text-between-symboles "it" "en"))
(defun tr-f-it2ru () (interactive) (get-text-between-symboles "it" "ru"))
(defun tr-f-it2fr () (interactive) (get-text-between-symboles "it" "fr"))
(defun tr-f-it2de () (interactive) (get-text-between-symboles "it" "de"))
(defun tr-f-it2uk () (interactive) (get-text-between-symboles "it" "uk"))
(defun tr-f-it2es () (interactive) (get-text-between-symboles "it" "es"))
(defun tr-f-it2pt () (interactive) (get-text-between-symboles "it" "pt"))

(defun tr-f-pt2en () (interactive) (get-text-between-symboles "pt" "en"))
(defun tr-f-pt2ru () (interactive) (get-text-between-symboles "pt" "ru"))
(defun tr-f-pt2fr () (interactive) (get-text-between-symboles "pt" "fr"))
(defun tr-f-pt2de () (interactive) (get-text-between-symboles "pt" "de"))
(defun tr-f-pt2uk () (interactive) (get-text-between-symboles "pt" "uk"))
(defun tr-f-pt2es () (interactive) (get-text-between-symboles "pt" "es"))
(defun tr-f-pt2it () (interactive) (get-text-between-symboles "pt" "it"))
