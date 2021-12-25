;;; org-ql-export.el --- Export QL results into anything -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Mikhail Skorzhisnkii
;;
;; Author: Mikhail Skorzhisnkii <https://github.com/mskorzhinskii>
;; Maintainer: Mikhail Skorzhisnkii <mskorzhinskiy@eml.cc>>
;; Created: December 24, 2021
;; Modified: December 24, 2021
;; Version: 0.0.1
;; Keywords: outlines convenience
;; Homepage: https://github.com/mskorzhinskiy/org-ql-export
;; Package-Requires: ((emacs "27.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org)
(require 'org-ql)

(defcustom org-ql-export-settings
  '((:title "Test"
     :file "~/test.org"
     :full-path t
     :tree t
     :view (tags-local "test")))
  "Settings for the exporting QL views.

title: title of the file (i.e. #+title: at the top)
file: absolute path to the file
full-path: if true every entry will have be saved with the full path
tree: if true every entry will be saved in with all parents
view: org-ql view"
  :type 'plist
  :group 'org-ql)

(defun org-ql-export--insert-title (title)
  "Insert TITLE under the point."
  (insert (concat "#+TITLE: " title))
  (insert "\n")
  (insert (concat "#+DATE:  " (ts-format (ts-now))))
  (insert "\n"))

(defun org-ql-export--get-title (file)
  "Return `org-mode' FILE value of #+title."
  (let (title)
    (when file
      (with-current-buffer
          (get-file-buffer file)
        (pcase (org-collect-keywords '("TITLE"))
          (`(("TITLE" . ,val))
           (setq title (car val)))))
      title)))

(defun org-ql-export--get-full-path ()
  "Return full path of the `org-mode' outline under the point."
  (let ((heading (org-format-outline-path (org-get-outline-path t nil)))
        (title (org-ql-export--get-title (buffer-file-name))))
    (if title
        (format "%s/%s" title heading)
      heading)))

;; * Function for exporting
(defun org-ql-export-to-file (&rest settings)
  "Save the QL view into the file based on the SETTINGS."
  (let ((title (plist-get settings :title))
        (file (plist-get settings :file))
        (tree (plist-get settings :tree))
        (full-path (plist-get settings :full-path))
        (view (or (plist-get settings :view)
                  (error "No view have been provided!"))))
    (find-file file)
    (erase-buffer)
    (org-ql-export--insert-title title)
    (let ((headlines (org-ql-select (org-agenda-files) view
                       :action #'element-with-markers)))
      (dolist (element headlines)
        (let* ((marker (plist-get (cadr element) :org-marker))
               (buffer (marker-buffer marker))
               (pos (marker-position marker))
               full-path-str)
          ;; Go to source heading and copy it
          (with-current-buffer buffer
            (goto-char pos)
            (setq full-path-str (org-ql-export--get-full-path))
            (if tree
                (org-copy-subtree)
              (org-copy-subtree 0 nil nil t)))
          ;; Put things to buffer
          (save-excursion
            ;; Insert new top-level heading
            (goto-char (point-max))
            (org-next-visible-heading 1)
            (org-insert-heading nil nil t)
            ;; Past things
            (org-paste-subtree)
            ;; Edit headline for full path
            (when full-path
              (org-edit-headline full-path-str))
            ;; Attach linked tasks
            (let* ((id (org-id-get))
                   (linked (when id
                             (org-ql-select (org-agenda-files)
                               `(property "LINKED" ,id)
                               :action #'org-ql-export--get-full-path))))
              (when (and id linked)
                (save-restriction
                  (org-narrow-to-element)
                  (goto-char (point-max))
                  (insert "\n")
                  (insert "Linked tasks:\n")
                  (dolist (task linked)
                    (insert (format "- [ ] %s\n" task))))))))))
    (save-buffer)
    (kill-buffer)))

(defun org-ql-export ()
  "Export `org-ql' views according to `org-ql-export-settings'."
  (cl-loop for export-setting in org-ql-export-settings
           do (org-ql-export-to-file
               :title (plist-get export-setting :title)
               :file (plist-get export-setting :file)
               :tree (plist-get export-setting :tree)
               :full-path (plist-get export-setting :full-path)
               :view (plist-get export-setting :view))))

(provide 'org-ql-export)
;;; org-ql-export.el ends here
