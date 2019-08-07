;;; org-treescope.el --- Time scoping sparse trees within org -*- lexical-binding: t; -*-

;; Copright (C) 2019 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-treescope.el
;; Keywords: outlines
;; Package-Requires: ((org "9.2.3"))
;; Version: 0.1

;;; Commentary:

;; Navigating through an org file to see what needs to be done
;; this week and what was completed last month can be tricky.
;; This tool provides a time window to analyse your org file.

;;; Code:
(require 'org)
(require 'diff)

(deftype actions-on-diff ()
  '(:external  ;; always overwrites with external
    :prompt    ;; prompts the user to overwrite
    :ediff     ;; performs an ediff between two buffers
    :custom    ;; performs a user action between buffers
    ))

(defcustom diff-action :prompt
  "Which default action to perform when a diff is detected between
   an internal block and the external file it is tangled to.
   This is overridden by the ':diff <action>' header on the block.")

(defcustom custom-diff-action nil
  "A user passed function for action on the internal and external
   buffer. Only takes effect when :custom is set") 


(defun get-blockbody-buffer (block-info)
  "Extract the body of the code block to compare
   with the external file later"
  (let ((buff (get-buffer-create "*block-info*")))
    (with-current-buffer buff
      (insert (car (cdr block-info))))
    buff))

(defun get-header-property (keyw block-info)
  "Extract a specific keyword property from a header"
  (let* ((heads (car (cdr (cdr block-info))))
         (kvalue (assoc keyw heads)))
    (cdr kvalue)))

(defun get-diffaction (block-info)
  "Extract the diff action if present on the block
   otherwise use the default `diff-action`"
  (let* ((action (get-header-property :diff block-info)))
    action))

(defun get-tangledfile (block-info)
  "Extract tangled info from block-data and
   returns either nil or the file"
  (let* ((tfile (get-header-property :tangle block-info)))
    (unless (string-equal tfile "no") tfile)))

(defun get-filedata-buffer (file)
  "Pulls the latest content from external file into a temp buffer"
  (let ((buff (get-buffer-create "*filedata*")))
    (with-current-buffer buff
      (insert-file-contents file))
    buff))

(defun domain-getallsrcblocks ()
  ;;(while
  (org-babel-next-src-block)
  (process-current-block))

(defun has-diff (internal external)
  (let ((bname (get-buffer-create "*Diff-file-against-block*"))
        (found-diff nil))
    (progn (diff-no-select internal external nil t bname)
           (let ((lastline (get-diffline bname)))
             (setq found-diff (not (string-match "no differences" lastline)))))
    (let ((buffer-modified-p nil))
      (kill-buffer bname))
    found-diff))

(defun get-diffline (diff-buffer)
  "Extracts the relevant diff line to parse"
  (with-current-buffer diff-buffer
    (save-excursion)
    (let ((point1 nil) (point2 nil)
          (marker "Diff finished"))
      (progn (search-forward marker)
             (setq point1 (- (point) (length marker)))
             (goto-char (line-end-position))
             (setq point2 (point))
             (string-trim (buffer-substring-no-properties point1 point2))))))

(defun process-current-block ()
  "Performs necessary actions on the block under cursor"
  (org-babel-goto-src-block-head)
  (let* ((block-info (org-babel-get-src-block-info))
         (tfile (get-tangledfile block-info)))
    (unless tfile
      (let ((buffer-external (get-filedata-buffer tfile))
            (buffer-internal (get-blockbody-buffer block-info)))
        (when (has-diff buffer-internal buffer-external)
          (perform-action buffer-internal buffer-external))))))

(defun perform-action (internal external)
  "Handle the diffs found between INTERNAL and EXTERNAL
   using either action specified in header or falling back
   to default package action"
  (let ((action-block (get-diffaction))
        (action-final diff-action)
        (method-final nil))
    (when action-block
      (setq action-final action-block))
    (cond
     ((eq action-block :prompt) (setq method-final 'perform-userask-overwrite))
     ((eq action-block :external (setq method-final 'perform-overwrite)))
     ((eq action-block :ediff (setq method-final 'perform-ediff)))
     ((eq action-block :custom (setq method-final 'perform-custom))))
    (method-final internal external)))

(defun perform-overwrite (internal external)
  "Overwrites the current code block")

(defun perform-userask-overwrite (internal external)
  "Asks user to overwrite, otherwise skips"
  (when (y-or-n-p "Block has changed externally. Pull changes? ")
      (org- external)))


  

(defun action-on-src-edit ()
  "This hooks into the org-babel-edit-src hook")
