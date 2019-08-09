;;; org-tanglesync.el --- Pulling external changes from tangled blocks -*- lexical-binding: t; -*-

;; Copright (C) 2019 Mehmet Tekman <mtekman89@gmail.com>

;; Author: Mehmet Tekman
;; URL: https://github.com/mtekman/org-tanglesync.el
;; Keywords: outlines
;; Package-Requires: ((org "9.2.3") (diff "?.?")
;; Version: 0.1

;;; Commentary:

;; Pulling external file changes to a tangled org-babel src block
;; is surprisingly not implemented. This addresses that.

;;; Code:
(require 'org)
(require 'diff)

(deftype actions-on-diff ()
  '((:external . "external")  ;; always overwrites with external
    (:prompt . "prompt")      ;; prompts the user to overwrite
    (:diff . "diff")          ;; performs a diff between two buffers
    (:custom . "custom")))    ;; performs a user action between buffers

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
  (get-header-property :diff block-info))

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
  (let* ((org-buffer (current-buffer))
         (block-info (org-babel-get-src-block-info))
         (tfile (get-tangledfile block-info)))
    (when tfile
      (let ((buffer-external (get-filedata-buffer tfile))
            (buffer-internal (get-blockbody-buffer block-info)))
        (when (has-diff buffer-internal buffer-external)
          (perform-action buffer-internal buffer-external org-buffer
                          (get-diffaction block-info)))))))

(defun perform-action (internal external org-buffer action-block)
  "Handle the diffs found between INTERNAL and EXTERNAL
   using either action specified in the ACTION-BLOCK header
    or falling back to default package action"
  (let ((do-action diff-action)) ;; default action from package
    (when action-block           ;; though, if header has action use that
      (setq do-action action-block))
    ;; At this point, the action is known. Just need to set the method
    (let ((method-do nil))
      (cond
       ((eq do-action :prompt) (fset 'method-do 'perform-userask-overwrite))
       ((eq do-action :external) (fset 'method-do 'perform-overwrite))
       ((eq do-action :diff) (fset 'method-do 'perform-diff))
       ((eq do-action :custom) (fset 'method-do 'perform-custom)))
      (progn (method-do internal external org-buffer)
             (kill-buffer internal)
             (kill-buffer external)))))

(add-hook 'org-src-mode-hook 'test-ctrl-c-hook)

(defun test-goto-block ()
    (with-current-buffer "conf.org"
      (org-babel-next-src-block 21)))

(defcustom skip-user-check nil
  "Just pull changes from external if different")

(defun test-ctrl-c-hook ()
  ;; Get parent pointer
  (let* ((mark (org-src-do-at-code-block))
         (org-buffer (marker-buffer mark))
         (org-position (marker-position mark))
         (edit-buffer (current-buffer)))
    (with-current-buffer org-buffer
      (goto-char org-position)
      (let* ((tangle-fname (get-tangledfile (org-babel-get-src-block-info)))
             (file-buffer (get-filedata-buffer tangle-fname)))
        (when (has-diff edit-buffer org-buffer)
          (let ((pullchanges nil))
            (cond (skip-user-check
                   (progn (setq pullchanges t)
                          (message "Changes were detected and external loaded.")))
                  ((y-or-n-p "Change detected, load external? ") (setq pullchanges t)))
            (when pullchanges
              (with-current-buffer edit-buffer
                (progn (erase-buffer)
                       (insert-buffer file-buffer)
                       (kill-buffer file-buffer))))))))))

(defun perform-custom (internal external org-buffer)
  "Calls the custom user function if not nil"
  (unless custom-diff-action
    (custom-diff-action internal external org-buffer)))

(defun perform-diff (internal external org-buffer)
  "Literally calls diff on INTERNAL and EXTERNAL"
  (diff internal external))

(defun perform-overwrite (internal external org-buffer)
  "Overwrites the current code block"
  (let ((cut-beg nil) (cut-end nil))
    (with-current-buffer org-buffer
      (org-babel-goto-src-block-head)
      (search-forward "\n")
      (setq cut-beg (point))
      (search-forward "#+END_SRC")
      (goto-char (- (line-beginning-position) 1))
      (setq cut-end (point))
      ;; cut out the old text
      (delete-region cut-beg cut-end)
      ;; insert the new text
      (goto-char cut-beg)
      (insert-buffer external))))

(defun perform-userask-overwrite (internal external org-buffer)
  "Asks user to overwrite, otherwise skips"
  (when (y-or-n-p "Block has changed externally. Pull changes? ")
    (perform-overwrite internal external org-buffer)))


(defun action-on-src-edit ()
  "This hooks into the org-babel-edit-src hook")
