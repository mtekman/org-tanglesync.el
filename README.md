# org-tanglesync

<!-- [![MELPA](http://melpa.org/packages/terminal-toggle-badge.svg)](http://melpa.org/#/org-tanglesync.el) -->

Tangled blocks provide a nice way of exporting code into external files, acting as a fantastic agent to write literate dotfile configs. However, such dotfiles tend to be changed externally, sometimes for the worse and sometimes for the better. In the latter case it would be nice to be able to pull those external changes back into the original org src block it originated from.

Pulling external file changes back into a tangled org-babel src block is surprisingly not already an implemented feature, despite many people wanting this simple request.  This package tries to address that.

## Overview 

Any src block that has :tangle <fname> will compare the block with the external <fname> it is tangled to.  When a diff is detected, 1 of 4 actions can occur:
   1. `external` - The <fname> contents will override the block contents
   2. `internal` - The block will retain the block contents
   3. `prompt` - The user will be prompted to pull or reject external changes
   4. `diff` - A diff of the <fname> and block contents will be produced
   5. `custom` - A custom user-defined function will be called instead

These 5 options can be set as the default action by changing the `org-tanglesync-default-diff-action` custom parameter.  Otherwise individual block actions can be set in the org src block header e.g. `:diff external` for pulling external changes without prompt into a specific block.

This package also provides a hook when the org src block is being edited (e.g. via `C-c '`) which asks the user if they want to pull external changes if a difference is detected.

The user can bypass this and always pull by setting the `org-tanglesync-skip-user-check` custom parameter.

## Installation

```elisp
(use-package org-tanglesync
    :bind
    (( "C-c p i" . org-tanglesync-process-entire-buffer-interactive)
     ( "C-c p a" . org-tanglesync-process-entire-buffer-automatic)))
```

## Usage

The whole org file can be parsed using one of the two commands above, performing actions on any org src block with a tangle property. If the block also has a `:diff` property then the action associated with that diff will be used (see above).

Otherwise the changes can act at an individual block label whenever the user enters the org-src-edit-code mode via the default `C-c '` binding.
