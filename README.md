# org-tanglesync

[![MELPA](https://melpa.org/packages/org-tanglesync-badge.svg)](https://melpa.org/#/org-tanglesync)

Tangled blocks provide a nice way of exporting code into external files, acting as a fantastic agent to write literate dotfile configs. However, such dotfiles tend to be changed externally, sometimes for the worse and sometimes for the better. In the latter case it would be nice to be able to pull those external changes back into the original org src block it originated from.


#### Live Demo
*Left - process entire buffer, Right - hook on edit buffer*

![screen](https://user-images.githubusercontent.com/20641402/63469413-7335e480-c46a-11e9-8a00-1825676f3b2d.gif)


## Overview 

Any src block that has `:tangle <fname>` will compare the block with the external `<fname>` it is tangled to.  When a diff is detected, 1 of 5 different actions can occur:
   1. `external` - The `<fname>` contents will override the block contents
   2. `internal` - The block will retain the block contents
   3. `prompt` - The user will be prompted to pull or reject external changes
   4. `diff` - A diff of the `<fname>` and block contents will be produced
   5. `custom` - A custom user-defined function will be called instead

These 5 options can be set as the default action by changing the `org-tanglesync-default-diff-action` custom parameter.  Otherwise individual block actions can be set in the org src block header e.g. `:diff external` for pulling external changes without prompt into a specific block.

This package also provides a hook when the org src block is being edited (e.g. via `C-c '`) which asks the user if they want to pull external changes if a difference is detected.

The user can bypass this and always pull by setting the `org-tanglesync-skip-user-check` custom parameter.

## Installation

```elisp
(use-package org-tanglesync
  :hook org-mode
  :bind (("C-c t" . org-tanglesync-minor-mode)))
```

## Usage

The whole org file can be parsed using one of the two commands above, performing actions on any org src block with a tangle property. If the block also has a `:diff` property then the action associated with that diff will be used (see above).

Otherwise the changes can act at an individual block label whenever the user enters the org-src-edit-code mode via the default `C-c '` binding.

## Commentary

A good literate dotfile config should ideally make use of `:comments link` and `:tangle-mode (identity #o444)` in the block headers (or the top global header via `#+PROPERTY:   header-args+ :tangle-mode (identity #o444)`) to ensure that the tangled file is not writable, and that it provides links back to the source file that the user can follow.

However the above - though being the correct method to maintain robust configs - is not faster or more preferable than editing the file directly to see the effects immediately. This package enables the external file changes to be pulled back into the org buffer the next time it is open.



