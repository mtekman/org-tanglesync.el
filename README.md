# org-tanglesync

[![MELPA](https://melpa.org/packages/org-tanglesync-badge.svg)](https://melpa.org/#/org-tanglesync)

Tangled blocks provide a nice way of exporting code into external files, acting as a fantastic agent to write literate dotfile configs. However, such dotfiles tend to be changed externally, sometimes for the worse and sometimes for the better. In the latter case it would be nice to be able to pull those external changes back into the original org src block it originated from.

## Overview

This package offers two complementary methods for syncing changes between a tangled block and the source org file it is tangled from: [Push Changes From External Tangled File](#push-changes-from-external-tangled-file), and [Pull Changes From Org Buffer](#pull-changes-from-org-buffer).

Both methods are illustrated with example of an org file `"mydotfiles.conf"` which contains literate dotfile configurations for various user config files, of which one of them is `~/.xinitrc` as given by the block starting with `+begin_src bash :tangle ~/.xinitrc`.


## Installation

```elisp
(use-package org-tanglesync
    :hook ((org-mode . org-tanglesync-mode)
           (org-mode . org-tanglesync-watch-mode))
    :custom
    (org-tanglesync-watch-files '("conf.org" "otherconf.org"))
    :bind
    (( "C-c M-i" . org-tanglesync-process-buffer-interactive)
     ( "C-c M-a" . org-tanglesync-process-buffer-automatic)))
```

## Usage

The whole org file can be parsed using one of the two commands above, performing actions on any org src block with a tangle property. If the block also has a `:diff` property then the action associated with that diff will be used (see above).

Otherwise the changes can act at an individual block label whenever the user enters the org-src-edit-code mode via the default `C-c '` binding.

When watch mode is activated, the user only needs to specify a list of org files to watch, and can use emacs normally with the knowledge that tangled files are synced automatically back to the correct org file.


## Syncing Modes

### Push Changes From External Tangled File

**(i.e. "watch" mode)**

*Left - the org buffer with the tangled file, Right - the external .xinitrc file*

![screen2](https://user-images.githubusercontent.com/20641402/71929804-b59e5a80-319a-11ea-83d5-20f4343f08ea.gif)

This is the use-case where you have `~/.xinitrc` open and want to automatically sync your changes back to `conf.org` every time you save the `~/.xinitrc` file (i.e. you don't want to open `conf.org` to sync). This is often the much more preferred use-case for many.

#### Configuration

The user needs to only set the `org-tanglesync-watch-files` to a list of org files which have tangled blocks that need to be 'watched', and enable `org-tanglesync-watch-mode` globally.

e.g. `(setq org-tanglesync-watch-files '("conf.org" "someotherdotfile.org"))` `


#### Execution

Since `conf.org` contains the block `~/.xinitrc`, whenever `~/.xinitrc` is modified and saved within emacs, the changes are automatically synced back to `conf.org`.

i.e. The user only needs to perform a save on `~/.xinitrc`

By default, all tangled blocks specified in `org-tanglesync-watch-files` are watched and updated upon saving, however this can be overridden at the individual block level by adding `:nowatch` to the header.


### Pull Changes From Org Buffer

*Left - process entire buffer, Right - hook on edit buffer*

![screen](https://user-images.githubusercontent.com/20641402/63469413-7335e480-c46a-11e9-8a00-1825676f3b2d.gif)

This is the use-case where you have opened `conf.org` containing - among many other tangled source blocks - a block with `:tangle ~/.xinitrc` as a header. You wish to sync changes either by exporting that block to the `~/.xinitrc` file, or by pulling the changes you have made externally to that file back into the block within your `conf.org`.

The above actions can be performed either globally or at the individual block level depending on your configuration.

#### Configuration

Any src block that has `:tangle <fname>` will compare the block with the external `<fname>` it is tangled to.  When a diff is detected, 1 of 5 different actions can occur:
   1. `prompt` - (*default*) The user will be prompted to pull or reject external changes
   1. `external` - The `<fname>` contents will override the block contents
   1. `internal` - The block will retain the block contents
   1. `diff` - A diff of the `<fname>` and block contents will be produced
   1. `custom` - A custom user-defined function will be called instead

These 5 options can be set as the default action by changing the `org-tanglesync-default-diff-action` custom parameter.  Otherwise individual block actions can be set in the org src block header e.g. `:diff external` for pulling external changes without prompt into a specific block. The default action is to simply prompt the user.

#### Execution

The user can either call `org-tanglesync-process-buffer-interactive` or `org-tanglesync-process-buffer-automatic` to interactively/automatically process all tangled blocks in the org buffer. This will go through each tangled block from top to bottom, pausing to prompt the user for an action if one has not been defined in its header.

If the user wishes to sync only a single block, a much easier way is to simply edit the block in the org-src mode (activated via `C-c '`). This executes a hook which prompts the user for an action on that specific block if a difference is detected. The user can bypass this and always pull changes by setting the `org-tanglesync-skip-user-check` custom parameter.

