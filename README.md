### This repo has migrated to GitLab
https://gitlab.com/mtekman/org-tanglesync.el
###### (The copy here is archived. Please see the above link for the latest developments)

********

# org-tanglesync

[![MELPA](https://melpa.org/packages/org-tanglesync-badge.svg)](https://melpa.org/#/org-tanglesync)

Tangled blocks provide a nice way of exporting code into external files, acting as a fantastic agent to write literate dotfile configs. However, such dotfiles tend to be changed externally, sometimes for the worse and sometimes for the better. In the latter case it would be nice to be able to pull those external changes back into the original org src block it originated from.

## Installation

```elisp
(use-package org-tanglesync
  :hook ((org-mode . org-tanglesync-mode)
         ;; enable watch-mode globally:
         ((prog-mode text-mode) . org-tanglesync-watch-mode))
  :custom
  (org-tanglesync-watch-files '("conf.org" "myotherconf.org"))
  :bind
  (( "C-c M-i" . org-tanglesync-process-buffer-interactive)
   ( "C-c M-a" . org-tanglesync-process-buffer-automatic)))
```

## Overview

This package offers two complementary methods for syncing changes between a tangled block and the source org file it is tangled from:

 1. [From External File, Push to Tangled Block](#from-external-file-push-to-tangled-block)
 1. [From Org File, Pull External Changes Into Tangled Blocks](#from-org-file-pull-external-changes-into-tangled-blocks)
 

## From External File, Push to Tangled Block

![screen2](https://user-images.githubusercontent.com/20641402/72112550-2d58ba80-333e-11ea-9616-1d9d5e7df947.gif)  
*Left - the org buffer with the tangled file, Right - the external .xinitrc file*

This is the use-case where you have `~/.xinitrc` open and want to automatically sync your changes back to `conf.org` every time you save the `~/.xinitrc` file (i.e. you don't want to open `conf.org` to sync).

#### Configuration and Execution

The user needs to only set the `org-tanglesync-watch-files` to a list of org files which have tangled blocks that need to be 'watched', and enable `org-tanglesync-watch-mode` globally, as given in the [installation](#installation) example.

In the above screencast specifically, we have set:

```elisp
(setq org-tanglesync-watch-files '("conf.org"))
(org-tanglesync-watch-mode 1)
```

Here our `conf.org` contains the tangled block `~/.xinitrc`, which has the changes synced whenever the `~/.xinitrc` is modified and saved.

**Note:** By default, *all* tangled blocks specified in `org-tanglesync-watch-files` are watched and updated upon saving, however this can be overridden at the individual block level by adding `:nowatch` to the org block src header.


## From Org File, Pull External Changes Into Tangled Blocks

![screen](https://user-images.githubusercontent.com/20641402/63469413-7335e480-c46a-11e9-8a00-1825676f3b2d.gif)
*Left - Sync all blocks, Right - Sync just this block.*

This is the use-case where you have opened `conf.org` containing - among many other tangled source blocks - a block with `:tangle ~/.xinitrc` as a header. You wish to sync changes either by exporting that block to the `~/.xinitrc` file, or by pulling the changes you have made externally to that file back into the block within your `conf.org`.

The above actions can be performed either globally or at the individual block level depending on your configuration.

#### Configuration

Any src block that has `:tangle <fname>` will compare the block with the external `<fname>` it is tangled to.  When a diff is detected, 1 of 5 different `:diff <action>` actions can occur:
   1. `prompt` - (*default*) The user will be prompted to pull or reject external changes
   1. `external` - The `<fname>` contents will override the block contents
   1. `internal` - The block will retain the block contents
   1. `diff` - A diff of the `<fname>` and block contents will be produced
   1. `custom` - A custom user-defined function will be called instead

These 5 options can be set as the default action by changing the `org-tanglesync-default-diff-action` custom parameter.  Otherwise individual block actions can be set in the org src block header e.g. `:diff external` for pulling external changes without prompt into a specific block. The default action is to simply prompt the user.

#### Execution

##### Sync All Tangled Blocks in Buffer

The user can either call `org-tanglesync-process-buffer-interactive` or `org-tanglesync-process-buffer-automatic` to interactively/automatically process all tangled blocks in the org buffer. This will go through each tangled block from top to bottom, pausing to prompt the user for an action if one has not been defined in its header.

##### Sync Just One Block

If the user wishes to sync only a single block, a much easier way is to simply edit the block in the org-src mode (activated via `C-c '`). This executes a hook which prompts the user for an action on that specific block if a difference is detected. The user can bypass this and always pull changes by setting the `org-tanglesync-skip-user-check` custom parameter.

