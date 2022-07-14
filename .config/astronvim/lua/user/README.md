# nvim mappings cheatsheet

My nvim bindings (using [AstroNvim](https://astronvim.github.io/).)

## Motion (normal and visual mode)

Shortcut | Action
---|---
 `n` | Cursor left
 `i` | Cursor right
 `u` | Cursor up
 `e` | Cursor down
 `N` | Cursor to beginning of line
 `I` | Cursor to end of line
 `U` | Cursor up 5
 `E` | Cursor down 5
 `h` | Back one page
 `'` | Forward one page

## Insert mode

Shortcut | Action
---|---
 `k` | Insert
 `l` | Undo

## Visual mode

Shortcut | Action
---|---
 `<` | Unindent line
 `>` | Indent line
 `K` | Insert before
 `A` | Insert after

## NeoTree

Shortcut | Action
 `<leader>e` | Toggle NeoTree
 `<leader>o` | Focus NeoTree
 `?` | Show help
 `s` | Open in vertical split
 `H` | Toggle hidden
 `a` | Add node
 `d` | Delete node
 `/` | Fuzzy search

## Search (telescope)

Shortcut | Action
---|---
 `<leader>sk` | Search keymaps
 `<leader>sc` | Search commands
 `<leader>fb` | Search buffers
 `<leader>ff` | Search files (ignore hidden)
 `<leader>fF` | Search files (include hidden)
 `<leader>fw` | Grep files for word (ignore hidden)
 `<leader>fW` | Grep files for word (include hidden)
 `<leader>sh` | Search help
 `<leader>sm` | Search manpages
 `<leader>ls` | Search symbols
 `<leader>lR` | Search references

## Windows

Shortcut | Action
---|---
 `<Up>` | Resize window up
 `<Down>` | Resize window down
 `<Left>` | Resize window left
 `<Right>` | Resize window right
 `<C-n>` | Move to left window 
 `<C-i>` | Move to right window
 `<C-u>` | Move to above window
 `<C-e>` | Move to below split
 `<leader>av` | Split window vertically
 `<leader>ah` | Split window horizontally

## Buffers

Shortcut | Action
---|---
 `<leader>bi` | Cycle to next buffer tab
 `<leader>bn` | Cycle to previous buffer tab
 `<leader>bN` | Move buffer tab to the right
 `<leader>bI` | Move buffer tab to the left
 `<leader>bc` | Close current buffer

## Markdown

Shortcut | Action
---|---
 `<leader>mo` | Open preview
 `<leader>ms` | Stop preview
 `<leader>mt` | Toggle preview

## Github

Shortcut | Action
---|---
 `<leader>gp` | Search GitHub pull requests
 `<leader>gi` | Search GitHub issues

## Git

Shortcut | Action
---|---
 `<leader>gd` | View diff
 `<leader>ge` | Next hunk
 `<leader>gu` | Previous hunk
 `<leader>gp` | Preview hunk
 `<leader>gh` | Reset hunk
 `<leader>gs` | Stage hunk
 `<leader>gr` | Reset buffer
 `<leader>gl` | Unstage hunk
 `<leader>gk` | View blame
 `<leader>gt` | Status
 `<leader>gb` | Branches
 `<leader>gc` | Commits

 ## Terminal

 Shortcut | Action
 ---|---
  `<leader>gg` | Open lazygit term
  `<leader>tn` | Open node term
  `<leader>tu` | Open NCDU term
  `<leader>tt` | Open htop term
  `<leader>tp` | Open python term
  `<leader>tf` | Open floating terminal
  `<leader>th` | Open horizontal split terminal
  `<leader>tv` | Open vertical split terminal
