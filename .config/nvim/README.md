# NeoVim Config

My NeoVim configuration, heavily modified/reduced version of [theniceboy's config](https://github.com/theniceboy/nvim).

## Install

Clone this repo into `~/.config/nvim`:
```zsh
cd ~/.config/
git clone git@github.com:willemml/nvim
```

Install Node.JS:
```zsh
curl -sL install-node.now.sh/lts | bash
```

Install [Rust](https://www.rust-lang.org/tools/install) and [FZF](https://github.com/junegunn/fzf#installation), then install ripgrep:
```zsh
cargo install ripgrep
```

## Keyboard Shortcuts
### 1 Basic Editor Features
#### 1.1 The Most Basics
**`k`** : switchs to **`INSERT`** : mode (same as key `i` in vanilla vim)

**`Q`** : quits current vim window (same as command `:q` in vanilla vim)

**`S`** : saves the current file (same as command `:w` in vanilla vim)

**_IMPORTANT_**

  Since the `i` key has been mapped to `k`, every command (combination) that involves `i` should use `k` instead (for example, `ciw` should be `ckw`).

#### 1.2 Remapped Cursor Movement
| Shortcut   | Action                                                    | Equivalent |
|------------|-----------------------------------------------------------|------------|
| `u`        | Cursor up a terminal line                                 | `k`        |
| `e`        | Cursor down a terminal line                               | `j`        |
| `n`        | Cursor left                                               | `h`        |
| `i`        | Cursor right                                              | `l`        |
| `U`        | Cursor up 5 terminal lines                                | `5k`       |
| `E`        | Cursor down 5 terminal lines                              | `5j`       |
| `N`        | Cursor to the start of the line                           | `0`        |
| `I`        | Cursor to the end of the line                             | `$`        |
| `Ctrl` `u` | Move the view port up 5 lines without moving the cursor   | `Ctrl` `y` |
| `Ctrl` `e` | Move the view port down 5 lines without moving the cursor | `Ctrl` `e` |
| `h`        | Move to the end of this word                              | `e`        |
| `W`        | Move cursor five words forward                            | `5w`       |
| `B`        | Move cursor five words forward                            | `5b`       |

#### 1.3 Remapped Insert Mode Keys
| Shortcut   | Action                                                               |
|------------|----------------------------------------------------------------------|
| `Ctrl` `a` | Move cursor to the end of the line                                   |
| `Ctrl` `u` | Move the character on the right of the cursor to the end of the line |

#### 1.4 Other Useful Normal Mode Remappings
| Shortcut        | Action                                         |
|-----------------|------------------------------------------------|
| `SPACE` `s` `c` | Toggle spell suggestion a                      |
| `SPACE` `d` `w` | Find adjacent duplicated word                  |
| `SPACE` `t` `t` | Convert every 4 Spaces to a tab                |
| `SPACE` `o`     | Fold                                           |
| `SPACE` `-`     | Previous quick-fix position                    |
| `SPACE` `+`     | Next quick-fix position                        |
| `SPACE` `/`     | Create a new terminal below the current window |

#### 1.5 Remapped Commands in Visual Mode
| Shortcut        | Action                                 |
|-----------------|----------------------------------------|
| `Y`             | Copy selected text to system clipboard |


### 2 Window Management
#### 2.1 Creating Window Through Split Screen
| Shortcut    | Action                                                                      |
|-------------|-----------------------------------------------------------------------------|
| `s` `u`     | Create a new horizontal split screen and place it above the current window  |
| `s` `e`     | Create a new horizontal split screen and place it below the current window  |
| `s` `n`     | Create a new vertical split screen and place it left to the current window  |
| `s` `i`     | Create a new vertical split screen and place it right to the current window |
| `s` `v`     | Set the two splits to be vertical                                           |
| `s` `h`     | Set the two splits to be horizontal                                         |
| `s` `r` `v` | Rotate splits and arrange splits vertically                                 |
| `s` `r` `h` | Rotate splits and arrange splits horizontally                               |

#### 2.2 Moving the Cursor Between Different Windows
| Shortcut      | Action                         |
|---------------|--------------------------------|
| `SPACE` + `w` | Move cursor to the next window |
| `SPACE` + `n` | Move cursor one window left    |
| `SPACE` + `i` | Move cursor one window right   |
| `SPACE` + `u` | Move cursor one window up      |
| `SPACE` + `e` | Move cursor one window down    |

#### 2.3 Resizing Different Windows
Use the arrow keys to resize the current window.

#### 2.4 Closing Windows
| Shortcut    | Action                                                                                                     |
|-------------|------------------------------------------------------------------------------------------------------------|
| `Q`         | Close the current window                                                                                   |
| `SPACE` `q` | Close the window below the current window. (The current window will be closed if there is no window below) |

### 3 Tab Management
| Shortcut    | Action           |
|-------------|------------------|
| `t` `u`     | Create a new tab |
| `t` `n`     | Go one tab left  |
| `t` `i`     | Go One tab right |
| `t` `m` `n` | Move tab left    |
| `t` `m` `i` | Move tab right   |

### 4 Terminal Keyboard Shortcuts
| Shortcut    | Action                                                      |
|-------------|-------------------------------------------------------------|
| `Space` `\` | Open a terminal                                             |
| `Ctrl` `n`  | Escape from terminal input mode                             |

## Plugins Keybindings (Screenshots/GIF provided!)
### AutoCompletion
#### [COC (AutoCompletion)](https://github.com/neoclide/coc.nvim)
| Shortcut        | Action                    |
|-----------------|---------------------------|
| `Space` `y`     | **Get yank history list** |
| `gd`            | Go to definition          |
| `gr`            | List references           |
| `gi`            | List implementation       |
| `gy`            | Go to type definition     |
| `Space` `r` `n` | Rename a variable         |

### File Navigation
#### [coc-explorer (file browser)](https://github.com/weirongxu/coc-explorer)
| Shortcut | Action                  |
|----------|-------------------------|
| `tt`     | **Open file browser**   |
| `?`      | show help (in explorer) |
