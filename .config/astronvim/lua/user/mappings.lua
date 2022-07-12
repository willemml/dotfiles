-- more in/all objects between 2 characters
local map = vim.keymap.set
for _, mode in ipairs { "n", "v" } do
  map(mode, "n", "e")
  map(mode, "e", "j")
  map(mode, "i", "j")
  map(mode, "u", "k")
  map(mode, "S-n", "0")
  map(mode, "S-i", "$")
  map(mode, "h", "<PageUp>")
  map(mode, "o", "<PageDown>")
end

return {
  v = {
    -- colemak movement
    ["n"] = { "h", desc = "Move left" },
    ["e"] = { "j", desc = "Move down" },
    ["i"] = { "l", desc = "Move right" },
    ["u"] = { "k", desc = "Move up" },
    ["N"] = { "0", desc = "Go to start of line" },
    ["I"] = { "$", desc = "Go to end of line" },
  },
  n = {
    -- disable default resize bindings
    ["<C-Down>"] = false,
    ["<C-Left>"] = false,
    ["<C-Right>"] = false,
    ["<C-Up>"] = false,
    -- resize with arrows
    ["<Up>"] = { function() require("smart-splits").resize_up(2) end, desc = "Resize split up" },
    ["<Down>"] = { function() require("smart-splits").resize_down(2) end, desc = "Resize split down" },
    ["<Left>"] = { function() require("smart-splits").resize_left(2) end, desc = "Resize split left" },
    ["<Right>"] = { function() require("smart-splits").resize_right(2) end, desc = "Resize split right" },
    -- colemak
    ["K"] = { "I", desc = "Insert" },
    ["k"] = { "i", desc = "Insert" },
    ["l"] = { ":undo<cr>", desc = "Undo" },
    ["n"] = { "h", desc = "Move left" },
    ["e"] = { "j", desc = "Move down" },
    ["i"] = { "l", desc = "Move right" },
    ["u"] = { "k", desc = "Move up" },
    ["N"] = { "0", desc = "Go to start of line" },
    ["I"] = { "$", desc = "Go to end of line" },
    -- disable default buffer bindings
    ["<S-l>"] = false,
    ["<S-h>"] = false,
    [">b"] = false,
    ["<b"] = false,
    -- buffers
    ["<leader>bi"] = { "<cmd>BufferLineCycleNext<cr>", desc = "Next buffer tab" },
    ["<leader>bn"] = { "<cmd>BufferLineCyclePrev<cr>", desc = "Previous buffer tab" },
    ["<leader>bI"] = { "<cmd>BufferLineMoveNext<cr>", desc = "Move buffer tab right" },
    ["<leader>bN"] = { "<cmd>BufferLineMovePrev<cr>", desc = "Move buffer tab left" },
    -- disable default move window bindings
    ["C-h"] = false,
    ["C-j"] = false,
    ["C-k"] = false,
    ["C-l"] = false,
    -- window navigation
    ["<C-n>"] = { function() require("smart-splits").move_cursor_left() end, desc = "Move to left split" },
    ["<C-e>"] = { function() require("smart-splits").move_cursor_down() end, desc = "Move to below split" },
    ["<C-u>"] = { function() require("smart-splits").move_cursor_up() end, desc = "Move to above split" },
    ["<C-i>"] = { function() require("smart-splits").move_cursor_right() end, desc = "Move to right split" },
    -- splits
    ["<leader>ah"] = { "<cmd>split<cr>", desc = "Split window horizontally" },
    ["<leader>av"] = { "<cmd>vsplit<cr>", desc = "Split window vertically" },
 },
  t = {
    -- ["<esc>"] = false,
  },
}
