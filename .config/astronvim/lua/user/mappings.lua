-- more in/all objects between 2 characters
local map = vim.keymap.set
for _, mode in ipairs { "n", "v" } do
  map(mode, "n", "h")
  map(mode, "e", "j")
  map(mode, "i", "l")
  map(mode, "u", "k")

  map(mode, "E", "<cmd>normal 5e<cr>")
  map(mode, "U", "<cmd>normal 5u<cr>")
  map(mode, "N", "0")
  map(mode, "I", "$")

  map(mode, "h", "<PageUp>")
  map(mode, "'", "<PageDown>")
end

return {
  n = {
    -- colemak
    ["K"] = { "I", desc = "Insert" },
    ["k"] = { "i", desc = "Insert" },
    ["l"] = { "<cmd>undo<cr>", desc = "Undo" },
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
