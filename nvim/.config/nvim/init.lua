vim.keymap.set('i', 'jk', '<Esc>')
vim.o.is = true
vim.o.hls = true
vim.g.mapleader = ","
vim.keymap.set("n", "<leader><Space>", ":nohls<cr>", { silent = true })
vim.keymap.set("n", "<leader>cc", ":e ~/.config/nvim/init.lua<cr>", { silent = true })
vim.keymap.set("n", "<leader>sc", ":source ~/.config/nvim/init.lua<cr>", { silent = true })
vim.keymap.set('n', '<leader>e', ':Ex<cr>')
vim.opt.guicursor = "n-v-ve-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20"
vim.o.number = true
vim.o.expandtab = true
vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.softtabstop = 2
vim.keymap.set('n', '<leader>n', ':bnext<cr>')
vim.keymap.set('n', '<leader>p', ':bprev<cr>')
vim.opt.clipboard = "unnamedplus"

vim.o.ic = true

require("config.lifehacks")

require("config.lazy")

vim.o.background = "dark" -- or "light" for light mode
vim.cmd([[colorscheme gruvbox]])

