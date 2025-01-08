-- Hybrid line numbering
local numbertogglegroup = vim.api.nvim_create_augroup("numbertoggle", {})
vim.api.nvim_create_autocmd({"BufEnter", "FocusGained", "InsertLeave"},
	{
		pattern = '*.*',
        callback = function()
            vim.wo.relativenumber = true
        end,
        group = numbertogglegroup
})

vim.api.nvim_create_autocmd({"BufLeave", "FocusLost", "InsertEnter"},
	{
	pattern = '*.*',
        callback = function()
            vim.wo.relativenumber = false
        end,
	group = numbertogglegroup
})


