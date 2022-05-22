local M = {}

function M.setup()
	vim.cmd [[set undofile]]
	vim.o.hidden = true
	vim.o.updatetime = 250
	vim.o.inccommand = "split"

	-- Space as leader key just like Doom Emacs
	vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
	vim.g.mapleader = " "
	vim.g.maplocalleader = " "

	vim.o.path = vim.o.path.."**"

	-- More Convenience Key Bindings
	vim.api.nvim_set_keymap("n", "<leader>db", "<Cmd>:bdelete<CR>", { noremap = true, silent = true })
	vim.api.nvim_set_keymap("n", "<leader>fs", "<Cmd>:w<CR>", { noremap = true, silent = true })
end

return M
