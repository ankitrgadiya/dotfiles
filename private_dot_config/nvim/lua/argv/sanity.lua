local sanity = {}

local convenience = function()
	vim.api.nvim_set_keymap("n", "<leader>db", "<Cmd>:bdelete<CR>", { noremap = true, silent = true })
end

sanity.setup = function()
	vim.cmd [[set undofile]]
	vim.o.hidden = true
	vim.o.updatetime = 250
	vim.o.inccommand = "split"

	-- Space as leader key just like Doom Emacs
	vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
	vim.g.mapleader = " "
	vim.g.maplocalleader = " "

	-- Auto Change Buffer Directory
	vim.api.nvim_exec(
		[[
			autocmd BufEnter * if expand("%:p:h") !~ '^/tmp' | silent! lcd %:p:h | endif
		]], 
		false
	)

	-- More Convenience Key Bindings
	convenience()
end

return sanity
