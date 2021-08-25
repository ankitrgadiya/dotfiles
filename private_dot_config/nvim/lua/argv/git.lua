local git = {}

git.setup = function()
	-- Setup Neogit
	require("neogit").setup({})
	vim.api.nvim_set_keymap("n", "<leader>gg", [[<cmd>lua require("neogit").open()<CR>]], { noremap = true, silent = true })

	-- Lazygit
	require("lazygit")
	vim.api.nvim_set_keymap("n", "<leader>lg", [[<cmd>lua require("lazygit").lazygit()<CR>]], { noremap = true, silent = true })
end

return git
