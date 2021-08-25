local narrower = {}

narrower.setup = function()
	-- Ivy-like narrowing framework
	require("telescope").setup({
		defaults = require("telescope.themes").get_ivy({
			mappings = {
				i = {
					["<C-u>"] = false,
					["<C-d>"] = false,
				},
			},
		})
	})

	-- Key Bindings
	local map = vim.api.nvim_set_keymap
	map("n", "<leader><space>", [[<cmd>lua require("telescope.builtin").find_files({previewer = false})<CR>]], { noremap = true, silent = true })
	map("n", "<leader>g<space>", [[<cmd>lua require("telescope.builtin").git_files({previewer = false})<CR>]], { noremap = true, silent = true })
	map("n", "<leader>*", [[<cmd>lua require("telescope.builtin").live_grep()<CR>]], { noremap = true, silent = true })
	map("n", "<leader>rr", [[<cmd>lua require("telescope.builtin").registers()<CR>]], { noremap = true, silent = true })
	map("n", "<leader>bi", [[<cmd>lua require("telescope.builtin").buffers()<CR>]], { noremap = true, silent = true })
	map("n", "<leader>ss", [[<cmd>lua require("telescope.builtin").current_buffer_fuzzy_find()<CR>]], { noremap = true, silent = true })
	map("n", "<leader>sh", [[<cmd>lua require("telescope.builtin").help_tags()<CR>]], { noremap = true, silent = true })
	map("n", "<leader>sc", [[<cmd>lua require("telescope.builtin").commands()<CR>]], { noremap = true, silent = true })
end

return narrower
