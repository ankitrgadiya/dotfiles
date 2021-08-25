local visual = {}

visual.setup = function()
	-- Use Gruvbox colors
	vim.o.background = "dark"
	vim.o.termguicolors = true
	require("gruvbox")
	vim.cmd [[colorscheme gruvbox]]

	-- Configure Tab bar
	require("luatab")
	vim.o.tabline = "%!v:lua.require\'luatab\'.tabline()"

	-- Configure Status bar
	require("lualine").setup({
		options = {
			icons_enabled = true,
			theme = "gruvbox",
		}
	})

	-- Nicer indentation
	vim.o.breakindent = true
	vim.g.indent_blankline_char = "┊"
	vim.g.indent_blankline_space_char = "·"
	vim.g.indent_blankline_filetype_exclude = { "help", "packer" }
	vim.g.indent_blankline_buftype_exclude = { "terminal", "nofile" }
	vim.g.indent_blankline_char_highlight = "LineNr"
	vim.g.indent_blankline_show_trailing_blankline_indent = false

	-- Line numbers, just the way I like
	vim.o.number = true
	vim.o.relativenumber = true

	-- Nicer searching experience
	vim.o.hlsearch = false
	vim.o.smartcase = true

	-- Show Git info in the gutter
	vim.wo.signcolumn = "yes"
	require("gitsigns").setup({
		signs = {
			add = { hl = "GitGutterAdd", text = "+" },
			change = { hl = "GitGutterChange", text = "~" },
			delete = { hl = "GitGutterDelete", text = "_" },
			topdelete = { hl = "GitGutterDelete", text = "‾" },
			changedelete = { hl = "GitGutterChange", text = "~" },
		},
	})
end

return visual
