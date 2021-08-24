-- Package Manager Setup
local install_path = vim.fn.stdpath("data").."/site/pack/packer/start/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
	vim.fn.system({"git", "clone", "https://github.com/wbthomason/packer.nvim", install_path})
	vim.api.nvim_command "packadd packer.nvim"
end

-- I think it configures the Auto-Group "Packer" to run automatically after
-- `BufWritePost` event on `init.lua` file.
vim.api.nvim_exec(
	[[
	augroup Packer
		autocmd!
		autocmd BufWritePost init.lua PackerCompile
	augroup end
	]],
	false
)

-- Define plugins
require("packer").startup(function(use)
	-- Let Packer manage itself from here onwards!
	use "wbthomason/packer.nvim"

	-- Fuzzy Search and all the greatness 🙌
	use { "nvim-telescope/telescope.nvim", requires = { { "nvim-lua/popup.nvim" }, { "nvim-lua/plenary.nvim" } } }

	-- Git goodies
	use { "TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim" }       -- Magit but in Neovim
	use { "kdheepak/lazygit.nvim", requires = "nvim-lua/plenary.nvim" }        -- Lazy Git bindings
	use { "lewis6991/gitsigns.nvim", requires = { "nvim-lua/plenary.nvim" } }  -- Git Gutter

	-- New Parser
	use "nvim-treesitter/nvim-treesitter"
	use "nvim-treesitter/nvim-treesitter-textobjects"

	-- Language Servers and Autocompletion
	use "neovim/nvim-lspconfig"    -- Collection of configurations for built-in LSP client
	use "nvim-lua/lsp-status.nvim" -- 
	use "hrsh7th/nvim-compe"       -- Autocompletion plugin
	use "ray-x/go.nvim"            -- Golang configuration for LSP

	-- Debug Adapter
	use "mfussenegger/nvim-dap"    -- DAP Client

	-- Utility
	use "tpope/vim-commentary" -- Easy Commenting
	use "tpope/vim-dadbod"     -- Database Interface
	use "tpope/vim-vinegar"    -- Netrw Enhanced
	use "tpope/vim-obsession"  -- Session Management

	-- Visual
	use "arcticicestudio/nord-vim"                                                                                              -- Colorscheme
	use {"ellisonleao/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
	use "kyazdani42/nvim-web-devicons"                                                                                          -- Fancy Icons
	use { "hoob3rt/lualine.nvim", requires = { {"kyazdani42/nvim-web-devicons", opt = true}, { "nvim-lua/lsp-status.nvim" } } } -- Status Line
	use { "alvarosevilla95/luatab.nvim", requires= { "kyazdani42/nvim-web-devicons" } }                                         -- Tab Line
	use "lukas-reineke/indent-blankline.nvim"                                                                                   -- Indent Guide
end)

-- ColorScheme Setup
vim.o.background = "dark"
vim.o.termguicolors = true
vim.cmd [[colorscheme gruvbox]]

-- Tab and Status Bar

-- Tabbar is configured to use Luatab plugin.
vim.o.tabline = "%!v:lua.require\"luatab\".tabline()"

-- Sets up the Lualine plugin
require("lualine").setup({
	options = {
		icons_enabled = true,
		theme = "gruvbox",
	}
})

-- Visual settings for Indentation
vim.o.breakindent = true
vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_space_char = "·"
vim.g.indent_blankline_filetype_exclude = { "help", "packer" }
vim.g.indent_blankline_buftype_exclude = { "terminal", "nofile" }
vim.g.indent_blankline_char_highlight = "LineNr"
vim.g.indent_blankline_show_trailing_blankline_indent = false

-- Sane Defaults
vim.cmd [[set undofile]]
vim.o.number = true
vim.o.relativenumber = true
vim.o.hidden = true
vim.o.hlsearch = false
vim.o.ignorecase = true
vim.o.inccommand = "split"
vim.o.smartcase = true
vim.o.updatetime = 250
vim.wo.signcolumn = "yes"

-- Space as leader key just like Doom Emacs
vim.api.nvim_set_keymap("", "<Space>", "<Nop>", { noremap = true, silent = true })
vim.g.mapleader = " "
vim.g.maplocalleader = " "


-- Display Git signs in the Gutter
require("gitsigns").setup {
	signs = {
		add = { hl = "GitGutterAdd", text = "+" },
		change = { hl = "GitGutterChange", text = "~" },
		delete = { hl = "GitGutterDelete", text = "_" },
		topdelete = { hl = "GitGutterDelete", text = "‾" },
		changedelete = { hl = "GitGutterChange", text = "~" },
	},
}

-- Setup Neogit
require("neogit").setup {}

vim.api.nvim_set_keymap("n", "<leader>gg", [[<cmd>lua require("neogit").open()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>lg", [[<cmd>lua require("lazygit").lazygit()<CR>]], { noremap = true, silent = true })

-- Fuzzy Search like Ivy
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

vim.api.nvim_set_keymap("n", "<leader><space>", [[<cmd>lua require("telescope.builtin").find_files({previewer = false})<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>bi", [[<cmd>lua require("telescope.builtin").buffers()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>ss", [[<cmd>lua require("telescope.builtin").current_buffer_fuzzy_find()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>sh", [[<cmd>lua require("telescope.builtin").help_tags()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>sp", [[<cmd>lua require("telescope.builtin").live_grep()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>sc", [[<cmd>lua require("telescope.builtin").commands()<CR>]], { noremap = true, silent = true })

-- Treesitter configuration
require("nvim-treesitter.configs").setup({
	highlight = {
		enable = true, -- false will disable the whole extension
	},
	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = "gnn",
			node_incremental = "grn",
			scope_incremental = "grc",
			node_decremental = "grm",
		},
	},
	indent = {
		enable = true,
	},
	textobjects = {
		select = {
			enable = true,
			lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
			keymaps = {
				-- You can use the capture groups defined in textobjects.scm
				["af"] = "@function.outer",
				["if"] = "@function.inner",
				["ac"] = "@class.outer",
				["ic"] = "@class.inner",
			},
		},
		move = {
			enable = true,
			set_jumps = true, -- whether to set jumps in the jumplist
			goto_next_start = {
				["]m"] = "@function.outer",
				["]]"] = "@class.outer",
			},
			goto_next_end = {
				["]M"] = "@function.outer",
				["]["] = "@class.outer",
			},
			goto_previous_start = {
				["[m"] = "@function.outer",
				["[["] = "@class.outer",
			},
			goto_previous_end = {
				["[M"] = "@function.outer",
				["[]"] = "@class.outer",
			},
		},
	},
})

-- Language Servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
local on_attach = function(_, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  local opts = { noremap = true, silent = true }
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<Cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<Cmd>lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<Cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<C-k>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>wa", "<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>wr", "<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>wl", "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>", opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, "v", "<leader>ca", "<cmd>lua vim.lsp.buf.range_code_action()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>e", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>q", "<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>so", [[<cmd>lua require("telescope.builtin").lsp_document_symbols()<CR>]], opts)
  vim.cmd [[ command! Format execute "lua vim.lsp.buf.formatting()" ]]
end

require("lspconfig").bashls.setup({
	on_attach = on_attach,
	capabilities = capabilities,
})

require("lspconfig").dockerls.setup({
	on_attach = on_attach,
	capabilities = capabilities,
})

-- Golang
require("go").setup({
  goimport="goimports",  -- goimport command
  gofmt = "goimports",   --gofmt cmd,
  max_line_len = 120,    -- max line length in goline format
  tag_transform = false, -- tag_transfer  check gomodifytags for details
  verbose = false,
  lsp_cfg = true,
  lsp_gofumpt = false,
  lsp_on_attach = true,
  lsp_diag_hdlr = true,
  dap_debug = true,
  dap_debug_keymap = true,
  dap_debug_gui = true,
  dap_debug_vt = true,
})

-- Completion
vim.o.completeopt = "menuone,noinsert"
require("compe").setup({
  source = {
    path = true,
    nvim_lsp = true,
    luasnip = true,
    buffer = false,
    calc = false,
    nvim_lua = false,
    vsnip = false,
    ultisnips = false,
  },
})
