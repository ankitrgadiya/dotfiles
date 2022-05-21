-- Setup Packer Package Manager
local install_path = vim.fn.stdpath("data").."/site/pack/packer/start/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
	vim.fn.system{"git", "clone", "https://github.com/wbthomason/packer.nvim", install_path}
	vim.api.nvim_command("packadd packer.nvim")
end

-- I think it configures the Auto-Group "Packer" to run automatically after
-- `BufWritePost` event on `init.lua` file.
vim.api.nvim_exec(
	[[
	augroup Packer
		autocmd!
		autocmd BufWritePost init.lua | PackerCompile
	augroup end
	]],
	false
)

-- Define plugins
local packer = require("packer")
packer.startup(function(use)
	-- Let Packer manage itself from here onwards!
	use "wbthomason/packer.nvim"

	-- Fuzzy Search and all the greatness 🙌
	use { "nvim-telescope/telescope.nvim", requires = { { "nvim-lua/popup.nvim" }, { "nvim-lua/plenary.nvim" } } }

	-- Git goodies
	use { "TimUntersberger/neogit",        requires = { "nvim-lua/plenary.nvim" } }        -- Magit but in Neovim
	use { "kdheepak/lazygit.nvim",         requires = { "nvim-lua/plenary.nvim" } }        -- Lazy Git bindings
	use { "lewis6991/gitsigns.nvim",       requires = { "nvim-lua/plenary.nvim" } }        -- Git Gutter

	-- New Parser
	use "nvim-treesitter/nvim-treesitter"
	use "nvim-treesitter/nvim-treesitter-textobjects"

	-- Language Servers and Autocompletion
	use "neovim/nvim-lspconfig"                                                            -- LSP Configuration
	use "nvim-lua/lsp-status.nvim"                                                         -- LSP Status
	use "hrsh7th/nvim-compe"                                                               -- Autocompletion plugin
	use "ray-x/go.nvim"                                                                    -- LSP Golang

	-- Debug Adapter
	use { "rcarriga/nvim-dap-ui",          requires = {"mfussenegger/nvim-dap"} }          -- DAP Client
	use "theHamsta/nvim-dap-virtual-text"                                                  -- DAP Virtual Text

	-- Utility
	use "tpope/vim-commentary"                                                             -- Easy Commenting
	use "tpope/vim-dadbod"                                                                 -- Database Interface
	use "tpope/vim-vinegar"                                                                -- Netrw Enhanced
	use "tpope/vim-obsession"                                                              -- Session Management

	-- Visual
	use "lukas-reineke/indent-blankline.nvim"                                              -- Indent Guide
	use "arcticicestudio/nord-vim"                                                         -- Nord Colors
	use { "ellisonleao/gruvbox.nvim",      requires = { "rktjmp/lush.nvim"} }              -- Gruvbox Colors
	use { "alvarosevilla95/luatab.nvim",   requires = { "kyazdani42/nvim-web-devicons" } } -- Tab Line  
	use { "hoob3rt/lualine.nvim",          requires = {"kyazdani42/nvim-web-devicons"} }   -- Status Line
end)


require("argv.defaults").setup()
require("argv.visual").setup()
require("argv.narrower").setup()
require("argv.git").setup()
require("argv.languages").setup()
