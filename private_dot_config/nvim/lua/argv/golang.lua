local golang = {}

golang.setup = function()
	-- Configure LSP, DAP and other things for Golang FileType
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

	-- More convenience
	vim.api.nvim_exec(
		[[
		augroup Golang
			autocmd! Golang
			autocmd FileType go nmap <buffer> <silent> <leader>tt :GoTest<CR>
			autocmd FileType go nmap <buffer> <silent> <leader>tf :GoTestFunc<CR>
			autocmd FileType go nmap <buffer> <silent> <leader>dg :GoDebug<CR>
			autocmd FileType go nmap <buffer> <silent> <leader>ds :GoDbgStop<CR>
			autocmd FileType go nmap <buffer> <silent> <leader>dt :GoDebug test<CR>
			autocmd FileType go nmap <buffer> <silent> <leader>bp :GoBreakToggle<CR>
		augroup end
		]],
		false
	)
end

return golang
