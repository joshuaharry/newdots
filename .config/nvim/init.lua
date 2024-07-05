-- Configure tabs/spaces/line numbers
vim.opt.number = true
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.hlsearch = false
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Formatting
local function read_file_to_string(file_path)
	local file = io.open(file_path, "r") -- Open the file in read mode
	if not file then
		error("Could not open file " .. file_path) -- Throw an exception if the file cannot be opened
	end

	local content = file:read("*all") -- Read the entire file
	file:close() -- Close the file
	return content
end

local function write_buffer_to_temp_file()
	local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
	local temp_file = vim.fn.tempname()
	local file = io.open(temp_file, "w")
	if file then
		for _, line in ipairs(lines) do
			file:write(line .. "\n")
		end
		file:close()
		return temp_file
	else
		vim.notify("Failed to open temporary file", vim.log.levels.ERROR)
	end
end

local function replace_buffer_content(new_content)
	local lines = {}
	for line in new_content:gmatch("([^\n]*)\n?") do
		table.insert(lines, line)
	end
	vim.api.nvim_buf_set_lines(0, 0, -1, false, lines)
	vim.cmd("write")
end

-- INPUT: A function f, with:
--   INPUT: The name of a temporary file containing the buffer contents.
--   OUTPUT: A string with a command that writes the formatted file to
--   stdout.
local function stdout_formatter(f)
	function format()
		local file_path = write_buffer_to_temp_file()
		local cmd = f(file_path)
		local result = vim.fn.system(cmd)
		replace_buffer_content(result)
	end
	return format
end

-- INPUT: A function f, with:
--   INPUT: The name of a temporary file containing the buffer contents.
--   OUTPUT: A string with a command that writes a formatted file in place.
local function file_formatter(f)
	function format()
		local file_path = write_buffer_to_temp_file()
		local cmd = f(file_path)
		vim.fn.system(cmd)
		local s = read_file_to_string(file_path)
		replace_buffer_content(s)
	end
	return format
end

formatters = {
	["lua"] = file_formatter(function(file_path)
		return "stylua " .. file_path
	end),
	["haskell"] = stdout_formatter(function(file_path)
		return "ormolu " .. file_path
	end),
}

local function jformat_buffer()
	local filetype = vim.bo.filetype
	format_fn = formatters[filetype]
	if format_fn then
		format_fn()
	else
		print("Don't know how to format a " .. filetype .. " file")
	end
end
_G.jformat_buffer = jformat_buffer

-- Useful vanilla keybindings
vim.api.nvim_set_keymap("n", "q", ":q<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>w", "<C-w><C-w>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>j", ":bnext<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>k", ":bprev<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>f", "za", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>p", ":lua jformat_buffer()<CR>", { noremap = true, silent = true })

