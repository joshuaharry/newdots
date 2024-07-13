-- Configure tabs/spaces/line numbers
vim.opt.number = true
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.hlsearch = true
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Run a shell command and get the result as a string
local function exec(cmd)
    local result = vim.fn.system(cmd)
    if vim.v.shell_error ~= 0 then
        error("Failed to run shell command")
    end
    return result
end

local function current_buffer_file_extension()
    local buffer_name = vim.api.nvim_buf_get_name(0)
    local extension = buffer_name:match("^.+%.(.+)$")
    return extension
end

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
    local temp_file = vim.fn.tempname() .. "." .. current_buffer_file_extension()
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
        local result = exec(cmd)
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
        exec(cmd)
        local s = read_file_to_string(file_path)
        replace_buffer_content(s)
    end
    return format
end

-- Format files with prettier.
local prettier_formatter = stdout_formatter(function(file_path)
    return "prettier " .. file_path
end)

formatters = {
    ["lua"] = file_formatter(function(file_path)
        return "stylua --indent-type Spaces " .. file_path
    end),
    ["haskell"] = stdout_formatter(function(file_path)
        return "ormolu " .. file_path
    end),
    ["nix"] = file_formatter(function(file_path)
        return "nixfmt " .. file_path
    end),
    ["javascript"] = prettier_formatter,
    ["json"] = prettier_formatter,
    ["html"] = prettier_formatter,
    ["typescript"] = prettier_formatter,
    ["javascriptreact"] = prettier_formatter,
    ["typescriptreact"] = prettier_formatter,
    ["vue"] = prettier_formatter,
    ["markdown"] = markdown_formatter,
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

-- Ugly hack so that we can bind the function to <leader>p
_G.jformat_buffer = jformat_buffer

-- Useful vanilla keybindings
vim.api.nvim_set_keymap("n", "q", ":q<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>w", "<C-w><C-w>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>j", ":bnext<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>k", ":bprev<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>f", "za", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>p", ":lua jformat_buffer()<CR>", { noremap = true, silent = true })

-- Make sure that vim-closetag works on ERB files. We have to set this
-- global variable *before* we configure our plugin manager; otherwise,
-- the plugin doesn't actually work for mysterious raisins.
vim.g.closetag_filetypes = "astro,eruby,template,typescriptreact,javascriptreact,vue,html,heex"

-- Bootstrap the Plugin manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

-- Install Plugins
require("lazy").setup({
    -- Manage language server installations
    { "williamboman/mason.nvim", opts = {} },
    {
        "williamboman/mason-lspconfig.nvim",
        opts = {
            ensure_installed = {
                "pyright",
                "gopls",
                "tsserver",
                "eslint",
                "solargraph",
                "clojure_lsp",
                "tailwindcss",
                "astro",
            },
            handlers = {
                function(server_name)
                    require("lspconfig")[server_name].setup({})
                end,
                ["hls"] = function()
                    require("lspconfig")["hls"].setup({
                        filetypes = { "haskell", "lhaskell", "cabal" },
                        cmd = { "haskell-language-server-wrapper", "--lsp" },
                    })
                end,
                ["solargraph"] = function()
                    require("lspconfig").solargraph.setup({
                        filetypes = { "ruby", "rakefile" },
                        settings = { solargraph = { diagnostics = true } },
                    })
                end,
                ["volar"] = function()
                    require("lspconfig").volar.setup({
                        filetypes = { "vue" },
                        init_options = {
                            vue = { hybridMode = false },
                            typescript = {
                                tsdk = vim.fn.getcwd() .. "/node_modules/typescript/lib",
                            },
                        },
                    })
                end,
                ["tailwindcss"] = function()
                    require("lspconfig").tailwindcss.setup({
                        filetypes = {
                            "heex",
                            "eruby",
                            "typescriptreact",
                            "html",
                            "astro",
                        },
                    })
                end,
            },
        },
    },
    "neovim/nvim-lspconfig",
    -- Manage comments
    { "numToStr/Comment.nvim", opts = {} },
    -- Manage auto pairs
    "cohama/lexima.vim",
    -- Manage HTML completions
    "alvan/vim-closetag",
    -- Manage snippets
    {
        "dcampos/nvim-snippy",
        opts = {
            mappings = {
                is = { ["<Tab>"] = "expand_or_advance", ["<S-Tab>"] = "previous" },
            },
        },
    },
    "dcampos/cmp-snippy",
    -- Manage autocomplete
    {
        "hrsh7th/nvim-cmp",
        dependencies = {
            "hrsh7th/cmp-nvim-lsp",
            "hrsh7th/cmp-buffer",
            "hrsh7th/cmp-path",
        },
        config = function()
            local cmp = require("cmp")
            -- Set up jump to definition
            vim.keymap.set("n", "gd", vim.lsp.buf.definition, { noremap = true, silent = true })

            -- Make error messages appear inside popups
            vim.o.updatetime = 250

            vim.diagnostic.config({
                virtual_text = true,
                float = { source = "always", width = 80, border = border },
            })

            vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
                callback = function()
                    if not cmp.visible() then
                        vim.diagnostic.open_float(nil, { focus = false })
                    end
                end,
            })

            cmp.setup({
                snippet = {
                    expand = function(args)
                        require("snippy").expand_snippet(args.body)
                    end,
                },
                completion = { autocomplete = false },
                mapping = cmp.mapping.preset.insert({
                    ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                    ["<C-f>"] = cmp.mapping.scroll_docs(4),
                    ["<C-n>"] = cmp.mapping(function()
                        if cmp.visible() then
                            cmp.select_next_item()
                        else
                            cmp.complete()
                        end
                    end),
                    ["<C-e>"] = cmp.mapping.abort(),
                    ["<C-space>"] = cmp.mapping.complete(),
                    ["<C-p>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c" }),
                    -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
                    ["<CR>"] = cmp.mapping.confirm({ select = true }),
                }),
                sources = cmp.config.sources({
                    { name = "nvim_lsp" },
                    { name = "snippy" },
                    { name = "path" },
                    { name = "buffer" },
                }),
            })
        end,
    },
    -- Manage Fuzzy Finding
    {
        "nvim-telescope/telescope.nvim",
        tag = "0.1.2",
        dependencies = { "nvim-lua/plenary.nvim" },
        config = function()
            vim.api.nvim_set_keymap("n", "<leader>sf", ":Telescope find_files<CR>", { noremap = true, silent = true })
            vim.api.nvim_set_keymap("n", "<leader>sg", ":Telescope live_grep<CR>", { noremap = true, silent = true })
            vim.api.nvim_set_keymap("n", "<leader>sb", ":Telescope buffers<CR>", { noremap = true, silent = true })
            vim.api.nvim_set_keymap("n", "<leader>sh", ":Telescope help_tags<CR>", { noremap = true, silent = true })
        end,
    },
    -- Manage JSDOC
    { "heavenshell/vim-jsdoc", build = "make install" },
    -- Manage Astro
    { "wuelnerdotexe/vim-astro" },
    -- LISP-y languages
    { "Olical/conjure" },
    -- Set the colorscheme
    {
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1000,
        config = function()
            require("catppuccin").setup({
                flavour = "auto", -- latte, frappe, macchiato, mocha
                background = { -- :h background
                    light = "latte",
                    dark = "mocha",
                },
                transparent_background = false, -- disables setting the background color.
                show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
                term_colors = false, -- sets terminal colors (e.g. `g:terminal_color_0`)
                dim_inactive = {
                    enabled = false, -- dims the background color of inactive window
                    shade = "dark",
                    percentage = 0.15, -- percentage of the shade to apply to the inactive window
                },
                no_italic = false, -- Force no italic
                no_bold = false, -- Force no bold
                no_underline = false, -- Force no underline
                styles = { -- Handles the styles of general hi groups (see `:h highlight-args`):
                    comments = { "italic" }, -- Change the style of comments
                    conditionals = { "italic" },
                    loops = {},
                    functions = {},
                    keywords = {},
                    strings = {},
                    variables = {},
                    numbers = {},
                    booleans = {},
                    properties = {},
                    types = {},
                    operators = {},
                    -- miscs = {}, -- Uncomment to turn off hard-coded styles
                },
                color_overrides = {},
                custom_highlights = {},
                default_integrations = true,
                integrations = {
                    cmp = true,
                    gitsigns = true,
                    nvimtree = true,
                    treesitter = true,
                    notify = false,
                    mini = {
                        enabled = true,
                        indentscope_color = "",
                    },
                },
            })
            vim.cmd.colorscheme("catppuccin-latte")
        end,
    },
    -- Manage Syntax Highlighting
    {
        "nvim-treesitter/nvim-treesitter",
        config = function()
            local configs = require("nvim-treesitter.configs")
            configs.setup({
                ensure_installed = {
                    "c",
                    "lua",
                    "vim",
                    "vimdoc",
                    "query",
                    "elixir",
                    "heex",
                    "javascript",
                    "html",
                    "heex",
                },
                sync_install = false,
                highlight = {
                    enable = true,
                    disable = {
                        "typescriptreact",
                        "javascript",
                        "typescript",
                        "python",
                        "html",
                    },
                },
                indent = { enable = true },
            })
        end,
    },
    -- Manage JSX
    {
        "maxmellon/vim-jsx-pretty",
        dependencies = { "yuezk/vim-js", "HerringtonDarkholme/yats.vim" },
    },
})

