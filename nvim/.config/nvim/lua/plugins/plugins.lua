return {
  {
    "ellisonleao/gruvbox.nvim",
    priority = 1000,
    config = true,
  },
  {
    "hrsh7th/nvim-cmp",
    lazy = false,
    dependencies = {
      'neovim/nvim-lspconfig',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-buffer',
      {
        'L3MON4D3/LuaSnip',
        build = 'make install_jsregexp',
        dependencies = {
          {
            "rafamadriz/friendly-snippets",
            config = function()
              require('luasnip.loaders.from_vscode').lazy_load()
            end,
          },
        },
      },
      "saadparwaiz1/cmp_luasnip",
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-cmdline',
    },
    config = function()
      -- Set up nvim-cmp.
      local cmp = require'cmp'
      local cmp_autopairs = require('nvim-autopairs.completion.cmp')
      local luasnip = require "luasnip"

      luasnip.config.setup {}

      cmp.setup({
        snippet = {
          -- REQUIRED - you must specify a snippet engine
          expand = function(args)
            require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
          end,
        },
       preselect = cmp.PreselectMode.None,
        window = {
          completion = cmp.config.window.bordered(),
          documentation = cmp.config.window.bordered(),
        },
        mapping = cmp.mapping.preset.insert({
          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<M-Space>'] = cmp.mapping.complete(),
          ['<C-e>'] = cmp.mapping.abort(),
          ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        }),
        sources = cmp.config.sources({
          { name = 'luasnip' }, -- For luasnip users.
          { name = 'nvim_lsp' },
        }, {
          { name = 'buffer' },
        })
      })

      cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())

      
      vim.keymap.set({ "i", "s" }, "<C-k>", function()
        if luasnip.expand_or_jumpable() then
          luasnip.expand_or_jump()
        end
      end, { silent = true })

      vim.keymap.set({ "i", "s" }, "<C-j>", function()
        if luasnip.jumpable(-1) then
          luasnip.jump(-1)
        end
      end, { silent = true })

      -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline({ '/', '?' }, {
        mapping = cmp.mapping.preset.cmdline(),
        sources = {
          { name = 'buffer' }
        }
      })

      -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
      cmp.setup.cmdline(':', {
        mapping = cmp.mapping.preset.cmdline(),
        sources = cmp.config.sources({
          { name = 'path' }
        }, {
          { name = 'cmdline' }
        }),
        matching = { disallow_symbol_nonprefix_matching = false }
      })
    end
  },
  {
    "neovim/nvim-lspconfig",
    lazy = false,
    version = false,
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
    },
    config = function ()
      require("mason").setup()
      require("mason-lspconfig").setup()
      require("mason-lspconfig").setup_handlers {
        function (server_name)
          local capabilities = require("cmp_nvim_lsp").default_capabilities()
          require("lspconfig")[server_name].setup {
            capabilities = capabilities
          }
        end,
        ["lua_ls"] = function ()
          require("lspconfig")["lua_ls"].setup {
            settings = {
              Lua = {
                diagnostics = {
                  globals = { "vim" }
                }
              }
            }
          }
        end,
      }

      vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action)
      vim.keymap.set('n', '<leader>d', vim.lsp.buf.definition)
      vim.keymap.set('n', '<leader>u', vim.lsp.buf.references)
      vim.keymap.set('n', '<leader>rr', vim.lsp.buf.rename)
    end
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons', 'nvim-lua/lsp-status.nvim' },
    lazy = false,
    opts = {
      options = {
        icons_enabled = true,
        theme = 'gruvbox',
        component_separators = { left = '', right = ''},
        section_separators = { left = '', right = ''},
        disabled_filetypes = {
          statusline = {},
          winbar = {},
        },
        ignore_focus = {},
        always_divide_middle = true,
        always_show_tabline = true,
        globalstatus = false,
        refresh = {
          statusline = 1000,
          tabline = 1000,
          winbar = 1000,
        }
      },
      sections = {
        lualine_a = {'mode'},
        lualine_b = {'branch', 'diff', 'diagnostics'},
        lualine_c = {
          {
            'filename',
            newfile_status = true,
            path = 3,
            shorting_target = 20,
          },
          "require'lsp-status'.status()"
        },
        lualine_x = {'encoding','filetype'},
        lualine_y = {'progress'},
        lualine_z = {'location'}
      },
      inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {'filename'},
        lualine_x = {'location'},
        lualine_y = {},
        lualine_z = {}
      },
      tabline = {},
      winbar = {},
      inactive_winbar = {},
      extensions = {}
    }
  },
  {
    "ibhagwan/fzf-lua",
    -- optional for icon support
    dependencies = {
      "nvim-tree/nvim-web-devicons",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      -- calling `setup` is optional for customization
      local fzf = require('fzf-lua')
      fzf.setup({
        "telescope",
        winopts = {
          on_create = function()
            vim.keymap.set("t", "<C-r>", [[<C-\><C-N>pi]])
          end,
        },
        lsp = {
          formatter = "path.filename_first",
        },
      })

      fzf.register_ui_select()
      vim.keymap.set('n', '<leader>ff', fzf.files)
      vim.keymap.set('n', '<leader>fp', fzf.git_files)
      vim.keymap.set('n', '<leader>fg', fzf.live_grep_glob)
      vim.keymap.set('n', '<leader>fb', fzf.buffers)
      vim.keymap.set('n', '<leader>fh', fzf.helptags)
      vim.keymap.set('n', '<leader>fo', fzf.oldfiles)
      vim.keymap.set('n', '<leader>fr', fzf.lsp_references)
      vim.keymap.set('n', '<leader>fs', fzf.lsp_workspace_symbols)
      vim.keymap.set('n', '<leader>fd', fzf.diagnostics_workspace)
      vim.keymap.set('n', '<leader>fq', fzf.quickfix)
    end
  },
  {
    "akinsho/bufferline.nvim",
    config = true,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    build = ":TSUpdate",
    opts = {
      ensure_installed = {
        "c",
        "cpp",
        "css",
        "html",
        "go",
        "gomod",
        "graphql",
        "hcl",
        "javascript",
        "json",
        "lua",
        "markdown",
        "markdown_inline",
        "python",
        "ruby",
        "rust",
        "sql",
        "terraform",
        "tsx",
        "typescript",
        "vim",
        "vimdoc",
        "yaml",
      },
      auto_install = true,
      highlight = {
        enable = true,
        -- additional_vim_regex_highlighting = { "markdown" },
      },
    },
    config = function(_, opts)
      require('nvim-treesitter.configs').setup(opts)
      vim.o.foldmethod = 'expr'
      vim.o.foldexpr = 'nvim_treesitter#foldexpr()'
      vim.o.foldlevelstart = 15
    end
  },
  {
    "romgrk/nvim-treesitter-context",
    opts = {
      enable = true,
    },
  },
  {
    "windwp/nvim-autopairs",
    opts = {
      fast_wrap = {},
    },
  },
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      signs = {
        add          = { text = '┃' },
        change       = { text = '┃' },
        delete       = { text = '_' },
        topdelete    = { text = '‾' },
        changedelete = { text = '~' },
        untracked    = { text = '┆' },
      },
      signs_staged = {
        add          = { text = '┃' },
        change       = { text = '┃' },
        delete       = { text = '_' },
        topdelete    = { text = '‾' },
        changedelete = { text = '~' },
        untracked    = { text = '┆' },
      },
      signs_staged_enable = true,
      signcolumn = true,  -- Toggle with `:Gitsigns toggle_signs`
      numhl      = false, -- Toggle with `:Gitsigns toggle_numhl`
      linehl     = false, -- Toggle with `:Gitsigns toggle_linehl`
      word_diff  = false, -- Toggle with `:Gitsigns toggle_word_diff`
      watch_gitdir = {
        follow_files = true
      },
      auto_attach = true,
      attach_to_untracked = false,
      current_line_blame = true, -- Toggle with `:Gitsigns toggle_current_line_blame`
      current_line_blame_opts = {
        virt_text = true,
        virt_text_pos = 'eol', -- 'eol' | 'overlay' | 'right_align'
        delay = 1000,
        ignore_whitespace = false,
        virt_text_priority = 100,
        use_focus = true,
      },
      current_line_blame_formatter = '<author>, <author_time:%R> - <summary>',
      sign_priority = 6,
      update_debounce = 100,
      status_formatter = nil, -- Use default
      max_file_length = 40000, -- Disable if file is longer than this (in lines)
      preview_config = {
        -- Options passed to nvim_open_win
        border = 'single',
        style = 'minimal',
        relative = 'cursor',
        row = 0,
        col = 1
      },
      on_attach = function(bufnr)
        local gitsigns = require('gitsigns')

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end
        -- Navigation
        map('n', ']c', function()
          if vim.wo.diff then
            vim.cmd.normal({']c', bang = true})
          else
            gitsigns.nav_hunk('next')
          end
        end)

        map('n', '[c', function()
          if vim.wo.diff then
            vim.cmd.normal({'[c', bang = true})
          else
            gitsigns.nav_hunk('prev')
          end
        end)

        map('n', '<leader>hb', function() gitsigns.blame_line{full=true} end)
        map('n', '<leader>tb', gitsigns.toggle_current_line_blame)
        map('n', '<leader>hd', gitsigns.diffthis)
        map('n', '<leader>hp', gitsigns.preview_hunk_inline)
        map('n', '<leader>td', gitsigns.toggle_deleted)
      end
    }
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    lazy = false,
    main = "ibl",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
    },
    opts = {
      scope = {
        enabled = true
      }
    }
  },
  {
    "rcarriga/nvim-notify",
    priority = 1000,
    opts = {
      background_colour = "#000000"
    },
  },
  {
    "folke/noice.nvim",
    config = true,
  },
  {
    "utilyre/barbecue.nvim",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons"
    },
    config = true
  },
  {
    "folke/which-key.nvim",
    config = true,
  },
  "leafgarland/typescript-vim"
}
