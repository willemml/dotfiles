local null_ls = require "null-ls"
return {
  sources = {
    -- Set a formatter
    null_ls.builtins.formatting.rufo,
    -- Set a linter
    null_ls.builtins.diagnostics.rubocop,
  },
  on_attach = function(client)
    -- NOTE: You can remove this on attach function to disable format on save
    if client.resolved_capabilities.document_formatting then
      vim.api.nvim_create_autocmd("BufWritePre", {
        desc = "Auto format before save",
        pattern = "<buffer>",
        callback = vim.lsp.buf.formatting_sync,
      })
    end
  end
}
