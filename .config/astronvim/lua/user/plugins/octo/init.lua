return {
  after = { 'telescope.nvim', },
  config = function ()
    require"octo".setup()
  end
}
