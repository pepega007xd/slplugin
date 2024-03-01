module Self = Plugin.Register (struct
  let name = "Shape analysis"
  let shortname = "SLplugin"
  let help = ""
end)

module Dump_queries = Self.False (struct
  let option_name = "-sl-dump-queries"
  let help = "Dump Astral queries to 'astral_queries' directory."
end)

module Use_cvc5 = Self.False (struct
  let option_name = "-sl-use-cvc5"
  let help = "Use CVC5 in Astral (default is Z3)"
end)

module Debug_output = Self.False (struct
  let option_name = "-sl-debug"
  let help = "Print debug progress of analysis"
end)
