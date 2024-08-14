module Self = Plugin.Register (struct
  let name = "Shape analysis"
  let shortname = "sl"
  let help = ""
end)

module Enable_analysis = Self.False (struct
  let option_name = "-sl"
  let help = "Run analysis"
end)

module Dump_queries = Self.False (struct
  let option_name = "-sl-dump-queries"
  let help = "Dump Astral queries to 'astral_queries' directory."
end)

module Use_cvc5 = Self.False (struct
  let option_name = "-sl-use-cvc5"
  let help = "Use CVC5 in Astral (default is Z3)"
end)
