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
  let help = "Use CVC5 in Astral"
end)

module Use_Bitvectors = Self.False (struct
  let option_name = "-sl-use-bitvectors"
  let help = "Set Astral's encoding to bitvectors"
end)
