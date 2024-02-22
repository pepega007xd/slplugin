module Self = Plugin.Register (struct
  let name = "Shape analysis"
  let shortname = "SLplugin"
  let help = ""
end)

module Dump_queries = Self.False (struct
  let option_name = "-sl-dump-queries"
  let help = "Dump Astral queries to 'astral_queries' directory."
end)
