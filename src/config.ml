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

module Edge_abstraction = Self.False (struct
  let option_name = "-sl-edge-abstraction"

  let help =
    "Do abstraction on every edge between stmts (default: abstraction is done \
     on loop return)"
end)

module Edge_deduplication = Self.False (struct
  let option_name = "-sl-edge-deduplication"

  let help =
    "Deduplicate states using join (entailments) on every edge between stmts"
end)

module Backend_solver = Self.Enum (struct
  let option_name = "-sl-backend-solver"
  let help = "Which solver should be used by Astral, default: Auto"
  let arg_name = "Auto | Bitwuzla | CVC5 | Z3"

  type t = Astral.Options.backend

  let default = `Auto
  let all_values = [ `Bitwuzla; `CVC5; `Z3; `Auto ]

  let to_string = function
    | `Bitwuzla -> "Bitwuzla"
    | `CVC5 -> "CVC5"
    | `Z3 -> "Z3"
    | `Auto -> "Auto"
end)

module Astral_encoding = Self.Enum (struct
  let option_name = "-sl-astral-encoding"
  let help = "Which location encoding should Astral use, default: Bitvectors"
  let arg_name = "Bitvectors | Sets"

  type t = Astral.Options.encoding

  let default = `Bitvectors
  let all_values = [ `Bitvectors; `Sets ]
  let to_string = function `Bitvectors -> "Bitvectors" | `Sets -> "Sets"
end)

module Print_sort = Self.False (struct
  let option_name = "-sl-print-sort"
  let help = "Print sort of variables along with their names"
end)

module Simple_join = Self.False (struct
  let option_name = "-sl-simple-join"
  let help = "Compute join of states using entailment on single formulas"
end)

module Astral_debug = Self.False (struct
  let option_name = "-sl-astral-debug"
  let help = "Print info about queries to Astral"
end)
