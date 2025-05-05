#let project(body) = {
  set page(numbering: "1", number-align: center)
  set text(font: "New Computer Modern", lang: "en")
  show math.equation: set text(weight: 400)
  set heading(numbering: "1.1")
  set par(justify: true)
  show table.cell.where(y: 0): strong
  set table(
    stroke: (x, y) => if y == 0 {
      (bottom: 0.7pt + black)
    },
  )
  // merges citations into one block: [1], [2] -> [1, 2]
  set cite(style: "springer-basic")

  // do not break lines after "a"
  show " a ": [ a~]

  body
}

#let repo_link(repo_url, filepath) = link(
  repo_url + filepath,
  raw(filepath),
)

#let plugin_link(filepath) = repo_link("https://github.com/pepega007xd/slplugin/blob/master/", filepath)

#let svcomp_link(filepath) = repo_link("https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/blob/main/", filepath)

// SL shortcuts

#let gneg = $ and_not $

#let ls(bound: none, x, y) = if bound == none {
  $"ls"(#x, #y)$
} else {
  $"ls"_(#bound+)(#x, #y)$
}

#let dls(bound: none, x, y, p, n) = if bound == none {
  $"dls"(#x, #y, #p, #n)$
} else {
  $"dls"_(#bound+)(#x, #y, #p, #n)$
}

#let nls(bound: none, x, y, z) = if bound == none {
  $"nls"(#x, #y, #z)$
} else {
  $"nls"_(#bound+)(#x, #y, #z)$
}

#let freed(x) = $"freed"(#x)$

#let fields(..x) = $angle.l #(x.pos().join(", ")) angle.r$

#let entl = sym.tack.double

#let nil = "nil"
#let emp = "emp"

#let f0 = $f'_0$
#let f1 = $f'_1$
#let f2 = $f'_2$
