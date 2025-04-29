#let project(body) = {
  set page(numbering: "1", number-align: center)
  set text(font: "New Computer Modern", lang: "en")
  show math.equation: set text(weight: 400)
  set heading(numbering: "1.1")
  set par(justify: true)

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

#let ls(x, y) = $"ls"(#x, #y)$
#let dls(x, y, p, n) = $"dls"(#x, #y, #p, #n)$
#let nls(x, y, z) = $"nls"(#x, #y, #z)$
#let freed(x) = $"freed"(#x)$

#let fields(..x) = $angle.l #(x.pos().join(", ")) angle.r$

#let entl = sym.tack.double

#let nil = "nil"
#let emp = "emp"
