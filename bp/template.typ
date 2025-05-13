#let project(body) = {
  set text(font: "New Computer Modern", lang: "en")
  show math.equation: set text(weight: 400)
  set par(justify: true)

  show table.cell.where(y: 0): strong
  set table(
    stroke: (x, y) => if y == 0 {
      (bottom: 0.7pt + black)
    },
    column-gutter: 1em,
    align: (x, y) => (
      if x > 0 { center } else { left + horizon }
    ),
  )

  // merge citations into one block: [1], [2] -> [1, 2]
  set cite(style: "springer-basic")

  // do not break lines after "a"
  show " a ": [ a~]

  // show figure caption above tables in figures
  show figure.where(kind: table): set figure.caption(position: top)

  // all top-level chapters start on new page
  show heading.where(depth: 1, numbering: "1.1"): body => {
    pagebreak()
    body
  }

  // do not break lines in inline equations
  show math.equation.where(block: false): box

  // show links as blue
  show link: set text(rgb("#3366CC"))

  body
}

#let czech_text(content) = {
  set text(lang: "cz")

  show " k ": [ k~]
  show " s ": [ s~]
  show " v ": [ v~]
  show " z ": [ z~]
  show " o ": [ o~]
  show " u ": [ u~]
  show " a ": [ a~]
  show " i ": [ i~]

  content
}

// links to files in ktsn repo and svcomp benchmarks

#let repo_link(repo_url, filepath) = link(
  repo_url + filepath,
  raw(filepath),
)

#let plugin_link(filepath) = repo_link("https://github.com/pepega007xd/ktsn/tree/bp_version/", filepath)

#let svcomp_link(filepath) = repo_link(
  "https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks/-/tree/svcomp25/c/",
  filepath,
)

// SL shortcuts

#let gneg = $ and_not $

#let sf(text) = raw(text)

#let ls(bound: none, x, y) = if bound == none {
  $sf("ls")(#x, #y)$
} else {
  $sf("ls")_(#bound+)(#x, #y)$
}

#let dls(bound: none, x, y, p, n) = if bound == none {
  $sf("dls")(#x, #y, #p, #n)$
} else {
  $sf("dls")_(#bound+)(#x, #y, #p, #n)$
}

#let nls(bound: none, x, y, z) = if bound == none {
  $sf("nls")(#x, #y, #z)$
} else {
  $sf("nls")_(#bound+)(#x, #y, #z)$
}

#let freed(x) = $sf("freed")(#x)$

#let fields(..x) = $angle.l #(x.pos().join(", ")) angle.r$

#let entl = sym.tack.double

#let nil = `nil`
#let emp = `emp`

#let f0 = $f'_0$
#let f1 = $f'_1$
#let f2 = $f'_2$
#let f3 = $f'_3$
#let f4 = $f'_4$
#let f5 = $f'_5$

#let state(..params) = {
  $
    or.big
    vec(
      delim: "[",
      ..params
    )
  $
}

#let min = `min`
