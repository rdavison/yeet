(lang dune 3.11)

(name yeet)

(generate_opam_files true)

(source
 (github rdavison/yeet))

(authors "Richard Davison")

(maintainers "Richard Davison")

(license LICENSE)

(documentation https://github.com/rdavison/yeet)

(package
 (name yeet)
 (synopsis "Yeet")
 (description "Yeeeeeeeeeeeeeeet")
 (depends ocaml dune core ppx_jane)
 (tags (yeet)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
