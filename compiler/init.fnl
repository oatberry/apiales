(local lexing (require :compiler.lexing))
(local parsing (require :compiler.parsing))
(local analysis (require :compiler.analysis))
(local codegen (require :compiler.codegen))

(fn compile-string [str ?source]
  (-> str
      (lexing.scan-string ?source)
      (parsing.parse-tokens)
      (analysis.analyze)
      (codegen.generate)))

(fn compile-file [filename]
  (with-open [file (assert (io.open filename))]
    (-> (file:read "a")
        (compile-string filename))))

{: compile-string
 : compile-file
 : lexing
 : parsing
 : analysis
 : codegen}
