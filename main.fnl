(local compiler (require :compiler))

(fn usage [invocation]
  (io.stderr:write
   "apiales 0.1.0 -- a sillyC compiler\n"
   (: "usage: %s <file>\n" :format invocation)))

(fn compile-and-output [filename]
  (-> filename
      (compiler.compile-file)
      (tostring)
      (io.write "\n")))

(match arg
  ["-h"] (usage (. arg 0))
  [filename] (compile-and-output filename)
  [] (io.stderr:write "error: no input files. See -h for help.\n"))
