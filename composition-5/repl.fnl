(global repl {})

(tset repl :reload
      (fn []
        (tset repl :fennel (dofile "../fennel-dist/fennel.lua"))
        (tset repl :mksco (repl.fennel.dofile "mksco.fnl"))))

(repl.reload)
