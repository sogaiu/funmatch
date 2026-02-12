#! /usr/bin/env janet

(use ./sh-dsl)

########################################################################

(print "* running niche...")
(def niche-exit ($ janet ./bin/niche.janet))
(assertf (zero? niche-exit)
         "niche exited: %d" niche-exit)
(print "done")

