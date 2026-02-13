#! /usr/bin/env janet

(use ./sh-dsl)

########################################################################

(print "* running niche...")
(def niche-exit ($ janet ./bin/niche.janet))
(assertf (zero? niche-exit)
         "niche exited: %d" niche-exit)
(print "done")

########################################################################

(print "* updating README...")
(def readme-update-ext ($ janet funmatch.janet > README))
(assertf (zero? readme-update-ext)
         "updating README exited: %d" readme-update-ext)
(print "done")

