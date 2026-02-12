(import ../funmatch :as fm)
(import ../bin/sh-dsl :as sd)

(comment

  # hack: make usable from inside editor as well as from cli testing
  (def data-dir
    (cond
      (= :directory (os/stat "../.git" :mode))
      (string (os/cwd) "/../data")
      #
      (= :directory (os/stat ".git" :mode))
      (string (os/cwd) "/data")
      #
      (errorf "unexpected current working directory" (os/cwd))))

  (def patterns
    ["hi"
     "*"
     ".jane*"
     "*.janet"
     "a*janet"
     "test.*anet"
     "a*b*c"
     "?"
     "jane?"
     "?anet"
     "?*"
     "a*?"
     "[t-z]"
     "[+--]"
     "[--0]"
     "[---]"
     "*.jane[t-z]"
     "[wxz]"
     "[aabb]"
     "[abba]"
     "[S-]"
     "[-axz]"
     "[--]"
     "[!-a]"
     "[!a]"
     "[!ab]"
     "[!-]"
     "[!s-t]"
     "[!--0]"
     # XXX: to make the test error, uncomment following
     #"[c-a]*"
     ])

  (defn try-a-bunch
    [shell]
    (def old-dir (os/cwd))
    (when (os/getenv "VERBOSE")
      (pp [:old-dir old-dir]))
    (def results @[])
    (defer (os/cd old-dir)
      (os/cd data-dir)
      (each patt patterns
        (def cmd-str (string "ls -d " patt))
        (def sh-res
          (let [result (string/trim (sd/$< ,shell -c ,cmd-str))]
            (if (not (empty? result))
              (sort (string/split "\n" result))
              @[])))
        (def peg-res (->> (filter |(fm/funmatch patt $)
                                  (os/dir data-dir))
                          sort))
        (when (os/getenv "VERBOSE")
          (print)
          (pp [:patt patt])
          (pp [:peg (fm/make-peg patt)]))
        (if (deep= sh-res peg-res)
          (array/push results :ok)
          (do
            (def peg-missed @[])
            (def sh-missed @[])
            (each s sh-res
              (when (not (index-of s peg-res))
                (array/push peg-missed s)))
            (each p peg-res
              (when (not (index-of p sh-res))
                (array/push sh-missed p)))
            (sort sh-missed)
            (sort peg-missed)
            (pp [:sh-res sh-res])
            (pp [:peg-res peg-res])
            (pp [:sh-missed sh-missed])
            (pp [:peg-missed peg-missed]))))
      #
      (all |(= :ok $) results)))

  (if (os/execute ["bash" "-c" "exit"] :px)
    (try-a-bunch "bash")
    true)
  # =>
  true

  (if (os/execute ["zsh" "-c" "exit"] :px)
    (try-a-bunch "zsh")
    true)
  # =>
  true

  )

