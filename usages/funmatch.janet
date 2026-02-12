(import ../funmatch :as fm)
(import ../bin/sh-dsl :as sd)

(comment

  (do
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
    (def test-dir (os/getenv "HOME"))
    (def old-dir (os/cwd))
    (def results @[])
    (defer (os/cd old-dir)
      (os/cd test-dir)
      (each patt patterns
        (def cmd-str (string "ls -d " patt))
        (def peg (fm/make-peg patt))
        (def sh-res
          (let [result (string/trim (sd/$< bash -c ,cmd-str))]
            (if (not (empty? result))
              (sort (string/split "\n" result))
              @[])))
        (def peg-res (->> (filter |(peg/match peg $)
                                  (os/dir test-dir))
                          sort))
        (print)
        (pp [:patt patt])
        (pp [:peg peg])
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
  # =>
  true

  )

