# funmatch - match pattern against string
#
# * the pattern is similar to a glob pattern
# * the string is typically a filename (no directory portions)
#
# e.g. (funmatch "*.janet" "test.janet") should return true

# patterns
#
# * * - zero or more characters (except . if leading part of pattern)
# * ? - one character (except . if leading part of pattern)
# * [x-y] - one character in the range x <= y
# * [!x-y] - one character not in the range x <= y
# * [xyz] - one character in the set x, y, ..., z
# * [!xyz] - one character not in the set x, y, ..., z

# implementation notes
#
# the general idea is to:
#
# 1. parse glob-like pattern into pieces (`parse-pattern`)
# 2. create peg from parsed pieces (`make-peg`)
# 3. apply peg/match with the generated peg to a string

# keywords: glob, fnmatch, wildmat

# references
#
# * fnmatch(3)
# * glob(7)
# * https://en.wikipedia.org/wiki/Glob_(programming)

``
  Consider the following example pattern:

    a*b*c

  It's parsed as:

    @["a"
      {:type :asterisk}
      "b"
      {:type :asterisk}
      "c"]

  A peg that appears to match the intent is:

    ~(sequence "a"
               (to (sequence "b" (to (sequence "c" -1))))
                             ##########################
               "b"
               (to (sequence "c" -1))
                             ######
               "c"
               -1)

  The peg can be constructed based on the parsed results
  using two passes.

  In pass 1, each non-asterisk portion has a corresponding
  peg subexpression inserted into `(sequence ...)` along
  with {:type :asterisk} bits for each asterisk.  Finally a
  -1 is added as the last element of the `(sequence ...)`
  form:

    ~(sequence "a"
               {:type :asterisk}
               "b"
               {:type :asterisk}
               "c"
               -1)

  In pass 2, starting from the end of the `(sequence ...)`
  form, each peg subexpression is "remembered" as progress
  is made backwards until a {:type :asterisk} is
  encountered.  At such an encounter, the struct is
  replaced with a `(to (sequence ...))` peg subexpression
  where the inner `...` is replaced with the "remembered"
  subexpressions from earlier:

    ~(sequence "a"
               {:type :asterisk}
               "b"
               (to (sequence "c" -1))
               "c"
               -1)

  Then the "remembered" portions are reset to be a copy
  of the newly placed `(to (sequence ...))`.  The process
  continues until the beginning of the outer
  `(sequence ...)` is reached:

    ~(sequence "a"
               (to (sequence "b" (to (sequence "c" -1))))
               "b"
               (to (sequence "c" -1))
               "c"
               -1)

``

(defn parse-pattern
  [pattern]
  (peg/match
    ~{:main
      (sequence (any (choice :ast :que :bang :rng :set :lit)) -1)
      :ast (cmt (capture "*")
                ,(fn [_] {:type :asterisk}))
      :que (cmt (capture "?")
                ,(fn [_] {:type :question}))
      :bang (cmt (sequence "["
                           "!"
                           (opt (capture "]"))
                           (any (sequence (not "]") (capture 1)))
                           "]")
                 ,|(if-let [_ (= 3 (length $&))
                            [begin mid end] $&
                            _ (= "-" mid)
                            _ (<= begin end)]
                     {:type :neg-range
                      :begin begin
                      :end end}
                     {:type :neg-set
                      :items (sort (distinct $&))}))
      :rng (cmt (sequence "["
                          (capture 1)
                          "-"
                          (capture (sequence (not "]") 1))
                          "]")
                ,|(when (<= $0 $1)
                    {:type :range
                     :begin $0
                     :end $1}))
      :set (cmt (sequence "["
                          (opt (capture "]"))
                          (any (sequence (not "]") (capture 1)))
                          "]")
                ,|{:type :set
                   :items (sort (distinct $&))})
      # XXX: was previous way (post-processing) better?
      :lit (accumulate (any (sequence (not :ast)
                                      (not :que)
                                      (not :bang)
                                      (not :rng)
                                      (not :set)
                                      (capture 1))))}
    pattern))

(comment

  (parse-pattern "")
  # =>
  @[]

  (parse-pattern "hi")
  # =>
  @["hi"]

  (parse-pattern "*")
  # =>
  @[{:type :asterisk}]

  (parse-pattern ".jane*")
  # =>
  @[".jane" {:type :asterisk}]

  (parse-pattern "*.janet")
  # =>
  @[{:type :asterisk} ".janet"]

  (parse-pattern "a*janet")
  # =>
  @["a" {:type :asterisk} "janet"]

  (parse-pattern "a*b*c")
  # =>
  @["a"
    {:type :asterisk}
    "b"
    {:type :asterisk}
    "c"]

  (parse-pattern "?")
  # =>
  @[{:type :question}]

  (parse-pattern "jane?")
  # =>
  @["jane" {:type :question}]

  (parse-pattern "?anet")
  # =>
  @[{:type :question} "anet"]

  (parse-pattern "?*")
  # =>
  @[{:type :question} {:type :asterisk}]

  (parse-pattern "a*?")
  # =>
  @["a" {:type :asterisk} {:type :question}]

  (parse-pattern "[t-z]")
  # =>
  @[{:begin "t"
     :end "z"
     :type :range}]

  (parse-pattern "*.jane[t-z]")
  # =>
  @[{:type :asterisk}
    ".jane"
    {:begin "t"
     :end "z"
     :type :range}]

  (parse-pattern "[wxz]")
  # =>
  @[{:items @["w" "x" "z"]
     :type :set}]

  (parse-pattern "[S-]")
  # =>
  @[{:items @["-" "S"]
     :type :set}]

  (parse-pattern "[!a]")
  # =>
  @[{:items @["a"]
     :type :neg-set}]

  (parse-pattern "[!ab]")
  # =>
  @[{:items @["a" "b"]
     :type :neg-set}]

  (parse-pattern "[!-]")
  # =>
  @[{:items @["-"]
     :type :neg-set}]

  (parse-pattern "[!-a]")
  # =>
  @[{:items @["-" "a"]
     :type :neg-set}]

  (parse-pattern "[!--0]")
  # =>
  @[{:begin "-"
     :end "0"
     :type :neg-range}]

  (parse-pattern "[!s-t]")
  @[{:begin "s"
     :end "t"
     :type :neg-range}]

  (parse-pattern "[!]]")
  # =>
  @[{:items @["]"]
     :type :neg-set}]

  (parse-pattern "[!]-_]")
  # =>
  @[{:begin "]"
     :end "_"
     :type :neg-range}]

  (parse-pattern "[]-_]")
  # =>
  @[{:begin "]"
     :end "_"
     :type :range}]

  )

(defn make-peg
  [parsed]
  (def scratch
    (if (and (def head (first parsed))
             (dictionary? head)
             (def the-type (get head :type))
             (get (invert [:asterisk :question]) the-type))
      @['sequence '(not ".")]
      @['sequence]))
  # pass 1: non-asterisk bits
  (each p parsed
    (if (string? p)
      (array/push scratch p)
      (let [the-type (get p :type)]
        (assertf the-type "failed to find :type for: %n" p)
        (case the-type
          :asterisk
          (array/push scratch p) # handle in pass 2
          :question
          (array/push scratch 1)
          :range
          (array/push scratch
                      ['range (string (get p :begin) (get p :end))])
          :set
          (array/push scratch
                      ['set (string/join (get p :items))])
          :neg-range
          (array/push scratch
                      ['not ['range (string (get p :begin) (get p :end))]]
                      1)
          :neg-set
          (array/push scratch
                      ['not ['set (string/join (get p :items))]]
                      1)
          (errorf "unexpected item: %n" p)))))
  # finish pass 1
  (array/push scratch -1)
  # pass 2: handle asterisks starting at the end
  (def temp @[])
  (loop [i :down-to [(dec (length scratch)) 0]
         :let [piece (get scratch i)]]
    (if (and (dictionary? piece) (= :asterisk (get piece :type)))
      (let [replacement ['to ['sequence ;(reverse temp)]]]
        (put scratch i replacement)
        (array/clear temp)
        (array/push temp replacement))
      (array/push temp piece)))
  # peg/match won't accept arrays
  (tuple/slice scratch))

(comment

  (make-peg @[])
  # =>
  ~(sequence -1)

  (make-peg @["hi"])
  # =>
  ~(sequence "hi"
             -1)

  (make-peg @[{:type :asterisk}])
  # =>
  ~(sequence (not ".")
             (to (sequence -1))
             -1)

  (make-peg @[".jane" {:type :asterisk}])
  # =>
  ~(sequence ".jane"
             (to (sequence -1))
             -1)

  (make-peg @[{:type :asterisk} ".janet"])
  # =
  ~(sequence (to (sequence ".janet" -1))
             ".janet"
             -1)

  (make-peg @["a" {:type :asterisk} "janet"])
  # =>
  ~(sequence "a"
             (to (sequence "janet" -1))
             "janet"
             -1)

  (make-peg @["a"
              {:type :asterisk}
              "b"
              {:type :asterisk}
              "c"])
  # =>
  ~(sequence "a"
             (to (sequence "b" (to (sequence "c" -1))))
             "b"
             (to (sequence "c" -1))
             "c"
             -1)

  (make-peg @[{:type :question}])
  # =>
  ~(sequence (not ".")
             1
             -1)

  (make-peg @["jane" {:type :question}])
  # =>
  ~(sequence "jane" 1 -1)

  (make-peg @[{:type :question} "anet"])
  # =>
  ~(sequence (not ".")
             1
             "anet"
             -1)

  (make-peg @[{:type :question}
              {:type :asterisk}])
  # =>
  ~(sequence (not ".")
             1
             (to (sequence -1))
             -1)

  (make-peg @["a"
              {:type :asterisk}
              {:type :question}])
  # =>
  ~(sequence "a"
             (to (sequence 1 -1))
             1
             -1)

  (make-peg @[{:begin "t"
               :end "z"
               :type :range}])
  # =>
  ~(sequence (range "tz") -1)


  (make-peg @[{:type :asterisk}
              ".jane"
              {:begin "t"
               :end "z"
               :type :range}])
  # =>
  ~(sequence (not ".")
             (to (sequence ".jane"
                           (range "tz")
                           -1))
             ".jane"
             (range "tz")
             -1)

  (make-peg @[{:items @["w" "x" "z"]
               :type :set}])
  # =>
  ~(sequence (set "wxz") -1)


  (make-peg @[{:items @["S" "-"]
               :type :set}])
  # =>
  ~(sequence (set "S-") -1)

  (make-peg @[{:items @["a"]
               :type :neg-set}])
  # =>
  ~(sequence (not (set "a"))
             1
             -1)

  (make-peg @[{:items @["a" "b"]
               :type :neg-set}])
  # =>
  ~(sequence (not (set "ab"))
             1
             -1)

  (make-peg @[{:items @["-"]
               :type :neg-set}])
  # =>
  ~(sequence (not (set "-"))
             1
             -1)

  (make-peg @[{:begin "!"
               :end "a"
               :type :range}])
  # =>
  ~(sequence (range "!a") -1)

  (make-peg @[{:begin "-"
               :end "0"
               :type :neg-range}])
  # =>
  ~(sequence (not (range "-0")) 1 -1)

  (make-peg @[{:begin "s"
               :end "t"
               :type :neg-range}])
  # =>
  ~(sequence (not (range "st")) 1 -1)

  (make-peg @[{:items @["]"]
               :type :neg-set}])
  # =>
  ~(sequence (not (set "]")) 1 -1)

  (make-peg @[{:begin "]"
               :end "_"
               :type :neg-range}])
  # =>
  ~(sequence (not (range "]_")) 1 -1)

  )

(defn funmatch
  [patt str]
  (def parsed (parse-pattern patt))
  (assertf parsed "failed to parse pattern: %s" patt)
  #
  (def peg (make-peg parsed))
  #
  (truthy? (peg/match peg str)))

(comment

  (funmatch "hi" "hi")
  # =>
  true

  (funmatch "*" "hi")
  # =>
  true

  (funmatch "*" ".hi")
  # =>
  false

  (funmatch ".jane*" ".janet")
  # =>
  true

  (funmatch "a*janet" "alpha.janet")
  # =>
  true

  (funmatch "a*janet" "a.janet")
  # =>
  true

  (funmatch "a*b*c*" "ant bee cat")
  # =>
  true

  (funmatch "?anet" "kanet")
  # =>
  true

  (funmatch "?anet" "Janet")
  # =>
  true

  (funmatch "[a-c]" "b")
  # =>
  true

  (funmatch "[a-a]" "a")
  # =>
  true

  (funmatch "x[x-z]z" "xyz")
  # =>
  true

  (funmatch "*.jane[t-z]" "zeta.janez")
  # =>
  true

  (funmatch "[!abc]" "a")
  # =>
  false

  (funmatch "[!abc]" "d")
  # =>
  true

  (funmatch "[!j-m]" "i")
  # =>
  true

  (funmatch "[!j-m]" "n")
  # =>
  true

  (funmatch "[!j-m]" "k")
  # =>
  false

  (funmatch "[]]" "]")
  # =>
  true

  (funmatch "[!]]" "_")
  # =>
  true

  (funmatch "[!]-_]" "a")
  # =>
  true

  (funmatch "[!]-_]" "_")
  # =>
  false

  )

