# 1. parse glob pattern into pieces
# 2. create peg from pieces

# keywords: glob, fnmatch, wildmat

# references
#
# * fnmatch(3)
# * glob(7)
# * https://en.wikipedia.org/wiki/Glob_(programming)

# features
#
# * * - zero or more characters (except . if leading part of pattern)
# * ? - one character (except . if leading part of pattern)
# * [x-y] - one character in the range x <= y
# * [!x-y] - one character not in the range x <= y
# * [xyz] - one character in the set x, y, ..., z
# * [!xyz] - one character not in the set x, y, ..., z

# todo
#
# * randomized testing
#
# * testing that compares with results from using ls with multiple
#   shells

# XXX: things not intended to handle?
#
#      [a!] - bash, zsh: event not found
#
#      [a!b] - bash, zsh: event not found
#
#      [!] - bash: no such file or directory - matches literally
#            zsh: event not found (whether file exists or not)
#
#      [c-a] - bash: literal match
#              zsh: no matches found (even if file exists) - use quotes
#
#      [c-a]* - bash: literal match
#               zsh: no matches found (even if file exists) - use quotes
#
#      ** - behavior differs among shells and may be complex to
#           implement as it typically involves paths beneath
#           the current directory
#
#      how to reject these as patterns?
#
# XXX: how / whether to handle...
#
#      * consecutive *s
#
#      * consider not supporting some other characters.  candidates
#        might be listed here:
#
#        https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file
#        https://stackoverflow.com/a/31976060
#
#        following should be avoided on windows (ascii order):
#
#        " (double quote)
#        * (asterisk)
#        / (forward slash)
#        : (colon)
#        < (less than)
#        > (greater than)
#        ? (question mark)
#        \ (backslash)
#        | (vertical bar or pipe)
#
#        additionally might want to avoid in windows:
#
#        ; (semicolon) - separator for things like PATH
#
#        better to avoid on posix too?
#
#        / (forward slash) - separator in paths
#        : (colon) - separator for things like PATH
#        < (less than) - used for redirection
#        > (greater than) - used for redirection
#        \ (backslash) - awkward because of escaping
#        | (vertical bar or pipe) - used when chaining commands
#
#        requires some care to use:
#
#        ! (exclamation point) - used for negation
#        * (asterisk) - used for zero or more
#        ? (question mark) - used for some one character
#        [ (opening / left square bracket) - delimiter for ranges and sets
#        ] (closing / right square bracket) - delimiter for ranges and sets
#        - (minus) - used in ranges
#
#        emit warning when detected in patterns?

(defn parse-bang
  [& args]
  # cmt has matched [!<some-stuff>]
  #
  # case a: negative range
  #
  # [!s-t] -> neg-range
  #
  # case b: negative set
  #
  # [!a] -> neg-set
  # [!ab] -> neg-set
  # [!-] -> neg-set
  # [!-a] -> neg-set
  (if-let [_ (= 3 (length args))
           [begin mid end] args
           _ (= "-" mid)
           _ (<= begin end)]
    {:type :neg-range
     :begin begin
     :end end}
    {:type :neg-set
     :items (sort (distinct args))}))

# XXX: could build peg directly?
#      maintenance might be harder?
#      debugging might be harder?
(defn parse-pattern
  [pattern]
  (def m
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
                   ,parse-bang)
        :rng (cmt (sequence "["
                            (capture 1)
                            "-"
                            (capture (sequence (not "]") 1))
                            "]")
                  ,|{:type :range
                     :begin $0
                     :end $1})
        :set (cmt (sequence "["
                            (opt (capture "]"))
                            (any (sequence (not "]") (capture 1)))
                            "]")
                  ,|{:type :set
                     :items (sort (distinct $&))})
        :lit (capture (sequence 1))}
      pattern))
  # massage results so that consecutive strings become continguous
  (when m
    (def results @[])
    (def buf @"")
    (each item m
      (if (dictionary? item)
        (do
          (when (not (empty? buf))
            (array/push results (string buf))
            (buffer/clear buf))
          (array/push results item))
        # item should be a string, append it to the buffer
        (buffer/push buf item)))
    # save any remaining bits in the buffer
    (when (not (empty? buf))
      (array/push results (string buf)))
    #
    results))

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

  (parse-pattern "test.*anet")
  # =>
  @["test."
    {:type :asterisk}
    "anet"]

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

  (parse-pattern "[+--]")
  # =>
  @[{:begin "+"
     :end "-"
     :type :range}]

  (parse-pattern "[--0]")
  # =>
  @[{:begin "-"
     :end "0"
     :type :range}]

  (parse-pattern "[---]")
  # =>
  @[{:begin "-"
     :end "-"
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

  (parse-pattern "[aabb]")
  # =>
  @[{:items @["a" "b"]
     :type :set}]

  (parse-pattern "[abba]")
  # =>
  @[{:items @["a" "b"]
     :type :set}]

  (parse-pattern "[S-]")
  # =>
  @[{:items @["-" "S"]
     :type :set}]

  (parse-pattern "[-axz]")
  # =>
  @[{:items @["-" "a" "x" "z"]
     :type :set}]

  (parse-pattern "[--]")
  # =>
  @[{:items @["-"]
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
  @[{:begin "-" :end "0"
     :type :neg-range}]

  (parse-pattern "[!s-t]")
  @[{:begin "s" :end "t"
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

(defn make-peg-helper
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
    (cond
      (string? p)
      (array/push scratch p)
      #
      (= :asterisk (get p :type))
      (array/push scratch p) # handle in pass 2
      #
      (= :question (get p :type))
      (array/push scratch 1)
      #
      (= :range (get p :type))
      (array/push scratch
                  ['range (string (get p :begin) (get p :end))])
      #
      (= :set (get p :type))
      (array/push scratch
                  ['set (string/join (get p :items))])
      #
      (= :neg-range (get p :type))
      (array/push scratch
                  ['not ['range (string (get p :begin) (get p :end))]]
                  1)
      #
      (= :neg-set (get p :type))
      (array/push scratch
                  ['not ['set (string/join (get p :items))]]
                  1)
      #
      (errorf "unexpected item: %n" p)))
  # finish pass 1
  (array/push scratch -1)
  # pass 2: modify to handle asterisks
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

  (make-peg-helper @[])
  # =>
  ~(sequence -1)

  (make-peg-helper @["hi"])
  # =>
  ~(sequence "hi"
             -1)

  (make-peg-helper @[{:type :asterisk}])
  # =>
  ~(sequence (not ".")
             (to (sequence -1))
             -1)

  (make-peg-helper @[".jane" {:type :asterisk}])
  # =>
  ~(sequence ".jane"
             (to (sequence -1))
             -1)

  (make-peg-helper @[{:type :asterisk} ".janet"])
  # =
  ~(sequence (to (sequence ".janet" -1))
             ".janet"
             -1)

  (make-peg-helper @["a" {:type :asterisk} "janet"])
  # =>
  ~(sequence "a"
             (to (sequence "janet" -1))
             "janet"
             -1)

  (make-peg-helper @["test."
                     {:type :asterisk}
                     "anet"])
  # =>
  ~(sequence "test."
             (to (sequence "anet" -1))
             "anet"
             -1)

  (make-peg-helper @["a"
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

  (make-peg-helper @[{:type :question}])
  # =>
  ~(sequence (not ".")
             1
             -1)

  (make-peg-helper @["jane" {:type :question}])
  # =>
  ~(sequence "jane" 1 -1)

  (make-peg-helper @[{:type :question} "anet"])
  # =>
  ~(sequence (not ".")
             1 
             "anet" 
             -1)

  (make-peg-helper @[{:type :question}
                     {:type :asterisk}])
  # =>
  ~(sequence (not ".")
             1
             (to (sequence -1))
             -1)

  (make-peg-helper @["a"
                     {:type :asterisk}
                     {:type :question}])
  # =>
  ~(sequence "a"
             (to (sequence 1 -1))
             1
             -1)

  (make-peg-helper @[{:begin "t"
                      :end "z"
                      :type :range}])
  # =>
  ~(sequence (range "tz") -1)

  (make-peg-helper @[{:begin "+"
                      :end "-"
                      :type :range}])
  # =>
  ~(sequence (range "+-") -1)

  (make-peg-helper @[{:begin "-"
                      :end "0"
                      :type :range}])
  # =>
  ~(sequence (range "-0") -1)

  (make-peg-helper @[{:begin "-"
                      :end "-"
                      :type :range}])
  # =>
  ~(sequence (range "--") -1)

  (make-peg-helper @[{:type :asterisk}
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

  (make-peg-helper @[{:items @["w" "x" "z"]
                      :type :set}])
  # =>
  ~(sequence (set "wxz") -1)

  (make-peg-helper @[{:items @["a" "b"]
                      :type :set}])
  # =>
  ~(sequence (set "ab") -1)

  (make-peg-helper @[{:items @["S" "-"]
                      :type :set}])
  # =>
  ~(sequence (set "S-") -1)

  (make-peg-helper @[{:items @["-" "a" "x" "z"]
                      :type :set}])
  # =>
  ~(sequence (set "-axz") -1)

  (make-peg-helper @[{:items @["-"]
                      :type :set}])
  # =>
  ~(sequence (set "-") -1)

  (make-peg-helper @[{:items @["a"]
                      :type :neg-set}])
  # =>
  ~(sequence (not (set "a"))
             1
             -1)

  (make-peg-helper @[{:items @["a" "b"]
                      :type :neg-set}])
  # =>
  ~(sequence (not (set "ab")) 
             1
             -1)

  (make-peg-helper @[{:items @["-"]
                      :type :neg-set}])
  # =>
  ~(sequence (not (set "-")) 
             1
             -1)

  (make-peg-helper @[{:begin "!" :end "a"
                      :type :range}])
  # =>
  ~(sequence (range "!a") -1)

  (make-peg-helper @[{:begin "-" :end "0"
                      :type :neg-range}])
  # =>
  ~(sequence (not (range "-0")) 1 -1)

  (make-peg-helper @[{:begin "s" :end "t"
                      :type :neg-range}])
  # =>
  ~(sequence (not (range "st")) 1 -1)

  (make-peg-helper @[{:items @["]"]
                      :type :neg-set}])
  # =>
  ~(sequence (not (set "]")) 1 -1)

  (make-peg-helper @[{:begin "]"
                      :end "_"
                      :type :neg-range}])
  # =>
  ~(sequence (not (range "]_")) 1 -1)

  )

(defn make-peg
  [pattern]
  (def parsed (parse-pattern pattern))
  (assertf parsed "failed to parse pattern: %s" pattern)
  #
  (make-peg-helper parsed))

(defn funmatch
  [patt str &opt flags]
  (def peg (make-peg patt))
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

