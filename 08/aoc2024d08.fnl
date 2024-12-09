(fn read-file [filename]
  "Read filename to a string."
  (with-open [f (io.open filename)]
    (f:read :a)))

(fn string.split [self sep]
  "Split a string with separator sep."
  (let [pattern (string.format "([^%s]+)" sep)
        fields {}]
    (self:gsub pattern
               (fn [c]
                 (tset fields (+ 1 (length fields)) c)))
    fields))

(fn string.chars [self]
  "Create a list of each character in a string."
  (fcollect [i 1 (length self)] (self:sub i i)))

(fn combinations [tbl]
  "Return a list of every 2-combination in tbl."
  (let [result []]
    (for [i 1 (- (length tbl) 1)]
      (for [j (+ 1 i) (length tbl)]
        (table.insert result [(. tbl i) (. tbl j)])))
    result))

(fn make-point [y x]
  "Simple (y, x) pair."
  {: y : x})

(fn make-grid [src]
  "Main grid type. Antennae that belong in a group are stored as points
associated with their character in the data table."
  (let [lines (src:split "\n")
        height (length lines)
        width (length (. lines 1))
        data {}]

    (fn hash-point [point]
      "Return a unique value for a point. Finding unique antinodes is done with
a hashset, but points cannot be used as hashset keys because tables are stored
as references in Lua, so two points won't hash to the same value even if they
are identical. This is a workaround."
      (+ point.x (* width point.y)))

    (fn in-bounds? [y x]
      "Determine whether a location is in-bounds for the grid."
      (and (<= 1 y height) (<= 1 x width)))

    (fn antinodes []
      "Find each unique antinode in the grid."
      (let [result {}]
        (each [_ points (pairs data)]
          (each [_ [p1 p2] (ipairs (combinations points))]
            (let [dy (- p2.y p1.y)
                  dx (- p2.x p1.x)
                  anti-1 (make-point (- p1.y dy) (- p1.x dx))
                  anti-2 (make-point (+ p2.y dy) (+ p2.x dx))]
              (when (in-bounds? anti-1.y anti-1.x)
                (tset result (hash-point anti-1) true))
              (when (in-bounds? anti-2.y anti-2.x)
                (tset result (hash-point anti-2) true)))))
        (icollect [_ v (pairs result)] v)))

    (fn antinodes-2 []
      "Find each unique antinode in the grid, but the other way."
      (let [result {}]
        (each [_ points (pairs data)]
          (each [_ [p1 p2] (ipairs (combinations points))]
            (let [dy (- p2.y p1.y)
                  dx (- p2.x p1.x)]
              (var i 0)
              (var done false)
              (while (not done)
                (set done true)
                (let [anti-1 (make-point (- p1.y (* i dy)) (- p1.x (* i dx)))
                      anti-2 (make-point (+ p2.y (* i dy)) (+ p2.x (* i dx)))]
                  (when (in-bounds? anti-1.y anti-1.x)
                    (set done false)
                    (tset result (hash-point anti-1) true))
                  (when (in-bounds? anti-2.y anti-2.x)
                    (set done false)
                    (tset result (hash-point anti-2) true)))
                (set i (+ 1 i))))))
        (icollect [_ v (pairs result)] v)))

    ;; Constructor.
    (each [y line (ipairs lines)]
      (each [x char (ipairs (line:chars))]
        (when (char:match "[%d%l%u]")
          (when (not (. data char))
            (tset data char []))
          (table.insert (. data char) (make-point y x)))))
    {: antinodes : antinodes-2}))

;; Main method.
(let [input (. _G.arg 1)]
  (if input
      (let [(ok data) (pcall read-file input)]
        (if ok
            (let [grid (make-grid data)]
              (io.write (string.format "Antinode count: %d\n"
                                       (length (grid.antinodes))))
              (io.write (string.format "Antinode count 2: %d\n"
                                       (length (grid.antinodes-2)))))
            (io.write (string.format "Could not read '%s'.\n" input))))
      (print "No input file.")))
