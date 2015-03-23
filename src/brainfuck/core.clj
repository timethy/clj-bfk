(ns brainfuck.core
  (:gen-class))

; If EOF is read, return 0
(defn- bf-read-char []
  (let [c (.read System/in)]
    (max 0 c)))

(defn bf-find-bracket [prog pos dir]
  (loop [i (+ pos dir) open 0]
    (if (= (get prog i) (if (= 1 dir) \] \[))
      (if (= open 0)
        i
        (recur (+ i dir) (dec open)))
      (if (= (get prog i) (if (= 1 dir) \[ \]))
        (recur (+ i dir) (inc open))
        (recur (+ i dir) open)))))

(def bf-cmds #{\< \> \+ \- \, \. \[ \]})

(defn bf-set [a j x]
  (if (< (count a) j)
    (bf-set (conj a 0) j x)
    (assoc a j x)))

(defn- bf-step [p state]
  (let [[i [a j]] state
        cmd (get p i)
        v (get a j 0)]
    (if (= cmd \.) (-> a (get j 0) (max 0) char print))
    [(inc
       (case cmd
          \[ (if (=    v 0) (bf-find-bracket p i  1) i)
          \] (if (not= v 0) (bf-find-bracket p i -1) i)
          i))
     (case cmd
       \< [a (dec j)]
       \> [a (inc j)]
       \+ [(bf-set a j (inc v)) j]
       \- [(bf-set a j (dec v)) j]
       \, [(bf-set a j (bf-read-char)) j]
       [a j])]))

(defn- bf-finished [p [i _]]
  (= (count p) i))

(defn- bf-exec [prog initial-state]
  (loop [state initial-state i 0]
    (when-not (or (bf-finished prog state) (< i -1))
      (recur (bf-step prog state) (inc i)))))

(defn bf [prog]
  (bf-exec (apply str (filter bf-cmds prog)) [0 [[] 0]]))

(defn bf-file [file]
  (bf (slurp file)))

(defn -main [file & args]
  (bf-file file))

