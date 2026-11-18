(ns bifurcan.bits-test
  (:require
   [clojure.test :refer :all]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [clojure.test.check.clojure-test :as ct :refer (defspec)])
  (:import
   [io.lacuna.bifurcan.utils
    Bits
    BitVector]))

(defn ->binary-string [x]
  (let [s (Long/toBinaryString x)]
    (str
      (->> "0"
        (repeat (- 64 (count s)))
        (apply str))
      s)))

(defn interleaved [bits a b]
  (->> [a b]
    (map ->binary-string)
    (map #(->> % (take-last bits) (apply str)))
    (apply interleave)
    (apply str)))

(defn branching-bit [a b]
  (let [bit (->> [(->> a ->binary-string (apply str))
                  (->> b ->binary-string (apply str))]
              (apply map
                (fn [index a b]
                  (when (not= a b)
                    index))
                (range))
              (remove nil?)
              first)]
    (if bit
      (- 63 bit)
      -1)))

#_(defspec branching-bit-vector-equivalence 1e4
  (prop/for-all [[a b] (->> gen/large-integer
                         (repeat 3)
                         (apply gen/tuple)
                         (repeat 2)
                         (apply gen/tuple))
                 offset (gen/choose 0 64)
                 len    (gen/choose 0 128)]
    ))

(defspec interleave-roundtrip 1e4
  (prop/for-all [a (gen/resize Short/MAX_VALUE gen/pos-int)
                 b (gen/resize Short/MAX_VALUE gen/pos-int)]
    (= (interleaved 16 a b)
      (->> (BitVector/interleave 16 (long-array [a b]))
        (map ->binary-string)
        (apply str)
        (take-last 32)
        (apply str)))))

(defspec branching-bit-equivalence 1e4
  (prop/for-all [a gen/large-integer, b gen/large-integer]
    (= (branching-bit a b) (Bits/branchingBit a b))))

(deftest full-word-mask-regression
  (is (= 0 (Bits/maskBelow 0)))
  (is (= -1 (Bits/maskAbove 0)))
  (is (= Long/MAX_VALUE (Bits/maskBelow 63)))
  (is (= -1 (Bits/maskBelow 64)))
  (is (= 0 (Bits/maskAbove 64)))
  (is (= -1 (Bits/slice -1 0 64)))
  (is (= Long/MIN_VALUE (Bits/slice Long/MIN_VALUE 0 64))))

(deftest debruijn-offset-regression
  (doseq [offset (range 64)]
    (let [one (bit-shift-left 1 offset)
          expected (if (= offset 63) Long/MAX_VALUE (dec one))]
      (is (= offset (Bits/bitOffset one)))
      (is (= expected (Bits/maskBelow offset)))
      (is (= (bit-not expected) (Bits/maskAbove offset))))))
