(ns projector.core
  (:require [clojure.core.match :refer [match]]))

; The error monad
(defn return [x] [:success x])
(defn bind [m f]
  (match m
         [:success x] (f x)
         failure      failure))

; Convenience constructors
(defn fail [x] [:failure x])
(def succeed return)

; A generic workflow
(defn workflow [& steps]
  (fn [x] (reduce bind (return x) steps)))

; A blank workflow
(def rubber-stamp (workflow succeed succeed succeed succeed))

; Convert a predicate and a message into a workflow stage
(defn check [ok? message]
  (fn [x]
    (if (ok? x)
      (succeed x)
      (fail message))))

; A workflow for parsing a natural number from a string payload. 
(defn parseInt [s]
  (try
    (succeed (Integer/parseInt s))
    (catch java.lang.NumberFormatException e
      (fail "Not a number"))))

(def natural
  (workflow
    (check (complement empty?) "Not non-empty")
    (check (partial re-find #"\d+") "Not numeric")
    parseInt
    (check pos? "Not positive")))

(defn handle [{:keys [body]}]
  (match (natural body)
         [:success x] (format "The number was %d." x)
         [:failure message] (format "Failed because: '%s'." message)))

; A workflow for parsing a rational number from two fields in a body.
(defn default [m] #(succeed (merge m %)))
(defn parseRatio [{:keys [numerator denominator]}]
  (match [(parseInt numerator) (parseInt denominator)]
         [[:success n] [:success d]] (succeed (/ n d))
         [_ _] (fail "Couldn't parse")))

(def rational 
  (workflow
    (check #(contains? % :numerator) "Numerator missing")
    (default {:denominator "1"})
    parseRatio))

(defn handle-ratio [{:keys [body]}]
  (match (rational body)
         [:success x] (format "The number was %s." (str x))
         [:failure message] (format "Failed because: '%s'." message)))
