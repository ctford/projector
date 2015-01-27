(ns projector.core-test
  (:require [clojure.test :refer :all]
            [projector.core :refer :all]))

(deftest test-sham 
  (testing "rubber-stamp does very little"
    (is (= [:success "value"] (rubber-stamp "value")))))

(deftest test-natural 
  (testing "natural wants positive integers"
    (is (= [:failure "Not non-empty"] (natural "")))
    (is (= [:failure "Not numeric"] (natural "foo")))
    (is (= [:failure "Not positive"] (natural "0")))
    (is (= [:success 7] (natural "7")))))

(deftest test-natural-handler
  (testing "handle parses the body"
    (is (= "Failed because: 'Not numeric'." (handle {:body "foo"})))
    (is (= "The number was 7." (handle {:body "7"})))))

(deftest test-rational
  (testing "rational parses the body for a numerator and a denominator"
    (is (= [:failure "Numerator missing"] (rational {})))
    (is (= [:failure "Couldn't parse"] (rational {:numerator "foo"})))
    (is (= [:success 2] (rational {:numerator "2"}))))
    (is (= [:success 2/3] (rational {:numerator "2" :denominator "3"}))))

(deftest test-rational-handler
  (testing "handle parses the body for a numerator and a denominator"
    (is (= "Failed because: 'Numerator missing'." (handle-ratio {:body {}})))
    (is (= "Failed because: 'Couldn't parse'." (handle-ratio {:body {:numerator "foo"}})))
    (is (= "The number was 2/3." (handle-ratio {:body {:numerator "2" :denominator "3"}})))
    (is (= "The number was 2." (handle-ratio {:body {:numerator "2"}})))))
