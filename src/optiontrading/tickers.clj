(ns optiontrading.tickers
  (:require
   [optiontrading.utils :as u]

   [net.cgrand.enlive-html :as enlive]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.data.json :as json]
   [incanter.zoo :as zoo]
   [clj-time.format :as tf]
   [clj-time.core :as tt]
   [clj-time.predicates :as pr]
   [clj-time.local :as l]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clj-http.client :as client]
                                        ;  [structure.ring-buffer :as rb]
                                        ;  [postal.core :as postal]

   [clojure.core.async
    :as a
    :refer [>! <! >!! <!! go chan buffer close! thread
            alts! alts!! timeout]]
   [incanter [core :refer [$]
              :as incanter$]
    [core :as incanter]
    [stats :as stats]
    [io :as io2]
    [charts :as charts]
    [datasets :as dataset]]

   [me.raynes.conch :refer [programs with-programs let-programs] :as sh]
   [optiontrading.utils :as utils]) (:use clojure.pprint)
  ;(:use [clj-highcharts.core])

  (:gen-class))

;; initialize nasdaq and nyse ticker, our trading universe
(do (def HOME u/HOME)
    (def mc-cutoff 1E7)
    (def price-cutoff 20)
    (def adv-cutoff-millions 10)
    (def urls ["https://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download" "https://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NYSE&render=download"])
    (defn retrieve-symbols
      " use the above urls to retrieve ticker list for nyse and nasdaq, cutoff at certain points"
      [mc-cutoff price-cutoff adv-cutoff-millions url] (let [data (io2/read-dataset url :header true)
                                                             nasdaq (incanter/to-dataset data)
                                                             symbols (incanter$/$ :Symbol nasdaq)
                                                             name_ (incanter$/$ :Name nasdaq)
                                                             lastsale (incanter$/$ :LastSale nasdaq)
                                                             market-cap (incanter$/$ :MarketCap nasdaq)
                                                             sector (incanter$/$ :Sector nasdaq)
                                                             industry (incanter$/$ :Industry nasdaq)
                                                             filter-data0 (map vector symbols name_ lastsale market-cap sector industry)
                                                             filter-data1 (filter #(number? (nth % 2)) filter-data0)
                                                             filter-data2-nasdaq (filter #(and (>= (nth % 2) price-cutoff) (>= (nth % 3) mc-cutoff)) filter-data1)]
                                                         filter-data2-nasdaq))

    (def nasdaq-data (retrieve-symbols mc-cutoff price-cutoff adv-cutoff-millions (first urls)))
    (def nyse-data (retrieve-symbols mc-cutoff price-cutoff adv-cutoff-millions (second urls)))
    (def tickers (set (concat (map first nyse-data)  (map first nasdaq-data))))
    (count tickers)
    (def huge-tickers (concat ["SPY"]  tickers))
    (def stock-names-nyse (zipmap (map first nyse-data)  (map second nyse-data)))
    (def stock-names-nasdaq (zipmap (map first nasdaq-data)  (map second nasdaq-data)))
    (def stock-names (merge stock-names-nyse stock-names-nasdaq))
    (count stock-names))

;(contains? (set huge-tickers) "SPY")
;(pprint (take 10 huge-tickers))


;;;;;;;; functions around date decision

(defn formatlocal [n offset]
  (let [nlocal (tt/to-time-zone n (tt/time-zone-for-offset offset))]
    (tf/unparse (tf/formatter-local "yyyy-MM-dd hh:mm:ss aa")
                nlocal)))

(defn currentTime []
  (formatlocal (tt/now) -5))

(defn pad_ [x] (if (= 2 (count (str x))) (identity x) (str "0" x)))

(defn convert_ [date]
  (str
   (tt/year date)
   (pad_ (tt/month date))
   (pad_ (tt/day date))))

(defn convert-javatime-to-yyyymmdd [datetime]
  (tt/date-time (read-string (subs datetime

                                   0 4)) (. Integer parseInt (subs datetime 5 7)) (. Integer parseInt (subs datetime 8 10))))

(def tt_date_  (tt/date-time (read-string (subs (currentTime) 0 4)) (. Integer parseInt (subs (currentTime) 5 7)) (. Integer parseInt (subs (currentTime) 8 10))))

;;asofdata logic
                                        ;(def tt_date_ (tt/date-time 2017 6 4 )  )

(currentTime)
(println "today is..")
(println tt_date_)
(identity tt_date_)

(def tt_date_1 (tt/minus tt_date_ (tt/days 1)))
(def tt_date_2 (tt/minus tt_date_ (tt/days 2)))
(def tt_date_3 (tt/minus tt_date_ (tt/days 3)))
(def tt_date_4 (tt/minus tt_date_ (tt/days 4)))
(def tt_date_5 (tt/minus tt_date_ (tt/days 5)))

(def tt_date_14 (tt/minus tt_date_ (tt/days 14)))
(def tt_date_5 (tt/minus tt_date_ (tt/days 5)))
(def tt_date_30 (tt/minus tt_date_ (tt/days 30)))
(def tt_date_96 (tt/minus tt_date_ (tt/days 96)))
(def tt_date_360 (tt/minus tt_date_ (tt/days 360)))

(def startDate (convert_ tt_date_14))
(identity startDate)

;; does all logic of weekend adjusting
(def today_
  (cond (pr/saturday? tt_date_) (identity  (str
                                            (tt/year tt_date_1)
                                            (pad_ (tt/month tt_date_1))
                                            (pad_ (tt/day tt_date_1))))
        (pr/sunday? tt_date_) (identity  (str
                                          (tt/year tt_date_2)
                                          (pad_ (tt/month tt_date_2))
                                          (pad_ (tt/day tt_date_2))))
        :else (convert_ tt_date_)))

(identity today_)
(def today_int (atom (read-string today_)))
                                        ;(def today_int 20170928)
                                        ;(identity yesterday_)


(def yesterday_
  ;; if saturday this is t-2
  ;; if sunday this t-3
  ;; else t-1
  (cond (pr/saturday? tt_date_) (identity  (str
                                            (tt/year tt_date_2)
                                            (pad_ (tt/month tt_date_2))
                                            (pad_ (tt/day tt_date_2))))
        (pr/sunday? tt_date_) (identity  (str
                                          (tt/year tt_date_3)
                                          (pad_ (tt/month tt_date_3))
                                          (pad_ (tt/day tt_date_3))))

        (pr/monday? tt_date_) (identity  (str
                                          (tt/year tt_date_3)
                                          (pad_ (tt/month tt_date_3))
                                          (pad_ (tt/day tt_date_3))))
        :else (convert_ tt_date_1)))

;;;;;;; functions around date decision


;; find advs


(defn dailyBars-iex
  [tic]
  (let [tail

        (str "https://api.iextrading.com/1.0/stock/" tic "/chart/5y?format=csv" )

        ]
    (:body (client/get tail))))




;(take 2 (str/split (dailyBars-iex "AAPL") #"\r\n"))





;; new define from iEX


(def apple-data (clojure.string/split (dailyBars-iex "AAPL") #"\r\n"))
(def N (count apple-data))
                                       (identity N)

(first apple-data)
;;;find advs

(defn ohlcv-dumb [tic]
  (let [apple-data (clojure.string/split (dailyBars-iex tic) #"\r\n")
        N-local (count apple-data)
        data (map #(clojure.string/split % #",") apple-data)
        close-price (map read-string (map  #(nth % 4) data))
        open-price  (map read-string (map #(nth % 1) data))
        high-price  (map read-string (map  #(nth % 2) data))
        low-price  (map read-string (map  #(nth % 3) data))
        volumes  (map read-string (map  #(nth % 5) data))
        avg-price (stats/mean close-price)
        stock-type (cond (<= avg-price 15) "small"
                         (>= avg-price 90) "large"
                         :else "mid")
        adv-in-millions (u/round2 2 (/ (stats/mean (map * volumes close-price)) 1E6))
        d [0]  ;; dummy data
        good-data {:aprice avg-price :type stock-type :aticker tic :status "Active" :o open-price :c close-price :h high-price :l low-price :v volumes :adv adv-in-millions}

        bad-data {:aprice 100 :type stock-type :aticker tic :status "Inactive" :o d :c d :h d :l d :v d :adv 0}] (if (< N-local N) (identity bad-data) (identity good-data))))

(defn ohlcv-dumb2 [tic]
  (let [apple-data (clojure.string/split (dailyBars-iex tic) #"\r\n")
        N-local (count apple-data)
        data (rest (map #(clojure.string/split % #",") apple-data))
        close-price (map read-string (map  #(nth % 4) data))
        dates-long (map  #(nth % 0) data)
        dates dates-long

        open-price  (map read-string (map #(nth % 1) data))
        high-price  (map read-string (map  #(nth % 2) data))
        low-price  (map read-string (map  #(nth % 3) data))
        volumes  (map read-string (map  #(nth % 5) data))
        avg-price (stats/mean close-price)
        stock-type (cond (<= avg-price 15) "small"
                         (>= avg-price 90) "large"
                         :else "mid")
        adv-in-millions (u/round2 2 (/ (stats/mean (map * volumes close-price)) 1E6))
        d [0]  ;; dummy data
        good-data {:aprice avg-price :type stock-type :aticker tic :status "Active" :o open-price :c close-price :h high-price :l low-price :v volumes :adv adv-in-millions :dates dates}] (if (< N-local N) (identity nil) (identity good-data))))

                                        ;(map #(take 10 (second %)) (ohlcv-dumb2 "TSLA"))


                                        ;(map class (map second (ohlcv-dumb2 "TSLA")))







(def ohlcv (clojure.core/memoize ohlcv-dumb2))


;(ohlcv "AMD")
;; new define from IEX
(defn recent-dates []
  (:dates (ohlcv "AAPL")))

;;;; finished tracking all eligible tickers for trading, now see earnings page.


(defn ema [HL values]
  (reductions (fn [running v]
                (let [f (/ 2 (+ 1 HL))
                      one-minus-F (- 1 f)] ;naming intermediate results can help with the readability of non-associative operators.
                  (+ (* f v)
                     (* one-minus-F running))))
              values))

;(def ohlcv-data (map ohlcv (take 10 (concat (map first filter-data2-nyse)  (map first filter-data2-nasdaq)))))


;; being rank indicators


(defn trend
  "simple trend using price series
indicators are maps with header and dates

"
  [fast slow aticker]
  (let [tech (ohlcv aticker)] (if (nil? tech) (identity nil)
                                  (let [p (:c tech)
                                        dates (rest (:dates tech))
                                        rets  (map u/return_bps (rest p)  (drop-last p))
                                        adj-price (u/csum (u/winsorize 2 rets))
                                        trend (map - (ema fast p) (ema slow p))]
                ;(count dates)
                                    {:data (utils/lag 1  (map (partial u/round2 2)  trend))
                                     :dates dates
                                     :header "trend"}))))


(defn delta-y
  "simple trend using price series
indicators are maps with header and dates

"
  [days aticker]
  (let [tech (ohlcv aticker)] (if (nil? tech) (identity nil)
                                  (let [p (:c tech)
                                        dates (rest (:dates tech))


                                        delta (map - (utils/lookahead days p) p)]
                ;(count dates)
                                    {:data (utils/lag 1  (map (partial u/round2 2)  delta))
                                     :dates dates
                                     :header "delta-y"}))))




;(println (filter #(< % (- 3)) (zscore rets)))
;(utils/lPlot2 (:data (delta-y 20 "TECH"))  (:data (trend 10 20 "TECH"))  )


(comment (utils/lPlot2

  (:c (ohlcv "TECH"))
  (:c (ohlcv "TECH"))

  ))


                                        ;(:data (trend 10 20 "TSLA"))

(use 'clojure.set)





(defn calc-correlation
"calculates correlation between trend signal and delta y"
  [tic]
  (println tic)
  (try
(let [ one (:data (delta-y 20 tic))
        two (:data (trend 10 20 tic))
        ]
(cond (or (nil? one)
            (nil? two)
            ) nil
        :else  (utils/round (stats/correlation (rest one) (rest two)))
        )
      )
    (catch Exception e nil))

    )
;(calc-correlation "SNX")
(def tickers (take 30 tickers))

;( correlation-map (map vector (take 10 tickers) (map calc-correlation (take 10 tickers)))   )


;(pprint (sort-by second signals ))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")






(def correlation-map (let [ correlation-map (map vector tickers (map calc-correlation tickers))
       ]
   (sort-by second (remove #(nil? (second %)) correlation-map))))


(def mean-reverting-tickers-corr correlation-map)
(println mean-reverting-tickers-corr)



;; anything below this is considered mean reverting
(def correlation-cut (stats/quantile  (map second mean-reverting-tickers-corr) :probs 0.05))


(def mean-reverting-tickers (map first (filter #(< (second %) correlation-cut)  mean-reverting-tickers-corr )) )

(pprint "Mean reverting tickers...")
(pprint mean-reverting-tickers)

(comment (utils/lPlot (utils/csum (drop 1 (map * (:data (delta-y 20 "SNX" )) (map - (:data (trend 10 20 "SNX"))))))))

(def signals (map vector mean-reverting-tickers (map #(- (last (:data (trend 10 20 %)))) mean-reverting-tickers)))








(pprint "Long short signals...")
  (pprint (sort-by second signals ))

(spit (str utils/HOME "/data/optiontrading_outfile.txt")  (pr-str signals) )

  )



;(utils/lPlot (:data (trend 10 20 "tech")))
;(utils/lPlot  (take-last 20 (:c (ohlcv "tech"))))


;(utils/lPlot (take-last 20 (:data (trend 10 20 "tech"))))


;(map vector (:dates (ohlcv "TSLA"))    (:c (ohlcv "TSLA")))

;(utils/lPlot2 (ema 1 (:c (ohlcv "TSLA")))    (ema 20 (:c (ohlcv "TSLA"))))

;;test for winsorize


;(conj (ATR-bps "AAPL") -99 )
