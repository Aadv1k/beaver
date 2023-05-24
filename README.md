# beaver 🦫

A common lisp library for data analysis and manipulation. Modelled after the [pandas](https://pandas.pydata.org/) library

- [Walkthrough](#walkthrough)

## Walkthrough

Let's analyze some [bitcoin data](./data/btc.csv), start my modifying `main.lisp`

```lisp
(load "./src/beaver.lisp")

(defvar data (beaver:read-csv "./data/btc.csv"))

(print data) ;; Let's go!
```

Well, let's first remove some columns to simplify our data

```lisp

(load "./src/beaver.lisp")

(defvar data (beaver:read-csv "./data/btc.csv"))

(print (beaver:drop-column data '("Symbol" "Data" "Open" "Close" "Volume" "Name" "SNo")))
```

```shell
(("Date" "High" "Low" "Marketcap")
 ("2013-04-29 23:59:59" "147.48800659179688" "134.0" "1603768864.5")
 ("2013-04-30 23:59:59" "146.92999267578125" "134.0500030517578"
  "1542813125.0")...
```

Now, let's sort the data by the maximum marketcap

```lisp
(load "./src/beaver.lisp")

(defvar data (beaver:read-csv "./data/btc.csv"))

(print (beaver:sort-by data "Marketcap"))
```

```shell
(("SNo" "Name" "Symbol" "Date" "High" "Low" "Open" "Close" "Volume"
  "Marketcap")
 ("68" "Bitcoin" "BTC" "2013-07-05 23:59:59" "80.0" "65.5260009765625"
  "79.98999786376953" "68.43099975585938" "0.0" "778411178.875")
 ("69" "Bitcoin" "BTC" "2013-07-06 23:59:59" "75.0" "66.81999969482422"
  "68.50499725341797" "70.27729797363281" "0.0" "799741618.54")...
```

Next let's check the mean of the `High` cap

```
(load "./src/beaver.lisp")

(defvar data (beaver:read-csv "./data/btc.csv"))

(print (float
        (beaver:get-mean
         (beaver:get-column data "High"))
        ))
```

```shell
6893.326 
```

### Credits

- [BTC pricing data](https://www.kaggle.com/datasets/sudalairajkumar/cryptocurrencypricehistory)
- [Surveymonkey data](https://github.com/kshashank03/Survey-Monkey-Tutorial)
