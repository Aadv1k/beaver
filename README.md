# cl-beaver 🦫

A common lisp library for data analysis and manipulation. Modelled after the [pandas](https://pandas.pydata.org/) library

## Usage

```lisp
(load "./src/beaver.lisp")
(in-package beaver)

(print
 (beaver:get-column
  (beaver:clean
   (beaver:read-csv "./data/btc.csv"))))
```


### Credits

- [BTC pricing data](https://www.kaggle.com/datasets/sudalairajkumar/cryptocurrencypricehistory)
