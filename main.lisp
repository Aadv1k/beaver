(load "./src/beaver.lisp")
(load "./src/utils.lisp")

(defvar data (beaver:read-csv "./data/btc.csv"))

(print
  (beaver:sort-by data "High")
)
