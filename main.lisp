(load "./src/beaver.lisp")
(load "./src/utils.lisp")

(defvar data (beaver:read-csv "./data/btc.csv"))

(print
  (beaver:drop-column data '("Name" "SNo" "Symbol"))
)
