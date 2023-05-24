(load "./src/beaver.lisp")

(defvar data (beaver:read-csv "./data/btc.csv"))

(print (beaver:drop-column data '("Symbol" "Data" "Open" "Close" "Volume" "Name" "SNo")))
