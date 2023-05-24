(load "./src/beaver.lisp")
(in-package beaver)

(print
 (beaver:get-median
 (beaver:get-column
   (beaver:read-csv "./data/btc.csv") "SNo")))
