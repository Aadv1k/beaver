(load "./src/beaver.lisp")
(in-package beaver)

(print
 (beaver:get-column
  (beaver:clean
   (beaver:read-csv "./data/btc.csv"))))
