open Logic
open Logicfunctions

let () =
  print_truth_table
    (truthTable
       "(ALCOHOL -> ~FRUITS) & (FRUITS -> (~ALCOHOL & ~SUGAR)) & (~SUGAR -> \
        ALCOHOL)")
