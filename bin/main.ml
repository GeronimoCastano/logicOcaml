open Logic
open Logicfunctions

let () =
  print_truth_table
    (truth_table
       "(ALCOHOL -> ~FRUITS) & (FRUITS -> (~ALCOHOL & ~SUGAR)) & (~SUGAR -> \
        ALCOHOL)")
