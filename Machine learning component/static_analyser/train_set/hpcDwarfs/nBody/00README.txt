Transformation sequences 2 arrays version:

Sequence 1:
   nbody_2arr_1_1.c: function inlining (Generate OpenCL - 2 kernels)

Sequence 2:
   nbody_2arr_1_1.c: function inlining
   nbody_2arr_2_2.c: Normalize for loop
   nbody_2arr_2_3.c: loop scheduling (Generate OpenCL - 2 kernels, SM)

Sequence 3:
   nbody_2arr_1_1.c: function inlining
   nbody_2arr_3_2.c: normalize for loop
   nbody_2arr_3_3.c: normalize for loop
   nbody_2arr_3_4.c: insert stms into for loop
   nbody_2arr_3_5.c: insert stms into for loop
   nbody_2arr_3_6.c: loop collapse (Generate OpenCL)

Sequence 4:
   nbody_2arr_1_1.c: function inlining
   nbody_2arr_4_1.c:  split var decl-assign
   nbody_2arr_4_2.c:  split var decl-assign
   nbody_2arr_4_3.c:  split var decl-assign
   nbody_2arr_4_4.c:  split var decl-assign
   nbody_2arr_4_5.c:  split var decl-assign
   nbody_2arr_4_6.c:  split var decl-assign
   nbody_2arr_4_7.c:  split var decl-assign
   nbody_2arr_4_8.c:  split var decl-assign
   nbody_2arr_4_9.c:  split var decl-assign

   nbody_2arr_4_10.c: roll-up stmts (identify features to apply this rule)
   nbody_2arr_4_11.c: roll-up stmts
   nbody_2arr_4_12.c: roll-up stmts

   nbody_2arr_4_13.c: expand +=
   nbody_2arr_4_14.c: expand +=
   nbody_2arr_4_15.c: expand +=
   nbody_2arr_4_16.c: expand +=
   nbody_2arr_4_17.c: expand +=
   nbody_2arr_4_18.c: expand +=

   nbody_2arr_4_19.c: insert stms into for loop

   nbody_2arr_4_20.c: normalize for loop
   nbody_2arr_4_21.c: loop collapse
   nbody_2arr_4_22.c: if_to_ternary
   nbody_2arr_4_23.c: if_to_ternary
   nbody_2arr_4_24.c: if_to_ternary
   nbody_2arr_4_25.c: if_to_ternary
   nbody_2arr_4_26.c: if_to_ternary
   nbody_2arr_4_27.c: if_to_ternary
   nbody_2arr_4_28.c: insert stms into for loop
   nbody_2arr_4_29.c: insert stms into for loop

   nbody_2arr_4_30.c: if_to_ternary
   nbody_2arr_4_31.c: if_to_ternary
   nbody_2arr_4_32.c: if_to_ternary
   nbody_2arr_4_33.c: if_to_ternary
   nbody_2arr_4_34.c: if_to_ternary
   nbody_2arr_4_35.c: if_to_ternary

   nbody_2arr_4_36.c: substitute expr (identify features/conditions to apply this rule. E.g. substitute only in first appearance not all)
   nbody_2arr_4_37.c: substitute expr
   nbody_2arr_4_38.c: substitute expr

   nbody_2arr_4_39.c: extract common expr from ternary
   nbody_2arr_4_40.c: extract common expr from ternary
   nbody_2arr_4_41.c: extract common expr from ternary

   nbody_2arr_4_42.c: normalize for loop
   nbody_2arr_4_43.c: loop collapse
   nbody_2arr_4_44.c: negative_index_scheme

(Generate MaxJ, OpenCL)

Sequence 5:
   nbody_2arr_1_1.c: function inlining
   nbody_2arr_4_1.c:  split var decl-assign
   nbody_2arr_4_2.c:  split var decl-assign
   nbody_2arr_4_3.c:  split var decl-assign
   nbody_2arr_4_4.c:  split var decl-assign
   nbody_2arr_4_5.c:  split var decl-assign
   nbody_2arr_4_6.c:  split var decl-assign
   nbody_2arr_4_7.c:  split var decl-assign
   nbody_2arr_4_8.c:  split var decl-assign
   nbody_2arr_4_9.c:  split var decl-assign

   nbody_2arr_5_1.c:  expand +=
   nbody_2arr_5_2.c:  expand +=
   nbody_2arr_5_3.c:  expand +=
   nbody_2arr_5_4.c:  expand +=
   nbody_2arr_5_5.c:  expand +=
   nbody_2arr_5_6.c:  expand +=

   nbody_2arr_5_7.c:  normalize for loop

   nbody_2arr_5_8.c:  insert stms into for loop
   nbody_2arr_5_9.c:  if_to_ternary
   nbody_2arr_5_10.c: if_to_ternary
   nbody_2arr_5_11.c: if_to_ternary

   nbody_2arr_5_12.c: insert stms into for loop
   nbody_2arr_5_13.c: if_to_ternary
   nbody_2arr_5_14.c: if_to_ternary
   nbody_2arr_5_15.c: if_to_ternary

   nbody_2arr_5_16.c: roll-up stmts (identify features/conditions to apply this rule)
   nbody_2arr_5_17.c: roll-up stmts
   nbody_2arr_5_18.c: roll-up stmts

   nbody_2arr_5_19.c: insert stms into for loop
   nbody_2arr_5_20.c: if_to_ternary
   nbody_2arr_5_21.c: if_to_ternary
   nbody_2arr_5_22.c: if_to_ternary
   nbody_2arr_5_23.c: if_to_ternary
   nbody_2arr_5_24.c: if_to_ternary
   nbody_2arr_5_25.c: if_to_ternary
   nbody_2arr_5_26.c: if_to_ternary
   nbody_2arr_5_27.c: if_to_ternary
   nbody_2arr_5_28.c: if_to_ternary
   nbody_2arr_5_29.c: if_to_ternary
   nbody_2arr_5_30.c: if_to_ternary
   nbody_2arr_5_31.c: if_to_ternary

   nbody_2arr_5_32.c: extract common expr from ternary
   nbody_2arr_5_33.c: extract common expr from ternary
   nbody_2arr_5_34.c: extract common expr from ternary

   nbody_2arr_5_35.c: substitute expr
   nbody_2arr_5_36.c: substitute expr
   nbody_2arr_5_37.c: substitute expr

   nbody_2arr_5_38.c: loop collapse

   nbody_2arr_5_39.c: normalize for loop
   nbody_2arr_5_40.c: loop collapse

Sequence 6:
   nbody_2arr_1_1.c: function inlining
   nbody_2arr_4_1.c:  split var decl-assign
   nbody_2arr_4_2.c:  split var decl-assign
   nbody_2arr_4_3.c:  split var decl-assign
   nbody_2arr_4_4.c:  split var decl-assign
   nbody_2arr_4_5.c:  split var decl-assign
   nbody_2arr_4_6.c:  split var decl-assign
   nbody_2arr_4_7.c:  split var decl-assign
   nbody_2arr_4_8.c:  split var decl-assign
   nbody_2arr_4_9.c:  split var decl-assign

   nbody_2arr_4_10.c: roll-up stmts (identify features/conditions to apply this rule)
   nbody_2arr_4_11.c: roll-up stmts
   nbody_2arr_4_12.c: roll-up stmts

   nbody_2arr_4_13.c: expand +=
   nbody_2arr_4_14.c: expand +=
   nbody_2arr_4_15.c: expand +=
   nbody_2arr_4_16.c: expand +=
   nbody_2arr_4_17.c: expand +=
   nbody_2arr_4_18.c: expand +=

   nbody_2arr_6_19.c: normalize for loop

   nbody_2arr_6_20.c: insert stms into for loop
   nbody_2arr_6_21.c: if_to_ternary
   nbody_2arr_6_22.c: if_to_ternary
   nbody_2arr_6_23.c: if_to_ternary

   nbody_2arr_6_24.c: insert stms into for loop
   nbody_2arr_6_25.c: if_to_ternary
   nbody_2arr_6_26.c: if_to_ternary
   nbody_2arr_6_27.c: if_to_ternary

   nbody_2arr_6_28.c: roll-up stmts (identify features/conditions to apply this rule)
   nbody_2arr_6_29.c: roll-up stmts
   nbody_2arr_6_30.c: roll-up stmts

   nbody_2arr_6_31.c: roll-up stmts (identify features/conditions to apply this rule)
   nbody_2arr_6_32.c: roll-up stmts
   nbody_2arr_6_33.c: roll-up stmts

   nbody_2arr_6_34.c: roll-up stmts (identify features/conditions to apply this rule)
   nbody_2arr_6_35.c: roll-up stmts
   nbody_2arr_6_36.c: roll-up stmts

   nbody_2arr_6_37.c: loop fusion
   nbody_2arr_6_38.c: insert stms into for loop
   nbody_2arr_6_39.c: if_to_ternary
   nbody_2arr_6_40.c: if_to_ternary
   nbody_2arr_6_41.c: if_to_ternary

   nbody_2arr_6_42.c: loop fusion   (It is tricky to continue from here...)
                                     - Next step would be loop fusion to join computations
                                       of the body of j-loop. But fusing the 2 k-loops is
                                       not easy due to the data dependency imposed by k==2

   nbody_2arr_6_43.c: insert stms into for loop
   nbody_2arr_6_44.c: insert wrapping if into for loop	
   nbody_2arr_6_45.c: if_to_ternary
   nbody_2arr_6_46.c: if_to_ternary

   nbody_2arr_6_47.c: insert stms into for loop
   nbody_2arr_6_48.c: if_to_ternary
   nbody_2arr_6_49.c: if_to_ternary
   nbody_2arr_6_50.c: if_to_ternary
   nbody_2arr_6_51.c: if_to_ternary
   nbody_2arr_6_52.c: if_to_ternary

   nbody_2arr_6_53.c: loop_colapse (with more steps, it is not clear how to obtaine code
   		      		    in a good shape to mecanically generate correct Maxj 
                                    code for the summation part)

Sequence 7:
   nbody_2arr_1_1.c: function inlining
   nbody_2arr_4_1.c:  split var decl-assign
   nbody_2arr_4_2.c:  split var decl-assign
   nbody_2arr_4_3.c:  split var decl-assign
   nbody_2arr_4_4.c:  split var decl-assign
   nbody_2arr_4_5.c:  split var decl-assign
   nbody_2arr_4_6.c:  split var decl-assign
   nbody_2arr_4_7.c:  split var decl-assign
   nbody_2arr_4_8.c:  split var decl-assign
   nbody_2arr_4_9.c:  split var decl-assign

   nbody_2arr_4_10.c: roll-up stmts (identify features/conditions to apply this rule)
   nbody_2arr_4_11.c: roll-up stmts
   nbody_2arr_4_12.c: roll-up stmts

   nbody_2arr_4_13.c: expand +=
   nbody_2arr_4_14.c: expand +=
   nbody_2arr_4_15.c: expand +=
   nbody_2arr_4_16.c: expand +=
   nbody_2arr_4_17.c: expand +=
   nbody_2arr_4_18.c: expand +=

   nbody_2arr_6_19.c: normalize for loop

   nbody_2arr_6_20.c: insert stms into for loop
   nbody_2arr_6_21.c: if_to_ternary
   nbody_2arr_6_22.c: if_to_ternary
   nbody_2arr_6_23.c: if_to_ternary

   nbody_2arr_7_24.c: substitute expr
   nbody_2arr_7_25.c: substitute expr
   nbody_2arr_7_26.c: substitute expr

   nbody_2arr_7_27.c: insert stms into for loop
   nbody_2arr_7_28.c: if_to_ternary
   nbody_2arr_7_29.c: if_to_ternary
   nbody_2arr_7_30.c: if_to_ternary

   nbody_2arr_7_31.c: roll-up stmts (identify features/conditions to apply this rule)
   nbody_2arr_7_32.c: roll-up stmts
   nbody_2arr_7_33.c: roll-up stmts

   nbody_2arr_7_34.c: roll-up stmts (identify features/conditions to apply this rule)
   nbody_2arr_7_35.c: roll-up stmts
   nbody_2arr_7_36.c: roll-up stmts

   nbody_2arr_7_37.c: insert stms into for loop
   nbody_2arr_7_38.c: if_to_ternary
   nbody_2arr_7_39.c: if_to_ternary
   nbody_2arr_7_40.c: if_to_ternary

   nbody_2arr_7_41.c: loop fusion   (It is tricky to continue from here...)
                                     - Next step would be loop fusion to join computations
                                       of the body of j-loop. But fusing the 2 k-loops is
                                       not easy due to the data dependency imposed by k==2

   nbody_2arr_7_42.c: insert stms into for loop
   nbody_2arr_7_43.c: insert wrapping if into for loop	
   nbody_2arr_7_44.c: if_to_ternary
   nbody_2arr_7_45.c: if_to_ternary

   nbody_2arr_7_46.c: insert stms into for loop
   nbody_2arr_7_47.c: if_to_ternary
   nbody_2arr_7_48.c: if_to_ternary
   nbody_2arr_7_49.c: if_to_ternary
   nbody_2arr_7_50.c: if_to_ternary

   nbody_2arr_7_51.c: loop_colapse 
   nbody_2arr_7_52.c: loop_colapse 
   nbody_2arr_7_53.c: normalize for loop
   nbody_2arr_7_54.c: loop_colapse 

   nbody_2arr_7_55.c: negative_index_scheme

----------------------------------------------------------

Transformation sequences 6 arrays version:

Sequence 1:
   nbody_6arr_1_1.c: function inlining (Generate OpenCL - 2 kernels)

Sequence 2:
   nbody_6arr_1_1.c: function inlining
   nbody_6arr_2_2.c: loop scheduling (Generate OpenCL - 2 kernels, 1st using SM)

Sequence 4:
   nbody_6arr_1_1.c:  function inlining
   nbody_6arr_3_4.c:  insert stms into for loop
   nbody_6arr_3_5.c:  insert stms into for loop
   nbody_6arr_3_6.c:  loop collapse
   nbody_6arr_4_1.c:  expand +=
   nbody_6arr_4_2.c:  expand +=
   nbody_6arr_4_3.c:  expand +=
   nbody_6arr_4_4.c:  expand +=
   nbody_6arr_4_5.c:  expand +=
   nbody_6arr_4_6.c:  expand +=
   nbody_6arr_4_7.c:  if_to_ternary
   nbody_6arr_4_8.c:  if_to_ternary
   nbody_6arr_4_9.c:  if_to_ternary
   nbody_6arr_4_10.c: if_to_ternary
   nbody_6arr_4_11.c: if_to_ternary
   nbody_6arr_4_12.c: if_to_ternary
   nbody_6arr_4_13.c: substitute expr
   nbody_6arr_4_14.c: substitute expr
   nbody_6arr_4_15.c: substitute expr
