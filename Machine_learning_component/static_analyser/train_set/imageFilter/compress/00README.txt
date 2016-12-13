Transformation sequences:

Sequence 1:
  compress_1_1.c:  Loop collapse
  compress_1_2.c:  function inlining
  compress_1_3.c:  aux 2D array removal
  compress_1_4.c:  Loop split
  compress_1_5.c:  Loop fussion
  compress_1_6.c:  Loop fussion
  compress_1_7.c:  Flatten 2D array (var temp2d)
  compress_1_8.c:  Flatten 2D array (var cos1)
  compress_1_9.c:  Flatten 2D array (var cos2)
  compress_1_10.c: Flatten 2D array (var image)  (Generate OpenCL)

Sequence 2:
  compress_2_1.c:  Block exchange (input_dsp <-> 2-nested for loops)
  compress_2_2.c:  Insert stmts into for loop
  compress_2_3.c:  Insert stmts into for loop
  compress_2_4.c:  Loop fussion
  compress_2_5.c:  Loop fussion
  compress_2_6.c:  function inlining
  compress_2_7.c:  Loop collapse
  compress_2_8.c:  Flatten 2D array (var temp2d)
  compress_2_9.c:  Flatten 2D array (var cos1)
  compress_2_10.c: Flatten 2D array (var cos2)
  compress_2_11.c: Flatten 2D array (var image)      
  compress_2_12.c: Flatten 2D array (var block)
  compress_2_13.c: Flatten 2D array (var result)
  compress_2_14.c: Flatten 2D array (var result)  (Generate OpenCL)

Sequence 3:
  compress_1_1.c:  Loop collapse
  compress_1_2.c:  function inlining

  compress_3_1.c:  Loop fussion
  compress_3_2.c:  Loop fussion
  compress_3_3.c:  Flatten 2D array (var temp2d)
  compress_3_4.c:  Flatten 2D array (var cos1)
  compress_3_5.c:  Flatten 2D array (var cos2)
  compress_3_6.c:  Flatten 2D array (var image)
  compress_3_7.c:  Flatten 2D array (var block)
  compress_3_8.c:  Flatten 2D array (var result)
  compress_3_9.c:  Flatten 2D array (var result)   (Generate OpenCL)
  compress_3_10.c: Remove transpose matrix         (Generate OpenCL)

Sequence 4:
  compress_2_1.c:  Block exchange (input_dsp <-> 2-nested for loops)
  compress_2_2.c:  Insert stmts into for loop
  compress_2_3.c:  Insert stmts into for loop
  compress_2_4.c:  Loop fussion
  compress_2_5.c:  Loop fussion
  compress_2_6.c:  function inlining
  compress_2_7.c:  Loop collapse
  compress_2_8.c:  Flatten 2D array (var temp2d)
  compress_2_9.c:  Flatten 2D array (var cos1)
  compress_2_10.c: Flatten 2D array (var cos2)
  compress_2_11.c: Flatten 2D array (var image)      
  compress_2_12.c: Flatten 2D array (var block)
  compress_2_13.c: Flatten 2D array (var result)
  compress_2_14.c: Flatten 2D array (var result)
  compress_4_1.c:  Loop split                      (Generate OpenCL)

Sequence 5: (same as Sequence 1 but remove transpose matrix first)
  compress_5_1.c:  Remove transpose matrix (remove cos2)
  compress_5_2.c:  Loop collapse
  compress_5_3.c:  function inlining
  compress_5_4.c:  aux 2D array removal
  compress_5_5.c:  Loop split
  compress_5_6.c:  Loop fussion
  compress_5_7.c:  Loop fussion
  compress_5_8.c:  Flatten 2D array (var temp2d)
  compress_5_9.c:  Flatten 2D array (var cos1)
  compress_5_10.c: Flatten 2D array (var image)  (Generate OpenCL)

Sequence 6: (same as Sequence 2 but remove transpose matrix first)
  compress_6_1.c:  Remove transpose matrix        (remove cos2)
  compress_6_2.c:  Block exchange (input_dsp <-> 2-nested for loops)
  compress_6_3.c:  Insert stmts into for loop
  compress_6_4.c:  Insert stmts into for loop
  compress_6_5.c:  Loop fussion
  compress_6_6.c:  Loop fussion
  compress_6_7.c:  function inlining
  compress_6_8.c:  Loop collapse
  compress_6_9.c:  Flatten 2D array (var temp2d)
  compress_6_10.c:  Flatten 2D array (var cos1)
  compress_6_11.c: Flatten 2D array (var image)      
  compress_6_12.c: Flatten 2D array (var block)
  compress_6_13.c: Flatten 2D array (var result)
  compress_6_14.c: Flatten 2D array (var result)  (Generate OpenCL)


Path X:
   compress_X_1.c: stencil reduction
