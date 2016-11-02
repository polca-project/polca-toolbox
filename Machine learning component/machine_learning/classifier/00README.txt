* Labels 
    # 0:  CPU
    # 1:  OpenMP
    # 2:  MPI
    # 3:  OpenMP/MPI
    # 4:  GPU
    # 5:  GPU/OpenMP
    # 6:  GPU/MPI
    # 7:  GPU/OpenMP/MPI
    # 8:  FPGA
    # 9:  FPGA/GPU
    # 10: FPGA/OpenMP
    # 11: FPGA/MPI
    # 12: FPGA/GPU/OpenMP
    # 13: FPGA/GPU/MPI
    # 14: FPGA/OpenMP/MPI
    # 15: FPGA/GPU/OpenMP/MPI



* Feature vector for each sample:
- 0: max nested loop depth (i.e. 0: one for loop, 1: two for loops and one nested, ...)
- 1: any function call? (0: no, 1: yes)
- 2: any array write shifted within for loops? (0: no, 1: yes)
- 3: any irregular loop? (i.e. containing break, continue, ...)
- 4: any global variable (0: no, 1: yes)
- 5: any if statement? (0: no, 1: yes)
- 6: foor loop limits static for all loops? (0: no, 1: yes)

* New features:
-  7: any SIMD/SPMD loop? (0: no, 1: yes)
      - iterates over with 1 for loop (not two nested for loops to iterate over an array)
      - read a set of arrays arrayIn using loop iterator variable
      - writes into set of arrays arrayOut using loop iterator variable
      - set arrayIn != arrayOut
      - iteration_independent STML property

-  8: any for loop with loop_schedule? (0: no, 1: yes)
      - any for loop with loop_schedule STML property

-  9: number of loop invariant var
      - is assigned outside the for loop where it is used, and is not
        modified within the loop

- 10: number of loop hoisted var modifications
      - modification within a loop of a variable assigned outside the
        loop (only scalar variables are accounted for in this feature)

- 11: number of non-1D array

- 12: number of aux var to access arrays  
      - it is used to access a STML array

- 13: number of for loops in total

- 14: any non-normalized for loop
      - inc step not 1

- 15: number of statements with pattern to apply roll-up

- 16: number of compund statments
      - This feature is computed for the function and code block for which the
        abstraction is computed.
      - If function and block are not specified then this feature is computed
        for the whole input program

- 17: any ternary operator?
  - This feature cuantifies if there is some conditional in the form of ternary
    Ex. x = y == 5 ? 3 : 0;


*********************************************************
* Code transformations requiring new features to be added
*********************************************************

- Application of rule "expand +="
  Ej. x += 5 -> x = x + 5

- Application of rule "split_var_decl-assign"
  Ej. int x = 0; -> int x;
      	      	    x = 0;
