
- Paths STML
    1. - 1.1 for_chunks sobre bucle i red
         1.2 for_chunks sobre bucle i green
         1.3 for_chunks sobre bucle i blue

    2. - 2.1 for_chunks sobre bucle j red
         2.2 for_chunks sobre bucle j green
         2.3 for_chunks sobre bucle j blue

	 Path that leads to generation of OpenMP and MPI
    3. - 3.1 loop fussion bucles i
       - 3.2 for_chunks sobre bucle i (generate MPI and/or OpenMP code)

    4. - loop fussion bucles i
       - for_chunks sobre bucle j

    5. - loop fussion bucles i
       - inline de funciones y merge de bucle i-j

    6. - 3 inline de funciones y 3 merge de bucle i-j

	 Path that leads to generation of OpenCL code
    7. - 7.1 loop fussion bucles i
         7.2 inline de funcion kernelRedFilter
         7.3 inline de funcion kernelGreenFilter
         7.4 inline de funcion kernelBlueFilter
         7.5 loop fussion 3 bucles j
	 7.6 normalizacion iteration step for loop
         7.7 collapse de bucle i-j (generate OpenCL code)

    8. - 8.1 loop fussion bucles i
         8.2 inline de funcion kernelRedFilter
         8.3 inline de funcion kernelGreenFilter
         8.4 inline de funcion kernelBlueFilter
         8.5 loop fussion 3 bucles j
         8.6 for_chunk

	 Path that leads to generation of Maxj code
    9. - 9.1  remove unused assignment ( in 3 functions kernel*Filter() )
         9.2  substitute assignment ( in 3 functions kernel*Filter() )
         9.3  roll 1 step for-j loop ( in 3 functions kernel*Filter() )
         9.4  roll 1 step for-j loop ( in 3 functions kernel*Filter() )
         9.5  roll 1 step for-j loop ( in 3 functions kernel*Filter() )
         9.6  eliminate redundant branch ( in 3 functions kernel*Filter() )
	 9.7  normalizacion iteration step for loop ( in 3 functions kernel*Filter() )
         9.8  collapse de bucles j-k (in 3 functions kernel*Filter())
         9.9  inline 3 functions kernel*Filter()
         9.10 collapse 3 pair of nested loops i-k (generate Maxj with 3 different kernels)
         9.11 loop fussion 3 for-k loops (generate Maxj with one kernel)

	 Path that leads to generation of OpenCL code
    10. - 10.1 inline de funcion kernelRedFilter
          10.2 inline de funcion kernelGreenFilter
          10.3 inline de funcion kernelBlueFilter
	  10.4 normalizacion iteration step for loop (3 bucles for-j)
          10.5 3 collapse de bucles i-j (generate OpenCL code)


- MPI
  * Generacion de codigo
    - vinculo codigo final con elementos regla for_chunks

-OpenMP
  * Generacion de codigo
    - una vez aplicada regla for_chunks se pueden meter pragmas OpenMP y el codigo
      seria correcto
    - otra posibilidad es revertir el for_chunks para OpenMP

- Maxj
  * Generacion de codigo
    - Llevar a FPGA los "kernels" de nuestras operaciones matematicas
    - Decidir que va a FPGA pq se ha anotado variable de tipo matriz y se itera sobre ella?

- OpenCL
  * Generacion de codigo
    - 
    - Condicion: que lo que se mande a GPU sea puro (i.e. no se modifica otra cosa
      que no sean los parametros de salida, esa prop. la tenemos)
    - Transformacion mas sencilla: cada iteracion se asigna a un hilo de GPU
    - Alguna otra condiciones de pq el bucle de j se manda a GPU?


- Semantica (propiedades generadas) operaciones (sobre vectores, matrices) en las anotaciones:
  - pure (solo se modifica lo que se declara como parametro de salida, o las modificaciones
    de variables tienen un scope local)
