ASMCount
==============

ASMCount statically analyzes the binary code generated for the compiler and predicts the computational cost of the different POLCA scopes in the code. The output is a JSON file that can be used by other tools to determine the code transformations to perform or visualization tools such as SDFpy and POGADE.


Usage
--------------

This application is written in python and does not need to be compiled. To use it, it has to be called with two parameters, the first one is the C source code file to statically analyze and the second parameter is the compilation flags to be used by GCC:

<pre>
$ asmcounter source_code_file.c "compilation flags"
$ asmcounter tp01.c "-O2 -march=native"
</pre>

The output file format contains a list of scopes. Each scope consists of a name, a weight (their relative computational effort compared to other scopes), the starting and end line (lstart and lend) and a list of subscopes. Example:

<pre>
{
    "scopes" : [
	{ "name" : "ddot",
	  "weight" : 17
	  "lstart" : 10
	  "lend" : 23
	  "scopes" : [
	      { "name" : "MSUM",
		"count" : 8
		"lstart" : 18
		"lend" : 20
		"scopes" : [
		]},
	  ]},
	{ "name" : "vvs",
	  "weight" : 13
	  "lstart" : 28
	  "lend" : 38
	  "weight" : [
	      { "name" : "SUB",
		"count" : 8
		"lstart" : 35
		"lend" : 37
		"scopes" : [
		]},
	  ]},
    ]
}
</pre>

