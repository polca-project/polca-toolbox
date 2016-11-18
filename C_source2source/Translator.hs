-- Copyright (c) 2013-2016, The IMDEA Software Institute and
-- Copyright (c) 2013-2016, Universidad Politécnica de Madrid

-- See LICENSE.txt and AUTHORS.txt for licensing and authorship


{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Translator where

import RulesLib
import Rules
import PragmaPolcaLib

import Language.C
import Language.C.Analysis
import Language.C.Data.Ident
--import Control.Arrow 

import System.IO
--import System.Directory

import Data.Generics
import Data.List
import Data.Maybe
import Data.Either

import Debug.Trace

import qualified Text.Groom as Gr

---------------------------------------------------------
-- Main functions
---------------------------------------------------------

--toOpenCL file =
--	trans_platform file "opencl" 

--toMaxJ file =
--	trans_platform file "maxj" 

--toMPI file =
--	trans_platform file "mpi" 

--toOMP file =
--	trans_platform file "omp" 

--trans_platform name mode = 
--	do 
--		(ast1, linkedPolcaAnn, includes) <- readFileInfo name 
--		trans_platform_internal name mode includes ast1 linkedPolcaAnn

trans_platform_internal name mode includes ast1 = 
	do
		let filename0 = name ++ ".c"
		--let filename = name ++ "_temp.c"
		--writeFile filename (unlines restOfFile)
		result <-  
			case mode of 
--TODOS: 
-- - Declarations disapear in the resulting code
-- - Need a useless declaration to be correctly read. 
				"opencl" ->
					toOpenCLFromAST ast1
				--"maxj" ->
				--	toMaxJFromAST name linkedPolcaAnn ast1
				--"mpi" ->
				--	toMPIFromAST linkedPolcaAnn ast1
				--"omp" ->
				--	toOMPFromAST linkedPolcaAnn ast1
		let trans_name = name ++ "_" ++ mode ++".c"
		writeFile trans_name
			((unlines includes) ++ (printStdLib includes) 
			++ (unlines (includesMode mode)) 
			++ "\n" ++ result)
		putStrLn ("Transformed " ++ mode ++ " code stored in " ++ trans_name)


---------------------------------------------------------
-- General functions
---------------------------------------------------------

--printStdLib includes = 
-- 	case [include | include <- includes, (trim include) == "#include <stdlib.h>"] of 
-- 		[] ->
-- 			"#include <stdlib.h>\n"
-- 		_ ->
-- 			""	
includesMode mode
	| mode == "opencl" = 
		["#include <fcntl.h>",
		"#include <string.h>",
		"#include <unistd.h>",
		"#include <sys/types.h>",
		"#include <sys/stat.h>",
		"#include <OpenCL/opencl.h>"]
	| mode == "maxj" = 
		["#include <MaxSLiCInterface.h>",
		 "#include \"Maxfiles.h\""]
	| mode == "mpi" = 
		["#include <mpi.h>"]
	| mode == "omp" = 
		["#include <omp.h>"]
	| otherwise = 
		[]

putIntoQuotes string = 
	unlines (map quotize (lines string))

quotize line = 
	"\"" ++ (trim line) ++ "\\n\"\\"

getNamePragma :: CBlockItemAnn -> String
getNamePragma (CBlockStmt block) =
	concat 
		[getDef pragma | pragma <- polcaPragmasAst]
	where 
		getDef pragma = 
			case pragma of
				("def":rest) ->
					(head rest)
				_ -> 
					""
		astProperties = 
			getAnnotation block 
		polcaPragmasAst = 
			extractPolcaPragmas astProperties

separateIO pragma =
	case pragma of 
		("input":rest) ->
			Left (head rest)
		("output":rest) ->
			Right (head rest)
		_ ->
			Left ""
	--case stripPrefix "input " (trim pragma) of
	--	Just rest ->
	--		Left (trim rest)
	--	Nothing -> 
	--		case stripPrefix "output " (trim pragma) of
	--			Just rest ->
	--				Right (trim rest)
	--			Nothing -> 
	--				Left ""

isKernel :: String -> CBlockItemAnn -> Bool
isKernel typeKernel stmt  =  
		(isPragmaAnn ["kernel", typeKernel] stmt )
	|| 	(isPragmaAnn ["target", typeKernel] stmt )

isIOAnn stmt  =  
	isPragmaAnn ["io"] stmt

isInit var stmt = 
	isPragmaAnn ["init", var] stmt

isPragmaAnn:: [String] -> CBlockItemAnn -> Bool
isPragmaAnn anns (CBlockStmt block) =  
	--case [pragma | (nodeName, pragma) <- pragmaInfo, (geq (Just nodeName) (nameOfNode (nodeInfo block)))] of 
	case polcaPragmasAst of 
		pragmas@(_:_) -> 
			or (map isPragmaAnnOne pragmas)
		[] ->
			False
	where 
		isPragmaAnnOne :: [String] -> Bool
		isPragmaAnnOne pragma = 
			fst $
				foldl 
					check_ann
					(True,pragma) 
					anns
		check_ann :: (Bool, [String]) -> String -> (Bool, [String])
		check_ann (cb, (hp:tp)) ann =
			(cb && (ann == hp), tp)
		check_ann (cb, []) ann =
			(False, [])
		--isPragmaAnnOne a ppragma
		--	| a == ppragma = True
		--	| otherwise = False
			--case pragma of 
			--	(a:_) ->
			--		True 
			--	_ ->
			--		False
		astProperties = 
			getAnnotation block 
		polcaPragmasAst = 
			extractPolcaPragmas astProperties
			--case stripPrefix ann (trim pragma) of
			--	Just rest ->
			--		True
			--	Nothing -> 
			--		False 
isPragmaAnn _ _ =  
	False

searchNodePragmaProperty property io ast = 
	--let 
	--	nameNodes
	--		= [nodeName
	--			| (nodeName, pragma) <- pragmaInfo,
	--				isJust (stripPrefix  (trim (property ++ io)) pragma)]
	--in 
		--case nameNodes of 
			--(name:_) ->
				case (applyRulesGeneral (lookForPropertyNode property io) ast) of 
					[]->
						error ("Declaration of " ++ property ++ " for " ++ io ++ " is needed.")
					(head_:_) ->
						--trace (property ++ " " ++ io ++ ": " ++ (snd head_)) 
						head_
			--[] ->
			--	error ("Declaration of " ++ property ++ "for " ++ io ++ " is needed.")

lookForPropertyNode :: String -> String -> CDeclAnn -> [(CDeclAnn, String)]
lookForPropertyNode property io (n@(CDecl _ ((Just (CDeclr (Just (Ident name _ _)) _ _ _ _),_, _):_) (Ann _ nP))) =
	let 
		polcaPragmasAst = 
			extractPolcaPragmas nP
	in 
		case 
			--trace (property ++ " " ++ io ++ " -> " ++ (show polcaPragmasAst)) 
			[rest | (property1:rest) <- polcaPragmasAst, property1 == property] 
		of 
			pragmas_prop@(_:_) ->
				case [rest | (io1:rest) <- pragmas_prop, io == io1] of 
					(_:_) ->
						--trace (property ++ " " ++ io ++ ": " ++ (show pragmas_prop)) 
						[(n,name)]
					[] ->
						[]
			[] ->
				[]
lookForPropertyNode _ _ _ = 
	[]

--searchNodePragmaPropertyMPI property io ast pragmaInfo = 
--	let 
--		nameNodes
--			= [nodeName
--				| (nodeName, pragma) <- pragmaInfo,
--					isJust (stripPrefix  (trim (property ++ io)) pragma)]
--	in 
--		case nameNodes of 
--			(name:_) ->
--				case (applyRulesGeneral (lookForAssign name) ast) of 
--					[]->
--						error ("Declaration of " ++ property ++ "for " ++ io ++ " is needed.")
--					(head_:_) ->
--						head_
--			[] ->
--				error ("Declaration of " ++ property ++ "for " ++ io ++ " is needed.")

lookForAssign nodeId (CExpr (Just (CAssign CAssignOp _ rhs _)) nI) =
	case (nameOfNode nI) of 
		Just nodeId' 
			| nodeId' == nodeId -> 
				[rhs]
			| otherwise -> 
				[]
		Nothing -> 
			[]
lookForAssign _ _ = 
	[]

--getPragmaInfo ann pragmaInfo (CBlockStmt block) =  
--	case [pragma | (nodeName, pragma) <- pragmaInfo, (geq (Just nodeName) (nameOfNode (nodeInfo block)))] of 
--		pragmas@(_:_) -> 
--			case concat (map getPragmaInfoOne pragmas) of 
--				[] ->
--					error ("Needed property " ++ ann)
--				(head_:_) ->
--					head_
--		[] ->
--			[]
--	where 
--		getPragmaInfoOne pragma =
--			case stripPrefix ann (trim pragma) of
--				Just rest ->
--					[rest]
--				Nothing -> 
--					[] 
--hasPragma _ _ _ =  
--	[]

lookForAssignVar var (CExpr (Just (CAssign CAssignOp (CVar (Ident lhs _ _) _) rhs _)) nI) =
	case lhs == var of 
		True ->
			[rhs]
		False -> 
			[]
lookForAssignVar _ _ = 
	[]


extract_rel_directory filename = 
	reverse (dropWhile (\x -> x /= '/') (reverse filename))

---------------------------------------------------------
-- OpenCL functions
---------------------------------------------------------

-- TODO list
-- - kernel: parameters need to be defined according input & outputs. This needs a annotation to know where they are declared
-- - kernel: One of the current parameters is not needed
-- - Remove the need of having size and sizetype as global. Put them as arguments of the container function
-- - There are places where * is included but it is not clear why. We should use some annotation or something to have this info
-- - Several kernels and calls from different places should be covered.


toOpenCLFromAST ast = 
	do 
		funsOpenCl <- readFile "opencl_main_funs"
		let (kernelStr, strAST) = printAST_OpenCL ast
		return 
			(if (kernelStr /= "")
			then	
				(kernelStr ++ funsOpenCl ++ "\n" ++ strAST)
			else 
				strAST)

printKernel ios name (CBlockStmt (CFor assign comp _ (CCompound _ body _) _)) =
	case (assign,comp) of 
		((Right (CDecl _ [(Just (CDeclr (Just (Ident iter _ _)) _ _ _ _),_,_)] _)),
		 (Just (CBinary CLeOp _ bound _))) ->
			fillKernel name iter body bound
		( (Left (Just (CAssign CAssignOp (CVar (Ident iter _ _) _) _ _)) ),
		 (Just (CBinary CLeOp _ bound _))) ->
			fillKernel name iter body bound
		_ ->
			("",(CVar (internalIdent "nothing") undefNodeAnn))
	where 
		fillKernel name iter body bound = 
			let 
				kernelStr = 
					"int " ++ iter ++ " = get_global_id(0);\n" 
					++ concat (map ((++ "\n").prettyMyASTAnn) body)
			in 
				("const char *KernelSource = \"\\n\" \\\n" 
				++ (putIntoQuotes (createKernelFunDecl ios name))
				++ (putIntoQuotes ("{\n" ++ kernelStr ++ "\n}\n") )
				++ ";" ++ "\n",bound)
printKernel _ _ other =
	("", (CVar (internalIdent "nothing") undefNodeAnn))



-- TODO: Improve to automatically using pragma info
createKernelFunDecl (ins, outs) name = 
	unlines $ ["__kernel void " ++ name ++ "("]
		++ [intercalate ", " (map printDecl (ins ++ (filterRepeteadVars outs ins) ))]
		++ ["   )"]

printDecl (n, (_, "typeInt"), _) = 
	"__global int* " ++ n 
printDecl (n, (_, "typeFloat"), _) = 
	"__global float* " ++ n 
printDecl (n, (_, typen), _) = 
	error $ "Type " ++ typen ++ " for " ++ n ++ " not recognized."

initialTab = "\t\t" 

openCLcalls kernelName = 
	unlines (map (initialTab++) 
		["selectOpenCLDevice(&platformIdCount,&platformIds,&deviceIdCount,&deviceIds);",
	    "createOpenCLContext(platformIds,deviceIdCount,deviceIds,&context);",
	    "createOpenCLQueue(deviceIds,context,&queue);",
	    "createOpenCLKernel(deviceIds,context,&program,&kernel,\"" ++ kernelName ++ "\");"])

openCLdefs = 
	unlines (map (initialTab++) 
	    ["cl_uint platformIdCount = 0;",
	    "cl_platform_id *platformIds = NULL;",
	    "cl_uint deviceIdCount = 0;",
	    "cl_device_id *deviceIds = NULL;",
	    "cl_context context;",
	    "cl_command_queue queue;",
	    "cl_program program;",
	    "cl_kernel kernel;",
	    "size_t local;",        
	    "cl_int error = CL_SUCCESS;"])

openCLRelease = 
	unlines (map (initialTab++) 
		["clReleaseProgram(program);",
	    "clReleaseKernel(kernel);",
	    "clReleaseCommandQueue(queue);",
	    "clReleaseContext(context);"])

printAST_OpenCL ast@(CTranslUnit defs _) = 
	let 
		(kernels, strPgm) = unzip (map (printAST_OpenCL_Def ast) defs)
	in 
		(concat  (map (++"\n") kernels), concat (map (++"\n") strPgm) )

printAST_OpenCL_Def ast fun@(CFDefExt (CFunDef _ _ _ (CCompound _ body _) _)) = 
	let
		(kernels, strBody) = unzip (map (printASTBlockItem_OpenCL ast) body)
	in 
		--trace ("*****" ++ (concat kernels) ++ "\n" ++ (concat (map (++"\n") strBody)) ++ "*****") 
		(concat kernels, 
		 (printUntilOpenCurvedBracket (lines (prettyMyASTAnn fun))) ++ (concat (map (++"\n") strBody)) ++ "}\n")
printAST_OpenCL_Def _ other = 
	("", prettyMyASTAnn other)

printASTBlockItem_OpenCL :: CTranslUnitAnn -> CBlockItemAnn -> (String, String)
printASTBlockItem_OpenCL ast block 
	 | (isKernel "opencl" block) && (isPragmaAnn ["iteration_independent"] block) = 
	 	let 
	 		nameKernel = getNamePragma block
	 		ios@(inps,outs) = getIOOpenCLKernel ast block
	 		(kernelStr, bound) = printKernel ios nameKernel block
	 	in
			(kernelStr, 
			-- OpenCL code construction
			"// inputs: " ++ (show [i | (i,_,_)<- inps]) ++ "\n// outputs: " ++ (show [i | (i,_,_)<- outs])  ++"\n"
			  ++ openCLdefs ++ "\n"
			  ++ (openCLcalls nameKernel)++ "\n"
			  ++ (gpuIODecl ios) ++ "\n"
			  ++ (gpuIOInit ios) ++ "\n"
			  ++ (checkErrorGPUInit ios) ++ "\n"
			  ++ (writeInputGPU inps) ++ "\n"
			  ++ (setKernelArgsGPU ios) ++ "\n"
			  ++ getMaxWGSizeGPU ++ "\n"
			  ++ (runKernelGPU bound) ++ "\n"
			  ++ initialTab ++ "clFinish(queue);\n" ++ "\n"
			  ++ (readResultsGPU outs) ++ "\n"
			  ++ (releaseIO ios) ++ "\n"
			  ++ openCLRelease 
			 )
	 | otherwise = 
	 	case block of 
	 		(CBlockStmt for@(CFor _ _ _ (CCompound _ body _) _)) ->
		 			let
						(kernels, strBody) = unzip (map (printASTBlockItem_OpenCL ast) body)
					in 
						(concat kernels, 
						 --trace ("****" ++ (printUntilOpenCurvedBracket (lines (prettyMyASTAnn for))) ++ "****") 
						 (printUntilOpenCurvedBracket (lines (prettyMyASTAnn for))) ++ (concat (map (++"\n") strBody)) ++ "}\n")
			_ ->
	 			("", prettyMyASTAnn block)

getIOOpenCLKernel ast (CBlockStmt block) =
	let 
		--allPragmasBlock = 
		--	[pragma | (nodeName, pragma) <- pragmaInfo, (geq (Just nodeName) (nameOfNode (nodeInfo block)))]
		astProperties = 
			getAnnotation block 
		polcaPragmasAst = 
			extractPolcaPragmas astProperties
		allPragmasBlock = polcaPragmasAst 
		ioNames = map separateIO allPragmasBlock
		buildIOINfo list = 
			[(io, searchNodePragmaProperty "type_size" io ast,
				 searchNodePragmaProperty "total_size" io ast)  
			| io <- list, io /= ""]
		inps = buildIOINfo (lefts ioNames)
		outs = buildIOINfo (rights ioNames)
	in 
		(inps, outs)

filterRepeteadVars outs ins = 
	let 
		vars_in = 
			[v | (v,_,_) <- ins]
	in [out | out@(v,_,_) <- outs, not $ elem v vars_in]
 
gpuIODecl :: ([(String, (CDeclAnn, String), (CDeclAnn, String))], [(String, (CDeclAnn, String), (CDeclAnn, String))]) -> String
gpuIODecl (inps,outs) =
	unlines (map (initialTab ++) 
		(["size_t global;"] 
    	++ (convertGPUDefs True inps)
    	++ (convertGPUDefs False (filterRepeteadVars outs inps))))

convertGPUDefs:: Bool -> [(String, (CDeclAnn, String), (CDeclAnn, String))] -> [String] 
convertGPUDefs b list@(_:_) =  
	["cl_mem gpu_" ++ s ++ ";" | (s,_,_) <- list]
	-- ++ ["unsigned int size"
	--	  ++ (if b then "Input" else "Output") ++ ";"]
convertGPUDefs _ [] =
	[] 

gpuIOInit (inps,outs) = 
	unlines (map (initialTab ++) 
		((convertGPUInit True inps)
    	++ (convertGPUInit False (filterRepeteadVars outs inps))))

convertGPUInit b list@((_,_,_):_) =  
	--let
	--	inpOut = (if b then "Input" else "Output")
	--in 
		--["size" ++ inpOut ++ " = " ++ size ++ ";"] ++ 
		["gpu_" ++ s ++ " = clCreateBuffer(context, CL_MEM_WRITE_ONLY, " ++ typeSize ++" * " ++ size ++ ", NULL, NULL);" | (s,(_,typeSize),(_,size)) <- list] 
convertGPUInit _ [] =
	[] 

checkErrorGPUInit (inps,outs) = 
	let 
		cond = build_cond_checkErrorGPUInit (inps ++ (filterRepeteadVars outs inps) )
	in 
		unlines (map (initialTab ++) 
			["if (" ++ cond ++ ")",
		    "{",
		    "\tprintf(\"Error: Failed to allocate device memory!\\n\");",
		    "\texit(1);",
		    "}"])

build_cond_checkErrorGPUInit list 
	| (length list) == 0 =
		"false"
	| (length list) == 1 =
		let (name,_,_) = (head list) 
		in "!gpu_" ++ name
	| otherwise = 
		(build_cond_checkErrorGPUInit [(head list)]) 
		++ " || " ++ (build_cond_checkErrorGPUInit (tail list)) 
	

releaseIO (inps,outs) = 
	unlines (map 
		((initialTab ++).(\(io,_,_) -> "clReleaseMemObject(gpu_" ++ io ++ ");")) 
		(inps ++ (filterRepeteadVars outs inps) ))

writeInputGPU inps = 
	unlines (map (initialTab ++) 
		(["error = 0;"]
	    ++ ["error |= clEnqueueWriteBuffer(queue, gpu_" ++ i ++ ", CL_TRUE, 0, " 
	    	++ typeSize ++ " * " ++ size ++ ", " ++ i ++  " , 0, NULL, NULL);" 
		| (i,(_,typeSize),(_,size)) <- inps]
	    ++ ["if (error != CL_SUCCESS)",
		    "{",
		    "\tprintf(\"Error: Failed to write to source array!\\n\");",
		    "\texit(1);",
		    "}"]))

setKernelArgsGPU (inps,outs) = 
    unlines (map (initialTab ++) 
		(["error = 0;"]
		++ (setKernelArgGPU 0 (inps ++ (filterRepeteadVars outs inps) ))
     	++["if (error != CL_SUCCESS)",
		    "{",
		    "\tprintf(\"Error: Failed to set kernel arguments! %d\\n\", error);",
		    "\texit(1);",
		    "}"]))

setKernelArgGPU n list
	| (length list) == 1 =
		let 
			(io,_,(_,size)) = (head list)
		in 
			[printGPUKernelArg n "cl_mem " io]
	| otherwise = 
		let 
			(io,_,_) = (head list)
		in 
			(printGPUKernelArg n "cl_mem " io):(setKernelArgGPU (n + 1) (tail list))
	where 
		printGPUKernelArg n tp io = 
			"error |= clSetKernelArg(kernel, " ++ (show n) ++ ", sizeof(" ++ tp ++ "), &gpu_" ++ io ++ ");"

getMaxWGSizeGPU =
	unlines (map (initialTab ++) 
	    --["error = clGetKernelWorkGroupInfo(kernel, deviceIds[0], CL_KERNEL_WORK_GROUP_SIZE, sizeof(local), &local, NULL);",
	    --"if (error != CL_SUCCESS)",
	    --"{",
	    --"\tprintf(\"Error: Failed to retrieve kernel work group info! %d\\n\", error);",
	    --"\texit(1);",
	    --"}"])
	    ["local = 1;"])

runKernelGPU bound = 
    unlines (map (initialTab ++) 
    	["global = " ++ (prettyMyASTAnn bound) ++ ";",
    	"error = clEnqueueNDRangeKernel(queue, kernel, 1, NULL, &global, &local, 0, NULL, NULL);",
    	"if (error != CL_SUCCESS)",
	    "{",
	    "\tprintf(\"Error: Failed to execute kernel!\\n\");",
	    "\texit(1);",
	    "}"])

readResultsGPU outs = 
		unlines (concat (map readResultGPU outs))
	where 
	readResultGPU (o,(_,typeSize),(_,size)) = 
		(map (initialTab ++) 
			["error = clEnqueueReadBuffer(queue, gpu_" ++ o ++ ", CL_TRUE, 0, " ++ typeSize ++ " * " ++ size ++ ", " ++ o ++ ", 0, NULL, NULL );",
			"if (error != CL_SUCCESS)",
		    "{",
		    "\tprintf(\"Error: Failed to read output array! %d\\n\", error);",
		    "\texit(1);",
		    "}"])

-----------------------------------------------------------
---- MaxJ functions
-----------------------------------------------------------

--toMaxJFromAST filename pragmaInfo ast = 
--	do 
--		let ([(name, kernelStr, managerStr)], strAST) = printAST_MaxJ pragmaInfo ast
--		result <- 
--			(if (managerStr /= "" && kernelStr /= "")
--				then	
--					do 
--						let nameKernel = name ++ "Kernel.maxj"
--						let nameManager = name ++ "Manager.maxj"
--						let relativedirectory = extract_rel_directory filename
--						writeFile (relativedirectory  ++ nameKernel) kernelStr
--						writeFile (relativedirectory  ++ nameManager) managerStr
--						putStrLn ("Manager maxj code stored in " ++ (relativedirectory  ++ nameKernel))
--						putStrLn ("Kernel maxj code stored in " ++ (relativedirectory  ++ nameManager))
--						return strAST
--				else 
--					return strAST)
--		return result

--printAST_MaxJ pragmaInfo ast@(CTranslUnit defs _) = 
--	let 
--		(kernels, strPgm) = unzip (map (printAST_MaxJ_Def ast pragmaInfo) defs)
--	in 
--		([(a,b,c) | (a,b,c) <- (concat kernels), a /= "", b /= "", c /= ""],
--		 concat (map (++"\n") strPgm) )

--printAST_MaxJ_Def ast pragmaInfo fun@(CFDefExt (CFunDef _ _ _ (CCompound _ body _) _)) = 
--	let
--		(kernels, strBody) = unzip (map (printASTBlockItem_MaxJ ast pragmaInfo) body)
--	in 
--		([(a,b,c) | (a,b,c) <- kernels, a /= "", b /= "", c /= ""], 
--		 (printUntilOpenCurvedBracket (lines (prettyMyAST fun))) ++ (concat (map (++"\n") strBody)) ++ "}\n")
--printAST_MaxJ_Def _ _ other = 
--	([("","","")], prettyMyAST other)

--printASTBlockItem_MaxJ ast pragmaInfo block 
--	 | (isKernel "maxj" pragmaInfo block) && (isPragmaAnn ("iteration_independent") pragmaInfo block) = 
--	 	let 
--	 		nameKernel = getNamePragma pragmaInfo block
--	 		ios@(inps,outs) = getIOMaxJKernel ast pragmaInfo block
--	 		(kernelMainLines, boundStr) = buildKernelMaxJ ios nameKernel block
--	 		funCall = buildKernelCallMaxJ nameKernel boundStr inps outs 
--	 	in
--			(
--			 (nameKernel, (createMaxJKernel nameKernel kernelMainLines), createMaxJManager nameKernel),
--			 "// inputs: " ++ (show [i | i<- inps]) ++ "\n// outputs: " ++(show [o | o<- outs])
--			 ++"\n" ++ 
--			 initialTab ++ funCall
--			)
--	 | otherwise = 
--	 	(("","",""), prettyMyAST block)

--buildKernelCallMaxJ name boundStr inps outs =
--	let 
--		strOuts = (foldl (\acc o -> acc ++ "*" ++ o ++ ", ") "" outs) 
--	in 
--		name ++ "(" ++ boundStr ++ ", " 
--			++ (foldl (\acc i -> acc ++ i ++ ", ") "" inps) 
--			++ (take ((length strOuts) -1) strOuts)
--			++ ")"

--getIOMaxJKernel ast pragmaInfo block =
--	let 
--		allPragmasBlock = 
--			[pragma | (nodeName, pragma) <- pragmaInfo, (geq (Just nodeName) (nameOfNode (nodeInfo block)))]
--		ioNames = map separateIO allPragmasBlock
--		inps = [io | io <- lefts ioNames, io /= ""]
--		outs = [io | io <- rights ioNames, io /= ""]
--	in 
--		(inps, outs)

--buildKernelMaxJ (inps, outs) name (CBlockStmt (CFor assign comp _ (CCompound _ body _) _)) =
--	case (assign,comp) of 
--		((Right (CDecl _ [(Just (CDeclr (Just (Ident ident _ _)) [] Nothing [] _),_,_)] _)),
--		 (Just (CBinary CLeOp _ bound _))) ->
--			-- Kernel function construction
--			let 
--				inpsStr = 
--					["DFEVar " ++ i ++ " = io.input(\"" ++ i ++ "\", dfeUInt(8));" 
--					 | i <- inps]
--				otherDefs = 
--					(applyRulesGeneral (operatesWith ident inps) body)
--				outsStr = 
--					applyRulesGeneral (outputDefs ident outs) body
--			in 
--				(inpsStr ++ otherDefs ++ outsStr, prettyMyAST bound)
--		_ ->
--			([], "")

--operatesWith::  String -> [String] -> CExpr -> [String]
--operatesWith ident inps (CAssign CAssignOp (CVar (Ident nameVar _ _) a) rhs _) =
--	case rhs of
--		(CBinary CRmdOp (CVar (Ident identVar _ _) _) cons@(CConst _) _) 
--			| (identVar == ident) ->
--				let 
--					consStr = prettyMyAST cons
--				in 
--					["DFEVar " ++ nameVar ++ " = control.count.simpleCounter(MathUtils.bitsToAddress(" ++ consStr ++ ")," ++ consStr ++ ");"]
--			| otherwise -> 
--				[]
--		(CIndex (CVar (Ident nameInp _ _) _) _ _) 
--			| elem nameInp inps ->
--				["DFEVar " ++ nameVar ++ " = " ++ nameInp ++ ";"]
--			| otherwise -> 
--				[]
--		_ ->
--			[]
--operatesWith _ _ _ =
--	[]

--outputDefs::  String -> [String] -> CExpr -> [String]
--outputDefs ident outs (CAssign CAssignOp (CIndex (CUnary CIndOp (CVar (Ident vName _ _) _) _) (CVar (Ident indIdent _ _) _) _) rhs _) 
--	| elem vName outs && ident == indIdent =
--		["io.output(\"" ++ vName ++ "\", " ++ (prettyMyAST rhs) ++ ", dfeUInt(8));"]
--	| otherwise =
--		[]
--outputDefs _ _ expr =
--	--trace ("entra" ++ (show expr)) []
--	[]


--createMaxJManager name = 
--	unlines
--		["package " ++ name ++ ";",
--		"",
--		"import com.maxeler.maxcompiler.v2.build.EngineParameters;",
--		"import com.maxeler.maxcompiler.v2.kernelcompiler.Kernel;",
--		"import com.maxeler.maxcompiler.v2.managers.standard.Manager;",
--		"import com.maxeler.maxcompiler.v2.managers.standard.Manager.IOType;",
--		"",
--		"class " ++ name ++ "Manager {",
--		"\tpublic static void main(String[] args) {",
--		"\t\tEngineParameters params = new EngineParameters(args);",
--		"\t\tManager manager = new Manager(params);",
--		"\t\tKernel kernel = new " ++ name ++ "Kernel(manager.makeKernelParameters());",
--		"\t\tmanager.setKernel(kernel);",
--		"\t\tmanager.setIO(IOType.ALL_CPU);",
--		"\t\tmanager.createSLiCinterface();",
--		"\t\tmanager.build();",
--		"\t}",
--		"}"]

--createMaxJKernel name kernelMainLines = 
--	unlines
--		(["package " ++ name ++ ";",
--		"",
--		"import com.maxeler.maxcompiler.v2.kernelcompiler.Kernel;",
--		"import com.maxeler.maxcompiler.v2.kernelcompiler.KernelParameters;",
--		"import com.maxeler.maxcompiler.v2.kernelcompiler.types.base.DFEVar;",
--		"import com.maxeler.maxcompiler.v2.utils.MathUtils;",
--		"",
--		"class " ++ name ++ "Kernel extends Kernel {",
--		"",
--		"\t" ++ name ++ "Kernel(KernelParameters parameters)",
--		"\t{",
--		"\t\tsuper(parameters);"] 
--		++ (map ("\t\t"++) kernelMainLines)
--		++ ["\t}","}"])

-----------------------------------------------------------
---- MPI functions
-----------------------------------------------------------

--toMPIFromAST pragmaInfo ast = 
--	do 
--		let (tags, strAST) = printAST_MPI [] pragmaInfo ast ast
--		return 
--			--(if (tags /= "")
--			--then	
--			("#define TAG_CHUNK 0\n"
--			 ++ (fst (foldl 
--				(\(acc,n) tag -> 
--					(
--						(acc ++ "#define TAG_" ++ tag ++ " " ++ (show n) ++ "\n"), 
--						(n+1) )
--					) 
--					("",1) tags))
--				++ defaultMPIfunsDefs ++ "\n" ++ strAST)
--			--else 
--			--	strAST)

----printAST_MPI pragmaInfo ast@(CTranslUnit defs _) = 
----	let 
----		(kernels, strPgm) = unzip (map (printAST_MPI_Def ast pragmaInfo) defs)
----	in 
----		(concat kernels,
----		 concat (map (++"\n") strPgm) )

----printAST_MPI :: [String] -> [(Name, String)] -> CTranslUnit -> ([String], String)
--printAST_MPI toBeSent pragmaInfo fullAST ast@(CTranslUnit (def:defs) _) = 
--	let 
--		(toBeSentDef, strPgmDef) = printAST_MPI_Def fullAST toBeSent pragmaInfo def
--		(toBeSentDefs, strPgmDefs) = printAST_MPI (toBeSent ++ toBeSentDef) pragmaInfo fullAST (CTranslUnit defs undefNodeAnn)
--	in 
--		(toBeSentDefs, 
--		 (strPgmDef ++ "\n" ++ strPgmDefs))
--printAST_MPI toBeSent _ _ (CTranslUnit [] _) = 
--	(toBeSent, [])


----printAST_MPI_Def :: CTranslUnit -> [String] -> [(Name, String)] -> CExtDecl -> ([String], String)
--printAST_MPI_Def ast toBeSent pragmaInfo fun@(CFDefExt (CFunDef _ (CDeclr (Just (Ident "main" _ _)) _ _ _ _) _ (CCompound _ body _) _)) = 
--	let
--		(toBeSentBody, strBody) = unzip (map (printASTBlockItem_MPI ast toBeSent pragmaInfo) body)
--	in 
--		(concat toBeSentBody, 
--		 (printUntilOpenCurvedBracket (lines (prettyMyAST fun))) 
--		 ++ (concat 
--		 		([initialTab ++ "mpi_init(argc, argv);\n\n"] 
--		 		++ strBody 
--		 		++ ["\n" ++ initialTab ++ "mpi_finalize();\n"])) 
--		 		++ "}\n")
--printAST_MPI_Def ast toBeSent pragmaInfo fun@(CFDefExt (CFunDef _ _ _ (CCompound _ body _) _)) = 
--	let
--		(toBeSentBody, strBody) = unzip (map (printASTBlockItem_MPI ast toBeSent pragmaInfo) body)
--	in 
--		(concat toBeSentBody, 
--		 (printUntilOpenCurvedBracket (lines (prettyMyAST fun))) ++ (concat strBody) ++ "}\n")
--printAST_MPI_Def _ _ _ other = 
--	([], prettyMyAST other)


----printASTBlockItem_MPI :: CTranslUnit -> [String] -> [(Name, String)] -> CBlockItem -> ([String], String)
--printASTBlockItem_MPI ast toBeSent pragmaInfo block = 
--	let 
--		(toBeSentAux, strBlockAux) = printASTBlockItem_MPI_aux ast pragmaInfo block
--		strMPIParamSend = concat (map (buildSendParamsMPI pragmaInfo block) toBeSent)
--	in 
--		(toBeSentAux, strBlockAux ++ "\n" ++ strMPIParamSend)

--printASTBlockItem_MPI_aux ast pragmaInfo block 
--	 | (isKernel "mpi" pragmaInfo block) && (isPragmaAnn ("iteration_independent") pragmaInfo block) = 
--	 	let 
--	 		nameKernel = getNamePragma pragmaInfo block
--	 		ios@(inps,outs) = getIOMPIKernel ast pragmaInfo block
--	 		infoChunks = getChunkInfo ast pragmaInfo block
--	 		chunksSizeStr = declareChunksSize infoChunks
--	 		inputSendingStr = concat (map (sendInputMPI infoChunks) inps)
--	 		iterVarBody = searchIterVarAndBody block 
--	 		bodyStr = rebuildBody iterVarBody infoChunks 
--	 		outputSendingStr = concat (map (sendOutputMPI infoChunks) outs)
--	 	in
--			(
--			 concat [deps | (_,_,_,(deps,_)) <- inps],
--			 	chunksSizeStr 
--			 ++ inputSendingStr
--			 ++ "\t\t//original body\n"  
--			 ++ bodyStr ++ "\n"
--			 ++ outputSendingStr
--			)
--	| isIOAnn pragmaInfo block = 
--		-- Guardar como pendiente el image si esta en init
--		([],  initialTab ++ "if(rank == 0)\n" 
--			++ (unlines (map ("\t"++) (lines (prettyMyAST block)))) )
--	| otherwise = 
--	 	([], prettyMyAST block)


--defaultMPIfunsDefs = 
--	unlines 
--		[
--		"int num_proc;",
--		"int rank;",
--		"",
--		"void mpi_init(int argc, char *argv[])",
--		"{",
--		"\tMPI_Init(&argc,&argv);",
--		"\tMPI_Comm_size(MPI_COMM_WORLD,&num_proc);",
--		"\tMPI_Comm_rank(MPI_COMM_WORLD,&rank);",
--		"",
--		"\tprintf(\"Starting rank %d\\n\",rank);",
--		"}",
--		"",
--		"void mpi_finalize()",
--		"{",
--		"\tMPI_Finalize();",
--		"}"
--		]

--buildSendParamsMPI pragmaInfo block toBeSent
--	| isInit toBeSent pragmaInfo block = 
--		unlines (map (initialTab ++) 
--			["if(rank == 0){",
--			"\tint procId;",
--			"\tfor(procId=1;procId<num_proc;procId++)",
--			"\t{",
--			"\t\tMPI_Send(&" ++ toBeSent ++ ", 1, MPI_INT, procId, TAG_" ++ toBeSent ++ ", MPI_COMM_WORLD);",
--			"\t}",
--			"}",
--			"else",
--			"{",
--			"\tMPI_Recv(&" ++ toBeSent ++ ", 1, MPI_INT, 0, TAG_" ++ toBeSent ++ ",MPI_COMM_WORLD, MPI_STATUS_IGNORE);",
--			"}"])
--	| otherwise = 
--		""

--getIOMPIKernel ast pragmaInfo block =
--	let 
--		allPragmasBlock = 
--			[pragma | (nodeName, pragma) <- pragmaInfo, (geq (Just nodeName) (nameOfNode (nodeInfo block)))]
--		ioNames = map separateIO allPragmasBlock
--		calculateIO list = 
--		   [
--			let 
--				(_,total_size_var) = searchNodePragmaProperty "total_size " io ast pragmaInfo
--				(_, type_size) = searchNodePragmaProperty "type_size " io ast pragmaInfo
--			in 
--				(io, total_size_var, type_size, 
--					get_deps_init total_size_var ast pragmaInfo)
--			| io <- list, io /= ""]
--		inps = calculateIO (lefts ioNames)
--		outs = calculateIO (rights ioNames)
--	in 
--		(inps, outs)

--get_deps_init var ast pragmaInfo = 
--	let
--		rhs = searchNodePragmaPropertyMPI "init " var ast pragmaInfo
--	in 
--		(applyRulesGeneral searchVarsName rhs, rhs)

--searchVarsName :: CExpr -> [String]
--searchVarsName (CVar (Ident name _ _) _) =
--	[name]
--searchVarsName _ = 
--	[]

--getChunkInfo :: CTranslUnit  -> [(Name, String)] -> CBlockItem -> (String, (String, CExpr), (String, CExpr))
--getChunkInfo ast pragmaInfo block = 
--	let
--		varChunked = getPragmaInfo "chunked " pragmaInfo block
--		varPrevChunk = getPragmaInfo "prev_chunk_size " pragmaInfo block
--		varChunkSize = getPragmaInfo "curr_chunk_size " pragmaInfo block
--		rhsPrevChunk = 
--			case (applyRulesGeneral (lookForAssignVar varPrevChunk) block) of 
--				[]->
--					error ("Variable linked to previous_chunck_size can not be found in the statement")
--				(head_:_) ->
--					head_
--		rhsChunkSize = 
--			case (applyRulesGeneral (lookForAssignVar varChunkSize) block) of 
--				[]->
--					error ("Variable linked to current_chunk_size can not be found in the statement")
--				(head_:_) ->
--					head_
--	in 
--		(varChunked, (varPrevChunk, rhsPrevChunk), (varChunkSize, rhsChunkSize))

--declareChunksSize (varChunked, _, _) = 
--	initialTab ++ "int prev_chunk_size = " ++ varChunked ++ " / num_proc;\n"
--  	++ initialTab ++ "int curr_chunk_size = rank != (num_proc-1) ? prev_chunk_size : prev_chunk_size + " ++  varChunked ++ "%num_proc;\n"

--searchIterVarAndBody (CBlockStmt (CFor assign _ _ (CCompound _ body _) _)) =
--	case assign of 
--		(Right (CDecl _ [(Just (CDeclr (Just (Ident iter _ _)) [] Nothing [] _),_,_)] _)) ->
--			(iter,body)
--		(Left (Just (CAssign CAssignOp (CVar (Ident iter _ _) _) _ _)) )  ->
--			(iter,body)
--		_ ->
--			error "Chuncked for has not expected form, i.e. for(iter=E;E;E)"

--rebuildBody (iter,body) (varChunked, (varPrevChunk, _), (varChunkSize, _)) =
--	let
--		cleanBody = removeAssignsTo [varPrevChunk,varChunkSize] body
--		substitutedBody =
--			substitute	(substitute	(substitute (CCompound [] cleanBody undefNodeAnn)
--							(CVar (builtinIdent iter) undefNodeAnn) 
--							(CVar (builtinIdent "rank") undefNodeAnn)) 
--								(CVar (builtinIdent varPrevChunk) undefNodeAnn)
--								(CVar (builtinIdent "prev_chunk_size") undefNodeAnn))
--									(CVar (builtinIdent varChunkSize) undefNodeAnn)
--									(CVar (builtinIdent "curr_chunk_size") undefNodeAnn)
--	in 
--		prettyMyAST substitutedBody
--		--prettyMyAST (CCompound [] cleanBody undefNodeAnn)




--removeAssignsTo 
--	(toRemove@(_:_)) 
--	(assign@(CBlockDecl (CDecl _ [(Just (CDeclr (Just (Ident lhs _ _)) _ _ _ _),_,_)] _)):body) =
--	let 
--		diff = toRemove \\ [lhs]
--	in 
--		if (diff == toRemove)
--		then (assign:(removeAssignsTo toRemove body))
--		else (removeAssignsTo diff body)
--removeAssignsTo toRemove (other:body) =
--	(other:(removeAssignsTo toRemove body))
--removeAssignsTo [] body =
--	body
--removeAssignsTo _ [] =
--	[]

--sendInputMPI:: (String, (String, CExpr), (String, CExpr)) -> (String, String, String, ([String], CExpr)) -> String
--sendInputMPI  (varChunked, (varPrevChunk, rhsPrevChunk), (varChunkSize, rhsChunkSize)) (input,totalS,typeS,(_,rhsSize)) = 
--	let 
--		(sizeRank0, sizeRankN, sizeCommon) = rewriteSize rhsSize varChunked
--	in
--		(unlines (map (initialTab++)
--			[
--				"if(rank == 0)",
--				"{",
--				"\tint procId;",
--				"\tfor (procId = 1; procId < num_proc; procId++)",
--				"\t{",
--				"\t\tint prev_chunk_size = " ++ varChunked ++ " / num_proc;",
--				"\t\tint curr_chunk_size = procId != (num_proc-1) ? prev_chunk_size : prev_chunk_size + " ++ varChunked ++ " % num_proc;",
--				"\t\tMPI_Send(" ++ input ++ " + (" ++ sizeRank0 ++ "), " ++ sizeCommon ++ ", MPI_CHAR, procId, TAG_CHUNK, MPI_COMM_WORLD);",
--				"\t}",
--				"}",
--				"else",
--				"{",
--				"\timage = malloc(" ++ typeS ++ "*" ++ totalS ++ ");",
--				"\tMPI_Recv(" ++ input ++ " + (" ++ sizeRankN ++ "), " ++ sizeCommon ++ ", MPI_CHAR, 0, TAG_CHUNK,MPI_COMM_WORLD, MPI_STATUS_IGNORE);",
--				"}"
--			]))

--sendOutputMPI:: (String, (String, CExpr), (String, CExpr)) -> (String, String, String, ([String], CExpr)) -> String
--sendOutputMPI  (varChunked, _, _) (output,_,_,(_,rhsSize)) = 
--	let 
--		(sizeRank0, sizeRankN, sizeCommon) =  rewriteSize rhsSize varChunked
--	in
--		(unlines (map (initialTab++)
--			[
--			"if(rank == 0)",
--			"{",
--			"\tint procId;",
--			"\tfor (procId= 1; procId < num_proc; procId++)",
--			"\t{",
--			"\t\tint prev_chunk_size = " ++ varChunked ++ " / num_proc;",
--			"\t\tint curr_chunk_size = procId != (num_proc-1) ? prev_chunk_size : prev_chunk_size + " ++ varChunked ++ " % num_proc;",
--			"\t\tMPI_Recv((*" ++ output ++ ")+(" ++ sizeRank0 ++ "), " ++ sizeCommon ++ ", MPI_CHAR, procId, TAG_CHUNK, MPI_COMM_WORLD, MPI_STATUS_IGNORE);",
--			"\t}",
--			"}",
--			"else",
--			"{",
--			"\tMPI_Send((*" ++ output ++ ")+(" ++ sizeRankN ++ "), " ++ sizeCommon ++ ", MPI_CHAR, 0, TAG_CHUNK, MPI_COMM_WORLD);",
--			"}"
--			]))

--rewriteSize rhsSize varChunked = 
--	let 
--		sizeRank0 = prettyMyAST (substitute rhsSize 
--				(CVar (builtinIdent varChunked) undefNodeAnn) 
--				(CBinary CMulOp 
--					(CVar (builtinIdent "procId")  undefNodeAnn) 
--					(CVar (builtinIdent "prev_chunk_size") undefNodeAnn) 
--					undefNodeAnn))
--		sizeRankN
--		 = prettyMyAST (substitute rhsSize 
--				(CVar (builtinIdent varChunked) undefNodeAnn) 
--				(CBinary CMulOp 
--					(CVar (builtinIdent "rank")  undefNodeAnn) 
--					(CVar (builtinIdent "prev_chunk_size") undefNodeAnn) 
--					undefNodeAnn))
--		sizeCommon = prettyMyAST (substitute rhsSize 
--				(CVar (builtinIdent varChunked) undefNodeAnn) 
--				(CVar (builtinIdent "curr_chunk_size")  undefNodeAnn))
--	in 
--		(sizeRank0, sizeRankN, sizeCommon)

-----------------------------------------------------------
---- OpenMP functions
-----------------------------------------------------------


--toOMPFromAST pragmaInfo ast = 
--	return (printAST_OMP [] pragmaInfo ast)


--printAST_OMP toBeSent pragmaInfo (CTranslUnit defs _) = 
--	concat (map (++"\n") (map (printAST_OMP_Def pragmaInfo) defs))


--printAST_OMP_Def pragmaInfo fun@(CFDefExt (CFunDef _ _ _ (CCompound _ body _) _)) = 
--	let  
--		bodyStr = map (printASTBlockItem_OMP pragmaInfo) body
--	in 
--		(printUntilOpenCurvedBracket (lines (prettyMyAST fun))) ++ (concat (map (++"\n") bodyStr)) ++ "}\n"
--printAST_OMP_Def _ other = 
--	prettyMyAST other

--printASTBlockItem_OMP pragmaInfo block 
--	| (isKernel "omp" pragmaInfo block) && (isPragmaAnn ("iteration_independent") pragmaInfo block) = 
--		let 
--			strStmt = prettyMyAST block
--		in
--			"\t\t" ++ "int num_proc = omp_get_num_threads();\n" ++ "#pragma omp parallel for\n" ++ strStmt
--	| otherwise =
--		prettyMyAST block

