// Copyright (c) 2013-2016, The IMDEA Software Institute and
// Copyright (c) 2013-2016, Universidad Politécnica de Madrid

// See LICENSE.txt and AUTHORS.txt for licensing and authorship


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
// Rules for expressions
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

// Remove the identity of a binary operator
remove_identity
{
	pattern:
	{
		bin_op(cop(op),cexpr(a),cexpr(b));
	}
	condition:
	{
        // b is the identity of op
		is_identity(cop(op),cexpr(b));
        // Because it is removed
        pure(cexpr(b)); 
	}
	generate:
	{
		cexpr(a);
	}
}

// Reduce an operation to 0 when the right operator is 0
reduce_to_0
{
    pattern:
    {
        cexpr(a) * 0;
    }
    condition:
    {
        // Because it is removed
        pure(cexpr(a));
    }
    generate:
    {
        0;
    }
}

undo_distributive
{
    pattern:
    {
        (cexpr(b) * cexpr(a)) + (cexpr(c) * cexpr(a));
    }
    condition:
    {
        // The evaluation order changes
        pure(cexpr(a));
        pure(cexpr(b));
        pure(cexpr(c));
    }
    generate:
    {
        cexpr(a) * (cexpr(b) + cexpr(c));
    }
}

// Change a substraction aa - bb to multiplication (a-b) + (a+b)
sub_to_mult
{
    pattern:
    {
        cexpr(a) * cexpr(a) - cexpr(b) * cexpr(b);
    }
    condition:
    {
        // The evaluation order changes
        pure(cexpr(a));
        pure(cexpr(b));
    }
    generate:
    {
        (cexpr(a) - cexpr(b)) * (cexpr(a) + cexpr(b));
    }
}


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
// Rules for loops
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

// rule to normalize iteration step of a for loop, for example:
// for(i=0;i<N;i+=3){body}
// becomes
// for(i=0;i<(N/3);i++){subs(body,i,i*3)}
normalize_iteration_step
{
    pattern:
    {
        for(cexpr(i) = cexpr(initial_value); cexpr(i) < cexpr(n); cexpr(i) += cexpr(step))
	    {
            cstmts(body);
        }
    }
    condition:
    {
        // Because body should not modify control
        no_writes(cexpr(i), cstmts(body));
        // Because it is replicated in the body
        pure(cexpr(step));
        // Because it is replicated in the body
        pure(cexpr(initial_value));
        // Because condition is checked less times
        pure(cexpr(n));
    }
    generate:
    {
        for(cexpr(i) = 0; cexpr(i) < (cexpr(n)/cexpr(step)); cexpr(i) ++)
        {
	       subs(cstmts(body), cexpr(i), cexpr(i)  * cexpr(step) + cexpr(initial_value));
        }
        // This assignment maintains the final value of cexpr(i)
        // The ternary is emulating the behavior of for expression, depending on 
        //   whether it is executed or not. 
        cexpr(i) = 
            cexpr(initial_value) < cexpr(n)? 
            cexpr(i)  * cexpr(step) + cexpr(initial_value): 
            cexpr(initial_value);
    }
}

// Change the order of the for loop. From decremental to incremental.
loop_reversal_d2i
{
    pattern:
    {
        for(cexpr(i) = cexpr(n) - 1; cexpr(i) >= 0; cexpr(i)--)
        {
            cstmts(body);
        }
    }
    condition:
    {
        // Because is executed more times.
        pure(cexpr(n));
        // Body should not modify control
        no_writes(cexpr(i), cstmts(body));
    }
    generate:
    {
        for(cexpr(i) = 0; cexpr(i) < cexpr(n); cexpr(i)++)
        {
            cstmts(body);
        }
        // Leaves the correct final value for i
        cexpr(i) = (cexpr(n) - 1) >= 0? -1 : cexpr(n) - 1;
    }
}

// Change the order of the for loop. From incremental to decremental.
loop_reversal_i2d
{
    pattern:
    {
        for(cexpr(i) = 0; cexpr(i) < cexpr(n); cexpr(i)++)
        {
            cstmts(a);
        }
    }
    condition:
    {
        // Because is executed less times.
        pure(cexpr(n));
        // Body should not modify control
        no_writes(cexpr(i),cstmts(a));
    }
    generate:
    {
        for(cexpr(i) = cexpr(n) - 1; cexpr(i) >= 0; cexpr(i)--)
        {
            cstmts(a);
        }
        // Leaves the correct final value for i
        cexpr(i) = cexpr(n);
    }
}

// Loop interchange
loop_interchange
{
    pattern:
    {
        cstmts(ini);
        for(cexpr(i) = cexpr(init_i);cexpr(i) < cexpr(n_i);cexpr(i)++)
        {
            for(cexpr(j) = cexpr(init_j);cexpr(j) < cexpr(n_j);cexpr(j)++)
            {
                cstmts(body);
            }
        }
        cstmts(end);
    }
    condition:
    {
        // The evaluation order for the following expressions is changed
        pure(cexpr(init_j));
        pure(cexpr(n_j));
        pure(cexpr(init_i));
        pure(cexpr(n_i));
        // Controls are not modified in body
        no_rw(cexpr(i),cexpr(body));
        no_rw(cexpr(j),cexpr(body));
        //for loop should be annotated with iteration_idependent
        //i.e. it has not loop carried dependecies
    }
    generate:
    {   
        cstmts(ini);
        for(cexpr(j) = cexpr(init_j);cexpr(j) < cexpr(n_j);cexpr(j)++)
        {
            for(cexpr(i) = cexpr(init_i);cexpr(i) < cexpr(n_i);cexpr(i)++)
            {
                cstmts(body);
            }
        }
        cstmts(end);
    }
}

// Loop interchange (pragma version)
loop_interchange_pragma
{
    pattern:
    {
        cstmts(ini);
        #pragma polca iteration_idependent
        #pragma polca def a
        for(cexpr(i) = cexpr(init_i);cexpr(cond_i) < cexpr(n_i);cexpr(i)++)
        {
            for(cexpr(j) = cexpr(init_j);cexpr(j) < cexpr(n_j);cexpr(j)++)
            {
                cstmts(body);
            }
        }
        cstmts(end);
    }
    condition:
    {
        // The evaluation order for the following expressions is changed
        pure(cexpr(init_j));
        pure(cexpr(n_j));
        pure(cexpr(init_i));
        pure(cexpr(n_i));
        // Controls are not modified in body
        no_rw(cexpr(i),cexpr(body));
        no_rw(cexpr(j),cexpr(body));
    }
    generate:
    {   
        cstmts(ini);
        #pragma polca iteration_idependent
        #pragma polca same_properties a
        for(cexpr(j) = cexpr(init_j);cexpr(j) < cexpr(n_j);cexpr(j)++)
        {
            for(cexpr(i) = cexpr(init_i);cexpr(i) < cexpr(n_i);cexpr(i)++)
            {
                cstmts(body);
            }
        }
        cstmts(end);
    }
}

// Chunks the iteration space of a for-loop
for_chunk
{
    pattern:
    {
        cstmts(ini);
        #pragma polca def a 
        for(cexpr(i) = 0; cexpr(i) < cexpr(n); cexpr(i)++)
        {
            cstmts(body);
        }
        cstmts(end);
    }
    condition:
    {
      // Body should not modified control var
      no_writes(cexpr(i),cstmts(body));
      // It appears more times in the generated code
      pure(cexpr(n));
    }
    generate:
    {
        //num_proc should be declared in the program
        // int num_proc = 4;
        cstmts(ini);
        #pragma polca same_properties a
        #pragma polca chunked cexpr(n)
        #pragma polca prev_chunk_size cexpr(prev_chunk_size)
        #pragma polca curr_chunk_size cexpr(curr_chunk_size)        
        // for(cexpr(i) = 0; cexpr(i) < num_proc; cexpr(i)++)
        for(cexpr(i) = 0; cexpr(i) < 4; cexpr(i)++)
        {
          cdecl(cint(),cexpr(prev_chunk_size));
          // cexpr(prev_chunk_size) = cexpr(n) / num_proc;
          cexpr(prev_chunk_size) = cexpr(n) / 4;
          cdecl(cint(),cexpr(curr_chunk_size));
          cexpr(curr_chunk_size) = 
            // cexpr(i) != (num_proc-1) ? 
            cexpr(i) != (4-1) ? 
            cexpr(prev_chunk_size) : 
            // cexpr(prev_chunk_size) + cexpr(n) % num_proc;
            cexpr(prev_chunk_size) + cexpr(n) % 4;
          cdecl(cint(),cexpr(j));

          for(cexpr(j)=cexpr(i)*cexpr(prev_chunk_size);
                cexpr(j) < ((cexpr(i)+1)*cexpr(curr_chunk_size));
                cexpr(j)++)
          {
            subs(cstmts(body), cexpr(i), cexpr(j));
          }
        }
        cstmts(end);
    }
}

// Duplicates the number of operations per iteration.
unrolling
{
    pattern:
    {
        cstmts(body0_1);
        for (cexpr(i) = cexpr(ini); cexpr(cond); cexpr(inc))
        {
            cstmts(body1);
        }
        cstmts(body0_2);
    }
    generate:
    {
        cstmts(body0_1);
        for (cexpr(i) = cexpr(ini); cexpr(cond); cexpr(inc))
        {
            cstmts(body1);
            cexpr(inc);
            if(cexpr(cond))
            {
               cstmts(body1); 
            }
        }
        cstmts(body0_2);
    }
}

// Move a statement which are before a for statement inside the loop
move_inside_for_pre
{
    pattern:
    {
        cstmts(pre0);
        cstmt(moved_inside);
        cstmts(pre1);
        for (cexpr(i) = cexpr(ini); cexpr(cond); cexpr(inc))
        {
            cstmts(body);
        }
        cstmts(post);
    }
    condition:
    {
        no_writes_in_read(cstmt(moved_inside),cstmts(cond));
        no_writes_in_read(cstmt(moved_inside),cstmts(ini));
        no_writes(cexpr(i), cstmt(moved_inside));
        all_are_decl(cstmts(pre1));
    }
    generate:
    {
        cstmts(pre0);
        cstmts(pre1);
        for (cexpr(i) = cexpr(ini); cexpr(cond); cexpr(inc))
        {
            if(cexpr(i) == cexpr(ini))
            {
                cstmt(moved_inside);
            }
            cstmts(body);
        }
        cstmts(post);
    }
}

// Move a statement which are after a for statement inside the loop
move_inside_for_post
{
    pattern:
    {
        cstmts(pre);
        for (cexpr(i) = cexpr(ini); cexpr(i) < cexpr(limit); cexpr(i)++)
        {
            cstmts(body);
        }
        cstmts(post0);
        cstmt(moved_inside);
        cstmts(post1);
    }
    condition:
    {
        no_writes(cexpr(i), cstmt(moved_inside));
        all_are_decl(cstmts(post0));
        pure(cexpr(limit));
    }
    generate:
    {
        cstmts(pre);
        cstmts(post0);
        for (cexpr(i) = cexpr(ini); cexpr(i) < cexpr(limit); cexpr(i)++)
        {
            cstmts(body);
            if(cexpr(i) == (cexpr(limit) - 1))
            {
                cstmt(moved_inside);
            }
        }
        cstmts(post1);
    }
}

move_enclosing_if_inside_for
{
    pattern:
    {
        cstmts(pre);
        if(cexpr(if_cond))
        {
            cstmts(pre1);
            for(cexpr(ini); cexpr(for_cond); cexpr(mod))
            {
                cstmts(for_body);
            }
        }
        cstmts(post);
    }
    condition:
    {
        no_writes_in_read(cstmts(for_body),cexpr(if_cond));
        no_writes_in_read(cstmts(ini),cexpr(if_cond));
        no_writes_in_read(cstmts(for_cond),cexpr(if_cond));
        no_writes_in_read(cstmts(mod),cexpr(if_cond));
        all_are_decl(cstmts(pre1));
    }
    generate:
    {
        cstmts(pre);
        cstmts(pre1);
        for(cexpr(ini); cexpr(for_cond); cexpr(mod))
        {
            if(cexpr(if_cond))
            {
                cstmts(for_body);
            }
        }
        cstmts(post);
    }  
}

// Collapse two for loops into a single one.
collapse_2_for_loops
{
    pattern:
    {
        // #pragma polca def a
        for(cexpr(i) = cexpr(initial_value); cexpr(i) < cexpr(limit1); cexpr(i)++)
       {
            cstmts(prelude);
            for(cexpr(j) = cexpr(initial_value); cexpr(j) < cexpr(limit2); cexpr(j)++)
            {
                cstmts(body);
            }
            cstmts(postlude);
       }
    }
    condition:
    {
        // There is not modification of control vars
        no_writes(cexpr(i),cstmts(body));
        no_writes(cexpr(j),cstmts(body));
        no_writes(cexpr(i),cstmts(prelude));
        no_writes(cexpr(i),cstmts(postlude));
        no_writes(cexpr(j),cstmts(prelude));
        no_writes(cexpr(j),cstmts(postlude));
        // They are used in several spots in the resulting code
        pure(cexpr(limit1));
        pure(cexpr(limit2));
    }
    generate:
    {
        // #pragma polca same_properties a
        filter_decl(cstmts(prelude));
        for(cexpr(j) = cexpr(initial_value); cexpr(j) < (cexpr(limit1) * cexpr(limit2)); cexpr(j)++)
        {
            if_then_else:
            {
                no_empty(cstmts(prelude));
                if(cexpr(j) % cexpr(limit2) == 0) 
                {
                    filter_no_decl(cstmts(prelude));
                }
                ;
            }
            subs(
                subs(cstmts(body),cexpr(j),cexpr(j)%cexpr(limit2)),
                cexpr(i),
                cexpr(j)/cexpr(limit2)
                ); 
            if_then_else:
            {
                no_empty(cstmts(postlude));
                if(cexpr(j) % cexpr(limit2) == 0) 
                {
                    cstmts(postlude);
                }
                ;
            }      
        }
        // These two assignments are needed to leave the correct values
        cexpr(i) = cexpr(limit1);
        cexpr(j) = cexpr(limit2);
    }
}


// Fission of two for-loops into a single one. AKA loop distribution or loop splitting
for_loop_fission
{
    pattern:
    {
        for(cexpr(i) = cexpr(init);cexpr(i) < cexpr(n);cexpr(i)++)
        {
            cstmts(body1);
            cstmt(mid);
            cstmts(body2);
        }
    }
    condition:
    {
        // Control var is not modified inside the body
        no_writes(cexpr(i),cstmts(body1));
        no_writes(cexpr(i),cstmt(mid));
        no_writes(cexpr(i),cstmts(body2));
        // This means that mid and body1 cannot read any value written by body2 in any iteration
        no_reads_in_written(cstmt(mid),cstmts(body2));
        no_reads_in_written(cstmts(body1),cstmts(body2));
        // To avoid leaving the second empty
        no_empty(cstmts(body2));
    }
    generate:
    {   
        for(cexpr(i) = cexpr(init);cexpr(i) < cexpr(n);cexpr(i)++)
        {
            cstmts(body1);
            cstmt(mid);
        }
        for(cexpr(i) = cexpr(init);cexpr(i) < cexpr(n);cexpr(i)++)
        {
            cstmts(body2);
        }
    }
}

// Fusion of two for-loops into a single one using pragma info
for_loop_fusion_mapmap
{
    pattern:
    {
        cstmts(ini);
        #pragma polca def a
        // By map we know that accesses are compatible
        #pragma polca map inputa outputa
        for(cexpr(i) = cexpr(init);cexpr(i) < cexpr(n);cexpr(i)++)
        {
            cstmts(bodyFOR1);
        }
        cstmts(mid);
        #pragma polca def b
        #pragma polca map inputb outputb
        for(cexpr(j) = cexpr(init);cexpr(j) < cexpr(n);cexpr(j)++)
        {
            cstmts(bodyFOR2);
        }
        cstmts(fin);
    }
    condition:
    {
        // bodyFOR1 cannot modify control var of second loop
        no_writes(cexpr(j),cstmts(bodyFOR1));
        // Rest of control var cannot be modified, because that could change the iteration space
        no_writes(cexpr(j),cstmts(bodyFOR2)); 
        no_writes(cexpr(i),cstmts(bodyFOR1)); 
        no_writes(cexpr(i),cstmts(bodyFOR2));  
        // mid does not read vars that can be modify by the first loop
        no_reads_in_written(cstmts(mid),cstmts(bodyFOR1));
        // To prevent side-effects
        not(has_calls(bodyFOR1));
    }
    generate:
    {
        cstmts(ini);
        cexpr(i) = cexpr(init) < cexpr(n)? cexpr(n) : cexpr(init);
        cstmts(mid);
        #pragma polca same_properties a
        #pragma polca same_properties b
        for(cexpr(j) = cexpr(init);cexpr(j) < cexpr(n);cexpr(j))
        {
            subs(cstmts(bodyFOR1), cexpr(i), cexpr(j));
            cstmts(bodyFOR2);
        }
        cstmts(fin);
    }
}

// Fusion of two for-loops into a single one using pragma info
// for_loop_fusion_mapfoldl
// {
//     pattern:
//     {
//         cstmts(ini);
//         #pragma polca def a
//         // By map and fold we know that accesses are compatible
//         #pragma polca map inputa outputa
//         for(cexpr(i) = cexpr(init);cexpr(i) < cexpr(n);cexpr(i)++)
//         {
//             cstmts(bodyFOR1);
//         }
//         cstmts(mid);
//         #pragma polca def b
//         #pragma polca foldl inputb outputa outputb
//         for(cexpr(j) = cexpr(init);cexpr(j) < cexpr(n);cexpr(j)++)
//         {
//             cstmts(bodyFOR2);
//         }
//         cstmts(fin);
//     }
//     condition:
//     {
//         // bodyFOR1 cannot modify control var of second loop
//         no_writes(cexpr(j),cstmts(bodyFOR1));
//         // Rest of control var cannot be modified, because that could change the iteration space
//         no_writes(cexpr(j),cstmts(bodyFOR2)); 
//         no_writes(cexpr(i),cstmts(bodyFOR1)); 
//         no_writes(cexpr(i),cstmts(bodyFOR2));  
//         // mid does not read vars that can be modify by the first loop
//         no_reads_in_written(cstmts(mid),cstmts(bodyFOR1));
//         // To prevent side-effects
//         not(has_calls(bodyFOR1));
//     }
//     generate:
//     {
//         cstmts(ini);
//         cexpr(i) = cexpr(init) < cexpr(n)? cexpr(n) : cexpr(init);
//         cstmts(mid);
//         #pragma polca same_properties a
//         #pragma polca same_properties b
//         for(cexpr(j) = cexpr(init);cexpr(j) < cexpr(n);cexpr(j))
//         {
//             subs(cstmts(bodyFOR1), cexpr(i), cexpr(j));
//             cstmts(bodyFOR2);
//         }
//         cstmts(fin);
//     }
// }

// Fusion of two for-loops into a single one. 
for_loop_fusion
{
    pattern:
    {
        cstmts(ini);
        for(cexpr(i) = cexpr(init);cexpr(i) < cexpr(n);cexpr(i)++)
        {
            cstmts(bodyFOR1);
        }
        cstmts(mid);
        for(cexpr(j) = cexpr(init);cexpr(j) < cexpr(n);cexpr(j)++)
        {
            cstmts(bodyFOR2);
        }
        cstmts(fin);
    }
    condition:
    {
        // bodyFOR1 cannot modify control var of second loop
        // Since j is different in each iteration reads are not allowed in bodyFOR1
        no_rw(cexpr(j),cstmts(bodyFOR1));
        // Rest of control var cannot be modified, because that could change the iteration space
        no_writes(cexpr(j),cstmts(bodyFOR2)); 
        no_writes(cexpr(i),cstmts(bodyFOR1)); 
        no_writes(cexpr(i),cstmts(bodyFOR2));  
        // mid does not read vars that can be modify by the first loop
        no_reads_in_written(cstmts(mid),cstmts(bodyFOR1));
        // To prevent side-effects
        not(has_calls(bodyFOR1));
        // bodyFOR2 should not read things written by bodyFOR1
        // The following condition can be relaxed. It can read things calculated in the itetarions from 0 to the current
        no_reads_in_written(cstmts(bodyFOR2),cstmts(bodyFOR1));
        // If bodyFOR2 is modifying things that bodyFOR1 uses then we cannot apply the rule
        no_reads_in_written(cstmts(bodyFOR1),cstmts(bodyFOR2));
        // The dependeces should be compatible. For instance, 
        // for(i...n) v[i-1] = x; for(j..n) w[i-1] = v[i]; is not compatible
    }
    generate:
    {
        cstmts(ini);
        cexpr(i) = cexpr(init) < cexpr(n)? cexpr(n) : cexpr(init);
        cstmts(mid);
        for(cexpr(j) = cexpr(init);cexpr(j) < cexpr(n);cexpr(j)++)
        {
            subs(cstmts(bodyFOR1), cexpr(i), cexpr(j));
            cstmts(bodyFOR2);
        }
        cstmts(fin);
    }
}

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
// Syntactic transformations / Normalization
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

// Transform a for statement without block in a if statemst with block
for_wo_block_2_for_w_block
{
    pattern:
    {
        for(cexpr(i) = cexpr(initial_value); cexpr(cond); cexpr(inc))
            cstmt(body);
    }
    condition:
    {
        // If it is a block we do not want to remove it
    	not(is_block(cstmt(body)));
    }
    generate:
    {
        for(cexpr(i) = cexpr(initial_value); cexpr(cond); cexpr(inc))
        {
            cstmt(body);
        }
    }
}

// Empty for loop removal
remove_empty_for
{
    pattern:
    {
        cstmts(ini);
        for(cexpr(i) = cexpr(initial_value); cexpr(i) < cexpr(limit); cexpr(i)++)
        {}
        cstmts(end);
    }
    condition:
    {
        // This expressions are replicated so they should be pure
        pure(cexpr(initial_value));
        pure(cexpr(limit));
    }
    generate:
    {
        cstmts(ini);
        cexpr(i) = cexpr(initial_value) < cexpr(limit)? cexpr(limit): cexpr(initial_value);
        cstmts(end);
    }
}

// Transform a for into a while
for_to_while
{
    pattern:
    {
        cstmts(ini);
        for(cexpr(i) = cexpr(initial_value); cexpr(i) < cexpr(n); cexpr(i)++)
		{
			cstmts(body);
		}
        cstmts(end);
    }
    generate:
    {
        cstmts(ini);
    	cexpr(i) = cexpr(initial_value);
        while(cexpr(i) < cexpr(n))
		{
			cstmts(body);
			cexpr(i)++;
		}
        cstmts(end);
    }
}

// Transform a while into a for
while_to_for
{
    pattern:
    {
        cstmts(ini);
        cexpr(i) = cexpr(initial_value);
        while(cexpr(i) < cexpr(n))
        {
            cstmts(body1);
            cexpr(i)++;
        }
        cstmts(end);
    }
    generate:
    {
        cstmts(ini);
        for(cexpr(i) = cexpr(initial_value); cexpr(i) < cexpr(n); cexpr(i)++)
        {
            cstmts(body1);
        }
        cstmts(end);
    }
}

// Transform a if statement without block in a if statemst with block
if_wo_block_2_if_w_block
{
    pattern:
    {
        if(cexpr(cond))
            cstmt(body);
    }
    condition:
    {
        // We do not want to apply the rule if it is already a block
        not(is_block(cstmt(body)));
    }
    generate:
    {
        if(cexpr(cond))
        {
            cstmt(body);
        }
    }
}

// Transform a if statement without else block into a if statement with
if_wo_else_2_if_w_else
{
    pattern:
    {
        if(cexpr(cond))
        {
            cstmts(body);
        }
    }
    generate:
    {
        if(cexpr(cond))
        {
            cstmts(body);
        }
        else
        {}
    }
}

// Transform a addition assignment into a simple assignment
split_addition_assign
{
    pattern:
    {
        cexpr(a) += cexpr(b);
    }
    condition:
    {
        // This expression is duplicated in the resulting code.
        pure(cexpr(a));
    }
    generate:
    {
        cexpr(a) = cexpr(a) + cexpr(b);
    }
}

// Transform a simple assignment into an addition assignment  
join_addition_assign
{
    pattern:
    {
        cexpr(a) = cexpr(a) + cexpr(b);
    }
    condition:
    {
        // One of these expression is lost in the resulting code
        pure(cexpr(a));
    }
    generate:
    {
        cexpr(a) += cexpr(b);
    }
}

// move external multiplication operation inside a ternary expression
mult_ternary_2_ternary
{
    pattern:
    {
        cexpr(lop) * (cexpr(cond)?cexpr(then_):cexpr(else_));
    }
    condition:
    {
        // It can be evaluated twice in the new code
        pure(cexpr(lop));
    }
    generate:
    {
        cexpr(cond)? cexpr(lop) * cexpr(then_):cexpr(lop) * cexpr(else_);
    }
}

// move external multiplication operation inside a ternary expression
sum_ternary_2_ternary
{
    pattern:
    {
        cexpr(lop) + (cexpr(cond)?cexpr(then_):cexpr(else_));
    }
    condition:
    {
        // The evaluation order changes, so we need to assure that
        // the evaluation of this expression does not affect the rest.
        pure(cexpr(lop));
    }
    generate:
    {
        cexpr(cond)? cexpr(lop) + cexpr(then_):cexpr(lop) + cexpr(else_);
    }
}

// converts an assignement to a ternary in an if statement
assign_ternary_2_if_else
{
    pattern:
    {
        cstmts(ini);
        cexpr(lhs) = cexpr(cond)?cexpr(then_):cexpr(else_);
        cstmts(end);
    }
    condition:
    {
        // The evaluation order changes, so we need to assure that
        // the evaluation of this expression does not affect the rest.
        pure(cexpr(lhs));
    }
    generate:
    {
        cstmts(ini);
        if (cexpr(cond))
        {
            cexpr(lhs) = cexpr(then_);
        }
        else
        {
            cexpr(lhs) = cexpr(else_);
        }
        cstmts(end);
    }
}

// converts an if-else statement in an assignement to a ternary 
if_else_2_assign_ternary
{
    pattern:
    {
        cstmts(ini);
        if (cexpr(cond))
        {
            cexpr(lhs) = cexpr(then_);
        }
        else
        {
            cexpr(lhs) = cexpr(else_);
        }
        cstmts(end);
    }
    condition:
    {
        // The evaluation order changes, so we need to assure that
        // the evaluation of this expression does not affect the rest.
        pure(cexpr(lhs));
    }
    generate:
    {
        cstmts(ini);
        cexpr(lhs) = cexpr(cond)?cexpr(then_):cexpr(else_);
        cstmts(end);
    }
}

// converts an if statement (without else) in an assignement to a ternary 
if_2_assign_ternary
{
    pattern:
    {
        cstmts(ini);
        if (cexpr(cond))
        {
            cexpr(lhs) = cexpr(then_);
        }
        cstmts(end);
    }
    condition:
    {
        // Its evaluation can be duplicated;
        // Morevover, the evaluation order has changed.
        pure(cexpr(lhs));
    }
    generate:
    {
        cstmts(ini);
        cexpr(lhs) = cexpr(cond)?cexpr(then_):cexpr(lhs);
        cstmts(end);
    }
}

// remove an empty else
empty_else
{
    pattern:
    {
        if (cexpr(cond))
        {
            cstmts(then_);
        }
        else {}
    }
    generate:
    {
        if (cexpr(cond))
        {
            cstmts(then_);
        }
    }
}

// Remove a ternary operator 
remove_ternary
{
    pattern:
    {
        cexpr(cond)?cexpr(then_):cexpr(else_);
    }
    condition:
    {
        // Evalauted twice in the resulting code
        pure(cexpr(cond));
        // Evaluated both in the resulting code while in the patter only one of them.
        pure(cexpr(then_));
        pure(cexpr(else_));
    }
    generate:
    {
        cexpr(cond) * cexpr(then_) + (1 - cexpr(cond)) * cexpr(else_);
    }
}


// Removes a block of code and introduces fresh varaibles to avoid clashes. 
remove_block
{
    pattern:
    {
        cstmts(ini);
        {
            cstmts(block);
        }
        cstmts(fin);
    }
    generate:
    {
        cstmts(ini);
        fresh(cstmts(block));
        cstmts(fin);
    }
}

//Removes a useless if statement
remove_empty_if
{
    pattern:
    {
        cstmts(ini);
        if(cexpr(cond))
        {}
        cstmts(end);
    }
    condition:
    {
        // Because it is going to dissapear.
        // An alternative would be to replace the if-statement by cond.
        pure(cexpr(cond));
    }
    generate:
    {
        cstmts(ini);
        cstmts(end);
    }
}

//Removes a statment that does not affect to the overall computation
remove_useless_statement
{
    pattern:
    {
        cstmts(ini);
        cstmt(mid);
        cstmts(end);
    }
    condition:
    {
        is_expr(cstmt(mid));
        pure(cstmt(mid));
    }
    generate:
    {
        cstmts(ini);
        cstmts(end);
    }    
}

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
// Optimization/ Partial Evaluation
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

// Loop-based strength reduction 
strength_reduction
{
    pattern:
    {
        cstmts(ini);
        for(cexpr(i) = cexpr(init);cexpr(i) < cexpr(n);cexpr(i)++)
        {
            cstmts(ini1);
            cexpr(a) = cexpr(b) + (cexpr(c) * cexpr(i));
            cstmts(end1);
        }
        cstmts(end);
    }
    condition:
    {
        // Should be a constant expression
        is_cons(cexpr(c));
        // Control var is not modified inside the body (here split in multiple vars)
        no_writes(cexpr(i), cstmts(ini1));
        no_writes(cexpr(i), cstmts(end1));
        no_writes(cexpr(i), cexpr(b));
        no_writes(cexpr(i), cexpr(a));
    }
    generate:
    {   
        cstmts(ini);
        cdecl(cint(),cexpr(t));
        cexpr(t) = 0;
        for(cexpr(i) = cexpr(init);cexpr(i) < cexpr(n);cexpr(i)++)
        {
            cstmts(ini1);
            cexpr(a) = cexpr(b) + t;
            t += cexpr(c);
            cstmts(end1);
        }
        cstmts(end);
    }
}

// remove a useless assignment 
useless_assign
{
    pattern:
    {
        cstmts(ini);
        cexpr(lhs) = cexpr(lhs);
        cstmts(fin);
    }
    condition:
    {
        // It dissapears in the resulting code
        pure(cexpr(lhs));
    }
    generate:
    {
        cstmts(ini);
        cstmts(fin);
    }
}

// Replace an expression that is equal to another in 
// an if statement context
replace_var_equal
{
    pattern:
    {
        if (cexpr(a) == cexpr(b))
        {
            cstmts(body);
        }
    }
    condition:
    {
        // One is going to be replaced by the other
        // So some will be removed, and some replicated.
        pure(cexpr(a));
        pure(cexpr(b));
    }
    generate:
    {
        if (cexpr(a) == cexpr(b))
        {
            gen_list:
            {   
                if_then:
                {
                    no_writes(cexpr(a),cstmts(body));
                    subs(cstmts(body),cexpr(a),cexpr(b));
                }
                if_then:
                {
                    no_writes(cexpr(b),cstmts(body));
                    subs(cstmts(body),cexpr(b),cexpr(a));
                }
            }
        }
    } 
}

contiguous_same_if
{
    pattern:
    {
        cstmts(pre);
        if (cexpr(cond))
        {
            cstmts(body1);
        }
        if (cexpr(cond))
        {
            cstmts(body2);
        }
        cstmts(post);
    }
    condition:
    {
        no_writes_in_read(cstmts(body1), cexpr(cond));
    }
    generate:
    {
        cstmts(pre);
        if (cexpr(cond))
        {
            cstmts(body1);
            cstmts(body2);
        }
        cstmts(post);
    }
}



// remove a for loop that just run one statemnt during its iterations
just_one_iteration_removal
{
    pattern:
    {
        for (cexpr(i) = cexpr(ini); cexpr(i) < cexpr(n); cexpr(i)++)
        {
            if (cexpr(i) == cexpr(other))
            {
                cstmt(one_stat);
            }
        }
    }
    condition:
    {
        // These vars dissapear
        pure(cexpr(ini));
        pure(cexpr(n));
        // This can be replicated
        pure(cexpr(other));
        // Control var should bot modifedf during the evaluation of one_stat
        no_writes(cexpr(i), cstmt(one_stat));
    }
    generate:
    {
        subs(cstmt(one_stat),cexpr(i),cexpr(other));
        // Final value of cexpr(i);
        cexpr(i) = cexpr(n);
    }
}

// Join two assignments to the same variable in a single one
join_assignments
{
    pattern:
    {
        cstmts(ini);
        cexpr(v1) = cexpr(val_v1);
        cstmts(mid);
        cexpr(v1) = cexpr(val_v2);
        cstmts(end);
    }
    condition:
    {
        // What v1 writes is not used by mid
        no_writes_in_read(cexpr(v1),cstmts(mid));
        // These vars are going to be replaced
        pure(cexpr(val_v1));  
        pure(cexpr(val_v2));        
    }
    generate:
    {
        cstmts(ini);
        cstmts(mid);
        cexpr(v1) = subs(cexpr(val_v2),cexpr(v1),cexpr(val_v1));
        cstmts(end);
    }  
}

// Propagate the value assign to a variable, replacing the occurrences of 
// the variable with the assigned expression.
propagate_assignment
{
    pattern:
    {
        cstmts(ini);
        cexpr(lhs) = cexpr(rhs);
        cstmts(fin);
    }
    condition:
    {
        // They are goint to be mutually replaced
        pure(cexpr(lhs));
        pure(cexpr(rhs));
        // lhs is not written in the rest of the block
        no_writes(cexpr(lhs), cstmts(fin));
        // Only apply the rule when fin reads the lhs of the removed assignment
        reads(cexpr(lhs), cstmts(fin));
        no_empty(cstmts(fin));
    }
    generate:
    {
        cstmts(ini);
        subs(cstmts(fin),cexpr(lhs),cexpr(rhs));
        // Needed to conserve the original value
        // an alternative could be to assure (using decl) that the scope of lhs is only this block
        cexpr(lhs) = cexpr(rhs);
    }
}



// Extracts an expression within a loop that evualuates to the same value 
// in all iterations. The extraction is performed by an assignment before 
// entering the loop and, then using the variable instead of the expression
// inside the loop.
loop_inv_code_motion
{
    pattern:
    {
        cstmts(body0_1);
        for (cexpr(i) = cexpr(ini); cexpr(cond); cexpr(inv))
        {
            cstmts(body1_1);
            cexpr(c) = cexpr(a) * cexpr(b);
            cstmts(body2_1);
        }
        cstmts(body0_2);
    }
    condition:
    {
        // The evaluation order and the number of times is is evaluated changes.
        pure(cexpr(b));
        // It does not depends on the control
        no_reads(cexpr(i), cexpr(b));
        // If it is a variable or a constant the transformation is useless. 
        // This, this is an optimization to reduce the number of candidates.
        not(is_cons(cexpr(b)));
        not(is_var(cexpr(b)));
    }
    generate:
    {   
        cstmts(body0_1);
        cdecl(cint(),cexpr(k));
        cexpr(k) = cexpr(b);
        for (cexpr(i) = cexpr(ini); cexpr(cond); cexpr(inv))
        {
            cstmts(body1_1);
            cexpr(c) = cexpr(a) * cexpr(k);
            cstmts(body2_1); 
        }
        cstmts(body0_2);
    }
}

// Function inlining. A call is substituted by the whole definition
// with the parameters bound to the arguments values. 
inlining
{
    pattern:
    {
        cstmts(ini);
        cstmt(with_call);
        cstmts(fin);
    }
    condition:
    {
        // Only if calls can be replaced
        has_calls(cstmt(with_call));
    }
    generate:
    {
        cstmts(ini);
        inline_stmt(cstmt(with_call));
        cstmts(fin);
    }
}

// Function inlining. A call is substituted by the whole definition
// with the parameters bound to the arguments values.
inlining_assignment
{
    pattern:
    {
        cstmts(ini);
        cexpr(lhs) = cexpr(rhs);
        cstmts(fin);
    }
    condition:
    {
        // Only if rhs is a call
        is_call(cexpr(rhs));
    }
    generate:
    {
        cstmts(ini);
        inline_expr(cexpr(lhs), cexpr(rhs));
        cstmts(fin);
    }
}

// Creates an auxiliar variable to avoid evaluating
// twice a common expression
common_subexp_elimination
{
    pattern:
    {
        cstmts(ini);
        cexpr(a) = cexpr(c);
        cstmts(mid);
        cexpr(b) = cexpr(c);
        cstmts(fin);
    }
    condition:
    {
        // Optimization to disable useless transformations
        not(is_cons(cexpr(c)));
        not(is_var(cexpr(c)));
    }
    generate:
    {
        cstmts(ini);
        cdecl(cint(),cexpr(t));
        cexpr(t) = cexpr(c);
        cexpr(a) = cexpr(t);
        cstmts(mid);
        cexpr(b) = cexpr(t);
        cstmts(fin);
    }
}

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
// Data transformation
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

// scalar expansion
introduce_aux_array
{
    pattern:
    {
        cstmts(ini);
        cexpr(c) = 0.0;
        cstmts(mid);
        for(cexpr(i) = 0;cexpr(i) < cexpr(n);cexpr(i)++)
        {
            cstmts(body1);
            cexpr(c) += cexpr(a)[cexpr(i)] * cexpr(b)[cexpr(i)];
            cstmts(body2);
        }
        cstmts(end);
    }
    generate:
    {
        cstmts(ini);
        cexpr(c) = 0.0;
        int d[cexpr(n)];
        cstmts(mid);
        for(cexpr(i) = 0;cexpr(i) < cexpr(n);cexpr(i)++)
        {
            cstmts(body1);
            d[cexpr(i)] = cexpr(a)[cexpr(i)] * cexpr(b)[cexpr(i)];
            cexpr(c) += d[cexpr(i)];
            cstmts(body2);
        }
        cstmts(end);
    }
}

// flaten an integer 2D aray into a 1D array
flatten_float_array
{
    pattern:
    {
        cstmts(ini);
        // l1 and l2 should be static, therefore no need to define that
        // this expressions should be pure
        cdecl(cfloat(),cexpr(v)[cexpr(l1)][cexpr(l2)]);
        cstmts(end);
    }
    generate:
    {
        cstmts(ini);
        cdecl(cfloat(),cexpr(v)[cexpr(l1)*cexpr(l2)]);
        change_access(cstmts(end), cexpr(v), flatten(cexpr(l2)));
    }   
}

// flaten a float 2D aray into a 1D array
flatten_int_array
{
    pattern:
    {
        cstmts(ini);
        // l1 and l2 should be static, therefore no need to define that
        // this expressions should be pure
        cdecl(cint(),cexpr(v)[cexpr(l1)][cexpr(l2)]);
        cstmts(end);
    }
    generate:
    {
        cstmts(ini);
        cdecl(cint(),cexpr(v)[cexpr(l1)*cexpr(l2)]);
        change_access(cstmts(end), cexpr(v), flatten(cexpr(l2)));
    }   
}

subs_struct_by_fields
{
    pattern:
    {
        cstmts(ini);
        cdecl(cstruct(cexpr(struct_name)),cexpr(v)[cexpr(size)]);
        cstmts(end);
    }
    generate:
    {
        cstmts(ini);
        decl_from_struct(cexpr(v), cexpr(struct_name), cexpr(size));
        change_array(cstmts(end), cexpr(v), from_struct(cexpr(struct_name)));
    }   
}


roll_up_init
{
    pattern:
    {
        cstmts(ini0);
        cdecl(cfloat(),cexpr(l1));
        cstmts(ini1);
        cdecl(cfloat(),cexpr(l2));
        cstmts(ini2);
        cexpr(l1) = cexpr(r1);
        cexpr(l2) = cexpr(r2);
        cstmts(fin);
    }
    condition:
    {
        is_rollable(cexpr(r1), cexpr(r2));
        not(is_array(cexpr(l1)));
        not(is_array(cexpr(l2)));
    }
    generate:
    {
        cstmts(ini0);
        cstmts(ini1);
        cdecl(cfloat(),cexpr(l2));
        cstmts(ini2);
        cdecl(cfloat(),cexpr(d)[1]);
        cdecl(cint(),cexpr(i));
        #pragma polca rolled-up
        for(cexpr(i) = 0; cexpr(i) < 1; cexpr(i)++)
        {
            cexpr(d)[cexpr(i)] = change_access(cexpr(r1), all(), add(cexpr(i)));
        }
        cexpr(l2) = cexpr(r2);
        subs(cstmts(fin), cexpr(l1), cexpr(d)[0]);
    }
}

roll_up
{
    pattern:
    {
        cstmts(ini0);
        cdecl(cfloat(),cexpr(l2));
        cstmts(ini1);
        cdecl(cfloat(),cexpr(d)[cexpr(rolls)]);
        cdecl(cint(),cexpr(i));
        #pragma polca rolled-up
        for(cexpr(i) = 0; cexpr(i) < cexpr(rolls); cexpr(i)++)
        {
            cexpr(l1) = cexpr(r1);
        }
        cexpr(l2) = cexpr(r2);
        cstmts(fin);
    }
    condition:
    {
        is_rollable(cexpr(r1), cexpr(r2));
        not(is_array(cexpr(l2)));
        // is_decl_in(lhs(cstmt(s2)), cstmts(ini));
    }
    generate:
    {
        cstmts(ini0);
        cstmts(ini1);
        cdecl(cfloat(),cexpr(d)[cexpr(rolls) + 1]);
        cdecl(cint(),cexpr(i));
        #pragma polca rolled-up
        for(cexpr(i) = 0; cexpr(i) < cexpr(rolls) + 1; cexpr(i)++)
        {
            cexpr(l1) = cexpr(r1);
        }
        subs(cstmts(fin), cexpr(l2), cexpr(d)[0 + cexpr(rolls)]);
    }
}


roll_up_array_init
{
    pattern:
    {
        cstmts(ini);
        cexpr(l1) = cexpr(r1);
        cexpr(l2) = cexpr(r2);
        cstmts(fin);
    }
    condition:
    {
        is_rollable(cexpr(r1), cstmt(r2));
        is_array(cexpr(l1));
        is_rollable(cexpr(l1), cexpr(l2));
    }
    generate:
    {
        cstmts(ini);
        cdecl(cint(),cexpr(i));
        #pragma polca rolled-up
        for(cexpr(i) = 0; cexpr(i) < 1; cexpr(i)++)
        {
            change_access(cexpr(l1), all(), add(cexpr(i))) = change_access(cexpr(r1), all(), add(cexpr(i)));
        }
        cexpr(l2) = cexpr(r2);
        cstmts(fin);
    }
}

roll_up_array
{
    pattern:
    {
        cstmts(ini);
        cdecl(cint(),cexpr(i));
        #pragma polca rolled-up
        for(cexpr(i) = 0; cexpr(i) < cexpr(rolls); cexpr(i)++)
        {
            cexpr(l1) = cexpr(r1);
        }
        cexpr(l2) = cexpr(r2);
        cstmts(fin);
    }
    condition:
    {
        is_rollable(cexpr(r1), cstmt(r2));
        is_array(cexpr(l1));
        is_rollable(cexpr(l1), cexpr(l2));
    }
    generate:
    {
        cstmts(ini);
        cdecl(cint(),cexpr(i));
        #pragma polca rolled-up
        for(cexpr(i) = 0; cexpr(i) < cexpr(rolls) + 1; cexpr(i)++)
        {
            cexpr(l1) = cexpr(r1);
        }
        cstmts(fin);
    }
}

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
// Feautures extraction
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////

// Counts the number of statements before a for loop that can be moved inside it
feat_move_inside_for_pre
{
    pattern:
    {
        cstmts(pre);
        for (cexpr(i) = cexpr(ini); cexpr(cond); cexpr(inc))
        {
            cstmts(body);
        }
        cstmts(fin);
    }
    condition:
    {
        no_writes(cexpr(i), cstmts(pre));
    }
    generate:
    {
        count_no_decl(cstmts(pre));
    }
}

// Counts the number of statements after a for loop that can be moved inside it
feat_move_inside_for_post
{
    pattern:
    {
        cstmts(pre);
        for (cexpr(i) = cexpr(ini); cexpr(i) < cexpr(limit); cexpr(i)++)
        {
            cstmts(body);
        }
        cstmts(post);
    }
    condition:
    {
        pure(cexpr(limit));
    }
    generate:
    {
        count_no_decl(cstmts(post));
    }
}

// // Remove a 2D array that is just copying the values of another
// remove_aux_2D_array_int
// {
//     pattern:
//     {
//         cstmts(ini);
//         // This assures that the scope of the replaced array is only this block
//         cdecl(cint(),cexpr(v)[cexpr(l1)][cexpr(l2)]);
//         cstmts(mid);
//         for (cexpr(k) = 0; cexpr(k) < cexpr(l1); cexpr(k)++) {
//             for (cexpr(l) = 0; cexpr(l) < cexpr(l2); cexpr(l)++) {
//                 cexpr(v)[cexpr(k)][cexpr(l)] = 
//                     cexpr(nv)[cexpr(m1)+cexpr(k)][cexpr(m2)+cexpr(l)];
//             }
//         }
//         cstmts(end);
//     }
//     condition:
//     {
//         // The value of nv should not be changed in the rest of the block
//         no_writes(cexpr(nv), cstmts(end));
//         // They are going to be replicated
//         pure(cexpr(m1));
//         pure(cexpr(m2));
//     }
//     generate:
//     {
//         cstmts(ini);
//         cdecl(cint(),cexpr(v)[cexpr(l1)][cexpr(l2)]);
//         // TODO: Change by  change_array(cstmts(end), cexpr(v), from_copy(cexpr(nv), cexpr(m1), cexpr(m2));
//         //       In this way it would match rule subs_struct_by_fields type
//         change_array(cstmts(end), cexpr(v), cexpr(nv), cexpr(m1), cexpr(m2));
//     }   
// }


// // Remove a 2D array that is just copying the values of another
// remove_aux_2D_array_float
// {
//     pattern:
//     {
//         cstmts(ini);
//         // This assures that the scope of the replaced array is only this block
//         cdecl(cint(),cexpr(v)[cexpr(l1)][cexpr(l2)]);
//         cstmts(mid);
//         for (cexpr(k) = 0; cexpr(k) < cexpr(l1); cexpr(k)++) {
//             for (cexpr(l) = 0; cexpr(l) < cexpr(l2); cexpr(l)++) {
//                 cexpr(v)[cexpr(k)][cexpr(l)] = 
//                     cexpr(nv)[cexpr(m1)+cexpr(k)][cexpr(m2)+cexpr(l)];
//             }
//         }
//         cstmts(end);
//     }
//     condition:
//     {
//         // The value of nv should not be changed in the rest of the block
//         no_writes(cexpr(nv), cstmts(end));
//         // They are going to be replicated
//         pure(cexpr(m1));
//         pure(cexpr(m2));
//     }
//     generate:
//     {
//         cstmts(ini);
//         cdecl(cint(),cexpr(v)[cexpr(l1)][cexpr(l2)]);
//         cstmts(mid);
//         change_array(cstmts(end), cexpr(v), cexpr(nv), cexpr(m1), cexpr(m2));
//     }   
// }


/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
// Testing (rules without real application)
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////


// test_all
// {
//     pattern:
//     {
//         cstmts(body0_1);
//         all:
//         {
//             cexpr(a) = cexpr(b) + cexpr(c); 
//             cexpr(d) = cexpr(b) - cexpr(c);
//         }
//         cexpr(e) = cexpr(a) + cexpr(d);
//         cstmts(body0_2);
//     }
//     generate:
//     {
//         cstmts(body0_1);
//         cexpr(e) = 2 * cexpr(b);
//         cstmts(body0_2);
//     }
// }

// test_some
// {
//     pattern:
//     {
//         cstmts(body0_1);
//         some:
//         {
//             cexpr(a) = cexpr(b) * 2;
//             cexpr(a) = cexpr(b) + cexpr(b);
//         }
//         cexpr(c) = cexpr(a)/2;
//         cstmts(body0_2);
//     }
//     generate:
//     {
//         cstmts(body0_1);
//         cexpr(c) = cexpr(b);
//         cstmts(body0_2);
//     }
// }

/////////////////////////////////////////////////////
/////////////////////////////////////////////////////
// Experimental (not working)
/////////////////////////////////////////////////////
/////////////////////////////////////////////////////



// foldl_2_foldl2par
// {
//     pattern:
//     {
//         cstmts(pre);
//         #pragma polca def init
//         #pragma polca output cexpr(acc)
//         cexpr(acc) = 0;
//         #pragma polca foldl init cexpr(vs)
//         for (cexpr(i) = 0; cexpr(i) < cexpr(limit); cexpr(i)++)
//             cexpr(acc) += cexpr(vs)[cexpr(i)];
//         cstmts(post);
//     }
//     generate:
//     {
//         cstmts(pre);
//         cdecl(cint(),cexpr(vss)[cexpr(limit1) / cexpr(limit2)][cexpr(limit2)]);
//         #pragma polca splitEvery cexpr(limit2) cexpr(vs)
//         for (cexpr(i) = 0; cexpr(i) < cexpr(limit1) / cexpr(limit2); cexpr(i)++)
//             for (cexpr(j) = 0; cexpr(j) < cexpr(limit2); cexpr(j)++) 
//                 cexpr(vss)[cexpr(i)][cexpr(j)] = cexpr(vs)[cexpr(i) * cexpr(limit2) + cexpr(j)];
//         cdecl(cint(),cexpr(pts)[cexpr(limit1) / cexpr(limit2)]);
//         #pragma polca map cexpr(vss) cexpr(pts)
//         for (cexpr(i) = 0; cexpr(i) < cexpr(limit1) / cexpr(limit2); cexpr(i)++)
//         {
//             #pragma polca def init_pts
//             #pragma polca output cexpr(pts)
//             cexpr(pts)[cexpr(i)] = 0;
//             #pragma polca foldl init_pts cexpr(vss)[cexpr(i)]
//             for (cexpr(j) = 0; cexpr(j) < cexpr(limit2); cexpr(j)++) 
//                 cexpr(pts)[cexpr(i)] += cexpr(vss)[cexpr(i)][cexpr(j)];
//         }
//         #pragma polca def init
//         #pragma polca output cexpr(acc)
//         cexpr(acc) = 0;
//         #pragma polca foldl init cexpr(pts)
//         for (cexpr(i) = 0; cexpr(i) < cexpr(limit1) / cexpr(limit2); cexpr(i)++)
//             cexpr(acc) += cexpr(pts)[cexpr(i)];
//         cstmts(post);
//     }
// }

