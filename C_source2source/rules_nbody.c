
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
        // no_writes(cexpr(j),cstmts(postlude));
        // They are used in several spots in the resulting code
        pure(cexpr(limit1));
        pure(cexpr(limit2));
        // no_empty(cstmts(prelude));
        // no_empty(cstmts(postlude));
    }
    generate:
    {
        // #pragma polca same_properties a
        filter_decl(cstmts(prelude));
        cdecl(cint(),cexpr(ni));
        for(cexpr(ni) = cexpr(initial_value); cexpr(ni) < (cexpr(limit1) * cexpr(limit2)); cexpr(ni)++)
        {
            if(cexpr(ni) % cexpr(limit2) == 0) 
            {
                filter_no_decl(cstmts(prelude));
            }
            subs(
                subs(cstmts(body),cexpr(j),cexpr(ni)%cexpr(limit2)),
                cexpr(i),
                cexpr(ni)/cexpr(limit2)
                ); 
            if(cexpr(ni) % cexpr(limit2) == 0) 
            {
                cstmts(postlude);
            }    
        }
        // These two assignments are needed to leave the correct values
        cexpr(i) = cexpr(limit1);
        cexpr(j) = cexpr(limit2);
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
                subs(cstmt(moved_inside), cexpr(i), cexpr(i) + 1);
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

divide_if
{
    pattern:
    {
        cstmts(pre);
        if (cexpr(cond))
        {
            cstmt(to_new_if);
            cstmts(rest_body);
        }
        cstmts(post);
    }
    condition:
    {
        no_writes_in_read(cstmt(to_new_if), cexpr(cond));
        no_empty(cstmts(rest_body));
    }
    generate:
    {
        cstmts(pre);
        if (cexpr(cond))
        {
            cstmt(to_new_if);
        }
        if (cexpr(cond))
        {
            cstmts(rest_body);
        }
        cstmts(post);
    }
}
