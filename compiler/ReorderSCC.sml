(* reorder/reorder-scc.sml
 * Find strongly connected components in a directed graph.
 * Algorithm from Aho, Hopcroft, Ullman: "Data Structures and Algorithms",
 * Addison-Wesley, 1983, page 224.
 *)
structure ReorderSCC : REORDER_SCC =
  struct

    datatype State = INIT | POST | SCC
    datatype 'a Vertex = V of {attr: 'a,
			       state: State ref,
			       succ: int list,
			       pred: int list ref}

    fun vertex(attr, succ) = V{attr=attr, state=ref INIT, succ=succ, pred=ref[]}

    fun digraph xs =
      let val vs = Vector.tabulate(Vector.length xs,
				   fn i => vertex(Vector.sub(xs, i)))
	  fun pred_by i j =	(* i --> j *)
	    let val V{pred,...} = Vector.sub(vs, j)
	    in
	      pred := i :: !pred
	    end
	  fun update_succs(i, V{succ,...}) = List.app (pred_by i) succ
      in
		Vector.appi update_succs (vs); (* , 0 ,NONE); *)
		vs
      end

    fun postorder vs =
      let fun down(i, V{state,succ,...}, order) =
	    case !state
	      of POST	=> order
	       | _	=> (state := POST; i :: List.foldl down' order succ)
	  and down'(i, order) = down(i, Vector.sub(vs, i), order)
      in
		Vector.foldli down [] (vs) (* ,0 ,NONE) *)
      end

    fun components(vs, is) =
      let fun down(i, scc) =
	    let val V{attr,state,pred,...} = Vector.sub(vs, i)
	    in
	      case !state
		of SCC	=> scc
		 | _	=> (state := SCC; List.foldl down (attr::scc) (!pred))
	    end
	  fun component(i, sccs) =
	    case down(i, [])
	      of []	=> sccs
	       | scc	=> scc :: sccs
      in
		List.foldl component [] is
      end

    fun scc xs =
      let val vs = digraph xs
      in
		components(vs, postorder vs)
      end

  end (* structure ReorderSCC *)
