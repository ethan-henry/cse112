(* Ethan Henry (efhenry@ucsc.edu)
Christopher Oey (caoey@ucsc.edu)*)
(* $Id: interp.ml,v 1.18 2021-01-29 11:08:27-08 - - $ *)

open Absyn

let want_dump = ref false

let source_filename = ref ""

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> eval_memref memref
    | Unary (oper, expr) -> 
        Hashtbl.find Tables.unary_fn_table oper (eval_expr expr)
    | Binary (oper, expr1, expr2) -> 
        Hashtbl.find Tables.binary_fn_table oper 
            (eval_expr expr1) (eval_expr expr2)

and eval_memref (memref : Absyn.memref) : float = match memref with
    | Arrayref (ident, expr) -> 
        Array.get (Hashtbl.find Tables.array_table ident) 
            (int_of_float (eval_expr expr))
    | Variable ident -> try Hashtbl.find Tables.variable_table ident
                        with Not_found -> 0.0

and eval_STUB reason = (
    print_string ("(" ^ reason ^ ")");
    nan)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continue -> match firstline with
       | _, _, None -> interpret continue
       | _, _, Some stmt -> (interp_stmt stmt continue)

and interp_stmt (stmt : Absyn.stmt) (continue : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim ident expr continue
    | Let (memref, expr) -> interp_let memref expr continue
    | Goto label -> interp_goto label continue
    | If (expr, label) -> interp_if expr label continue
    | Print print_list -> interp_print print_list continue
    | Input memref_list -> interp_input memref_list continue

and interp_print (print_list : Absyn.printable list)
                 (continue : Absyn.program) =
    let print_item item = match item with
        | String string ->
          let regex = Str.regexp "\"\\(.*\\)\""
          in print_string (Str.replace_first regex "\\1" string)
        | Printexpr expr ->
          print_string " "; print_float (eval_expr expr)
    in (List.iter print_item print_list; print_newline ());
    interpret continue

and interp_goto (label : Absyn.label) (continue : Absyn.program) =
    match Hashtbl.find_opt Tables.label_table label with
    | None -> Etc.die ["ERROR: LABEL NOT FOUND"]
    | Some n -> interpret n

and interp_if (expr : Absyn.relexpr) (label : Absyn.label) 
    (continue : Absyn.program) =
    match expr with
    | Relexpr (oper, expr1, expr2) -> 
        if Hashtbl.find Tables.bool_fn_table
              oper (eval_expr expr1) (eval_expr expr2)
              then interp_goto label continue
              else interpret continue;

and handle_eof (memref_list : Absyn.memref list)
               (continue : Absyn.program) =
        let handle memref =
            match memref with
            | Variable n ->
                    Hashtbl.add Tables.variable_table n (0.0 /. 0.0);
        in List.iter handle memref_list;

and interp_input (memref_list : Absyn.memref list)
                 (continue : Absyn.program)  =
    let input_number memref =
        if (Hashtbl.find Tables.variable_table) "eof" = 0.0
        then 
        try let number = Etc.read_number ()
             in (begin
                    match memref with
                    | Variable n ->
                        Hashtbl.add Tables.variable_table n number;
                 end);
        with End_of_file ->
                begin
                Hashtbl.add Tables.variable_table "eof" 1.0;
                (handle_eof memref_list continue);
                end
    in List.iter input_number memref_list;
    interpret continue

and interp_let (memref : Absyn.memref) (let_expr : Absyn.expr)
               (continue : Absyn.program) =
        match memref with
                | Variable n ->
                Hashtbl.add Tables.variable_table n 
                    (eval_expr let_expr);
                interpret continue
                | Arrayref (arr, n) ->
                    Array.set (Hashtbl.find Tables.array_table arr) 
                    (int_of_float (eval_expr n)) (eval_expr let_expr);
                    interpret continue 

and interp_dim (ident : Absyn.ident) (expr : Absyn.expr) 
    (continue : Absyn.program) =
    Hashtbl.add Tables.array_table ident 
        (Array.create_float (int_of_float (eval_expr expr)));
    interpret continue

and interp_STUB reason continue = (
    print_string "Unimplemented: ";
    print_string reason;
    print_newline();
    interpret continue)

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
        interpret program;
     if !want_dump then Tables.dump_label_table ())
