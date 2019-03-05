type defs = def list
and
def = 
    | Defun of string * (string list) * (statement list)
    | Defun_Tail of string * (string list) * (statement list)
    | Defconst of string * int
and
statement = 
    | Let of (let_env list) * (statement list)
    | If of (statement * statement * statement)
    | Begin of statement list
    | Function_Call of string * (statement list)
    | Tail_Function_Call of string * (statement list)
    | Cons of statement * statement
    | Car of statement
    | Cdr of statement
    | Atomp of statement
    | Greater_Equal of statement * statement
    | Greater of statement * statement
    | Less_Equal of statement * statement
    | Less of statement * statement
    | Equal of statement * statement
    | Not_Equal of statement * statement
    | Plus of statement * statement
    | Minus of statement * statement
    | Times of statement * statement
    | Divide of statement * statement
    | Constant of int
    | Symbol of string
    | List of statement list
    | Quote of term list
    | Break
    | Lambda of (string list) * (statement list)
and
let_env = string * statement
and
term = TermQuote of term list
    | TermConstant of int
    | TermSymbol of string

type symbol_table_entry =
    | FunctionSymbol of int
    | ConstantSymbol of int

type state = { current_function_code : string list; environment_stack : string list list; function_list : (string*(string list)) list;
 next_temp_number : int; symbol_table : (string * symbol_table_entry) list; pc : int; is_tail_recursive : bool;
    top_level_env : string list}

let index_of l v =
    let rec index_of_iter l v n =
        match l with
        | x::xs ->  if (x = v) then n else (index_of_iter xs v (n+1))
        | [] -> -1
    in
        index_of_iter l v 0

let get_temp_number state =
    ( state.next_temp_number, { state with next_temp_number = state.next_temp_number + 1 } )

let allocate_temp_symbol state =
    let (next_num, state) = get_temp_number state in
    ( "tmp"^(string_of_int next_num), state)

let add_code_line state code_line =
    { state with current_function_code = (List.append state.current_function_code [code_line])}

let add_code_lines state code_lines =
    { state with current_function_code = (List.append state.current_function_code code_lines) }

let generate_not state =
    add_code_lines state [ "LDC 0"; "CEQ" ]

let generate_constant state c =
    add_code_line state ("LDC " ^ (string_of_int c))

let add_symbol state name sym =
    if not (List.mem_assoc name state.symbol_table)  then
        { state with symbol_table = (name,sym) :: state.symbol_table }
    else
        state

let print_environment e =
    let print_env_list l =
        print_string("[");
        List.iter (fun l1 -> print_string (l1^" ")) l;
        print_string("]")
    in
        print_string "[";
        List.iter print_env_list e;
        print_string "]\n"

let get_tail_name fn = fn ^ "__tail"
    
let find_symbol_in_env state c =
    let rec find_symbol_in_list env_list c frame =
        match env_list with
        | env :: envs ->
            let idx = index_of env c in
                if idx >= 0 then Some (frame, idx) else find_symbol_in_list envs c (frame+1)
        | [] -> None
    in
        find_symbol_in_list state.environment_stack c 0

let rec ends_with_tail_call statements =
    match statements with
        | [] -> false
        | (s :: []) ->
           (match s with
                | Tail_Function_Call _ -> true
                | _ -> false)
        | (s :: ss) -> ends_with_tail_call ss

let is_tail_call statement =
    match statement with
    | Tail_Function_Call _ -> true
    | Begin statements -> ends_with_tail_call statements
    | _ -> false
        
let main_func_name="main"

let rec generate_symbol_constant state c =
    match find_symbol_in_env state c with
    | Some (frame, idx) -> add_code_line state ("LD "^(string_of_int frame)^" "^(string_of_int idx))
    | None -> add_code_line state ("%load("^c^")%")
and
    generate_symbol_function state c =
    match find_symbol_in_env state c with
    | Some (frame, idx) -> add_code_line state ("LD "^(string_of_int frame)^" "^(string_of_int idx))
    | None -> add_code_line state ("LDF %"^c^"%")
and
    generate_single_arg_call state arg operator =
        let state = generate_statement state arg in
        add_code_line state operator
and
    generate_two_arg_call state arg1 arg2 operator =
        let state = generate_statement state arg1 in
        let state = generate_statement state arg2 in
        add_code_line state operator
and
    generate_statement state stmt =
        match stmt with
        | Car arg -> generate_single_arg_call state arg "CAR"
        | Cdr arg -> generate_single_arg_call state arg "CDR"
        | Atomp arg -> generate_single_arg_call state arg "ATOM"
        | Cons (arg1,arg2) -> generate_two_arg_call state arg1 arg2 "CONS"
        | Greater (arg1,arg2) -> generate_two_arg_call state arg1 arg2 "CGT"
        | Greater_Equal (arg1,arg2) -> generate_two_arg_call state arg1 arg2 "CGTE"
    (* There are no instructions for Less or Less_Equal, just use greater and reverse the arguments *)
        | Less (arg1,arg2) -> generate_two_arg_call state arg2 arg1 "CGTE"
        | Less_Equal (arg1,arg2) -> generate_two_arg_call state arg2 arg1 "CGT"
        | Equal (arg1,arg2) -> generate_two_arg_call state arg1 arg2 "CEQ"
        | Not_Equal (arg1,arg2) -> 
            generate_not (generate_two_arg_call state arg1 arg2 "CEQ")
        | Plus (arg1,arg2) -> generate_two_arg_call state arg1 arg2 "ADD"
        | Minus (arg1,arg2) -> generate_two_arg_call state arg1 arg2 "SUB"
        | Times (arg1,arg2) -> generate_two_arg_call state arg1 arg2 "MUL"
        | Divide (arg1,arg2) -> generate_two_arg_call state arg1 arg2 "DIV"
        | Constant arg -> generate_constant state arg
        | Symbol arg -> generate_symbol_constant state arg 
        | Function_Call (fn,statements) -> generate_function_call state fn statements "RAP"
        | Tail_Function_Call (fn,statements) -> generate_tail_function_call state fn statements
        | Let (env,statements) -> generate_let state env statements
        | If (test, true_statements, false_statements) -> generate_if state test true_statements false_statements
        | Begin statements -> generate_statements state statements
        | List (statements) -> generate_list state statements
        | Quote (statements) -> generate_quote state statements
        | Break -> add_code_line state "BRK"
        | Lambda (params, statements) -> generate_lambda state params statements
and
    generate_if state test true_statement false_statement =
        let (true_symbol, state) = allocate_temp_symbol state in
        let (false_symbol, state) = allocate_temp_symbol state in
        let state = generate_statement state test in
        let sel_type = if state.is_tail_recursive then "TSEL" else "SEL" in
        let state = add_code_line state (sel_type^" %"^true_symbol^"% %"^false_symbol^"%") in
        let state = generate_if_body state true_symbol state.environment_stack true_statement "JOIN" in
        generate_if_body state false_symbol state.environment_stack false_statement "JOIN"
and
    generate_function_call state fn statements call_instr =
        let state = List.fold_left generate_statement state statements in
        let state = generate_symbol_function state fn in
        let state = if call_instr = "RAP" then add_code_line state ("DUM "^(string_of_int (List.length statements))) else state in 
        add_code_line state (call_instr^" "^(string_of_int (List.length statements)))
and
    generate_tail_function_call state fn statements =
        let state = List.fold_left generate_statement state statements in
(*        let state = add_code_line state ("DUM "^(string_of_int (List.length statements))) in *)
        let state = generate_symbol_function state fn in
        add_code_line state ("TAP "^(string_of_int (List.length statements)))
and
    generate_let state env statements =
        let env_names = List.map fst env in
        let env_statements = List.map snd env in
        let (let_fn_name, state) = allocate_temp_symbol state in
        let state = generate_statements state env_statements in
        let state = add_code_line state ("DUM "^(string_of_int (List.length env_names))) in
        let state = generate_symbol_function state let_fn_name in
        let state = add_code_line state ("RAP "^(string_of_int (List.length env_names))) in
        generate_function state let_fn_name state.environment_stack env_names statements "RTN"
and
    generate_lambda state env statements =
        let (lambda_name, state) = allocate_temp_symbol state in
        let state = generate_symbol_function state lambda_name in
        generate_function state lambda_name state.environment_stack env statements "RTN"
and
    generate_function state fn env_stack env statements return_type =
        let curr_code = state.current_function_code in
        let state = { state with current_function_code=[]; environment_stack=env::env_stack} in
        let state = add_code_line state (";FN="^fn) in
        let state = generate_statements state statements in
        let state = add_code_line state return_type in
        let state = { state with function_list = (fn, state.current_function_code)::state.function_list} in
        let state = add_symbol state fn (FunctionSymbol (-1)) in
        { state with current_function_code = curr_code; environment_stack=List.tl state.environment_stack }
and
    generate_tail_function state fn env_stack env statements =
        let curr_code = state.current_function_code in
        let state = { state with current_function_code=[]; environment_stack=env::env_stack} in
        let state = add_code_line state (";FN="^fn) in
        let state = generate_statements state statements in
        let state = add_code_line state "RTN" in
        let state = { state with function_list = (fn, state.current_function_code)::state.function_list} in
        let state = add_symbol state fn (FunctionSymbol (-1)) in
        { state with current_function_code = curr_code }
and
    generate_if_body state fn env_stack statement return_type =
        let curr_code = state.current_function_code in
        let state = { state with current_function_code=[]; environment_stack=env_stack} in
        let state = add_code_line state (";FN="^fn) in
        let state = generate_statement state statement in
        let state = if not state.is_tail_recursive then 
                        add_code_line state return_type
                     else if not (is_tail_call statement) then
                        add_code_line state "RTN"
                     else
                        state in
        let state = { state with function_list = (fn, state.current_function_code)::state.function_list} in
        let state = add_symbol state fn (FunctionSymbol (-1)) in
        { state with current_function_code = curr_code }
and
    generate_list state statements =
        let state = List.fold_left generate_statement state statements in
        let state = add_code_line state "LDC 0" in
        List.fold_left (fun st s -> (add_code_line st "CONS")) state statements
and generate_term state term =
        match term with
        | TermConstant i -> generate_constant state i
        | TermSymbol s -> generate_symbol_constant state s
        | TermQuote body -> generate_term_quote state body
and
    generate_quote state terms =
        let state = List.fold_left generate_term state terms in
        let state = add_code_line state "LDC 0" in
        List.fold_left (fun st s -> (add_code_line st "CONS")) state terms
and
    generate_term_quote state terms =
        let state = List.fold_left generate_term state terms in
        let state = add_code_line state "LDC 0" in
        List.fold_left (fun st s -> (add_code_line st "CONS")) state terms
and
    generate_statements state statements =
        List.fold_left generate_statement state statements
and
    generate_def state def =
        match def with
        | Defconst (name,value) -> add_symbol state name (ConstantSymbol value)
        | Defun (name,env,statements) -> generate_function { state with current_function_code=[]; is_tail_recursive=false } name [state.top_level_env] env statements "RTN"
        | Defun_Tail (name,env,statements) -> generate_tail_function { state with current_function_code=[]; is_tail_recursive=true } name [state.top_level_env] env statements

let new_state = { current_function_code=[] ; environment_stack=[]; function_list=[];
    next_temp_number=0; symbol_table=[] ; pc=0; is_tail_recursive=false; top_level_env=[]}

let print_function (fn,code) = 
    print_string("function "); print_string fn; print_string(":"); print_newline();
    let print_code_line l =
        print_string("  "); print_string l; print_newline()
    in
        List.iter print_code_line code

let main_comes_first state =
    { state with function_list = (main_func_name, (List.assoc main_func_name state.function_list)) :: (List.remove_assoc main_func_name state.function_list) }
    
let update_symbol state x =
    let sym = String.sub x 4 ((String.length x) - 4) in
    { state with symbol_table = (sym, FunctionSymbol (state.pc)) :: (List.remove_assoc sym state.symbol_table) }

let update_symbol_values state instructions =
    let is_fn_comment x = (((String.length x) > 4) && ((String.sub x 0 4) = ";FN=")) in
    let is_comment x = (((String.length x) > 0) && (x.[0] = ';')) in
    let is_LDC x = (((String.length x) > 4) && ((String.sub x 0 3) = "LDC")) in
    let is_LDF x = (((String.length x) > 4) && ((String.sub x 0 3) = "LDF")) in
    let is_LD x = (((String.length x) > 3) && ((String.sub x 0 3) = "LD ")) in
    let is_AP x = (((String.length x) > 3) && ((String.sub x 0 3) = "AP ")) in
    let is_RAP x = (((String.length x) > 4) && ((String.sub x 0 3) = "RAP")) in
    let is_DUM x = (((String.length x) > 4) && ((String.sub x 0 3) = "DUM")) in
    let is_SEL x = (((String.length x) > 4) && ((String.sub x 0 3) = "SEL")) in
    let is_TSEL x = (((String.length x) > 5) && ((String.sub x 0 4) = "TSEL")) in
    let check_for_update state x =
        if is_fn_comment x then
            update_symbol state x
        else if is_comment x then
            state
        else if is_LDC x || is_LDF x then
            { state with pc = state.pc + 5 }
        else if is_SEL x || is_TSEL x then
            { state with pc = state.pc + 9 }
        else if is_AP x || is_RAP x || is_DUM x then
            { state with pc = state.pc + 2 }
        else if is_LD x then
            { state with pc = state.pc + 3 }
        else
            { state with pc = state.pc + 1 } in
    List.fold_left check_for_update state instructions
    
let replace_symbol_load state load =
    try
        let sym = List.assoc load (state.symbol_table) in
        match sym with
        | FunctionSymbol i -> "LDF "^(string_of_int i)
        | ConstantSymbol i -> "LDC "^(string_of_int i)
    with exn ->
        print_string ("Symbol "^load^" not found\n"); flush stdout;
        raise (Failure ("Symbol "^load^" not found\n"))

let get_symbol_value state symbol =
    try
        let sym = List.assoc symbol (state.symbol_table) in
        match sym with
        | FunctionSymbol i -> i
        | ConstantSymbol i -> i
    with exn ->
        print_string ("Symbol "^symbol^" not found\n"); flush stdout;
        raise (Failure ("Symbol "^symbol^" not found\n"))

let replace_symbol state instr =
    let pct_start = String.index instr '%' in
    let pct_end = String.index_from instr (pct_start + 1) '%' in
    let rest = if ((String.length instr) > pct_end) then
        String.sub instr (pct_end+1) ((String.length instr) - pct_end - 1)
        else "" in
    (String.sub instr 0 pct_start) ^ (string_of_int (get_symbol_value state (String.sub instr (pct_start+1) (pct_end-pct_start-1)))) ^ rest

let rec replace_symbol_references state instr =
    if (((String.length instr) > 6) && ((String.sub instr 0 6) = "%load(")) then
        replace_symbol_load state (String.sub instr 6 ((String.length instr)-8))
    else if (String.contains instr '%') then
        replace_symbol_references state (replace_symbol state instr)
    else
        instr

let generate_program tree outbuf =
    let state = List.fold_left generate_def new_state tree in
    let state = main_comes_first state in
    let instructions = List.concat (List.map snd state.function_list) in
    let state = update_symbol_values state instructions in
    let new_instrs = List.map (replace_symbol_references state) instructions in
(*        List.iter (fun l -> print_string l; print_newline()) new_instrs; *)
    new_instrs
    
