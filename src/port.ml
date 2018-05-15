open OpamParserTypes

(* What kind of build system has been recognised *)
type action = None | Jbuilder

let rec reduce_action = function
    | [] -> None
    | Jbuilder::_ -> Jbuilder
    | _::next -> reduce_action next

let to_cmd pos str = String (pos, str)

let rec translate_build_command pkg_name = function
    | List (pos, (String (p1, "jbuilder"))::(String (_, "build"))::next) ->
        let command = [ "jbuilder"; "build"; "-x"; "esp32"; "-p"; pkg_name ] in
        let new_cmd = List.map (to_cmd p1) command in
        List (pos, new_cmd), Jbuilder
    | List (pos, values) -> 
        let res = List.map (translate_build_command pkg_name) values in
        let new_values, actions = List.split res in
        List (pos, new_values), reduce_action actions
    | item -> item, None

let rec is_runtime = function
    | [] -> true
    | Ident (_, "build")::next -> false
    | Ident (_, "with-test")::next -> false
    | Ident (_, "test")::next -> false
    | Logop (_, `And, i1, i2)::next -> is_runtime [i1] && is_runtime [i2] && is_runtime next
    | Logop (_, `Or, i1, i2)::next -> (is_runtime [i1] || is_runtime [i2]) && is_runtime next
    | _::next -> is_runtime next

let rec translate_depends acc values = function
    | (String (x, dep))::next -> translate_depends (dep::acc) ((String (x, dep^"-esp32"))::values) next
    | (Option (y, String (x, dep), opt))::next when is_runtime opt -> translate_depends (dep::acc) ((Option (y, String (x, dep^"-esp32"), opt))::values) next
    | value::next -> translate_depends acc (value::values) next 
    | [] -> acc, values

let rec translate_items pkg_name deps new_ast action = function
    | Variable (pos, "depends", List (p2, values))::next -> 
        let deps, new_values = translate_depends deps [] values in
        let new_ast = Variable (pos, "depends", List (p2, new_values))::new_ast in
        translate_items pkg_name deps new_ast action next 
    | Variable (pos, "build", values)::next ->
        let values, action = translate_build_command pkg_name values in
        let new_ast = Variable (pos, "build", values)::new_ast in 
        translate_items pkg_name deps new_ast action next 
    | s::next -> translate_items pkg_name deps (s::new_ast) action next 
    | [] -> List.rev new_ast, deps, action

(* Returns a new opamfile AST for the -esp32 version of the package, and the list of runtime dependencies. *)
let translate (package_definition : opamfile) pkg_name = 
    let new_ast, deps, action = translate_items pkg_name [] [] None package_definition.file_contents in
    {
        file_name = package_definition.file_name;
        file_contents = new_ast;
    }, deps, action