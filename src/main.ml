
let handle_package pkg source dest arch = 
    let source_pkg = match Explorer.get_last_version pkg (source^pkg)   with 
        | Some x -> x
        | None -> failwith "No package found."
    in
    let version_number = Explorer.extract_version_number pkg source_pkg in
    let cmd = "cp -r "^source^pkg^"/"^source_pkg^" "^dest^pkg^"-"^arch^version_number in
    let _ = Sys.command cmd in
    let ast = OpamParser.file (source^pkg^"/"^source_pkg^"/opam") in 
    let new_ast, deps, action = Port.translate ast pkg arch in 
    let file = open_out (dest^pkg^"-"^arch^version_number^"/opam") in
    output_string file (OpamPrinter.opamfile new_ast);
    deps, action

let rec solve_dependencies source dest failures arch = function
    | [] -> failures
    | pkg::next when Explorer.get_last_version (pkg^"-"^arch) dest == None -> 
        let deps, action = handle_package pkg source dest arch in 
        begin
            List.iter (fun x -> Printf.printf "%s |> %s \n" pkg x) deps;
            match action with 
                | Port.None -> solve_dependencies source dest (pkg::failures) arch (deps @ next)
                | Port.Jbuilder -> solve_dependencies source dest failures arch (deps @ next)
        end
    | _::next -> solve_dependencies source dest failures arch next 


let usage_message = "ezport <system> <package> <host_repo> <cross_repo>\n\n\
\ \ <system>: for now, android|ios|windows|esp32.\n\
\ \ <package>: the package to port.\n\
\ \ <host_repo>: source opam repository, containing the package you want to port.\n\
\ \ <cross_repo>: destination opam repository, which contains cross-compiled packages.\n"

let () =
    let args = ref [||] in
    Arg.parse [] (fun x -> args := Array.append !args [|x|]) usage_message;
    match Array.length !args with
    | 4 ->
    begin
        let arch = !args.(0)
        and target_package = !args.(1)
        and opam_repository = !args.(2)
        and target_repository = !args.(3)
        in
        let failures = solve_dependencies (opam_repository) (target_repository) ([]) arch ([target_package])
        in
        Printf.printf "Done! Failures: \n";
        List.iter (fun x -> Printf.printf " | %s \n" x) failures
    end
    | _ -> Arg.usage [] usage_message
