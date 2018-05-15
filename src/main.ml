
let target_package = ref "cohttp-mirage"
let opam_repository = ref "/home/lucas/.opam/repo/default/packages/"
let target_repository = ref "/home/lucas/esp/projects/opam-cross-esp32/packages/"



let handle_package pkg source dest = 
    let source_pkg = match Explorer.get_last_version pkg (source^pkg)   with 
        | Some x -> x
        | None -> failwith "No package found."
    in
    let version_number = Explorer.extract_version_number pkg source_pkg in
    let cmd = "cp -r "^source^pkg^"/"^source_pkg^" "^dest^pkg^"-esp32"^version_number in
    let _ = Sys.command cmd in
    let ast = OpamParser.file (source^pkg^"/"^source_pkg^"/opam") in 
    let new_ast, deps, action = Port.translate ast pkg in 
    let file = open_out (dest^pkg^"-esp32"^version_number^"/opam") in
    output_string file (OpamPrinter.opamfile new_ast);
    deps, action

let rec solve_dependencies source dest failures = function
    | [] -> failures
    | pkg::next when Explorer.get_last_version (pkg^"-esp32") dest == None -> 
        let deps, action = handle_package pkg source dest in 
        begin
            List.iter (fun x -> Printf.printf "%s |> %s \n" pkg x) deps;
            match action with 
                | Port.None -> solve_dependencies source dest (pkg::failures) (deps @ next)
                | Port.Jbuilder -> solve_dependencies source dest failures (deps @ next)
        end
    | _::next -> solve_dependencies source dest failures next


let () = 
    let failures = solve_dependencies (!opam_repository) (!target_repository) ([]) ([!target_package])
    in
    Printf.printf "Done! Failures: \n";
    List.iter (fun x -> Printf.printf " | %s \n" x) failures
