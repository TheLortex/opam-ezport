

let extract_version_number name name_version = Printf.printf "%s | %s \n%!" name name_version;
    let n = String.length name in
    let m = String.length name_version in 
    String.sub name_version n (m-n)

let get_last_version name directory =
    let n = String.length name in
    let dir_content = Array.to_list (Sys.readdir directory) in
    let versions = List.sort String.compare dir_content in
    let rec explore acc = function
        | [] -> acc
        | str::next when String.equal (String.sub str 0 (min n (String.length str))) name -> explore (Some str) next
        | _::next -> explore acc next
    in
    explore None versions 