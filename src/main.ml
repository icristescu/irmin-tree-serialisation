open Lwt.Infix

let root =
  "/Users/icristes/Documents/tezos_docs/archive_store/context"

let store_hash_hex =
  "2ef3daa18c79b75446b8608885afde542404c63f9ca5f4029017898a48f6cb7d"

let output = "snap"

let ( let* ) = Lwt.bind

let reporter () =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_stamp h _tags k fmt =
      let dt = Sys.time () in
      Fmt.kpf k Fmt.stderr
        ("%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  { Logs.report }


let dump_tree () =
  let* repo = Store.Repo.v (Irmin_pack.config ~readonly:true root) in
  let hash = Store_hash.Hash.of_hex_string store_hash_hex in
  let* tree = Store.Tree.of_hash repo hash >|= Option.get in
  let* fd =
    Lwt_unix.openfile output Lwt_unix.[ O_WRONLY; O_CREAT; O_TRUNC ] 0o666
  in
  Tree_fold.dump tree fd

let target = "CoVHqwb5Push49CDYWpCw8dksQ6xKqUS2ChEyuSNgC3arvwwGi4m"

let search_index () =
  Printf.printf "%s (target)\n\n" target;
  let count = ref 0 in
  Store_index.v ~log_size:0 root
  |> Store_index.iter (fun k v ->
         let hash =
           Fmt.to_to_string Tezos_crypto.Context_hash.pp
             (Store_hash.to_context_hash k)
         in
         let pp_value ppf (a, b, c) =
           Format.fprintf ppf "(%Ld, %d, %c)" a b c
         in
         if hash = target then (
           Format.printf
             "DONE!\n\n\
              Found the context hash `%s' at index `%d' in the iteration over \
              the index. The true key is `%s' and the true value is `%a'\n\n\
              %!"
             target !count
             (Store_hash.to_hex_string k)
             pp_value v;
           raise (Failure "DEBUG") );
         incr count;
         if !count mod 100_000 = 0 then
           Printf.printf "Not found @ %d entries\n%!" !count);
  Printf.printf "Didn't find anything%!"

let find_max () =
  Fmt.epr "index iter...@.";
  let max = ref 0 in
  let index = Store_index.v ~log_size:0 root in
  let pp_value ppf (a, b, c) = Format.fprintf ppf "(%Ld, %d, %c)" a b c in
  Store_index.iter
    (fun _ ((_, len, _) as x) ->
      if len > !max then (
        Fmt.epr "new max %a@." pp_value x;
        max := len ))
    index;
  Fmt.epr "max len = %d@." !max


let config = Irmin_pack.config ~fresh:false ~readonly:false root

let reconstruct () =
  Printexc.record_backtrace true;
  Logs.app (fun l -> l "Reconstruct index");
  Store.reconstruct_index config

let open_store () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (reporter ());
  Printexc.record_backtrace true;
  let conf = Irmin_pack.config ~fresh:false ~readonly:false root in
  let conf = Irmin_pack.config_layers ~conf ~copy_in_upper:true ~with_lower:true () in
  Store.Repo.v conf >>= fun repo -> Store.Repo.close repo

let migrate () =
  Logs.app (fun l -> l "Migrating store to v2, this may take a while") ;
  Store.migrate config

let () = migrate () ; reconstruct ()
