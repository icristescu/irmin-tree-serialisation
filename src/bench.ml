module Hash = Store_hash

module Block_level = struct
  type t = int32

  let equal x y = Int32.equal x y

  let hash = Hashtbl.hash

  let hash_size = 30

  let encoded_size = 4

  let encode v =
    Bytes.unsafe_to_string Data_encoding.(Binary.to_bytes_exn int32 v)

  let decode str i =
    let str = Bytes.unsafe_of_string str in
    Data_encoding.(Binary.of_bytes_exn int32 (Bytes.sub str i encoded_size))

  let pp fmt i = Format.fprintf fmt "%ld" i
end

let key_size = 32

let value_size = 13

let entry_size = key_size + value_size

let ( // ) = Filename.concat

let random_char () = char_of_int (33 + Random.int 94)

let random_string string_size =
  String.init string_size (fun _i -> random_char ())

module Key = struct
  type t = string

  let v () = random_string key_size

  let hash = Hashtbl.hash

  let hash_size = 30

  let encode s = s

  let decode s off = String.sub s off key_size

  let encoded_size = key_size

  let equal = String.equal

  let pp s = Fmt.fmt "%s" s
end

module Value = struct
  type t = string

  let v () = random_string value_size

  let encode s = s

  let decode s off = String.sub s off value_size

  let encoded_size = value_size

  let pp s = Fmt.fmt "%s" s
end

module Block_key = Store_hash.Key (Hash)
module Cemented_block_level_index =
  Index_unix.Make (Key) (Value) (Index.Cache.Unbounded)

let default_index_log_size = 100_000

let _size dir =
  let command = "ls -alh " ^ dir ^ ">> tmp_file_100" in
  (* Format.printf "execute command %s" command ; *)
  match Unix.system command with
  | Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED _ ->
      failwith "failing  command"
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      failwith "`lsof` command was interrupted"

let ps () =
  let pid = string_of_int (Unix.getpid ()) in
  let command = "ps x -o rss=,vsz= -p " ^ pid ^ " >> tmp_file_100" in
  (* Format.printf "execute command %s" command ; *)
  match Unix.system command with
  | Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED _ ->
      failwith "failing  command"
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      failwith "`lsof` command was interrupted"

let test () =
  let name = "index" in
  let index =
    Cemented_block_level_index.v ~readonly:false
      ~log_size:default_index_log_size name
  in
  let create_block i =
    let b = Bytes.create 32 in
    Bytes.set_int32_be b 0 i ; Bytes.to_string b
  in
  let rec loop i =
    (* store 1000 blocks then flush *)
    let target = Int32.add i 1000l in
    let rec inner j =
      if Int32.equal j target then Cemented_block_level_index.flush index
      else (
        Cemented_block_level_index.replace index (create_block j)
          (Int32.to_string j) ;
        inner (Int32.succ j) )
    in
    Format.printf "i : %ld@." i ;
    (* size (Filename.concat name "index") ; *)
    ps () ;
    inner i ;
    Gc.major () ;
    loop target ;
    ()
  in
  loop 0l
