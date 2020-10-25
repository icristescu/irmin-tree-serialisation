(** This module recreates the [Store] instantiation in Tezos'
    [lib_storage/context.ml]. *)

module S = struct
  module Hash = Store_hash
  module Context_hash = Tezos_crypto.Context_hash
  module Path = Irmin.Path.String_list
  module Metadata = Irmin.Metadata.None

  module Node = struct
    module M = Irmin.Private.Node.Make (Hash) (Path) (Metadata)

    module V1 = struct
      module Hash = Irmin.Hash.V1 (Hash)

      type kind = [`Node | `Contents of Metadata.t]

      type entry = {kind: kind; name: M.step; node: Hash.t}

      let s = Irmin.Type.(string_of `Int64)

      let pre_hash_v = Irmin.Type.(unstage (pre_hash s))

      (* Irmin 1.4 uses int64 to store string lengths *)
      let step_t =
        let pre_hash = Irmin.Type.(stage @@ fun x -> pre_hash_v x) in
        Irmin.Type.like M.step_t ~pre_hash

      let metadata_t =
        let some = "\255\000\000\000\000\000\000\000" in
        let none = "\000\000\000\000\000\000\000\000" in
        Irmin.Type.(map (string_of (`Fixed 8)))
          (fun s ->
            match s.[0] with
            | '\255' ->
                None
            | '\000' ->
                Some ()
            | _ ->
                assert false)
          (function Some _ -> some | None -> none)

      (* Irmin 1.4 uses int64 to store list lengths *)
      let entry_t : entry Irmin.Type.t =
        let open Irmin.Type in
        record "Tree.entry" (fun kind name node ->
            let kind =
              match kind with None -> `Node | Some m -> `Contents m
            in
            {kind; name; node})
        |+ field "kind" metadata_t (function
             | {kind= `Node; _} ->
                 None
             | {kind= `Contents m; _} ->
                 Some m)
        |+ field "name" step_t (fun {name; _} -> name)
        |+ field "node" Hash.t (fun {node; _} -> node)
        |> sealr

      let entries_t : entry list Irmin.Type.t =
        Irmin.Type.(list ~len:`Int64 entry_t)

      let import_entry (s, v) =
        match v with
        | `Node h ->
            {name= s; kind= `Node; node= h}
        | `Contents (h, m) ->
            {name= s; kind= `Contents m; node= h}

      let import t = List.map import_entry (M.list t)

      let pre_hash_entries = Irmin.Type.(unstage (pre_hash entries_t))

      let pre_hash entries = pre_hash_entries entries
    end

    include M

    let pre_hash_v1 x = V1.pre_hash (V1.import x)

    let t = Irmin.Type.(like t ~pre_hash:(stage @@ fun x -> pre_hash_v1 x))
  end

  module Commit = struct
    module M = Irmin.Private.Commit.Make (Hash)
    module V1 = Irmin.Private.Commit.V1 (M)
    include M

    let pre_hash_v1_t = Irmin.Type.(unstage (pre_hash V1.t))

    let pre_hash_v1 t = pre_hash_v1_t (V1.import t)

    let t = Irmin.Type.(like t ~pre_hash:(stage @@ fun x -> pre_hash_v1 x))
  end

  module Contents = struct
    type t = string

    let ty = Irmin.Type.(pair (string_of `Int64) unit)

    let pre_hash_ty = Irmin.Type.(unstage (pre_hash ty))

    let pre_hash_v1 x = pre_hash_ty (x, ())

    let t =
      Irmin.Type.(like string ~pre_hash:(stage @@ fun x -> pre_hash_v1 x))

    let merge = Irmin.Merge.(idempotent (Irmin.Type.option t))
  end

  module Conf = struct
    let entries = 32

    let stable_hash = 256
  end

  module Store =
    Irmin_pack.Make_ext_layered (Conf) (Irmin.Metadata.None) (Contents)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Hash)
      (Node)
      (Commit)
end

include S.Store
