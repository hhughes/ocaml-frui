(*pp camlp4o -I `ocamlfind query lwt.syntax` lwt-syntax-options.cma lwt-syntax.cma *)
(*
 * Copyright (c) 2009 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf
open Cohttp
open Cohttpserver
open Lwt
open Events

module Resp = struct
  (* respond with an error *)
  (* HACK *)
  let root = "/home/henry/proj/ocaml-frui/src/visualiser"
  let not_found req err = 
    let status = `Not_found in
    let headers = [ "Cache-control", "no-cache" ] in
    let resp = sprintf "<html><body><h1>Error</h1><p>%s</p></body></html>" err in
    let body = [`String resp] in
    Http_response.init ~body ~headers ~status ()

  (* internal error *)
  let internal_error err = 
    let status = `Internal_server_error in
    let headers = [ "Cache-control", "no-cache" ] in
    let resp = sprintf "<html><body><h1>Internal Server Error</h1><p>%s</p></body></html>" err in
    let body = [`String resp] in
    Http_response.init ~body ~headers ~status ()

  (* dynamic response *)
  let dyn req body =
    let status = `OK in
    let headers = [] in
    Http_response.init ~body ~headers ~status ()

  let get_file file req = 
    let size = (Unix.stat file).Unix.st_size in
    let fd = Unix.openfile file [Unix.O_RDONLY] 0o444 in
    let ic = Lwt_io.of_unix_fd ~close:(fun () -> Unix.close fd; Lwt.return ()) ~mode:Lwt_io.input fd in
    let t,u = Lwt.wait () in
    let body = [`Inchan (Int64.of_int size,  ic, u)] in
    return (dyn req body)

  let events = get_file "/home/henry/proj/ocaml-frui/src/visualiser/dummy.json"
  
  let next_msg req =
    let body = [`String (Thread_state.get_events ())] in
    return (dyn req body)

  let get_msg req =
    let body = [`String (Thread_state.get_msg ())] in
    return (dyn req body)

  (* index page *)
  let index req =
    let body = [`String "HELLO WORLD"] in
    return (dyn req body)

  (* dispatch non-file URLs *)
  let dispatch req = function
    | [], _
(*    | "" :: "index.html" :: [], _->
        index req *)
    | "" :: "events" :: [], _ -> events req
    | "" :: "next_msg" :: [], _ -> next_msg req
    | "" :: "msg" :: [], _ -> get_msg req
    | _, path -> try get_file (root ^ path) req
      with _ -> return (not_found req "dispatch")

end

(* main callback function *)
let t con_id req =
  let path = Http_request.path req in

  printf "%s %s [%s]\n%!" (Http_common.string_of_method (Http_request.meth req)) path 
    (String.concat "," (List.map (fun (h,v) -> sprintf "%s=%s" h v) 
      (Http_request.params_get req)));

  (* normalize path to strip out ../. and such *)
  let path_elem = Neturl.norm_path (Pcre.split ~pat:"/" path) in
  lwt resp = Resp.dispatch req (path_elem, path) in
  Http_daemon.respond_with resp
