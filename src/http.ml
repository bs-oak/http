module Scheduler = BsOakCore.Fx.Scheduler

module Dict = struct
  include Map.Make(String)

  let get key' dict =
    try
      Some (find key' dict)
    with
    | _ -> None

  let update key' update_fn dict =
    let prev_value = get key' dict in
    match update_fn prev_value with
    | None -> dict
    | Some new_value -> add key' new_value dict
end

module FormData = struct
  type t

  external make : unit -> t = "FormData" [@@bs.new]
  external append : t -> string -> string -> unit = "append" [@@bs.send]

  let from_list pairs =
    let append_pair t (k, v) = append t k v; t in
    List.fold_left append_pair (make ()) pairs
end

module XMLHttpRequest = struct
  type t
  type blob
  type response
    = ArrayBuffer of Js.Typed_array.array_buffer
    | Blob of blob
    | Document of Dom.document
    | Object of Js.Json.t
    | DOMString of string
    | Other of string
    | Null

  external make : unit -> t = "XMLHttpRequest" [@@bs.new]
  external add_event_listener : t -> string  -> (Dom.progressEvent -> unit)  -> unit = "addEventListener" [@@bs.send]
  external response_url : t -> string = "responseURL" [@@bs.get]
  external status : t -> int = "status" [@@bs.get]
  external status_text : t -> string = "statusText" [@@bs.get]
  external response_type : t -> string = "responseType" [@@bs.get]
  external set_response_type : t -> string -> unit = "responseType" [@@bs.set]
  external set_with_credentials : t -> bool -> unit = "withCredentials" [@@bs.set]
  external set_timeout : t -> int -> unit = "timeout" [@@bs.set]
  external response_text : t -> string Js.Nullable.t = "responseText" [@@bs.get]
  external response_as_array_buffer : t -> Js.Typed_array.array_buffer Js.Nullable.t = "response" [@@bs.get]
  external response_as_blob : t -> blob Js.Nullable.t = "response" [@@bs.get]
  external response_as_document : t -> Dom.document Js.Nullable.t = "response" [@@bs.get]
  external response_as_object : t -> Js.Json.t Js.Nullable.t = "response" [@@bs.get]
  external response_as_dom_string : t -> string Js.Nullable.t = "response" [@@bs.get]
  external open' : t -> string -> string -> bool -> unit = "open" [@@bs.send]
  external get_all_response_headers : t -> string Js.Nullable.t = "getAllResponseHeaders" [@@bs.send]
  external set_request_header : t -> string -> string -> unit = "setRequestHeader" [@@bs.send]
  external send : t -> unit = "send" [@@bs.send]
  (* external send_array_buffer : t -> Js.Typed_array.array_buffer -> unit = "send" [@@bs.send]
  external send_document : t -> Dom.document -> unit = "send" [@@bs.send] *)
  external send_string : t -> string -> unit = "send" [@@bs.send]
  external send_form_data : t -> FormData.t -> unit = "send" [@@bs.send]
  external abort : t -> unit = "abort" [@@bs.send]
  
  let response xhr = 
    match response_type xhr with
    | "arraybuffer" -> 
      (match Js.Nullable.toOption (response_as_array_buffer xhr) with
      | Some v -> ArrayBuffer v
      | None -> Null)
    | "blob" -> 
      (match Js.Nullable.toOption (response_as_blob xhr) with
      | Some v -> Blob v
      | None -> Null)
    | "document" -> 
      (match Js.Nullable.toOption (response_as_document xhr) with
      | Some v -> Document v
      | None -> Null)
    | "json" -> 
      (match Js.Nullable.toOption (response_as_object xhr) with
      | Some v -> Object v
      | None -> Null)
    | "text" -> 
      (match Js.Nullable.toOption (response_as_dom_string xhr) with
      | Some v -> DOMString v
      | None -> Null)
    | "" -> 
      (match Js.Nullable.toOption (response_as_dom_string xhr) with
      | Some v -> DOMString v
      | None -> Null)
    | unknown_type -> 
      Other unknown_type
end

(* header *)

type header = Header of string * string

let header k v =
  Header (k,v)

(* parts *)

type part = Part of string * string

let string_part k v =
  Part (k, v)

(* body *)

type body
  = EmptyBody
  | StringBody of string * string
  | FormDataBody of FormData.t

let empty_body =
  EmptyBody

let json_body value =
  StringBody ("application/json", BsOakJson.Encode.encode 0 value)
  
let string_body v1 v2 =
  StringBody (v1, v2)

let multipart_body parts =
  let part_to_pair (Part (k,v)) = (k,v) in
  let pairs = List.map part_to_pair parts in
  FormDataBody (FormData.from_list pairs)

(* response *)

type blob = XMLHttpRequest.blob

type response_status =
  { code : int 
  ; message : string  
  }

type response_body
  = ArrayBuffer of Js.Typed_array.array_buffer
  | Blob of blob
  | Document of Dom.document
  | Object of Js.Json.t
  | DOMString of string
  | Other of string
  | Null

let xhr_response_to_response_body = function
  | XMLHttpRequest.ArrayBuffer v -> ArrayBuffer v
  | XMLHttpRequest.Blob v -> Blob v
  | XMLHttpRequest.Document v -> Document v
  | XMLHttpRequest.Object v -> Object v
  | XMLHttpRequest.DOMString v -> DOMString v 
  | XMLHttpRequest.Other v -> Other v
  | XMLHttpRequest.Null -> Null

type response =
  { url : string
  ; status : response_status
  ; headers : string Dict.t
  ; body : response_body
  }

(* expect *)

type 'a expect =
  { response_type : string
  ; response_to_result : response -> ('a, string) Belt.Result.t
  }

let expect_string_response fn =
  { response_type = "text"
  ; response_to_result = fn
  }
  
let expect_string =
  expect_string_response (fun response -> 
    match response.body with
    | ArrayBuffer _buf -> Belt.Result.Error "expected string response but received array buffer"
    | Blob _blob -> Belt.Result.Error "expected string response but received blob"
    | Document _doc -> Belt.Result.Error "expected string response but dom document"
    | Object _obj -> Belt.Result.Error "expected string response but received javascript object"
    | Other typ -> Belt.Result.Error ("expected string response but received " ^ typ)
    | DOMString s -> Belt.Result.Ok s
    | Null -> Belt.Result.Error "expected string response but got no response"
  )

let expect_json decoder =
  expect_string_response (fun response -> 
    match response.body with
    | ArrayBuffer _buf -> Belt.Result.Error "expected string response but received array buffer"
    | Blob _blob -> Belt.Result.Error "expected string response but received blob"
    | Document _doc -> Belt.Result.Error "expected string response but dom document"
    | Object _obj -> Belt.Result.Error "expected string response but received javascript object"
    | Other typ -> Belt.Result.Error ("expected string response but received " ^ typ)
    | DOMString s -> BsOakJson.Decode.decode_string decoder s
    | Null -> Belt.Result.Error "expected string response but got no response"
  )

(* request *)

type 'a request = 
  { method' : string
  ; headers : header list
  ; url : string
  ; body : body
  ; expect : 'a expect
  ; timeout : int option
  ; with_credentials : bool
  }

let request ~method' ~headers ~url ~body ~expect ~timeout ~with_credentials =
  { method'; headers; url; body; expect; timeout; with_credentials}
  
let get_string url =
  request
    ~method': "GET"
    ~headers: []
    ~url: url
    ~body: empty_body
    ~expect: expect_string
    ~timeout: None
    ~with_credentials: false

let get url decoder =
  request
    ~method': "GET"
    ~headers: []
    ~url: url
    ~body: empty_body
    ~expect: (expect_json decoder)
    ~timeout: None
    ~with_credentials: false

let post url body decoder =
  request
    ~method': "POST"
    ~headers: []
    ~url: url
    ~body: body
    ~expect: (expect_json decoder)
    ~timeout: None
    ~with_credentials: false

type error
  = BadUrl of string
  | Timeout
  | NetworkError
  | BadStatus of response
  | BadPayload of string * response

let xhr_headers_to_dict xhr =
  let split_header s = 
    Js.String.split {|\u000d\u000a|} s 
  in
  let split_pair s =
    match Js.String.split {|\u003a\u0020|} s with
    | [|k;v|] -> Some (k, v) 
    | _ -> None
  in
  let merge_update v = function
    | Some prev_v ->  Some (prev_v ^ ", " ^ v)
    | None -> Some v
  in
  let add_pair_to_dict dict s =
    match split_pair s with
    | Some (k,v) -> Dict.update k (merge_update v) dict
    | None -> dict
  in
  match Js.Nullable.toOption (XMLHttpRequest.get_all_response_headers xhr) with
  | Some header_str -> 
    Array.fold_left add_pair_to_dict Dict.empty (split_header header_str)
  | None -> 
    Dict.empty

let xhr_response_text_to_response_body xhr =
  match Js.Nullable.toOption (XMLHttpRequest.response_text xhr) with
  | Some s -> DOMString s
  | None -> Null
  
let to_task req =
  Scheduler.binding (fun callback -> 
    let xhr = XMLHttpRequest.make () in
    let () = XMLHttpRequest.add_event_listener xhr "error" (fun _event ->  
      callback (Scheduler.fail NetworkError)
    )
    in
    let () = XMLHttpRequest.add_event_listener xhr "timeout" (fun _event ->  
      callback (Scheduler.fail Timeout)
    )
    in
    let () = XMLHttpRequest.add_event_listener xhr "load" (fun _event ->  
      let response = 
        { url = XMLHttpRequest.response_url xhr 
        ; status = 
          { code = XMLHttpRequest.status xhr
          ; message = XMLHttpRequest.status_text xhr } 
        ; headers = xhr_headers_to_dict xhr
        ; body = xhr_response_to_response_body (XMLHttpRequest.response xhr) }
      in
      if response.status.code < 200 || 299 < response.status.code then
        callback (Scheduler.fail (BadStatus response))
      else
        match req.expect.response_to_result response with
        | Ok v ->
          callback (Scheduler.succeed v)
        | Error err ->
          callback (Scheduler.fail (BadPayload (err, { response with body = xhr_response_text_to_response_body xhr })))
    )
    in    

    try
      let () = XMLHttpRequest.open' xhr req.method' req.url true in
      let () = List.iter (fun (Header (k,v)) -> XMLHttpRequest.set_request_header xhr k v) req.headers in
      let () = XMLHttpRequest.set_response_type xhr req.expect.response_type in
      let () = XMLHttpRequest.set_with_credentials xhr req.with_credentials in
      let () = 
        match req.timeout with
        | Some timeout -> XMLHttpRequest.set_timeout xhr timeout
        | None -> ()
      in
      let () =
        match req.body with
        | EmptyBody ->
          XMLHttpRequest.send xhr

        | StringBody (content_type, value) ->
          let () = XMLHttpRequest.set_request_header xhr "Content-Type" content_type in
          XMLHttpRequest.send_string xhr value
        
        | FormDataBody form_data ->
          XMLHttpRequest.send_form_data xhr form_data
      in
        fun _ -> XMLHttpRequest.abort xhr

    with
    | _ -> 
      let () = callback (Scheduler.fail (BadUrl req.url)) in
      fun _ -> ()
  )


let send result_to_msg request =
  BsOakCore.Task.attempt result_to_msg (to_task request)