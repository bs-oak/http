module Dict : Map.S with type key = string

(* header *)

type header

val header : string -> string -> header

(* part *)

type part

val string_part : string -> string -> part

(* body *)

type body

val empty_body : body

val json_body : BsOakJson.Encode.value -> body

val string_body : string -> string -> body

val multipart_body : part list -> body

(* response *)

type blob

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

type response =
  { url : string
  ; status : response_status
  ; headers : string Dict.t
  ; body : response_body
  }

(* expect *)

type 'a expect

val expect_string : string expect

val expect_json : 'a BsOakJson.Decode.decoder -> 'a expect

val expect_string_response : (response -> ('a, string) Belt.Result.t) -> 'a expect

(* request *)

type 'a request

val get_string : string -> string request

val get : string -> 'a BsOakJson.Decode.decoder -> 'a request

val post : string -> body -> 'a BsOakJson.Decode.decoder -> 'a request

val request : 
  method': string ->
  headers: header list ->
  url: string ->
  body: body ->
  expect: 'a expect -> 
  timeout: int option ->
  with_credentials : bool ->
  'a request

(* task & command *)

type error
  = BadUrl of string
  | Timeout
  | NetworkError
  | BadStatus of response
  | BadPayload of string * response

val to_task : 'a request -> ('a, error) BsOakCore.Task.t

val send : (('a, error) Belt.Result.t -> 'msg) -> 'a request -> 'msg BsOakCore.Cmd.t