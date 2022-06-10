(*
module Comp = struct
  type t =
    { key : string option
    ; children : component list
    }

  and component = InnerText of string
end

module type Component = sig
  type props

  val get_props : unit -> props
  val render : props -> Comp.t
end

module App = struct
  type props = { name : string }

  let get_props () = { name = "My App" }
  let render props = { Comp.key = Some "key"; children = [ InnerText props.name ] }
end

module type React = sig
  val render : unit -> unit
end

module Make_component (C : Component) : React = struct
  let render () =
    C.get_props ()
    |> C.render
    |> (fun c -> c.children)
    |> List.iter (function Comp.InnerText i -> Stdio.print_endline i)
  ;;
end

module Display = Make_component (App)

let () = Display.render ()
*)

type _ component =
  | String : string -> _ component
  | Component : 'props 'otherprops. 'props node -> 'otherprops component

and 'props node =
  { make_fn : 'props -> 'props component
  ; props : 'props
  ; children : 'props. 'props component list
  }

module Fragment = struct
  let make () = String ""
end

module Box = struct
  type props = { name : string }

  let make props =
    let new_name = props.name ^ " in box" in
    Component { make_fn = Fragment.make; props = (); children = [ String new_name ] }
  ;;
end

module App = struct
  let make () = Component { make_fn = Box.make; props = { name = "dick" }; children = [] }
end

let rec string_renderer : type a. a component -> string = function
  | String s -> s
  | Component { make_fn; props; children } ->
    children
    |> List.map string_renderer
    |> String.concat "|"
    |> fun children -> string_renderer (make_fn props) ^ "[children= " ^ children ^ "]"
;;

let () = App.make () |> string_renderer |> Stdio.print_endline
