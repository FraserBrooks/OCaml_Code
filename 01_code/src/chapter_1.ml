
 open Core.Std;;

 let my_list = ["Apple";"Pear";"Grape";"Orange"];;

 List.length my_list;;

 List.map my_list ~f:String.length;;

 List.map my_list String.length;;

 List.map ~f:String.length my_list;;

 let divide x y =
    if y = 0 then None else Some (x/y) ;;

 let log_entry maybe_time (message: string) : string = 
      let time =
         match maybe_time with
          | Some x -> x
          | None -> Time.now ()
      in
      (Time.to_sec_string ~zone:Time.Zone.local time) ^ " -- " ^ message
      ;;

 log_entry None "No time given";;

 log_entry (Some Time.epoch) "Long ago";;
 
 (* Record type *)
 type point2d = { x : float; y : float };;
 
 let p = { x = 3. ; y = -4. };;

 (* gradient *)
 let magnitude {x ; y} = sqrt (x ** 2. +. y ** 2.);;

 (* dot notation for accessing record fields *)
 let distance v1 v2 =
     magnitude { x = v1.x -. v2.x; y = v1.y -. v2.y };;

 (* UDTs as components in UDTs *)
 type circle_desc = { center: point2d; radius: float }
 type rect_desc = { lower_left: point2d; width: float; height: float }
 type segment_desc = { endpoint1: point2d; endpoint2: point2d } ;;

 (* Combine multiple objects together as a description of a scene/state *)
 (* Variant Type *)
 type scene_element = 
     | Circle of circle_desc
     | Rect of rect_desc
     | Segment of segment_desc
 ;;

 let is_inside_scene_element point scene_element =
      match scene_element with
        | Circle {center ; radius} -> 
                    distance center point < radius
        | Rect { lower_left; width; height; } -> 
                    point.x > lower_left.x && point.x < lower_left.x +. width &&
                    point.y > lower_left.y && point.y < lower_left.y +. height
        | Segment {endpoint1; endpoint2} -> false;;

 let my_circle : circle_desc = { center = {x = 4.5 ; y = 5.6} ; radius =  6. };;
 
 let my_scene_el : scene_element = Circle  my_circle ;;
 
 let far_point = {x = 3. ; y = 33. };;
 let inside_point = { x = 5. ; y = 5. };;


 is_inside_scene_element far_point my_scene_el;;

 is_inside_scene_element inside_point my_scene_el;;

 let rec is_inside_scene point scene =
       match scene with
       | x::xs   -> is_inside_scene_element point x || 
                    is_inside_scene point xs
       | []      -> false ;;

 let my_scene = [my_scene_el; 
                 Rect {lower_left = {x= 2.; y = 1.}; width = 7.; height = 7. }; 
                 Segment {endpoint1 = {x = 2.; y = 3.}; endpoint2 = {x = 5.; y = 5.} };
                 Rect {lower_left = {x = 33.; y = 44.}; width = 3.; height = 4. }];;

 is_inside_scene far_point my_scene;;
 is_inside_scene inside_point my_scene;; 

 (* alternative using Core.List *)
 let alt_is_inside_scene point scene =
     List.exists ~f:(fun e  -> is_inside_scene_element point e) scene;;
 
 alt_is_inside_scene far_point my_scene;;
 alt_is_inside_scene inside_point my_scene;; 

 (* IMPERATIVE PROGRAMMING *)
 
 (* arrays *)
 let numbers = [|1;2;3;4;55|];;
 numbers.(2);;
 numbers.(2) <- 33;;
 numbers;;
 
 (* Record with mutable fields *)
 type running_sum = 
     { mutable sum: float;
       mutable sum_squares: float;
       mutable samples: int;
     };;

 let mean rsum = rsum.sum /. float rsum.samples
 let stdeviation rsum  = 
        sqrt (rsum.sum_squares /. float rsum.samples -.
              (rsum.sum /. float rsum.samples) ** 2.) ;; 

 let create () = { sum = 0.; sum_squares = 0.; samples = 0 }
 let update rsum x = 
         rsum.sum <- rsum.sum +. x;
         rsum.sum_squares <- rsum.sum_squares +. x *. x;
         rsum.samples <- rsum.samples + 1;;

 let rsum = create();;

 List.iter [1.;2.;3.;6.;3.;6.;3.;5.;6.;4.] ~f:(fun v -> update rsum v);;

 mean rsum;;
 stdeviation rsum;;
 rsum;;
 
 (* Refs *)
 (* ref type predefined and is simply a 
    record with one a single mutable field 'contents'*)
 let x = { contents = 0};;
 x;;
 let x = ref 0;; (* same as above *)
 !x;;  (* same as x.contents *)
 x := !x + 1 ;;(* assignment *)
 !x;;

 (* Define your own operators *)
 type some_type = {mutable value : int };;
 let (!!!) x = x.value <- x.value + 10 ;;
 let one = {value = 1};;
 !!!one ;;
 let now_eleven = one;;

 
 (* For and While loops *)
 
 let permute array = 
    let length = Array.length array in
    for i = 0 to length -2 do
       let j = i + Random.int(length - i) in
       let tmp = array.(i) in 
         array.(i) <- array.(j);
         array.(j) <- tmp
       done
    ;;

 let find_first_negative_entry array =
        let pos = ref 0 in
        (* note use of short-circuit evaluation to avoid out of bounds error *)
        while !pos < Array.length array && array.(!pos) >= 0 do
            pos := !pos +1
        done;
        if !pos = Array.length array then None else Some !pos
        ;;
 
 let my_array = [| 3; 4; 5; 6; 3; -1; -3; 5|];;

 find_first_negative_entry my_array;;
 permute my_array;;
 my_array;;
 

 (******************************************************************************) 
 (********* First program (visible beyond the top level) *)
        
 (* open Core.Std *)
 
 let rec read_and_accumulate accum =
         let line = In_channel.input_line In_channel.stdin in
         match line with
         |None -> accum
         |Some x -> read_and_accumulate (accum +. Float.of_string x)
 
 let () = printf "Total: %F\n" (read_and_accumulate 0.) 








































