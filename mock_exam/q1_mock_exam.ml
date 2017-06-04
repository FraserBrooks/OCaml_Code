

 (*------------------------   Elements of Functional Computing Mock Exam 2017 *)

 (* Question 1 *)

 (* a) write an efficient sub-list function so that given two list arguments it should return
        the lowest index (if any) at which the first argument (called the ~key~) occurs as a 
        a sub-sequence of consecutive elements in the second argument. For example
              - 1;1;3 occurs as a subsequence of 0;1;1;1;1;1;1;3;1 at position 5
              - 1;1;1 occurs as a subsequence of 0;1;1;1;1;1;1;3;1 at position 1
              - 1;2;3 does not occur
 *)

 let test_list = [0;1;1;1;1;1;1;3;1];;
 
 let sub_list_start list1 list2 =
    
         let rec sub_list_found list1 list2 =
           match list2 with
              |y::ys ->
                   begin
                   match list1 with
                       |x::xs -> x = y  && sub_list_found xs ys
                       |[] -> true
                   end
              |[] -> false
         in
        
         let rec find_position l1 l2 pos =  
                 match l2 with
                 | _::tl -> if sub_list_found l1 l2 then
                                 !pos
                            else
                                 let _ = pos := !pos + 1 in
                                 find_position l1 tl pos
                 | []    -> -1
         in

         let index = ref 0 in
         if find_position list1 list2 index = -1 then
                 None
         else
                 Some  !index
         ;;
            

 sub_list_start [1;1;3] test_list;;
 sub_list_start [1;2;3] test_list;;
 sub_list_start [1;1;1] test_list;;
   
 (* b) same again but not necessarily consecutive *)
type point_in_array = {index:int; ls: int list};;

 let non_consecutive_sub_list_start list1 list2 =
     
     
     let rec find_lowest_index x ls pos =
             match ls with
              | hd::tl -> if hd = x then {index = !pos; ls = tl} else
                          let _ = pos := !pos + 1 in
                          find_lowest_index x tl pos
              | []  -> {index = -1; ls = []}
      in
      
      let rec sub_list_valid l1 record last_index =
             match l1 with
             | x::xs ->  let pia = find_lowest_index x record.ls {contents = last_index + 1} in
                         pia.index > last_index && sub_list_valid xs pia pia.index
             | []    ->  true
      in
      match list1 with
      | x::xs -> let startpia = find_lowest_index x list2 {contents = 0} in
                     if startpia.index = -1 then None else
                           if sub_list_valid xs {index = 0; ls = startpia.ls} 0 then
                                Some startpia.index else None
      | [] -> None
      ;;
 
 non_consecutive_sub_list_start [1;1;3] test_list;;
 non_consecutive_sub_list_start [1;2;3] test_list;;
 non_consecutive_sub_list_start [1;1;1] test_list;;

 

