

 (*------------------------   Elements of Functional Computing Mock Exam 2017 *)

 (* Question 1 *)

 (* a) write an efficient sub-list function so that given two list arguments it should return
        the lowest index (if any) at which the first argument (called the ~key~) occurs as a 
        a sub-sequence of consecutive elements in the second argument. For example
              - 1;1;3 occurs as a subsequence of 0;1;1;1;1;1;1;3;1 at position 5
              - 1;1;1 occurs as a subsequence of 0;1;1;1;1;1;1;3;1 at position 1
              - 1;2;3 does not occur
 *)

 test_list = [0;1;1;1;1;1;1;3;1];;
 
 let sub_list_start list1 list2 =

     let position = ref 0 in
    
         let rec sub_list_found list1 list2 =
           match list2 with
              |y::ys ->
                   match list1 with
                       |x::xs -> x =  && sub_list_found xs ys
                       |[] -> true
              |[] -> false
         in

     match list2 with
     | hd::tl -> if sub_list_found list1 list2 then
                     !position
                 else
                     position := !position +1
                     sub_list_start list1 tl 
     | []    -> -1;;

 sub_list_start [1;1;3] test_list;;
 sub_list_start [1;2;3] test_list;;
 sub_list_start [1;1;1] test_list;;
   


