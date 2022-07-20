module Exam2021_2
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertantly
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2021_2 = 
 *)

(* 1: Binary lists *)

(* Question 1.1 *)

    type binList<'a, 'b> =
    | Nil
    | Cons1 of 'a * binList<'a, 'b>
    | Cons2 of 'b * binList<'a, 'b>

    let rec length (ls: binList<'a,'b>) =
        match ls with
        | Nil -> 0
        | Cons1(_, binList) -> 1 + length binList
        | Cons2(_, binList) -> 1 + length binList
    
(* Question 1.2 *)
    let split (lst: binList<'a,'b>) =
        let rec aux acc lst' =
            match lst' with
            | Nil -> acc
            | Cons1(a, binList) ->
                aux ((fst acc) @ [a],snd acc) binList
            | Cons2(b, binList) ->
                aux (fst acc,(snd acc) @ [b]) binList
        aux ([],[]) lst
    
    let length2 (lst: binList<'a,'b>) =
        let rec aux acc lst' =
            match lst' with
            | Nil -> acc
            | Cons1(_, binList) -> aux ((fst acc + 1),snd acc) binList
            | Cons2(_, binList) -> aux (fst acc, (snd acc) + 1) binList
        aux (0,0) lst
    let length2v2 (lst: binList<'a,'b>) =
        let tuples = split lst
        let fstl = List.length (fst tuples)
        let sndl = List.length (snd tuples)
        (fstl,sndl)


(* Question 1.3 *)

    let rec map f g (lst: binList<'a,'b>) =
        match lst with
        | Nil -> Nil
        | Cons1(a, binList) -> Cons1(f a, map f g binList)
        | Cons2(b, binList) -> Cons2(g b,map f g binList)

(* Question 1.4 *)

    let rec filter f g (lst: binList<'a,'b>) =
        match lst with
        | Nil -> Nil
        | Cons1(a, binList) when f a -> Cons1(a, filter f g binList)
        | Cons1(_, binList) -> filter f g binList
        | Cons2(b, binList) when g b -> Cons2(b, filter f g binList)
        | Cons2(_, binList) -> filter f g binList

(* Question 1.5 *)

    let rec fold f g acc (lst: binList<'a,'b>) =
        match lst with
        | Nil -> acc
        | Cons1(a, binList) -> fold f g (f acc a) binList
        | Cons2(b, binList) -> fold f g (g acc b) binList

(* 2: Code Comprehension *)
    let rec foo xs ys =
      match xs, ys with
      | [], ys -> ys
      | xs, [] -> xs
      | x :: xs, y :: ys when x < y ->
        x :: (foo xs (y :: ys))
      | x :: xs, y :: ys ->
        y :: (foo (x :: xs) ys)

    let rec bar =
      function
      | [] -> []
      | [x] -> [x]
      | xs ->
        let (a, b) = List.splitAt (List.length xs / 2) xs
        foo (bar a) (bar b)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: 
    foo: 'a list -> 'a list -> 'a list
    bar:  'a list -> 'a list


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: bar takes a list and sorts it in ascending order
    
    Q: What would be appropriate names for functions 
       foo and bar?
       
    

    A: bar: listSort
       foo: listMerge
    
    Q: What would be appropriate names of the values a and b in bar.
    
    
    A: list1, list2
    
    *)
        

(* Question 2.2 *)

 
    (* 
    The code includes the keyword "and".

    
    Q: What function does this keyword serve in general
       (why would you use "and" when writing any program)?

    A: You would use and if you have 2 functions that are mutually dependent being able to call eachother


    Q: What would happen if you removed it from this particular program and
       replaced it with a standard "let"
       (change the line "and bar = " to "let rec bar = ")?
       Explain why the program either does or does not work.

    A: In this case everything would be fine since it is only bar which uses foo, and not the other way around.

    *)

(* Question 2.3 *) 
    (* let foo2 xs ys = List.unfold (fun acc elem ->) (xs, ys) *)
    
    (* use the following code as a starting template
    let foo2 xs ys = List.unfold <a function goes here> (xs, ys)
    *)

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: Ill pick bar. Lets look at the evaluation of this function call: bar [3;2;1]
    
    let rec bar =
      function
      | [] -> []
      | [x] -> [x]
      | xs ->
        let (a, b) = List.splitAt (List.length xs / 2) xs
        foo (bar a) (bar b)
        
    let([3],[2;1]) = List.splitAt 1 xs
    
    foo (bar a) (bar b)
    
    foo (3) let ([1],[2]) 
    
    foo (3) ([1;2])
    
    [1;2;3] 
    *)

(* Question 2.5 *)

    let fooTail _ = failwith "not implemented"

(* Question 2.5 *)

    let barTail _ = failwith "not implemented"

(* 3: Approximating square roots *)

(* Question 3.1 *)

    let rec approxSquare (x: int) (num: int) =
        let rec closestPerfectSquareRoot x (prevCPSR:int) =
            let diffprev = System.Math.Abs ((prevCPSR * prevCPSR) - x)
            let diffnext = System.Math.Abs (((prevCPSR+1) * (prevCPSR+1)) - x)
            if diffprev > diffnext then
                closestPerfectSquareRoot x (prevCPSR + 1)
            else prevCPSR
        let rec approximate (x:float) (r:float) num  =
            match num with
            | 0 -> r
            | _ -> let r' = ((x/r)+r)/2.0
                   approximate x r' (num - 1)
        approximate x (closestPerfectSquareRoot x 0) num
        
        

(* Question 3.2 *)

    let quadratic (a: int) (b: int) (c: int) (num: int) =
        let a' = float a
        let b' = float b
        let x1 = (-b' + (approxSquare(b*b - 4 * a * c) num))/( 2.0 * a')
        let x2 = (-b' - (approxSquare(b*b - 4 * a * c) num))/( 2.0 * a')
        (x1,x2)
                  

(* Question 3.3 *)

    let parQuadratic _ = failwith "not implemented"

(* Question 3.4 *)

    let solveQuadratic _ = failwith "not implemented"

(* 4: Rational numbers *)

(* Question 4.1 *)

    type rat = unit (* replace this entire type with your own *)

(* Question 4.2 *)

    let mkRat _ = failwith "not implemented"
    let ratToString _ = failwith "not implemented"

(* Question 4.3 *)

    let plus _ = failwith "not implemented"
    let minus _ = failwith "not implemented"
    let mult _ = failwith "not implemented"
    let div _ = failwith "not implemented"

(* Question 4.4 *)

    type SM<'a> = SM of (rat -> ('a * rat) option)
    let ret x = SM (fun st -> Some (x, st))
    let bind (SM m) f =
        SM (fun st ->
            match m st with
            | None -> None
            | Some (x, st') ->
                let (SM g) = f x
                g st')
        
    let (>>=) m f = bind m f
    let (>>>=) m n = m >>= (fun () -> n)
    let evalSM (SM f) s = f s 

    let smPlus _ = failwith "not implemented"
    let smMinus _ = failwith "not implemented"
    let smMult _ = failwith "not implemented"
    let smDiv _ = failwith "not implemented"

(* Question 4.5 *)

    (* You may solve this exercise either using monadic operators or 
        using computational expressions. *)

    type StateBuilder() =

        member this.Bind(x, f)    = bind x f
        member this.Zero ()       = ret ()
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let calculate _ = failwith "not implemented"