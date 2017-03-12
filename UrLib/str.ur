fun plural (n : int) (x : string) =
    let
        fun vowel c = case strindex "aeiou" c of
                          None => False
                        | Some _ => True
        val lenMinus1 = strlen x - 1
    in
        show n ^ " " ^
        if n = 1 || lenMinus1 < 0 then
            x
        else
            case strsub x lenMinus1 of
                #"s" => x ^ "es"
              | #"x" => x ^ "es"
              | #"y" =>
                if lenMinus1 = 0 || vowel (strsub x (lenMinus1 - 1)) then
                    x ^ "s"
                else
                    substring x 0 lenMinus1 ^ "ies"
              | _ => x ^ "s"
    end

fun stringList (xs : list string) =
    let
        fun stringList' xs =
            case xs of
                [] => "]"
              | x :: xs => ", " ^ x ^ stringList' xs
    in
        case xs of
            [] => "[]"
          | x :: xs => "[" ^ x ^ stringList' xs
    end

fun showList [t] (_ : show t) (xs : list t) = stringList (List.mp show xs)
