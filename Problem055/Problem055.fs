let isPalindrome n = 
   let str = n.ToString()
   str = new string(str.ToCharArray() |> Array.rev)

let revInt (n:bigint) = 
   bigint.Parse(new string(n.ToString().ToCharArray() |> Array.rev))

let isLychrel (n:bigint) = 
   let rec loop n i = 
      if i >= 50 then true
      else
         let rev = revInt(n)
         let n2 = rev + n
         if isPalindrome n2 then
            false
         else
            loop n2 (i+1)
   loop n 1

let answer = 
   [1I..9999I]
   |> List.filter(isLychrel)
   |> List.length

printfn "%d" answer