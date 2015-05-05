open Frame

module All2All : Pairing = 
struct
  let pair s1 s2 =
    let f x y = (x,y) in
    let pomo sez x = List.map (f x) sez in
    List.flatten (List.map (pomo s1) s2) 
end