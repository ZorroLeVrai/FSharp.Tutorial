module Equation

let solveLinear a b =
    [(-b / a)]

let solveQuadratic a b c =
    if a = 0. then
        solveLinear b c
    else
        let delta = b**2. - 4.*a*c
        let baseSolution = -b / (2.*a)
        match delta with
            | d when d < 0. -> []
            | d when d = 0. -> [baseSolution]
            | d -> [baseSolution - (sqrt d) / (2.*a); baseSolution + (sqrt d) / (2.*a)]