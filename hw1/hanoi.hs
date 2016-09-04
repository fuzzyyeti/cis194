-- Excercise 5

-- Define the types
type Peg = String
type Move = (Peg, Peg)


hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 starting temp ending = []
hanoi n starting temp ending = hanoi (n-1) starting ending temp ++ [(starting, ending)] ++ hanoi (n-1) temp starting ending 


-- Excercise 6
