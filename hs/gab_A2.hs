import Data.List
import System.Random

runGA 0 pop val _ _ _ _ _ = (qsort_ (evaluatePop val pop)) !! 0 
runGA n [] val popSize crossoverRate elitismRate mutationRate initialSeed = runGA n (genPop popSize initialSeed val) val popSize crossoverRate elitismRate mutationRate initialSeed
runGA n pop val popSize crossoverRate elitismRate mutationRate initialSeed = runGA (n-1) (popEvolution pop val crossoverRate mutationRate elitismRate initialSeed) val popSize crossoverRate elitismRate mutationRate (initialSeed + length(pop))

--convert decimal to binary
toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
        
--number of bins to represent the number n
nBins n = (length (toBin n) -1) --considering unsigned

--generation population of popSize elements at random
genPop popSize initialSeed val = [[(randomRs (0,1) (mkStdGen (initialSeed+y)))!!k | k <- [0..((nBins val)-1)]] | y <- [0..popSize]]

--evaluating the population
evaluatePop _ [] = []
evaluatePop val (p:ps) = [(ev, element)] ++ evaluatePop val ps
						 where
							ev = evaluateInd val p
							element = p

--population evolution: new population composed by elitism, crossover, mutation operations
--popEvolution 0 [] _ _ _ _ _ = []
popEvolution pop val crossoverRate mutationRate elitismRate initialSeed = 
					elitismPop pop val elitismRate
					++ mutationPop (qsort_ (evaluatePop val pop)) (round (mutationRate * fromIntegral(length pop))) initialSeed
					++ crossoverPop (qsort_ (evaluatePop val pop)) (round ((crossoverRate/2) * fromIntegral(length pop))) initialSeed
					++ genPop (round ((1 - elitismRate - crossoverRate - mutationRate) * fromIntegral(length pop))) initialSeed val
							
--individual evaluation function
evaluateInd val p = abs (val - (sum [(p!!(n-y))*2^(y) |y <- [0..n]]))
				 where
					n = (length p)-1
			
--individual mutation function				
mutateInd p initialSeed = (take n p) ++ [if (p!!n)>0 then 0 else 1] ++ (drop (n+1) p)
					   where
					     n = (randomRs (0,((length p)-1)) (mkStdGen initialSeed))!!0

--pair of individuals crossover function
crossoverInd a b initialSeed = [(take n a) ++ (drop n b), (take n b) ++ (drop n a)]
 			   where
				 n = (randomRs (0,((length a)-1)) (mkStdGen initialSeed))!!0

--elitism population function
elitismPop pop val elitismRate = 
	removeEv(take (round ((elitismRate)*(fromIntegral (length pop)))) (qsort_ (evaluatePop val pop)))

--mutation population function
mutationPop evSortedPop nElements initialSeed = [mutateInd (roulette evSortedPop (initialSeed+y)) (initialSeed+y) | y <- [0..nElements]]

--crossover population function
crossoverPop evSortedPop 0 initialSeed = []
crossoverPop evSortedPop nElements initialSeed = (crossoverInd (roulette evSortedPop (initialSeed)) (roulette evSortedPop (initialSeed+nElements)) (initialSeed)) ++ crossoverPop evSortedPop (nElements-1) (initialSeed+2)


--roulette function: gets an index from a given list of probabilities
roulette pop initialSeed = (snd (pop!!(rouletteEv (getEv pop) initialSeed)))
rouletteEv list initialSeed = indexIsBigger list 0 n
					   where
					     n = (randomRs (0,(sum list)) (mkStdGen initialSeed))!!0
indexIsBigger list ix n = if ((sum (take ix list)) > n) then (ix-1) else (indexIsBigger list (ix+1) n)

	
--aux functions
removeEv ((a,b):xs) = b : removeEv xs
removeEv _          = []

getEv ((a,b):xs) = a : getEv xs
getEv _          = []

qsort_ [] = []
qsort_ (l:ls) = qsort_ smaller ++ [l] ++ qsort_ greater
				  where
					smaller = [s | s <- ls, (fst s) <= (fst l)]					
					greater = [g | g <- ls, (fst g) > (fst l)]
