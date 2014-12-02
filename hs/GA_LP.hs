import System.Random

input=[(1,2),(3,4),(5,6)]

main = do print (runGA 100 input (genPop 100 1 [(0.2,0.5),(1.1, 1.3),(2.3, 2.9)]))

-- input: number of generations; input data; population 
-- output: new population
runGA :: Int -> [(Double,Double)] -> [(Double,Double,Double)] -> [(Double,Double,Double)]
runGA 0 _ pop = pop 
runGA n input pop = runGA (n-1) input (popEvolution n pop (evaluatePop input pop))

-- input: population size; initial seed for random gen; population ranges 
-- output: random population
genPop :: Int -> Int -> [(Double,Double)] -> [(Double,Double,Double)]
genPop 0 initialSeed _ = []
genPop n initialSeed a = do
   		let l1 = randomRs (fst(a!!0),snd(a!!0)) (mkStdGen initialSeed)
   		let l2 = randomRs (fst(a!!1),snd(a!!1)) (mkStdGen (initialSeed + 1))
   		let l3 = randomRs (fst(a!!2),snd(a!!2)) (mkStdGen (initialSeed + 2))
   		[(l1!!m,l2!!m,l3!!m) | m <- [1..n]]

-- input: input data; population; 
-- output: evaluation            
evaluatePop :: [(Double,Double)] -> [(Double,Double,Double)] -> [Double]
evaluatePop _ _ = []

-- input: generation for random seed use; population; population evaluation
-- ouput: new population
popEvolution :: Int -> [(Double,Double,Double)] -> [Double] -> [(Double,Double,Double)]
popEvolution _ _ _ = []

-- input: population; evaluation; elitism rate; 
-- output: new population
elitism :: [(Double,Double,Double)] -> [Double] -> Double -> [(Double,Double,Double)]
elitism _ _ 0 = []

-- input: population; evaluation; mutation rate; 
-- output: new population
mutation :: [(Double,Double,Double)] -> [Double] -> Double -> [(Double,Double,Double)]
mutation _ _ 0 = []

-- input: population; evaluation; crossover rate; 
-- output: new population
crossover :: [(Double,Double,Double)] -> [Double] -> Double -> [(Double,Double,Double)]
crossover _ _ 0 = []
