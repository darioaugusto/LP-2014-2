import Debug.Trace
-- define type operator
type Op = (String,[String],[String],[String])

--define auxiliary functions to get tuple fields
op_action (a,_,_,_) = a
op_preconds (_,a,_,_) = a
op_add_list (_,_,a,_) = a
op_del_list (_,_,_,a) = a

--Remove duplicate elements from a list.
makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs) | pertence x xs = makeSet xs
			   | otherwise = [x] ++ makeSet xs

--True if the list contains no duplicate elements
pertence  :: Eq a => a -> [a] -> Bool
pertence x [] = False
pertence x (y:ys) = x==y || pertence x ys

--The set of values that are in the first set but not in the second set.
setDiff :: Eq a => [a] -> [a] -> [a]
setDiff [] [] = []
setDiff xs [] = xs
setDiff [] ys = []
setDiff (x:xs) ys | pertence x ys = setDiff xs ys
			   	  | otherwise = [x] ++ setDiff xs ys

--The set of values that are in either or both sets.
setUnion :: Eq a => [a] -> [a] -> [a]
setUnion xs ys = setDiff xs ys ++ makeSet ys

--check if element e is in list xs
member e [] = False
member e (x:xs) | e == x = True
				 | otherwise = member e xs
			
-- check if operator is appropriate for goal				 
appropriate_op goal operator = member goal (op_add_list operator)
				
				
-- apply the operator and update the data				
apply_op :: [String] -> String -> Op -> [String]
apply_op currents goal operator = if every currents (op_preconds operator)
							      then (setUnion (setDiff currents (op_del_list operator)) (op_add_list operator))
							      else currents

--Return true if true for all elements
every currents [] = True
every currents (precond:preconds) = (member precond currents) && (every currents preconds)

-- check if the goal is achievable from the current states and list of operators 
achieve :: [String] -> String -> [Op] -> Bool
achieve currents goal (operator:operators) | trace (concat currents) False = undefined
achieve currents goal (operator:operators) = if (every currents (op_preconds operator)) then apply_op currents goal operator else currents) || (achieve currents goal operators) 
achieve currents goal [] = (member goal currents) 

-- main call. Ex:  gps ["son_at_home","car_works"] ["son_at_school"] list_of_ops
gps currents [] ops = True				 
gps currents (goal:goals) ops = (achieve currents goal ops) && (gps currents goals ops)

-- list of available ops
list_of_ops = [("drive_son_to_school",["son_at_home","car_works"],["son_at_school"],["son_at_home"]),
				("shop_install_battery",["car_needs_battery","shop_knows_problem","shop_has_money"],["car_works"],["car_needs_battery"]),
				("tell_shop_problem",["in_communication_with_shop"],["shop_knows_problem"],[]),
				("telephone_shop",["know_phone_number"],["in_communication_with_shop"],[]),
				("look_up_number",["have_phone_book"],["know_phone_number"],[]),
				("give_shop_money",["have_money"],["shop_has_money"],["have_money"])]
