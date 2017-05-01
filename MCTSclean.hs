{- a se elimina clean din nume -}
{-# LANGUAGE MultiParamTypeClasses #-}

module MCTS where

import GameState

import Prelude hiding (traverse)
import System.Random
import Data.List
data Node s a = NodeConstructor { current_state :: s
                                ,action_whose_result :: a
                                ,number_visited :: Int
                                ,score :: Float}
instance (Show s, Show a) => Show (Node s a) where
    show (NodeConstructor {current_state = state , action_whose_result = action, number_visited = visits, score = scor}) = 
            "(" ++ (show state) ++ "|" ++ (show action) ++ "|" ++ (show visits) ++ "|" ++ (show scor) ++ ")"
data Tree s a = TreeConstructor { current_node :: Node s a
                                , children :: [Tree s a] }
data Crumbs s a = CrumbsConstructor (Node s a) [Tree s a] [Tree s a]
data Zipper s a = ZipperConstructor {current_tree :: Tree s a 
                                    , crumb :: [Crumbs s a]
                                    , generator_of_random :: StdGen}
space :: Int -> String
space level = until (test_function) (do_function) "" where
    do_function :: String -> String
    do_function s = s ++ " "
    test_function :: String -> Bool
    test_function string = length string /= level
instance (Show s, Show a) => Show (Tree s a) where
    show tree = (showing_func 0 tree True) where
        showing_func level (TreeConstructor {current_node = c, children = (x:xs)}) True = 
            (showing_func level (TreeConstructor c xs) False) ++ (showing_func (level + 1) x False)
        showing_func level (TreeConstructor {current_node = c, children = (x:xs)}) False =
            (showing_func level (TreeConstructor c xs) False) ++ (showing_func (level + 1) x False)
        showing_func level (TreeConstructor {current_node = c, children = []}) _ =
            (space level) ++ (show c) ++ "\n"
treeState :: Tree s a -> s
treeState (TreeConstructor {current_node = (NodeConstructor {current_state = state_now})}) = state_now  
treeAction :: Tree s a -> a
treeAction (TreeConstructor {current_node = (NodeConstructor {action_whose_result = action_resultant})}) = action_resultant
treeScore :: Tree s a -> Float
treeScore (TreeConstructor {current_node = (NodeConstructor {score = score_of_node})}) = score_of_node
treeVisits :: Tree s a -> Int
treeVisits (TreeConstructor {current_node = (NodeConstructor {number_visited = visits})})  = visits
treeNode :: Tree s a -> Node s a
treeNode (TreeConstructor {current_node = node}) = node
treeChildren :: Tree s a -> [Tree s a]
treeChildren (TreeConstructor {children = ch}) = ch
zipperTree :: Zipper s a -> Tree s a
zipperTree (ZipperConstructor {current_tree = der_tree}) = der_tree
zipperGen :: Zipper s a -> StdGen
zipperGen (ZipperConstructor {generator_of_random = random_generator}) = random_generator
zipperCrumbs :: Zipper s a -> [Crumbs s a]
zipperCrumbs (ZipperConstructor {crumb = list_of_crumbs}) = list_of_crumbs
nodeCrumbs :: Crumbs s a -> Node s a
nodeCrumbs (CrumbsConstructor node left_brotha right_brotha) = node
leftBrotha :: Crumbs s a -> [Tree s a]
leftBrotha (CrumbsConstructor _ left_brotha _ ) = left_brotha
rightBrotha :: Crumbs s a -> [Tree s a]
rightBrotha (CrumbsConstructor _ _ right_brotha) = right_brotha
expand :: (s -> [(a, s)])  -- Generatorul stărilor succesoare
       -> s                -- Starea inițială
       -> Tree s a         -- Arborele de căutare
expand generator initial_state = expand_helper generator initial_state initial_action where
    initial_action = fst $ (generator initial_state) !! 0
    expand_helper :: (s -> [(a, s)]) -> s -> a -> Tree s a
    expand_helper generator state action = (TreeConstructor {current_node = (NodeConstructor {current_state = state, action_whose_result = action, number_visited = 0, score = 0})
                                                                , children = [(expand_helper generator (snd $ next_state) (fst $ next_state)) | next_state <- generator state]})
isLeaf :: Tree s a -> Bool
isLeaf (TreeConstructor node children) = null children
rolloutTree :: GameState s a => Tree s a -> StdGen -> ([Tree s a], Outcome, StdGen)
rolloutTree (TreeConstructor {current_node = node, children = ch}) random_function = rolloutTree_help (TreeConstructor {current_node = node, children = ch}) random_function [] where
    rolloutTree_help :: GameState s a => Tree s a -> StdGen -> [Tree s a] -> ([Tree s a], Outcome, StdGen)
    rolloutTree_help (TreeConstructor {current_node = node, children = ch}) random_function list =
        if isLeaf (TreeConstructor {current_node = node, children = ch}) then (((TreeConstructor {current_node = node, children = ch}) : list), outcome $ treeState (TreeConstructor {current_node = node, children = ch}), new_random_generator)
            else rolloutTree_help (ch !! random_number) new_random_generator ((TreeConstructor {current_node = node, children = ch}) : list) where
                random_number = (fst (next $ random_function)) `mod` (length ch)
                new_random_generator = snd (next $ random_function)
copilometru :: Tree s a -> Tree s a -> Ordering
copilometru (TreeConstructor {current_node = (NodeConstructor {number_visited = visits1, score = scor1})})
            (TreeConstructor {current_node = (NodeConstructor {number_visited = visits2, score = scor2})})
    | value1 > value2 = GT
    | value1 < value2 = LT
    | otherwise = EQ
    where
        value1 = scor1 / (fromIntegral visits1)
        value2 = scor2 / (fromIntegral visits2)
bestChild :: Tree s a -> Tree s a
bestChild (TreeConstructor {children = ch}) = bestChild_helper ch where
    bestChild_helper :: [Tree s a] -> Tree s a
    bestChild_helper = maximumBy copilometru-- point-free programming
getZipper :: Tree s a -> StdGen -> Zipper s a
getZipper tree random_generator = ZipperConstructor tree [] random_generator
isRoot :: Zipper s a -> Bool
isRoot zipper = not $ null $ filter (\x -> null x) [zipperCrumbs $ zipper]
ucb1 :: Float  -- scorul copilului                      // v mediu din formula
     -> Int    -- numărul de vizitări ale copilului     // n mic din formula
     -> Int    -- numărul de vizitări ale părintelui    // N mare din formula
     -> Float  -- estimarea                             // valoarea lui ucb1
ucb1 scor visits parent_visits = (scor / (fromIntegral $ visits))+ c * (sqrt ((log $ fromIntegral $ parent_visits) / (fromIntegral visits))) where c = 2.0

ucb1_wrapper :: (Float, Int, Int) -> Float
ucb1_wrapper (a, b, c) = ucb1 a b c 
order_function :: Int -> Tree s a -> Tree s a -> Ordering
order_function visits_parent tree1 tree2
    | ((treeVisits tree1) == 0) = GT
    | ((treeVisits tree2) == 0) = LT
    | ucb1 (treeScore tree1) (treeVisits tree1) visits_parent > ucb1 (treeScore tree2) (treeVisits tree2) visits_parent = GT
    | ucb1 (treeScore tree1) (treeVisits tree1) visits_parent < ucb1 (treeScore tree2) (treeVisits tree2) visits_parent = LT
    | otherwise = EQ
instance Eq s => Eq (Tree s a) where
    (TreeConstructor (NodeConstructor s1 a1 n1 sc1) ch1) == (TreeConstructor (NodeConstructor s2 a2 n2 sc2) ch2) = s1 == s2
select :: Eq s => Zipper s a -> Zipper s a
select zipper = (ZipperConstructor best_child new_crumbs randomizer) where
    randomizer = zipperGen zipper
    new_crumbs = (new_crumb : zipperCrumbs zipper) where
        new_crumb = (CrumbsConstructor (treeNode $ zipperTree zipper) left_brotha right_brotha) where
            {-left_brotha = take (best_child `elemIndex` (treeChildren $ zipperTree zipper)) (treeChildren $ zipperTree zipper) where-}
            left_brotha = take (index $ best_child `elemIndex` (treeChildren $ zipperTree zipper)) (treeChildren $ zipperTree zipper) where
                index :: Maybe Int -> Int
                index Nothing = -1
                index (Just x) = x
            right_brotha = drop ((index $ best_child `elemIndex` (treeChildren $ zipperTree zipper)) + 1) (treeChildren $ zipperTree zipper) where
                --best_child = maximumBy (order_function $ treeVisits $ zipperTree zipper) (treeChildren $ zipperTree zipper)
                index :: Maybe Int -> Int
                index Nothing = -1
                index (Just x) = x
    best_child = maximumBy (order_function $ treeVisits $ zipperTree zipper) (treeChildren $ zipperTree zipper)
traverse :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
traverse = until (test_function) (do_function) where
    do_function :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
    do_function = select
    test_function :: (Eq s, GameState s a) => Zipper s a -> Bool
    test_function zipper = ((treeVisits $ zipperTree $ zipper) == 0) || (null $ treeChildren $ zipperTree $ zipper)
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_,x, _) = x
trd3 :: (a, b, c) -> c
trd3 (_,_,x) = x 
isWin :: Outcome -> Bool
isWin (Win x) = True
isWin _ = False
getPointsFromOutcome :: Outcome -> Float
getPointsFromOutcome (Win x) = x
getPointsFromOutcome (Draw x) = x
rolloutZipper :: GameState s a => Zipper s a -> (Float, Maybe Int, Zipper s a)
rolloutZipper zipper = (updated_score, player, updated_zipper) where
    updated_score = 
        if (isWin $ snd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)) then getPointsFromOutcome $ snd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)
            else (getPointsFromOutcome $ snd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)) / (fromIntegral $ maxPlayers $ treeState $ zipperTree $ zipper)
    player = 
        if (isWin $ snd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)) then (Just ((playerIndex $ treeState $ zipperTree $ zipper) + 1))
            else Nothing
    updated_zipper = (ZipperConstructor (zipperTree $ zipper) (zipperCrumbs $ zipper) (trd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)))
toParent :: Zipper s a -> Zipper s a
toParent zipper = (ZipperConstructor new_tree new_crumb (zipperGen $ zipper)) where
    new_tree = (TreeConstructor current_node children) where
        current_node = nodeCrumbs $ ((zipperCrumbs $ zipper) !! 0)
        children = (leftBrotha $ ((zipperCrumbs $ zipper) !! 0)) ++ [zipperTree $ zipper] ++ (rightBrotha $ ((zipperCrumbs $ zipper) !! 0))
    new_crumb = drop 1 $ zipperCrumbs $ zipper
stateNode :: Node s a -> s
stateNode (NodeConstructor {current_state = state}) = state
actionNode :: Node s a -> a
actionNode (NodeConstructor {action_whose_result = action}) = action
visitsNode :: Node s a -> Int
visitsNode (NodeConstructor {number_visited = visit}) = visit
scoreNode :: Node s a -> Float
scoreNode (NodeConstructor {score = scor}) = scor
helper :: Float -> Maybe Int -> Zipper s a -> Zipper s a
helper scor player zipper = (ZipperConstructor new_tree2 [] (zipperGen $ zipper)) where 
    new_tree2 = (TreeConstructor current_node children) where
        current_node = (NodeConstructor state action visits score) where
            state = stateNode $ treeNode $ zipperTree $ zipper
            action = actionNode $ treeNode $ zipperTree $ zipper
            visits = (visitsNode $ treeNode $ zipperTree $ zipper) + 1
            score = (scoreNode $ treeNode $ zipperTree $ zipper) + scor
        children = treeChildren $ zipperTree $ zipper
backProp :: GameState s a => Float -> Maybe Int -> Zipper s a -> Zipper s a
backProp scor player zipper =
    if null $ zipperCrumbs $ zipper 
        then helper scor player zipper
        else backProp scor player new_zipper where 
            new_zipper = (ZipperConstructor new_tree new_crumbs random_generator)
            random_generator = zipperGen $ zipper
            new_crumbs = drop 1 $ zipperCrumbs $ zipper
            new_tree = (TreeConstructor current_node children) where
                current_node = nodeCrumbs $ ((zipperCrumbs $ zipper) !! 0)
                children = (leftBrotha $ ((zipperCrumbs $ zipper) !! 0)) ++ [current_tree] ++ (rightBrotha $ ((zipperCrumbs $ zipper) !! 0)) where
                    current_tree = (TreeConstructor modified_current_node current_children) where
                        current_children = treeChildren $ zipperTree $ zipper
                        modified_current_node = (NodeConstructor new_state new_action new_visits new_score) where
                            new_state = stateNode $ treeNode $ zipperTree $ zipper
                            new_action = actionNode $ treeNode $ zipperTree $ zipper
                            new_visits = (visitsNode $ treeNode $ zipperTree $ zipper) + 1
                            new_score = (scoreNode $ treeNode $ zipperTree $ zipper) + scor
backPropConverter :: GameState s a => (Float, Maybe Int, Zipper s a) -> Zipper s a
backPropConverter (x, y, z) = backProp x y z 
exploreOne_help :: (Eq s, GameState s a) => [Zipper s a] -> [Zipper s a]
exploreOne_help zippers_list = map (\zipper -> backPropConverter $ rolloutZipper $ traverse $ zipper) zippers_list
exploreOne :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
exploreOne zipper = (exploreOne_help [zipper]) !! 0
exploreMany :: (Eq s, GameState s a) => Int -> Zipper s a -> Zipper s a
exploreMany times zipper = snd $ until (test_function) (do_function) (times, zipper) where
    do_function :: (Eq s, GameState s a) => (Int, Zipper s a) -> (Int, Zipper s a)
    do_function (many, zipper) = (many - 1, exploreOne zipper)
    test_function :: (Eq s, GameState s a) => (Int , Zipper s a) -> Bool
    test_function (many, zipper) = many == 0
choose :: (Eq s, GameState s a) => Int -> s -> StdGen -> (a, s)
choose no_of_it state random_generator = (best_action, best_state) where 
    best_action = treeAction $ zipperTree $ select $ exploreMany no_of_it $ getZipper (expand successors state) random_generator
    best_state = treeState $ zipperTree $ select $ exploreMany no_of_it $ getZipper (expand successors state) random_generator
