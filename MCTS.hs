{-# LANGUAGE MultiParamTypeClasses #-}

module MCTS where

import GameState

import Prelude hiding (traverse)
import System.Random
import Data.List

{-
    *** TODO ***

    Implementați tipul `Tree s a`, al arborilor de căutare, unde `s` reprezintă
    tipul stărilor, iar `a`, tipul acțiunilor.

    Sunt necesare câmpuri, precum:
    * starea curentă
    * acțiunea prin care s-a ajuns la stare
    * numărul de vizitări
    * scorul
    * copiii.
-}
data Node s a = NodeConstructor { current_state :: s
                                ,action_whose_result :: a
                                ,number_visited :: Int
                                ,score :: Float}
-- OK : ori e nod null ori e un nod asamblat din campurile mentionate in TODO schelet

instance (Show s, Show a) => Show (Node s a) where
    show (NodeConstructor {current_state = state , action_whose_result = action, number_visited = visits, score = scor}) = 
            "(" ++ (show state) ++ "|" ++ (show action) ++ "|" ++ (show visits) ++ "|" ++ (show scor) ++ ")"
            {- Posibil aici sa existe erori pentru ca , de exemplu current_state si number_visited nu sunt stringuri -}

data Tree s a = TreeConstructor { current_node :: Node s a
                                , children :: [Tree s a] }
-- OK: am un nod si un copii acestuia

{-
    *** TODO ***

    Implementați tipul `Zipper s a`, pentru parcurgerea arborilor de căutare
    de tipul `Tree s a`, unde `s` reprezintă tipul stărilor, iar `a`, tipul
    acțiunilor.

    Pe lângă componentele specifice unui zipper (vezi tutorialul din enunț),
    se va reține și un generator de numere aleatoare, modificat pe parcursul
    explorării arborelui.
-}
data Crumbs s a = CrumbsConstructor (Node s a) [Tree s a] [Tree s a]
-- OK: construcotrul pentru crumb contine Node s a si 2 liste -> fratii din st si dr

data Zipper s a = ZipperConstructor {current_tree :: Tree s a 
                                    , crumb :: [Crumbs s a]
                                    , generator_of_random :: StdGen}

{-
    *** TODO ***

    Instanțiați clasa `Show` cu tipul `Tree s a`.
-}
{-space :: Int -> String
space level
    | level == 0 = ""
    | otherwise = " " ++ (space (level - 1))-}

space :: Int -> String
space level = until (test_function) (do_function) "" where
    do_function :: String -> String
    do_function s = s ++ " "
    test_function :: String -> Bool
    test_function string = length string /= level

instance (Show s, Show a) => Show (Tree s a) where
    show tree = (showing_func 0 tree True) where
{-        showing_func :: Int -> Tree -> String-}
        showing_func level (TreeConstructor {current_node = c, children = (x:xs)}) True = 
            --(space level) ++ (show c) ++ "\n" ++ 
            (showing_func level (TreeConstructor c xs) False) ++ (showing_func (level + 1) x False)
        showing_func level (TreeConstructor {current_node = c, children = (x:xs)}) False =
            (showing_func level (TreeConstructor c xs) False) ++ (showing_func (level + 1) x False)
        showing_func level (TreeConstructor {current_node = c, children = []}) _ =
            (space level) ++ (show c) ++ "\n"
             {-where
                space :: Int -> String
                space level
                    | 0 = ""
                    | level = " " ++ (space (level - 1))-}

{-
    ****************
    Funcții de acces
    ****************
-}

{-
    *** TODO ***

    Întoarce starea asociată unui nod.
-}
treeState :: Tree s a -> s
treeState (TreeConstructor {current_node = (NodeConstructor {current_state = state_now})}) = state_now  

{-
    *** TODO ***

    Întoarce starea asociată unui nod.
-}
treeAction :: Tree s a -> a
treeAction (TreeConstructor {current_node = (NodeConstructor {action_whose_result = action_resultant})}) = action_resultant

{-
    *** TODO ***

    Întoarce scorul unui nod.
-}
treeScore :: Tree s a -> Float
treeScore (TreeConstructor {current_node = (NodeConstructor {score = score_of_node})}) = score_of_node

{-
    *** TODO ***

    Întoarce numărul de vizitări ale unui nod.
-}
treeVisits :: Tree s a -> Int
treeVisits (TreeConstructor {current_node = (NodeConstructor {number_visited = visits})})  = visits

treeNode :: Tree s a -> Node s a
treeNode (TreeConstructor {current_node = node}) = node

{-
    *** TODO ***

    Întoarce copiii unui nod.
-}
treeChildren :: Tree s a -> [Tree s a]
treeChildren (TreeConstructor {children = ch}) = ch

{-
    *** TODO ***

    Întoarce nodul pe care este centrat zipper-ul.
-}
zipperTree :: Zipper s a -> Tree s a
zipperTree (ZipperConstructor {current_tree = der_tree}) = der_tree

{-
    *** TODO ***

    Întoarce generatorul de numere aleatoare din interiorul zipper-ului.
-}
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

{-
    *****************
    Funcții pe arbori
    *****************
-}

{-
    *** TODO ***

    Construiește un arbore de căutare (eventual infinit), pornind de la funcția
    de generare a succesoarelor unei stări și de la starea inițială.
-}
expand :: (s -> [(a, s)])  -- Generatorul stărilor succesoare
       -> s                -- Starea inițială
       -> Tree s a         -- Arborele de căutare
expand generator initial_state = expand_helper generator initial_state initial_action where
    initial_action = fst $ (generator initial_state) !! 0
    expand_helper :: (s -> [(a, s)]) -> s -> a -> Tree s a
    expand_helper generator state action = (TreeConstructor {current_node = (NodeConstructor {current_state = state, action_whose_result = action, number_visited = 0, score = 0})
                                                                , children = [(expand_helper generator (snd $ next_state) (fst $ next_state)) | next_state <- generator state]})

{-TreeConstructor {current_node = (NodeConstructor {current_state = initial_state
                                                                                    , number_visited = 0,
                                                                                    , score = 0})
                                                    , children = [  |(children_action, children_state) <- generator initial_state]}-}
{--Deci ce as vrea eu sa fac ar fi sa iau starea initiala, din care sa derivez starile copii. Fiecare stare copil devine la randul ei
o stare initiala si asa mai departe. Fiecare nod va contine doar starea actuala si actiunea din care a provenit--}

{-
    *** TODO ***
    Explorează arborele, alegând la fiecare pas un succesor aleator,
    până la atingerea unei stări terminale (victorie/ remiză).

    Întoarce:
    * calea urmată, în ordine inversă, astfel că primul element din cale
      este nodul terminal
    * semnificația stării terminale (victorie/ remiză)
    * varianta finală a generatorului de numere aleatoare.
-}
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
{-
    *** TODO ***

    Determină cel mai bun copil al unui nod, din perspectiva raportului
    scor / număr de vizitări.

    Hint: `maximumBy` și `comparing`.
-}
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

{-
    *******************
    Funcții de zipper-e
    *******************
-}

{-
    *** TODO ***

    Construiește un zipper centrat pe arborele dat, care stochează generatorul
    de numere aleatoare precizat.
-}
getZipper :: Tree s a -> StdGen -> Zipper s a
getZipper tree random_generator = ZipperConstructor tree [] random_generator

{-
    *** TODO ***

    Verifică dacă zipper-ul este centrat pe rădăcina arborelui.
-}

isRoot :: Zipper s a -> Bool
isRoot (ZipperConstructor { crumb = [] })  = True
isRoot _ = False

{-isRoot :: Zipper s a -> Bool
isRoot zipper = not $ null $ filter (\x -> null x) [zipperCrumbs $ zipper]-}
-- Varianta cu functionala filter


{-
    *** TODO ***

    Valoarea ucb1 din filmuleț (constanta C = 2).
-}
ucb1 :: Float  -- scorul copilului                      // v mediu din formula
     -> Int    -- numărul de vizitări ale copilului     // n mic din formula
     -> Int    -- numărul de vizitări ale părintelui    // N mare din formula
     -> Float  -- estimarea                             // valoarea lui ucb1
ucb1 scor visits parent_visits = (scor / (fromIntegral $ visits))+ c * (sqrt ((log $ fromIntegral $ parent_visits) / (fromIntegral visits))) where c = 2.0

ucb1_wrapper :: (Float, Int, Int) -> Float
ucb1_wrapper (a, b, c) = ucb1 a b c 

{-
    *** TODO ***

    Pentru nodul pe care este centrat zipper-ul dat ca parametru, selectează
    copilul având valoarea ucb1 maximă. Întoarce zipper-ul centrat pe copilul
    respectiv.

    Atenție! Așa cum rezultă și din filmuleț, un nod nevizitat are valoarea ucb1
    infinită, și va fi întotdeauna ales în defavoarea altor noduri deja vizitate.
-}
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

                                
{-
    *** TODO ***
    Aplică repetat `select` până la atingerea unui nod nevizitat sau terminal.
-}
{-traverse :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
traverse zipper
    | (treeVisits $ zipperTree $ zipper) == 0 = zipper
    | null $ treeChildren $ zipperTree $ zipper = zipper
    | otherwise = traverse $ select $ zipper-}

traverse :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
traverse = until (test_function) (do_function) where
    do_function :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
    do_function = select
    test_function :: (Eq s, GameState s a) => Zipper s a -> Bool
    test_function zipper = ((treeVisits $ zipperTree $ zipper) == 0) || (null $ treeChildren $ zipperTree $ zipper)

--
{-
    *** TODO ***

    Aplică `rolloutTree` pentru arborele pe care este centrat zipper-ul.

    Întoarce:
    * scorul cu care vor fi actualizate nodurile de pe calea către rădăcină
    * numărul jucătorului pentru care se realizează actualizarea
      (se poate ignora pentru cerința cu un singur jucător)
    * noul zipper, actualizat cu generatorul de numere aleatoare întors
      de `rolloutTree`.

    Pentru cerința cu cel puțin doi jucători, scorul și numărul jucătorului
    se calculează astfel:
    * Pentru victorie, se utilizează scorul din obictul `Outcome` și numărul
      jucătorului aferent stării terminale.
    * Pentru remiză, se utilizează scorul din obiectul `Outcome` împărțit
      la numărul de jucători, și `Nothing`.
-}
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
{-    updated_score = getPointsFromOutcome $ snd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)
    player = (Just (playerIndex $ treeState $ zipperTree $ zipper))
    updated_zipper = (ZipperConstructor (zipperTree $ zipper) (zipperCrumbs $ zipper) (trd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)))-}
    updated_score = 
        if (isWin $ snd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)) then getPointsFromOutcome $ snd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)
            else (getPointsFromOutcome $ snd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)) / (fromIntegral $ maxPlayers $ treeState $ zipperTree $ zipper)
    player = 
        if (isWin $ snd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)) then (Just ((playerIndex $ treeState $ zipperTree $ zipper) + 1))
            else Nothing
    updated_zipper = (ZipperConstructor (zipperTree $ zipper) (zipperCrumbs $ zipper) (trd3 $ rolloutTree (zipperTree zipper) (zipperGen zipper)))



{-
    *** TODO ***

    Urcă un nivel în arbore.
-}
toParent :: Zipper s a -> Zipper s a
toParent zipper = (ZipperConstructor new_tree new_crumb (zipperGen $ zipper)) where
    new_tree = (TreeConstructor current_node children) where
        current_node = nodeCrumbs $ ((zipperCrumbs $ zipper) !! 0)
        children = (leftBrotha $ ((zipperCrumbs $ zipper) !! 0)) ++ [zipperTree $ zipper] ++ (rightBrotha $ ((zipperCrumbs $ zipper) !! 0))
    new_crumb = drop 1 $ zipperCrumbs $ zipper
{-
    *** TODO ***

    Implementează pasul de backpropagation, unde cei trei parametri sunt cele
    trei componente întoarse de `rolloutZipper`.

    Astfel, se urmează calea către rădăcină și se crește cu 1 numărul
    de vizitări ale tuturor nodurilor. În plus, scorul se modifică astfel:
    * Pentru cerința cu un singur jucător, se modifică toate nodurile.
    * Pentru cerința cu mai mulți jucători, avem următoarele cazuri:
      * În caz de victorie, se actualizează doar nodurile cu numărul de jucător
        dat de parametru.
      * În caz de remiză, se actualizează toate nodurile.
    
    Zipper-ul final este centrat pe rădăcină.
-}
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
{-problema : copii nu sunt updatati aici -}

{-
    *** TODO ***

    Realizează o iterație completă a MCTS, incluzând toate etapele, pornind
    de la un nod oarecare din arbore și finalizând pe rădăcină.
-}
backPropConverter :: GameState s a => (Float, Maybe Int, Zipper s a) -> Zipper s a
backPropConverter (x, y, z) = backProp x y z 

{-exploreOne_help :: (Eq s, GameState s a) => [Zipper s a] -> [Zipper s a]
exploreOne_help zippers_list = map (\zipper -> backPropConverter $ rolloutZipper $ traverse $ zipper) zippers_list

exploreOne :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
exploreOne zipper = (exploreOne_help [zipper]) !! 0-}
--varianta cu functionala map

exploreOne :: (Eq s, GameState s a) => Zipper s a -> Zipper s a
exploreOne zipper = backPropConverter $ rolloutZipper $ traverse $ zipper

{-
    *** TODO ***

    Realizează un număr dat de iterații complete ale MCTS.
-}
{-exploreMany :: (Eq s, GameState s a) => Int -> Zipper s a -> Zipper s a
exploreMany 0 zipper = zipper
exploreMany x zipper = exploreMany (x - 1) $ exploreOne zipper-}

exploreMany :: (Eq s, GameState s a) => Int -> Zipper s a -> Zipper s a
exploreMany times zipper = snd $ until (test_function) (do_function) (times, zipper) where
    do_function :: (Eq s, GameState s a) => (Int, Zipper s a) -> (Int, Zipper s a)
    do_function (many, zipper) = (many - 1, exploreOne zipper)
    test_function :: (Eq s, GameState s a) => (Int , Zipper s a) -> Bool
    test_function (many, zipper) = many == 0

{-
    *** TODO ***

    Alege o acțiune pornind de la o stare dată și un număr de iterații ale MCTS.
    Întoarce o pereche cu acțiunea și starea următoare.

    Funcția ar trebui să verifice mai întâi dacă nu cumva una dintre stările
    imediat următoare reprezintă o victorie, caz în care o alege direct.
    Altfel, demarează procesul standard de explorare.

    Atenție! La prima iterație a algoritmului, cu toate că numărul de vizitări
    ale rădăcinii este 0, NU se face rollout la rădăcină, ci se trece direct
    la explorarea copiilor acesteia. Acest lucru este vizibil și în filmuleț.

    După realizarea numărului dat de iterații, se alege efectiv acțiunea,
    utilizând `bestChild`.
-}
choose :: (Eq s, GameState s a) => Int -> s -> StdGen -> (a, s)
choose no_of_it state random_generator = (best_action, best_state) where 
    best_action = treeAction $ zipperTree $ select $ exploreMany no_of_it $ getZipper (expand successors state) random_generator
    best_state = treeState $ zipperTree $ select $ exploreMany no_of_it $ getZipper (expand successors state) random_generator
