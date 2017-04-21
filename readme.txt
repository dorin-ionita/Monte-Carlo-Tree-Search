Rulare tester din terminalul sistemului de operare:

    > stack runhaskell MCTSTest

Rularea unui test individual, din ghci, după încărcarea fișierului MCTSTest.hs:

    > runTestPP <numeTest>

Exemplu:

    > runTestPP testTreeShow

Având în vedere numărul mare de teste, ar putea fi util să comentați testele
din lista de la sfrârșitul fișierului MCTSTest.hs și să le decomentați
pe măsură ce avansați cu implementarea.

Testele utilizează exemplul de arbore din filmuleț, pe care realizează pașii
de backpropagation exact cu valorile din exemplu. Astfel, puteți urmări pas
cu pas evoluția algoritmului în testele `testSelectBackProp`
și `testTraverseBackProp`.

La bonus, pentru testul funcției `step`, asigurațivă că folosiți un număr
suficient de mare de iterații ale algoritmului MCTS (~ 500), pentru a obține
o estimare bună.