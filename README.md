# Polynomes
Implantation des algorithmes **Karatsuba** et **Toom-3** en langage **Ocaml**

## Diverses fonctions sur les grands nombre (polynomes)

- `cut poly k` → Coupe le polynôme en deux au k-ième élément.
- `cut3 poly k` → Coupe le polynôme en 3 au k-ième élément
- `find_cut poly1 poly2` → Renvoi l'entier n tel que le degres maximum des polynômes
soit <= 3n-1
- `create_random_list n d` → Créer une liste de n nombres de 0 à 10 puis de 10 à d
- `create_poly degre` → Créer un polynôme de degré “degre” 3
- `create_ten_poly degre` → Créer 10 polynômes de degré “degre”
- `create_ten_poly_from_list l` → Créer 10 polynômes à partir d'une liste de polynômes.
- `create_all_poly n d` → Créer une liste de n nombres de 0 à 10 puis de 10 à d puis
créer 10 polynômes à partir de cette liste.
- `time f` → Renvoi le temps d'exécution de la fonction f.
- `return_pair l` → Retourne deux éléments aléatoirement choisis dans la liste l.
- `pairs_from_list_poly listPoly` → Retourne une liste de paires de polynômes tirés au
hasard dans une liste de liste de polynômes.
- `eval_poly_from_couple_list_toom3 l alpha` → Retourne une liste des temps d'exécution
de la multiplication des polynômes de la liste des paires l en utilisant Toom3.
- `eval_poly_from_couple_list_karatsuba l alpha` → Retourne une liste des temps
d'exécution de la multiplication des polynômes de la liste des paires l en utilisant
Karatsuba revisité.
- `eval_poly_from_couple_list_naive l` → Retourne une liste des temps d'exécution de la
multiplication des polynômes de la liste des paires l en utilisant la multiplication
naïve.
- `mean_from_list_sum` → Retourne la moyenne des éléments d'une liste l.
- `eval_toom3 n d alpha` → Retourne la liste des temps d'exécution de la multiplication
utilisant Toom3 sur une liste de n degrés allant de 0 a d.
- `eval_karatsuba n d alpha` → Retourne la liste des temps d'exécution de la multiplication
utilisant karatsuba revisité sur une liste de n degrés allant de 0 a d.
- `eval_naive n d` → Retourne la moyenne de la liste des temps d'exécution de la
multiplication utilisant la multiplication naïve sur une liste de n degrés allant de 0 a d
