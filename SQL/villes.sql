Requête SQL sur les villes de france : 

1) Obtenir la liste des 10 villes les plus peuplées en 2012
    SELECT *
    FROM villes_france_free
    ORDER BY ville_population_2012 DESC
    LIMIT 10; 

2) Obtenir la liste des 50 villes ayant la plus faible superficie
    SELECT *
    FROM villes_france_free
    ORDER BY ville_surface 
    LIMIT 50;

3) Obtenir la liste des départements d’outres-mer, c’est-à-dire ceux dont le numéro de département commencent par “97”
    SELECT *
    FROM departement
    WHERE departement_code LIKE '97%';

4) Obtenir le nom des 10 villes les plus peuplées en 2012, ainsi que le nom du département associé

    SELECT ville_nom_reel, departement_nom
    FROM villes_france_free AS V
    LEFT JOIN departement AS D ON V.ville_departement = D.departement_code;

5) Obtenir la liste du nom de chaque département, associé à son code et du nombre de commune au sein de ces département, en triant afin d’obtenir en priorité les départements qui possèdent le plus de communes
    SELECT D.departement_nom, V.ville_departement, COUNT(*) AS nb_villes
    FROM villes_france_free AS V
    LEFT JOIN departement AS D ON V.ville_departement = D.departement_code
    GROUP BY V.ville_departement
    ORDER BY nb_villes;

6) Obtenir la liste des 10 plus grands départements, en terme de superficie
    SELECT D.departement_nom ,  SUM(ville_surface) AS dept_surface
    FROM departement AS D
    LEFT JOIN villes_france_free AS V ON V.ville_departement = D.departement_code
    GROUP BY departement_nom 
    ORDER BY dept_surface DESC
    LIMIT 10;

7) Compter le nombre de villes dont le nom commence par “Saint”
    SELECT COUNT(*)
    FROM villes_france_free
    WHERE ville_nom_reel LIKE 'Saint%'

8) Obtenir la liste des villes qui ont un nom existants plusieurs fois, et trier afin d’obtenir en premier celles dont le nom est le plus souvent utilisé par plusieurs communes
    SELECT  V.*, COUNT(*) AS nb_ville
    FROM villes_france_free AS V
    GROUP BY ville_nom_reel
    HAVING COUNT(*)>1
    ORDER BY nb_ville DESC;

9) Obtenir en une seule requête SQL la liste des villes dont la superficie est supérieur à la superficie moyenne
    SELECT ville_nom_reel , AVG(ville_surface) AS surface_moy
    FROM villes_france_free
    WHERE ville_surface > SELECT AVG(ville_surface) FROM villes_france_free;

10) Obtenir la liste des départements qui possèdent plus de 2 millions d’habitants
    SELECT D.*
    FROM departement AS D
    LEFT JOIN villes_france_free AS V ON V.ville_departement = D.departement_code
    GROUP BY D.departement_code
    HAVING SUM(ville_population_2010) > 2000000;

11) Remplacez les tirets par un espace vide, pour toutes les villes commençant par “SAINT-” (dans la colonne qui contient les noms en majuscule)
    UPDATE villes_france_free
    SET ville_nom = REPLACE(ville_nom, '-', ' ')
    WHERE ville_nom LIKE 'SAINT-%';