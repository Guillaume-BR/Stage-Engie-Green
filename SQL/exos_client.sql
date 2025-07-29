1) Obtenir l’utilisateur ayant le prénom “Muriel” et le mot de passe “test11”, 
    sachant que l’encodage du mot de passe est effectué avec l’algorithme Sha1.
    SELECT id
    FROM client
    WHERE prenom = 'Muriel' AND password = SHA1('test11');

2) Obtenir la liste de tous les produits qui sont présent sur plusieurs commandes.
    SELECT nom
    FROM commande_ligne
    GROUP BY nom
    HAVING COUNT(*)>1;

3) Obtenir la liste de tous les produits qui sont présent sur plusieurs commandes et 
    y ajouter une colonne qui liste les identifiants des commandes associées.

    SELECT nom , GROUP_CONCAT(`commande_id`) AS liste_commandes
    FROM commande_ligne
    GROUP BY nom
    HAVING COUNT(*)>1;

4) Enregistrer le prix total à l’intérieur de chaque ligne des commandes, 
    en fonction du prix unitaire et de la quantité
    UPDATE commande_ligne
    SET prix_total = quantite * prix_unitaire;

5) Obtenir le montant total pour chaque commande et y voir facilement la date associée à 
    cette commande ainsi que le prénom et nom du client associé
    SELECT  CL.commande_id, 
            client.date_achat, 
            client.nom, 
            client.prenom,
            SUM(CL.quantite * CL.prix_unitaire) AS prix_commande
    FROM commande_ligne AS CL
    LEFT JOIN commande AS C ON C.id = CL.commande_id
    LEFT JOIN client ON client.id = C.client_id
    GROUP BY CL.commande_id;
    
6) Enregistrer le montant total de chaque commande dans le champ intitulé “cache_prix_total”
    UPDATE commande as t1
    INNER JOIN ( 
        SELECT commande_id, SUM(prix_total) AS p_total
        FROM commande_ligne
        GROUP BY commande_id
        ) AS t2 ON t1.id = t2.commande_id
    SET t1.cache_prix_total = t2.p_total;

7) Obtenir le montant global de toutes les commandes, pour chaque mois
    SELECT YEAR(date_achat) AS annee, MONTH(date_achat) AS mois, SUM(chache_prix_total) AS montant_global
    FROM commande
    GROUP BY annee, mois

8) Obtenir la liste des 10 clients qui ont effectué le plus grand montant de commandes, 
    et obtenir ce montant total pour chaque client.
    SELECT client.nom, client.prenom, C.client_id, SUM(cache_prix_total) as montant_client_total
    FROM commande AS C
    LEFT JOIN client ON client.id = C.client_id
    GROUP BY client_id
    ORDER BY montant_client_total DESC
    LIMIT 10;

9) Obtenir le montant total des commandes pour chaque date
    SELECT date_achat, SUM(cache_prix_total) = total_date
    FROM commande
    GROUP BY date_achat;

10) Ajouter une colonne intitulée “category” à la table contenant les commandes. 
    Cette colonne contiendra une valeur numérique
    ALTER TABLE commande
    ADD category TINYINT UNSIGNED NOT NULL

11) Enregistrer la valeur de la catégorie, en suivant les règles suivantes :
    “1” pour les commandes de moins de 200€ 
    “2” pour les commandes entre 200€ et 500€
    “3” pour les commandes entre 500€ et 1.000€
    “4” pour les commandes supérieures à 1.000€
    UPDATE commande
    SET category = (
        CASE 
            WHEN cache_prix_total < 200 THEN 1
            WHEN (cache_prix_total >= 200 AND cache_prix_total <500) THEN 2
            WHEN (cache_prix_total >= 500 AND cache_prix_total <1000) THEN 3
            ELSE 4
    )

12) Créer une table intitulée “commande_category” qui contiendra le descriptif de ces catégories
    CREATE TABLE IF NOT EXISTS  `commande_category` (
    `id` TINYINT UNSIGNED NOT NULL,
    `description` varchar(255) NOT NULL,
    PRIMARY KEY (`id`)

13) Insérer les 4 descriptifs de chaque catégorie au sein de la table précédemment créée
    INSERT INTO commande_category (description)
        VALUES("Commande inférieur à 200€",
              "Commande comprise entre 200€ inclu et 500€",
              "Commande comprise entre 500€ et 1000€",
              "Commande supérieure à 1000€"
              )

14) Supprimer toutes les commandes (et les lignes des commandes) inférieur au 1er février 2019. 
    Cela doit être effectué en 2 requêtes maximum
    DELETE CL  
    FROM commande_ligne AS CL
    JOIN commande AS C ON CL.commande_id = C.id
    WHERE C.date_achat < '2019-02-01';

    DELETE FROM commande
    WHERE date_achat < '2019-02-01';


