.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _�quipement:
.. _�quipements:



Mod�lisation des �quipements (``Equipment``)
============================================


Pr�sentation
------------


 Un �quipement est un `acteur`_ sp�cialis�, susceptible de tomber en panne et d'�tre r�par�.

Pour cela il fait appel � un `mod�le de panne`_ et � un `mod�le de r�paration`_.




Mod�lisation
------------



Relations avec les autres mod�les
.................................


Ce mod�le est amen� � interagir avec :

 - les `mod�les de panne`_, notamment ceux fond�s sur une distribution exponentielle (cf mod�lisation du MTTF)

 - les `mod�les de r�paration`_, notamment ceux fond�s sur une distribution gaussienne (cf mod�lisation du MTTR)

 - les `sondes de fiabilit�`_ : tout �quipement peut avoir son �tat de fonctionnement suivi par une telle sonde



Impl�mentation
--------------

La classe ``Equipment``, dont le mod�le g�n�rique est impl�ment� dans class_Equipment.erl_, red�finit automatiquement la m�thode ``act/1`` h�rit�e de la classe ``Actor``.

Chaque sp�cialisation de la classe ``Equipment`` doit en retour d�finir deux m�thodes, ``actNominal/1`` et ``actInDysfunction/1``, pour sp�cifier le comportement de l'�quipement respectivement en r�gime nominal (i.e. sans �tre en panne) et en r�gime d�faillant (i.e. en �tant en panne).

Si le comportement effectif est d�l�gu� � ces deux m�thodes, la terminaison de pas de temps reste � la charge de la m�thode ``act/1``, car c'est elle qui appellera, en interne � l'instance, celle de ces deux m�thodes qui sera appropri�e en fonction de l'�tat de l'objet, de mani�re synchrone (bloquante et sans �change de message).

Les transitions (changements d'�tat de l'�quipement en terme de panne) sont g�r�es de mani�re transparente par la classe-m�re ``Equipment`` . En pratique elles sont d�clench�es par des appels de m�thodes synchronis�es, entre l'�quipement g�n�rique et les mod�les de panne et de r�paration qui lui sont associ�s.

L'�tat d'un �quipement est notamment d�fini par les attributs suivants :

 - ``current_failure_state = nominal | dysfunction``, selon qu'il fonctionne correctement ou non

 - ``next_failure_tick = Integer | undefined``, �gal � la valeur du pas de temps pour la prochaine panne (si cette valeur est disponible)

 - ``next_repair_tick = Integer | undefined``, �gal � la valeur du pas de temps pour la prochaine r�paration (si cette valeur est disponible)




Voir aussi
----------

 - class_TestEquipment.erl_ : �quipement de test, et source d'inspiration pour d�finir son propre �quipement sp�cialis�

 - equipment_integration_test.erl_ : test d'int�gration entre des �quipements, des mod�les de panne et de r�paration, et des sondes de fiabilit�
