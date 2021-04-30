.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _gestionnaire de temps:
.. _gestionnaires de temps:

.. _ordonnanceur:
.. _ordonnanceurs:



Mod�lisation des gestionnaires de temps (``TimeManager``)
=========================================================


Objectif
--------

Le gestionnaire de temps synchronise finement chaque acteur de la simulation, afin de maintenir causalit� et reproductibilit� des simulations dans un contexte massivement parall�le et distribu�.



Fonctionnement
--------------

Le temps de la simulation (temps ``virtuel``) est compl�tement d�corr�l� du temps r�el (per�u par l'utilisateur). Le temps virtuel est discret et progresse � pas constant (``simulation tick``). La dur�e - exprim�e en temps virtuel - de chaque pas peut �tre choisie librement, sa valeur par d�faut est ``TickDuration = 20 ms`` (50 Hz, la fr�quence du secteur).

Le temps courant d'un acteur donn� s'obtient avec la fonction ``class_Actor:get_current_tick/1``.

Pour convertir un nombre de pas de temps en dur�e simul�e, utiliser ``class_TimeManager:convertTicksToSeconds/{1,2``}. La conversion inverse s'obtient avec ``class_TimeManager:convertSecondsToTicks/{1,2}``.


Impl�mentation
--------------

Le gestionnaire de temps est impl�ment� dans la classe d�finie par class_TimeManager.hrl_ et class_TimeManager.erl_.

Il est test� notamment dans :

 - ``class_TimeManager_interactive_test.erl`` pour le mode interactif et ``class_TimeManager_batch_test.erl`` pour le mode non-interactif (tests unitaires)

 - ``timeManagerAndActorPair_test.erl`` (test d'int�gration avec les acteurs de la simulation)



Voir aussi :

 - les `acteurs`_ de la simulation, cadenc�s par le gestionnaire de temps

 - le `gestionnaire de variables stochastiques`_, devant fonctionner en synergie avec le gestionnaire de temps
