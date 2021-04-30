.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




.. _acteur:
.. _acteurs:



Mod�lisation des acteurs de la simulation (``Actor``)
=====================================================


Pr�sentation
------------

La notion d'*acteur de la simulation* (ou plus simplement d'*acteur*) est la brique de base (la classe-m�re de plus haut niveau) de toute simulation : tout �l�ment simul� est un acteur (directement ou indirectement, via sp�cialisation par h�ritage), et chaque acteur est synchronis� finement par le `gestionnaire de temps`_ (**TimeManager**) afin de pr�server la causalit� et la reproductibilit� des comportement d'acteurs.


Synchronisation & �changes de messages entre acteurs
----------------------------------------------------


Ordonnancement des requ�tes
...........................

Le fait que la prise en compte des messages re�us s'effectue juste apr�s la fronti�re de pas de temps implique que si A envoie une requ�te � B � T (n�cessitant une r�ponse), il n'aura sa r�ponse qu'� T+2 au lieu de T+1 (mais dans tous les cas il ne pourra pas r�agir avant T+2, comme pr�visible).


Impl�mentation
--------------


Terminaison d'un pas de temps
.............................

A la fin de son pas, chaque acteur, qu'il ait envoy� un message inter-acteurs ou non, doit appeler ``class_Actor:manage_end_of_tick/1``. Cette fonction g�re compl�tement la terminaison du pas courant : attente asynchrone des accus�s de r�ception des �ventuels messages inter-acteurs envoy�s, envoi de la notification au gestionnaire de temps, pr�paration du pas suivant, etc.

Chaque clause de la m�thode ``act/1`` (probablement surcharg�e) finit donc g�n�ralement par : ``?wooper_return_state_only( class_Actor:manage_end_of_tick(LatestState) )``



.. comment

   Chaque m�thode d'acteur (*Actor Method*) doit positionner son attribut ``termination_mode``

   % This method must set its termination_mode attribute to 'terminated' if
   % it determines the actor has to disappear at next tick (in this case this
   % actor should notify the actors that may send it messages, as otherwise they
   % would freeze, since never receiving acknowlegment for their message to this
   % actor).
   % If this method does not send any actor message, then it must call
   % send_end_of_tick_notification just before returning.
   % If this method sends at least one actor message, then it must not send any
   % end-of-tick notification here, as it will be sent automatically as soon as
   % all sent messages will have been acknowledged by their recipients.



Les acteurs sont impl�ment�s par la classe d�finie dans ``class_Actor.hrl`` et ``class_Actor.erl``.

Voir aussi:

 - le `gestionnaire de temps`_, qui cadence tous les acteurs, et rien qu'eux

 - l'`acteur stochastique`_, un acteur sp�cialis� dont le mod�le fait usage de variables stochastiques

 - l'`�metteur de traces`_, classe dont d�rive chaque acteur
