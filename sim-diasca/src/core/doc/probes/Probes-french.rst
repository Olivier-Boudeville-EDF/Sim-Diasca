.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. _sonde:
.. _sondes:


Mod�lisation des sondes applicatives (``Probe``)
================================================


Pr�sentation
------------

Ces mod�les permettent de collecter des donn�es en cours de simulation, et d'en obtenir une vue graphique.


Mod�lisation
------------


Sondes g�n�riques
.................

Une sonde g�n�rique peuvent �tre configur�e dynamiquement pour collecter et g�n�rer une visualisation graphique de donn�es quelconques, y compris en terme de nombre de courbes suivies.

Leur sortie graphique est du type :

:raw-html:`<img src="Generic_probe_example.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{Generic_probe_example.png}`


La sonde g�n�rique est d�finie dans class_Probe.erl_. Elle est test�e avec class_Probe_test.erl_.




Sondes sp�cialis�es
...................

.. _sonde de fiabilit�:
.. _sondes de fiabilit�:


Sondes de fiabilit�
___________________


Une fois associ�e � un `�quipement`_, une sonde de fiabilit� enregistre � chaque pas de temps l'�tat de fonctionnement de l'�quipement (nominal ou en panne), et peut en g�n�rer la chronologie sous la forme suivante :

:raw-html:`<img src="Reliability_probe_example.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{Reliability_probe_example.png}`


La sonde de fiabilit� est d�finie dans ``class_ReliabilityProbe.hrl`` et ``class_ReliabilityProbe.erl``.
