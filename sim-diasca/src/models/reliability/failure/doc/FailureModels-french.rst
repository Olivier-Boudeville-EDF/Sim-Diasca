.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




.. _mod�le de panne:
.. _mod�les de panne:



Mod�lisation des pannes d'�quipement (``FailureModel``)
=======================================================


Pr�sentation
------------

Le mod�le de panne g�n�rique permet de d�terminer les modalit�s des pannes affectant le syst�me, c'est-�-dire notamment les instants de survenue de pannes d'�quipements pr�alablement fonctionnels.


:raw-html:`<img src="failure-models.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{failure-models.png}`


Les sp�cialisations du mod�le de panne g�n�rique sont :

 - loi exponentielle, la plus fid�le aux pannes rencontr�es sur le terrain dans le cas g�n�ral : class_ExponentialFailureModel.erl_

 - loi gaussienne : class_GaussianFailureModel.erl_



Relations avec les autres mod�les
.................................


Ces mod�les sont amen�s � interagir avec :

 - les `�quipements`_, qui sont affect�s par les pannes



Impl�mentation
--------------

Le mod�le de panne g�n�rique est impl�ment� dans class_FailureModel.erl_.

Les classes sp�cialis�es qui en h�ritent sont class_ExponentialFailureModel.erl_ et class_GaussianFailureModel.erl_.
