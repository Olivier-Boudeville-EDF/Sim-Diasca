.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex



.. _mod�le de r�paration:
.. _mod�les de r�paration:


Mod�lisation des r�parations d'�quipement (``RepairModel``)
===========================================================


Pr�sentation
------------


Le mod�le de r�paration g�n�rique permet de d�terminer les modalit�s des r�parations op�r�es sur le syst�me, c'est-�-dire les dur�es de r�paration des �quipements tomb�s pr�alablement en panne.


:raw-html:`<img src="repair-models.png"></img>`
:raw-latex:`\includegraphics[scale=0.5]{repair-models.png}`



Les sp�cialisations du mod�le de r�paration g�n�rique sont :

 - loi uniforme (instants de r�paration �quiprobables, bruit blanc) : class_UniformRepairModel.erl_

 - loi gaussienne, la plus fid�le aux r�parations rencontr�es sur le terrain dans le cas g�n�ral : class_GaussianRepairModel.erl_




Relations avec les autres mod�les
.................................


Ces mod�les sont amen�s � interagir avec :

 - les `�quipements`_, qui, une fois affect�s par une panne, peuvent �tre r�par�s



Impl�mentation
--------------

Le mod�le de r�paration g�n�rique est impl�ment� dans class_RepairModel.erl_.

Les classes sp�cialis�es qui en h�ritent sont class_UniformRepairModel.erl_ et class_GaussianRepairModel.erl_.
