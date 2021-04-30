.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex


.. contents::
	:depth: 2
	:local:


.. _fiabilit�:


Fiabilit� des �quipements
=========================


Pr�sentation
------------

La fiabilit� dans le simulateur est mod�lis�e sous la forme de pannes (totales) et de r�paration (totales aussi) affectant un �quipement.

Ces pannes et ces r�parations suivent des lois probabilistes d�termin�es par diff�rents mod�les de pannes et diff�rents mod�les de r�paration.

Les notions suivantes sont utilis�es :

 - ``MTTF`` signifie *Mean Time To Failure*, la dur�e moyenne avant qu'un �quipement initialement fonctionnel tombe en panne

 - ``MTTR`` signifie *Mean Time To Repair*, la dur�e moyenne avant qu'un �quipement initialement en panne soit r�par� et devienne � nouveau fonctionnel

 - ``MTBF`` signifie *Mean Time Between Failures*, la dur�e moyenne entre deux pannes successives du syst�me : ``MTBF = MTTF + MTTR``.



:raw-latex:`\pagebreak`

.. include:: FailureModels-french.rst


:raw-latex:`\pagebreak`

.. include:: RepairModels-french.rst
