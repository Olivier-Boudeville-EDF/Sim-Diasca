.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




.. _gestionnaire de hasard:
.. _gestionnaires de hasard:

.. _gestionnaire de variables stochastiques:
.. _gestionnaires de variables stochastiques:




Mod�lisation des gestionnaires de variables stochastiques (``RandomManager``)
=============================================================================


Objectif
--------

Beaucoup d'�l�ments simul�s ont un comportement r�gi par une ou plusieurs variables stochastiques, ob�issant � diverses distributions (densit�s de probabilit�).

L'objectif du gestionnaire de variables stochastiques (appel� plus simplement ``gestionnaire de hasard``) est de fournir d'embl�e dans Sim-Diasca les lois de probabilit� les plus courantes :

 - loi uniforme ("bruit blanc")

 - loi exponentielle

 - loi gaussienne (i.e. loi normale)


tout en pr�servant certaines propri�t�s de la simulation, comme la reproductibilit�, y compris dans un contexte distribu�.


Fonctionnement
--------------

Les g�n�rateurs al�atoires utilis�s ont g�n�ralement un �tat, qui est cach�, et initialis� par d�faut si aucune "graine" (``seed``) n'est d�finie. D�s lors la suite de valeurs qu'ils produisent est enti�rement reproductible d'un lancement � l'autre.

Si plusieurs processus Erlang faisaient appel � ces primitives du syst�me sans qu'un ordre reproductible des requ�tes soit assur�, chaque appel changerait l'�tat du g�n�rateur pour les appels suivants, et la simulation ne serait pas reproductible car par d�faut l'ordonnancement de processus Erlang dans un pas de temps ne l'est pas (il d�pend notamment des autres activit�s de la machine).

Une solution serait de faire en sorte que chaque processus Erlang embarque son propre g�n�rateur. Alors le syst�me serait alors reproductible, mais tous les g�n�rateurs produiraient la m�me suite pseudo-al�atoire, ce qui n'est pas souhaitable (par exemple tous les compteurs tomberaient en panne � la m�me date).

La solution est d'avoir un acteur de la simulation (il est donc synchronis�) qui d�tient le g�n�rateur partag�. La synchronisation assure la reproductibilit�, et le fait que de multiples mod�les acc�dent � ce g�n�rateur assure qu'ils n'utiliseront pas des suites al�atoires identiques : chaque nombre demand� fera avancer d'un cran dans la suite. Donc tout consommateur de hasard doit faire usage d'un gestionnaire de hasard.

Cette approche rec�le un d�faut toutefois : un acteur ayant besoin d'une valeur al�atoire doit faire une requ�te synchronis�e � un g�n�rateur, ce qui induit une latence de deux pas de temps (le temps que l'acteur soit pr�t � exploiter la valeur retourn�e par le g�n�rateur). Cette latence perturbe de mani�re ind�sirable les mod�les, et les acteurs simul�s "consommant" beaucoup de valeurs al�atoires seraient incorrectement dilu�s dans le temps simul�.

Le moteur de simulation actuel int�gre donc, au niveau de chaque `acteur stochastique`_ (i.e. consommateur de valeurs al�atoires) un m�canisme g�n�rique, transparent, lui permettant de maintenir un cache de valeurs al�atoires, et de le renouveler automatiquement aupr�s du g�n�rateur correspondant d�s lors qu'il atteint un seuil minimal. Le syst�me est relativement �volu� car il permet � tout acteur stochastique de g�rer en parall�le autant de suites al�atoires (en nombre et en distribution: gaussienne, uniforme, etc.) que voulu.

Notons qu'en g�n�ral un gestionnaire de hasard (``RandomManager``) est lanc� comme un singleton auquel tous les acteurs stochastiques acc�dent. Ce n'est pas imp�ratif : par exemple chaque mod�le de panne ou de r�paration embarque son gestionnaire de hasard priv�, car ils sont assur�s d'�tre sollicit�s un grand nombre de fois par un grand nombre d'acteurs.



G�n�rateur uniforme
...................


Une `sonde`_ pos�e en sortie d'une distribution uniforme g�n�r�e donne le r�sultat suivant :

:raw-html:`<img src="RandomManager-Uniform_probe.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{RandomManager-Uniform_probe.png}`

Les tirages al�atoires dans la plage de valeurs demand�e sont �quiprobables.
Il est possible d'activer un g�n�rateur de bruit blanc de qualit� sup�rieure (con�u pour la cryptographie).


G�n�rateur exponentiel
......................

Cette distribution, d�finie par un param�tre unique, ``lambda``, donne les r�sultats suivants :

:raw-html:`<img src="RandomManager-Exponential_probe.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{RandomManager-Exponential_probe.png}`

Elle est g�n�r�e directement par le simulateur, � partir de la source de bruit blanc pr�c�dente.



G�n�rateur gaussien
...................

Deux param�tres caract�risent la loi gaussienne (ou loi normale) : ``mu``, la valeur moyenne, et ``sigma``, la variance.

Cela donne la courbe suivante :

:raw-html:`<img src="RandomManager-Gaussian_probe.png"></img>`
:raw-latex:`\includegraphics[scale=0.6]{RandomManager-Gaussian_probe.png}`




Impl�mentation
--------------


Le gestionnaire de variables stochastiques est impl�ment� dans la classe d�finie par class_RandomManager.hrl_ et class_RandomManager.erl_.

Il est test� notamment dans :

 - class_RandomManager_test.erl_ (test unitaire)

 - randomManagerAndStochasticActorPair_test.erl_ (test d'int�gration avec les acteurs stochastiques)


Ses diff�rentes lois �l�mentaires fournies d'embl�e (``Sim-Diasca built-ins``) sont test�es sp�cifiquement dans :

 - class_RandomManager_Uniform_test.erl_

 - class_RandomManager_Exponential_test.erl_

 - class_RandomManager_Gaussian_test.erl_


Voir aussi :

 - le `gestionnaire de temps`_, qui cadence tous les acteurs, y compris le ou les gestionnaires de variables stochastiques

 - l'`acteur stochastique`_, un acteur sp�cialis� dont le mod�le fait usage de variables stochastiques et qui int�gre un m�canisme de gestion automatique de ces variables
