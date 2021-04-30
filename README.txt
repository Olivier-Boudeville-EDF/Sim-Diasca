This is Sim-Diasca, a parallel and distributed discrete-time simulation engine for complex systems.

The name stands for 'Simulation of Discrete Systems of All Scales'.

This is the 2.3.13 version of Sim-Diasca.

Sim-Diasca is released under the LGPL licence (GNU Lesser General Public License, version 3).

Please refer to the GPL-licence.txt and LGPL-licence.txt text files, in the sim-diasca/doc/common-elements/licence directory.

See also: http://www.gnu.org/licenses/lgpl-3.0.html

Official Sim-Diasca website: http://sim-diasca.com.


A good start to understand how the engine should be used is to have a look in the mock-simulators/soda-test/src directory.

If you want to make some testing, once you ensured that you are relying on a GNU/Linux distribution offering a recent stable version of Erlang (typically 18.1 or newer, with the crypto and wx modules available - please refer to the associated 'Sim-Diasca Technical Manual' for more information) and that the Sim-Diasca codebase is compiled (ensure documented prerequisites are installed, then issue 'make all' from the root directory), just run, from the previous directory (mock-simulators/soda-test/src):

  make soda_deterministic_integration_run CMD_LINE_OPT="--batch"

as a simple yet complete example.


Please refer to the 'Sim-Diasca Technical Manual' or to the 'Sim-Diasca Developer Guide' for further information.

We hope that you will enjoy using Sim-Diasca!
