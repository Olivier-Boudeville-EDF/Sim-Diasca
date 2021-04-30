If you see this file, then you did not mount a volume here!

You probably need to:
1. Create a working directory on the host machine
2. Mount it as a volume to the container at /opt/sim-diasca, using the docker -v flag: '-v /path/to/host/working/directory:/opt/sim-diasca'
3. In this volume, either clone the Sim-Diasca GIT repository, or unpack a source archive thereof
