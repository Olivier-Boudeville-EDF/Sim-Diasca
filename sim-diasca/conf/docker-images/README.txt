From this directory, various Docker images related to Sim-Diasca can be built, for various purposes:

- sim-diasca-base-runtime-image: this image contains a ready-to-use base runtime environment for Sim-Diasca, i.e. comprising all base tools for the build and execution of Sim-Diasca, and Sim-Diasca itself (both as sources and prebuilt) to obtain a more minimal, ready-to-use runtime environment able to execute simulations

- sim-diasca-documentation-generation-image: to obtain an environment appropriate to generate the Sim-Diasca documentation, notably in the context of continuous integration (ex: a GitLab one)

- sim-diasca-development-image: to obtain a full Sim-Diasca environment for development, comprising at least the main user-level dependencies (like a trace browser)

These directories contain the recipes to automatically generate the corresponding Docker images. These recipes can also serve as guidelines to create other images, for example more specialised, typically comprising not only Sim-Diasca but all the other elements necessary to cover any full simulator based on it (adding code and data for models, simulation cases, etc.).

These Docker images can then be used to run actual containers, typically by Docker itself or by Singularity, in various contexts (ex: virtualized development environment, automated testing of documentation generation in continuous integration, direct execution on an HPC cluster, or driven by a third-party simulation platform).


The 'Dockerfile' (the default name used by Docker) file contains the actual recipe (a series of commands operating on stacked read-only layers) for the build of a given image, to be triggered with 'docker build'.

The role of the 'docker-context' local filesystem tree is to contain the elements needed for the build of an image. This build will start by transferring the full non-ignored content of this tree to the Docker daemon, which is the actual creator of the target image (the build is not made directly by the docker command-line tool itself).


See also:
- Dockerfile reference: https://docs.docker.com/engine/reference/builder/
- Best practices for writing Dockerfiles: https://docs.docker.com/develop/develop-images/dockerfile_best-practices/
