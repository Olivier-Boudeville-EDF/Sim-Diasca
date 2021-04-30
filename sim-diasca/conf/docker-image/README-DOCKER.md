# Building an image from a dockerfile.

# Based on Debian Jessie.
# Provides notably Erlang, ErlPort, LogMX, Python, vim, and xfce4-terminal.

# User name: 'dev'.

Before build :

1. Mandatory, for LogMX:
* copy the LogMX *zip* archive in the 'docker/' context, once named *exactly* as: **LogMX_v????.zip**
* if using the professional version, copy also the license.properties file in the 'docker/' directory
2. ensure that a functional X server is available (on Windows, you can probably use 'Xming', available at http://xming.sf.net; you can in this case use the docker/unsecureXming.xlaunch)
3. properly configure the DISPLAY environment : DISPLAY=<IP_OF_YOUR_COMPUTER>:0.0

Then you are ready to go:

```
$ docker-compose up
```

Note that:
1. The first run will take a lot of time (maybe 30 minutes or an hour), but afterwards it will start immediately
2. If everything goes fine, a Terminal window pops up, in which you can work; you can clone Sim-Diasca there, and start working with your favourite tools
3. CAUTION: The 'dev' user is a sudoer so you have all the rights
4. CAUTION 2: Only the content of /home/dev is guaranteed to be preserved between runs of 'docker-compose up' command
