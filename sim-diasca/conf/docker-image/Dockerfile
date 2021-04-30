FROM debian:jessie

# Defines:
ENV ERL_PORT_BASE /opt/Software/ErlPort
ENV ERL_PORT_GIT https://github.com/hdima/erlport.git
ENV ERL_PORT_GIT_REV 246b7722d62b87b48be66d9a871509a537728962


# Base install:
RUN apt-get update &&\
	apt-get install --yes bzip2 coreutils build-essential g++ \
	libncurses5-dev openssl libssl-dev libwxgtk3.0-dev \
	libgl1-mesa-dev libglu1-mesa-dev libpng3 uuid-runtime \
	python-docutils eog evince gcc gnuplot gnuplot-x11 \
	geeqie graphviz uuid-runtime make mplayer nedit subversion ant \
	openjdk-7-jdk texlive git sudo unzip wget && \
	/bin/rm -rf /var/lib/apt/lists/*


# Install erlang:
COPY myriad/conf/install-erlang.sh /tmp/install-erlang.sh
RUN mkdir -p /opt/Software/Erlang;\
	cd /opt/Software/Erlang; \
	sudo /tmp/install-erlang.sh --doc-install /opt/Software; \
	echo 'export PATH=/opt/Software/Erlang/Erlang-current-install/bin:$PATH' >> ~/.bashrc

# Install LogMX:
ADD docker/LogMX_*.zip /tmp/
RUN mkdir -p /opt/Software/LogMX; \
	cd /opt/Software/LogMX;\
	unzip /tmp/LogMX_*.zip;\
	ln -sf LogMX_v* LogMX-current-install;\
	chmod +x LogMX-current-install/logmx.sh;\
	/bin/rm -f /tmp/LogMX_*.zip; \
	echo 'export PATH=/opt/Software/LogMX/LogMX-current-install:$PATH' >> ~/.bashrc

WORKDIR /opt/Software/LogMX
ADD traces/conf/logmx/logmx.properties traces/conf/logmx/managers.properties traces/conf/logmx/parsers.properties LogMX-current-install/config/
RUN mkdir -p LogMX-current-install/parsers/classes/ceylan/parser;
ADD traces/conf/logmx/CeylanTraceParser.class LogMX-current-install/parsers/classes/ceylan/parser/
ADD docker/license.* /opt/Software/LogMX/LogMX-current-install/config/


# Install Erlport:
RUN mkdir -p ${ERL_PORT_BASE};\
 cd ${ERL_PORT_BASE};\
 git clone ${ERL_PORT_GIT};\
 ln -s erlport ErlPort-current-install;\
 cd ${ERL_PORT_BASE}/erlport;\
 git checkout ${ERL_PORT_GIT_REV};\
 . ~/.bashrc; \
 make

COPY docker/GIT-README.md README.md

CMD xterm

# Add a user and preserve his home as a volume:
RUN useradd -ms /bin/bash dev;\
	echo 'dev ALL=(ALL:ALL) NOPASSWD:ALL' >> /etc/sudoers;


# Set-up user environment:
USER dev
WORKDIR /home/dev
RUN echo 'export PATH=/opt/Software/Erlang/Erlang-current-install/bin:/opt/Software/LogMX/LogMX-current-install:$PATH' >> ~/.bashrc;\
	echo 'export USER=dev' >> ~/.bashrc;\
	sudo chgrp dev /opt/Software/LogMX/LogMX-current-install/config/logmx.properties;\
	sudo chmod g+w /opt/Software/LogMX/LogMX-current-install/config/logmx.properties;\
	ln -s /opt/Software ~/Software


# Install Python:
RUN sudo apt-get update && sudo apt-get install -y curl;\
	curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash;\
	echo 'export PATH="/home/dev/.pyenv/bin:$PATH"' >> ~/.bashrc;\
	echo 'eval "$(pyenv init -)"' >> ~/.bashrc;\
	echo 'eval "$(pyenv virtualenv-init -)"' >> ~/.bashrc;

ENV PATH /home/dev/.pyenv/bin:$PATH
RUN	. ~/.bashrc;\
	pyenv install 3.5.2;\
	pyenv global 3.5.2;


# Install additional tools for convenience:
RUN sudo apt-get install -y vim vim-vimerl vim-vimerl-syntax vim-gtk vim-ctrlp bash-completion vim-addon-manager xfce4-terminal;\
	vim-addons install vimerl;\
	vim-addons install vimerl-syntax;\
	vim-addons install ctrlp;

VOLUME /home/dev

CMD xfce4-terminal
