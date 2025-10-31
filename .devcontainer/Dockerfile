FROM docker.io/almalinux:10-minimal AS base

ARG USER_UID=1000

# Set bash as the default shell for RUN commands.
SHELL ["/bin/bash", "-eo", "pipefail", "-c"]

RUN --mount=type=cache,target=/var/cache/yum <<"EOF"
cat <<EOT > /etc/yum.repos.d/oneAPI.repo
[oneAPI]
name=Intel® oneAPI repository
baseurl=https://yum.repos.intel.com/oneapi
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://yum.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
EOT

microdnf update --assumeyes
microdnf install --assumeyes \
    binutils elfutils diffutils tar gzip bzip2 xz sudo make procps strace which tree curl git \
    intel-oneapi-common-vars intel-oneapi-compiler-dpcpp-cpp intel-oneapi-compiler-fortran intel-oneapi-mkl-devel intel-oneapi-mpi-devel
EOF

RUN cat >> /etc/bashrc <<"EOF"
# Source Intel OneApi 
source /opt/intel/oneapi/setvars.sh

# Set toolchain environment variables.
export FC=ifx
export CXX=icpx
export CC=icx
EOF

# Create non-root user and grant it sudo privileges:
# See: https://code.visualstudio.com/remote/advancedcontainers/add-nonroot-user
RUN <<"EOF"
useradd --uid $USER_UID -ms /bin/bash dev
echo "dev ALL=(root) NOPASSWD:ALL" > /etc/sudoers.d/dev
chmod 0440 /etc/sudoers.d/dev
EOF

# Change user from 'root' to 'dev', and set 'dev's home directory as the image workdir.
USER dev
WORKDIR /home/dev

# Create mount points for bash history and MinIO credentials in the user's home directory.
RUN <<"EOF"
mkdir ~/.history ~/.aws
touch ~/.history/.bash_history
EOF

# Install uv, python, and python tools.
RUN curl -LsSf https://astral.sh/uv/0.8.4/install.sh | sh
ENV PATH=/home/dev/.local/bin:$PATH \
    UV_LINK_MODE=copy
RUN <<"EOF"
uv python install 3.12
uv tool install fortls
uv tool install fprettify
EOF

# Install git prompt and completion scripts.
ADD --chmod=755 \
    https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash \
    https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh \
    /home/dev/.local/bin/

# Configure interactive bash environment.
RUN cat >> /home/dev/bashrc <<EOF
# Bash history
export PROMPT_COMMAND='history -a'
export HISTFILE=/home/dev/.history/.bash_history

# Git completion and prompt support.
source /home/dev/.local/bin/git-completion.bash
source /home/dev/.local/bin/git-prompt.sh

# Aliases
alias ls='ls --color=auto'

# Set bash prompt.
export PS1='[\u@\h \w\$(__git_ps1 " (%s)")]\n\\$ '
EOF
