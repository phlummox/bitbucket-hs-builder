# bitbucket-builder

A few functions for building haskell projects on private Bitbucket repositories.

## Installation

Download to current directory with:

```
curl -L https://github.com/phlummox/bitbucket-hs-builder/releases/download/v0.1.0.0/bitbucket-builder-0.1.0.0_linux_x86_64.tgz | tar xvz
```

Or clone, cd in and `stack --stack-yaml stack-lts-12.yaml build`.

## Prerequisites for use

The environment should have the following installed:

- `curl`
- `git`
- `tar`

Also probably need CA root certificates for use by call.
(On Ubuntu, install with `apt-get install ca-certificates`.)

## Use

```bash
BB_APP_PASSWORD=<some password> \
  BB_USERNAME=<some user> \
  BB_REPO_PATH=bitbucket.org/myorg/myproject.git \
  EXTRA_GIT_ARGS="--single-branch --branch a-special-branch" \
  STACK_INSTALL_DIR="~/.local/bin" \
    ./bitbucket-builder
```

Required environment variables:

- `BB_APP_PASSWORD` and `BB_USERNAME`: authorization for bitbucket
- `BB_REPO_PATH`: path of repo to be cloned

Optional environment variables:

- `STACK_INSTALL_DIR`: otherwise use default of `~/.local/bin`
- `STACK_URL`: otherwise default to
  <https://github.com/commercialhaskell/stack/releases/download/v2.1.3/stack-2.1.3-linux-x86_64.tar.gz>
- `EXTRA_GIT_ARGS`: things passed to `git` when cloning; otherwise
  defaults to empty string


