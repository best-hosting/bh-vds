
### Build dependencies:

- [Haskell stack](https://docs.haskellstack.org/en/stable/README/).

### Runtime dependencies:

- [virsh](https://libvirt.org/virshcmdref.html). On Debian it is in package
  `libvirt-clients`.

### Build.

Specify correct `--prefix` during _build_ phase, because it will be compiled
in. Changing prefix later for `install` target _won't work_.

    ./make.hs --verbose --prefix /usr/local

### Install.

    ./make.hs --verbose install --prefix /usr/local

### Clean.

    ./really_clean.sh

After that only `stack` directories remain: `.stack-work` in project directory
and `~/.stack` in user's home.

### Use.

See `bh-vm --help` output and also `--help` for particular subcommand.
