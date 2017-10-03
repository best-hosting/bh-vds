
### Build dependencies:

- [Haskell stack](https://docs.haskellstack.org/en/stable/README/).

### Runtime dependencies:

- [virsh](https://libvirt.org/virshcmdref.html). On Debian it is in package
  `libvirt-clients`.

### Build.

    ./make.hs --verbose --prefix /usr/local

### Install.

    ./make.hs --verbose install --prefix /usr/local

### Clean.

    ./really_clean.sh

After that only `stack` directories remain: `.stack-work` in project directory
and `~/.stack` in user's home.

### Use.

See `bh-vm --help` output and also `--help` for particular subcommand.
