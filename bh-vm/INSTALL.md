
### Build dependencies:

- [Haskell stack](https://docs.haskellstack.org/en/stable/README/).

### Runtime dependencies:

- [virsh](https://libvirt.org/virshcmdref.html). On Debian it is in package
  `libvirt-clients`.

### Build.

    ./make.hs --verbose --prefix /usr/local

### Install.

Install everything:

    ./make.hs --verbose --prefix /usr/local install

or configs only:

    ./make.hs --verbose --prefix /usr/local configs

or binaries only:

    ./make.hs --verbose --prefix /usr/local binaries

### Clean.

    ./really_clean.sh

After that only `stack` directories remain: `.stack-work` in project directory
and `~/.stack` in user's home.

### Use.

See `bh-vm --help` output and also `--help` for particular subcommand.
