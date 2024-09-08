# ninep :: a simple 9p protocol implementation

[9P](http://9p.cat-v.org) is a network protocol developed at Bell Labs for the
Plan 9 from Bell Labs distributed operating system as the means of accessing
and manipulating resources and applications transparently in a distributed
environment. 9P works both as a distributed file system and as a network
transparent and language agnostic ‘API’.

The section 5 man pages from plan 9 cover the [protocol](http://man.cat-v.org/plan_9/5/).


## A simple demo

The [examples/server.rs](examples/server.rs) file contains a minimal filesystem
that demos the functionality provided by this crate. You can use the `9p`
command from https://github.com/9fans/plan9port to interact with the server and
test it out.

See [the 9p man page](https://9fans.github.io/plan9port/man/man1/9p.html) for
more information on how the tool works

```bash
# Let 9p know where to find the socket we have opened
$ export NAMESPACE="/tmp/ns.$USER.$DISPLAY"

# List the contents of the filesystem and read the contents of a file
$ 9p ls ninep-server
$ 9p read ninep-server/foo

# List the contents of a subdirectory and a file in that subdirectory
$ 9p ls ninep-server/bar
$ 9p read ninep-server/bar/baz

# Read and then update the contents of a file
$ 9p read ninep-server/rw
$ echo "updated" | 9p write ninep-server/rw
$ 9p read ninep-server/rw
```

## A non-trivial filesystem

The [ad](https://github.com/sminez/ad) text editor provides a full virtual
filesystem interface in the style of plan9's [acme](http://acme.cat-v.org/)
text editor via its [fsys module](https://github.com/sminez/ad/tree/develop/src/fsys).
