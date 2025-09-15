# ad_repl

A simple REPL that syncs an `rc` shell session with an ad buffer.

  - "Execute" is defined to be "send as input to the shell".
  - Hitting return at the end of the buffer will send that line to the shell.
  - Running "clear" will clear the ad buffer
  - Running "exit" will close the shell subprocess as well as the ad buffer

## The rc shell

This program relies on the [rc][0] [shell][1] being present on your system. On Ubuntu / Debian
based systems you can install this using `sudo apt-get install rc`, or alternatively
you can compile it from source via [plan9port][2].

[0]: https://doc.cat-v.org/plan_9/4th_edition/papers/rc
[1]: https://github.com/9fans/plan9port/tree/master/src/cmd/rc
[2]: https://github.com/9fans/plan9port
