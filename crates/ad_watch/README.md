# ad-watch

A simple file watcher program that can run a command each time an `ad` buffer is saved.


### Usage
This program needs to be run from within [ad](github.com/sminez/ad). Type `!` to enter
command mode and then type the following:

```bash
ad-watch <command> [args...]
```

A new `+watch` window will be opened with the output of your command. Each time the
buffer is saved, the command will re-run.
