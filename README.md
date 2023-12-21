# ad :: an adaptable text editor

`ad` (pronounced A.D.) is an attempt at combining a modal editing interface of likes of `vi`
and `kakoune` with the approach to extensibility of Plan9's `Acme`. It is primarily intended
as playground for experimenting with implementing various text editor features and currently
is not at all optimised or feature complete enough for use as your main text editor.

That said, if this sounds like something you might find interesting then please to take a
look and see what you think! For now there isn't a whole lot of user facing documentation so
you will need to read through the source code to learn about what is and is not implemented.


## Project Status

This is not even remotely usable or stable enough for real world day-to-day use yet. All aspects of
the code are changing constantly as I hack on things and try to work out how I want my ideal text
editor to work.

You have been warned!


![screenshot](https://raw.githubusercontent.com/sminez/ad/develop/screenshot.png)


## Why?

I've used [vim][0] for years now (more recently [neovim][1] and [kakoune][2]) and I really love the
core editor itself. A while back I discovered [acme][3] through a fantastic [screencast][4] from
Russ Cox, showing how you could interface with it via plan filesystem protocol, allowing you to run
pretty much whatever you want inside of the editor (in any language) so long as you can interact with
that protocol. _That_ I absolutely love, but the lack of modal editing and requirement to use the mouse
when I'm sat with my laptop is proving hard to get used to, so I set about looking at how to port
over some of the acme ideas into vim (namely the load/execute semantics via the plumber and the
idea of exposing the editor state in a really simple way to client programs).

Turns out, vim has a _lot_ more built into it that I was previously aware (and I've been hacking on
my vimrc for years now) which was more than a little scary. What I want is a small, usable editor
that I can hack on.

So...How hard could it be?

## Simplicity
For things that are going to be core parts of the experience (bindings, per-filetype configuration)
I'm just going to hard code stuff. I'll try to do it in a way that makes it easy to update / change
but the whole thing will be a lot easier to write if there isn't really any config parsing.

## Goals
- Simple modal editing to the extent that I use VIM
- Sed/Sam style edit commands
- Acme style use of external commands rather than an embedded language:
  - Exposing current buffer / window state to external programs
  - Exposing events to external programs
  - Accepting events from other programs
- Virtual buffers for command output that can be hidden


## Sam style structural regular expressions

One aim of this project is to provide an implementation of "Structural Regular Expressions" as first
presented (to my knowledge) in the [Sam text editor][5] from plan9 by Rob Pike. [This tutorial][6]
from Pike covers the command language of Sam which I am using as a starting point for the command
language for `ad`. So far I'm not aiming for a perfect match with the functionality of Sam or Acme
but I _am_ looking to make use of the pieces that feel particularly useful. As the project develops
I may well end up pulling in more but for now I'm happy to have a decent starting point for an
implementation of the structural regular expression engine.

There is still a fair amount to do but so far the idea is to allow for repeated narrowing and looping
over sub-matches within a buffer or file loaded from disk. (A streaming interface working over stdin
is coming but I need to have a think about how best to buffer the input and track partial matches in
the regex engine to avoid slowing things down too much or requiring the engine to buffer and collect
_all_ of its standard input before matching).

The current engine can be used via the `-e` and `-f` flags to run `ad` in headless mode, but hooking
things into the interactive editor directly should be coming soon. For now, here is a demo of some
simple functionality of the engine:

```sh
$ cat examples/exec_scripts/result_fns.ad
,                              # set dot to be the full input (not required as this is the default)
x/fn@*?\{/                     # select all Rust function signatures up to the opening brace
g/->.*Result.*\{/              # keep those that return some form of Result
x/fn (\w+)@*?-> (.*?) \{/      # extract the function name and return type from the signature
p/($FILENAME) $1 returns $2/   # print them along with the filename using a template


$ ad -f examples/exec_scripts/result_fns.ad src/**/*.rs | head
(src/buffer/buffers.rs) open_or_focus returns io::Result<()>
(src/buffer/dot/cur.rs) fmt returns fmt::Result
(src/buffer/dot/mod.rs) fmt returns fmt::Result
(src/buffer/dot/range.rs) fmt returns fmt::Result
(src/buffer/edit.rs) fmt returns fmt::Result
(src/buffer/mod.rs) new_from_canonical_file_path returns io::Result<Self>
(src/exec/parse.rs) execute returns Result<(usize, usize), Error>
(src/exec/parse.rs) step returns Result<(usize, usize), Error>
(src/exec/parse.rs) try_parse returns Result<Self, Error>
(src/exec/parse.rs) validate returns Result<(), Error>
```

  [0]: https://www.vim.org/
  [1]: https://neovim.io/
  [2]: https://github.com/mawww/kakoune
  [3]: https://en.wikipedia.org/wiki/Acme_(text_editor)
  [4]: https://www.youtube.com/watch?v=dP1xVpMPn8M
  [5]: http://doc.cat-v.org/plan_9/4th_edition/papers/sam/
  [6]: http://doc.cat-v.org/bell_labs/sam_lang_tutorial/sam_tut.pdf
