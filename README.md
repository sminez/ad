# ad :: an adaptable text editor

[![Build](https://github.com/sminez/ad/workflows/Build/badge.svg)](https://github.com/sminez/ad/actions?query=workflow%3ABuild) [![crates.io version](https://img.shields.io/crates/v/ad-editor)](https://crates.io/crates/ad-editor) [![docs.rs](https://img.shields.io/docsrs/ad-editor?logo=rust)](https://docs.rs/ad-editor)

`ad` (pronounced A.D.) is an attempt at combining a modal editing interface of likes of `vi`
and `kakoune` with the approach to extensibility of Plan9's `Acme`. Inside of `ad` text is
something you can execute as well as edit.

It is primarily intended as playground for experimenting with implementing various text editor
features and currently is not at all optimised or feature complete enough for use as your main
text editor.

That said, if this sounds like something you might find interesting then please to take a
look and see what you think! For now there isn't a whole lot of user facing documentation so
you will need to read through the source code to learn about what is and is not implemented.


## Project Status

`ad` is stable enough and feature complete enough that you can try it out and see what you
think. That said, there is currently very little documentation and there are likely to be
a variety of bugs and crashes in places that I've not managed to fully track down yet. If
you do try it out and spot something that is broken, please raise an issue on GitHub so I
can look into it.

You have been warned!


[![tour](https://img.youtube.com/vi/jb2pAi5hLUg/0.jpg)](https://www.youtube.com/watch?v=jb2pAi5hLUg)


## The design of ad

`ad` is aiming to be a hybrid of the pieces of various editors that I find most useful:
  - vim style modal editing to allow for convenient key bindings
  - convenient text navigation and selection from vim/kakoune
  - mini-buffer based user defined minor modes from emacs
  - sam/acme style editing commands for larger editing actions
  - acme style extension through exposing editor state and functionality for
    external client programs.
  - support for mouse based navigation and selection but not requiring that as the main
    way of using the editor like in acme. That's fine for desktop but most of the time
    I'm working with a laptop which makes that far too clunky.

`ad` is _not_ trying to replace vim (or kakoune, or emacs) in terms of being a massively
hackable editor. Rather it is trying to follow the philosophy of acme in being an
integrat**ing** development environment (rather than integrat**ed**). By which I mean
that the aim is to provide a comfortable editing environment to work in that supports
direct interaction with external tools and programs from the outside rather than pulling
everything **in**.


## Repo structure

Given the (arguably questionable) goal of implementing everything from scratch, there is a fair amount
of functionality included in `ad` which in turn is split out into a number of modules within the crate.
For now, I'm not structuring things as individual crates but that may change in future.

### Modules
_This is a non-exhaustive list of some of the more interesting parts of the internals of `ad`_

- **buffer/internal**: a [gap buffer](https://en.wikipedia.org/wiki/Gap_buffer) implementation for the
  internal state of a Buffer.
- **dot**: manipulation of the current selection in a given buffer (including vim-like motions)
- **exec**: minimal implementation of the core of the [sam editing language](http://doc.cat-v.org/bell_labs/sam_lang_tutorial/sam_tut.pdf)
- **fsys**: virtual filesystem interface to the editor state in the style of [acme](http://acme.cat-v.org/)
- **ninep**: [9p protocol](http://9p.cat-v.org/) implementation that backs the fsys module
  - Now moved out to its own crate with source code available [here](https://github.com/sminez/ad/crates/ninep).
- **regex**: custom regex engine that is able to work on character streams. This is nowhere near as performant as
  the [regex crate](https://github.com/rust-lang/regex) (obviously) but it allows for some flexability in tinkering
  with the exec command language.
- **trie**: [trie](https://en.wikipedia.org/wiki/Trie) data structure for handling sequence based keybindings


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
but the whole thing will be a lot easier to write if there isn't too much config parsing.

That said, the more I work on this, the more I wonder if it might be interesting to structure `ad`
in the same way as [penrose](https://github.com/sminez/penrose) and have it as a library for writing
your own text editor? That would require some restructuring but might be interesting to explore...

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
