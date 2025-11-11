# ad :: an adaptable text editor

[![Build](https://github.com/sminez/ad/workflows/Build/badge.svg)](https://github.com/sminez/ad/actions?query=workflow%3ABuild)
[![crates.io version](https://img.shields.io/crates/v/ad-editor)](https://crates.io/crates/ad-editor)
[![docs.rs](https://img.shields.io/docsrs/ad-editor?logo=rust)](https://docs.rs/ad-editor)

`ad` (pronounced A.D.) is an attempt at combining a modal editing interface of likes of `vi`
and `kakoune` with the approach to extensibility of Plan9's `Acme`. Inside of `ad` text is
something you can execute as well as edit.

:warning: In the spirit of Plan9 itself, `ad` is primarily intended as playground for experimenting with
implementing various text editor features and currently is not recommended for use as your
primary text editor.

That said, if this sounds like something you might find interesting then please do take a
look and see what you think! For now there isn't a whole lot of user facing documentation other
than the built in `:help` section, so you will need to read through the source code and GitHub
issues to learn about what is and is not implemented.


![screenshot](https://raw.githubusercontent.com/sminez/ad/develop/screenshot.png)


## :warning: Project Status

`ad` is now stable enough that you can try it out and see what you think. That said, there is
currently very little documentation and there are likely to be a variety of bugs and crashes in
places that I've not managed to fully track down yet. If you do try it out and spot something that
is broken, please raise an issue on GitHub so I can look into it. The project is also under active
development and while I aim to avoid arbitrary incompatible changes to the way the editor works,
I am not yet fully committing to a stable keybindings or default behaviours. If you are using `ad`
and you find that something is no longer working as expected following an update, please refer to
the git log for details of what has changed.

You have been warned!


### :technologist: Contributing

The project as a whole isn't particularly well suited for external contributors in its current
state so please do [raise an issue](https://github.com/sminez/ad/issues/new/choose) to discuss
any proposed changes or feature requests first rather than directly opening PRs. Outside of
minor bug fixes and typo corrections I am unlikely to be able to do anything other than close
PRs that have been opened without prior discussion of the issue they are intending to address.


## :eyes: Getting started

Packaging of the project to include the default config files and helper scripts isn't currently
in place, so the recommended way to try out `ad` is to clone this repo and compile from source:

```bash
$ git clone git@github.com:sminez/ad.git
$ cd ad
$ cargo install --path .
$ cargo xtask setup-dotfiles
```

Please see [here](https://github.com/sminez/ad/blob/develop/xtask/src/setup.rs#L59) for the
actions that are carried out by the `setup-dotfiles` task. You should review these and make
sure that you are happy to proceed before running the task.

From there you should be able to open `ad` and run the `:help` command to view the built-in help.
If you would like to watch a tour of how `ad` works there is one available [here](https://www.youtube.com/watch?v=jb2pAi5hLUg).

> Please be aware that given the early stage of the project and frequent changes to the codebase,
> the exact content of the video tour may not accurately reflect the current state of `ad`.


## :straight_ruler: The design of ad

`ad` is aiming to be a hybrid of the pieces of various editors that I find most useful:
  - vim style modal editing
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
integratING development environment (rather than integratED). By which I mean
that the aim is to provide a comfortable editing environment to work in that supports
direct interaction with external tools and programs from the outside rather than pulling
everything **in**.


### :building_construction: Building on top of ad

In addition to the `data/bin` directory of this repo you might want to check out the following links for examples of
what you can achieve with `ad`'s filesystem interface:

- A [Zettelkasten note taking application](https://gist.github.com/davcam/a4570acb520dce3a25a98cf2ddbb9ef2) from [@davcam](https://github.com/davcam)


## :package: Modules
Given the (arguably questionable) goal of implementing as much as possible from scratch, there is a fair
amount of functionality included in `ad` which in turn is split out into a number of modules within the
crate. For now, I'm not structuring things as individual crates but that may change in future.

_This is a non-exhaustive list of some of the more interesting parts of the internals of `ad`_

- **buffer/internal**: a [gap buffer](https://en.wikipedia.org/wiki/Gap_buffer) implementation for the
  internal state of a Buffer.
- **dot**: manipulation of the current selection in a given buffer (including vim-like motions)
- **exec**: minimal implementation of the core of the [sam editing language](http://doc.cat-v.org/bell_labs/sam_lang_tutorial/sam_tut.pdf)
- **fsys**: virtual filesystem interface to the editor state in the style of [acme](http://acme.cat-v.org/)
- **lsp**: a minimal [LSP](https://microsoft.github.io/language-server-protocol/) client
- **ninep**: [9p protocol](http://9p.cat-v.org/) implementation that backs the fsys module
  - Now moved out to its own crate with source code available [here](https://github.com/sminez/ad/crates/ninep).
- **regex**: custom regex engine that is able to work on character streams. This is nowhere near as performant as
  the [regex crate](https://github.com/rust-lang/regex) (obviously) but it allows for some flexibility in tinkering
  with the exec command language.
- **syntax**: [tree-sitter](https://tree-sitter.github.io/tree-sitter/) based syntax highlighting (with
  optional per-line regex based highlighting if grammars are unavailable for a particular filetype)
- **trie**: [trie](https://en.wikipedia.org/wiki/Trie) data structure for handling sequence based keybindings


## :question: Why?

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


## A note on the structural regular expressions

One aim of this project is to provide an implementation of "Structural Regular Expressions" as first
presented (to my knowledge) in the [Sam text editor][5] from plan9 by Rob Pike. [This tutorial][6]
from Pike covers the command language of Sam which I am using as a starting point for the command
language for `ad`. So far I'm not aiming for a perfect match with the functionality of Sam or Acme
but I _am_ looking to make use of the pieces that feel particularly useful.

The engine developed for `ad` has now been extracted out into its own crate [here][7] which contains
more information on how the engine works and how it can be used in other projects.

  [0]: https://www.vim.org/
  [1]: https://neovim.io/
  [2]: https://github.com/mawww/kakoune
  [3]: https://en.wikipedia.org/wiki/Acme_(text_editor)
  [4]: https://www.youtube.com/watch?v=dP1xVpMPn8M
  [5]: http://doc.cat-v.org/plan_9/4th_edition/papers/sam/
  [6]: http://doc.cat-v.org/bell_labs/sam_lang_tutorial/sam_tut.pdf
  [7]: https://github.com/sminez/structex
