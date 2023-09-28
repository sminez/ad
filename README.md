# ad :: an adaptable text editor

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
- Simple modal editing to the extent that I use VIM (maybe including counts, maybe not)
- Sed/Sam style edit commands
- Acme style use of external commands rather than an embedded language:
  - Exposing current buffer / window state to external programs
  - Exposing events to external programs
  - Accepting events from other programs
- Virtual buffers for command output that can be hidden


  [0]: https://www.vim.org/
  [1]: https://neovim.io/
  [2]: https://github.com/mawww/kakoune
  [3]: https://en.wikipedia.org/wiki/Acme_(text_editor)
  [4]: https://www.youtube.com/watch?v=dP1xVpMPn8M
