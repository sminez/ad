# Design of the ad text editor

`ad` is aiming to be a hybrid of the pieces of various editors that I find most useful:
  - vim style modal editing to allow for convenient key bindings
  - convenient text navigation and selection from vim/kakoune
  - mini-buffer based user defined minor modes from emacs
  - vim/sam/acme style editing commands for larger editing actions
  - acme style functionality extension through exposing editor state and functionality
    for external client programs 
    - though not as the _only_ way to add functionality
  - support for mouse based navigation and selection but not requiring that as the main
    way of using the editor like in acme. That's fine for desktop but most of the time
    I'm working with a laptop which makes that far too clunky.

`ad` is _not_ trying to replace vim (or kakoune, or emacs) in terms of being a massively
hackable editor. Rather it is trying to follow the philosophy of acme in being an
integrat**ing** development environment (rather than integrat**ed**). By which I mean
that the aim is to provide a comfortable editing environment to work in that supports
direct interaction with external tools and programs from the outside rather than pulling
everything **in**.


## Functionality influences

### Emacs
Emacs is probably the smallest influence on `ad` given that most of the time that I have
made use of emacs I've pulled things into it to make it behave more like vim. That said,
there are some things in emacs that I do really like with the main one being the mini-buffer.
Or rather, packages like Ivy and Helm which use the mini-buffer to provide a rich way of
providing input to drive other code.
In neovim there is `Telescope` but that doesn't feel quite as flexible as what you can
get in emacs: it's tied to heavily to the concept of you always picking something from a
menu and then using that to drive an action.

### Kakoune
I **really** like the way kakoune handles multiple selections and cursors for doing more
complicated interactive edits. The ability to refine or extend a selection feels really
nice and all but replaced my use of vim's `.` operator for repeating the last edit.

I also really like the way that kakoune handles its command line with prompting for
additional input / showing autocomplete (though I'm on the fence around the virtual text
dialogue pop-ups for pending key presses and displaying text from things like LSP). In
particular I like that the prompt where you type is at the _bottom_ of the "mini-buffer"
so that your eye stays in one place on opening it rather than having to jump up to the
top.

### Vim / Neovim
This is my comfort place for editors so there's a lot that I use on a daily basis, but
at the same time there is also a **tonne** that I never touch (macros being a big one)
which I can easily do without. Vim purists will probably hate me for saying this but I
really don't care how few keystrokes it takes to do something. For me what matters is
being able to quickly and easily navigate and edit in a way that feels natural to my
thought process. So that's things like:
  - Normal mode for editing rather than inserting text
  - selecting inside of delimiters
  - jumping between blocks
  - visual mode for selecting larger sections of the file
  - command mode for running editor commands directly rather than needing to remember
    key bindings for _everything_

### Acme / sam
Last but not least we have these two. I've used them the least (largely due to their
need for using the mouse for navigation) but they have some _really_ nice features
that I wish were more widely adopted. Sam's use of each buffer having a "dot" that
represents the current focus of future editor actions and supporting manipulation
of that dot as a first class part of the editor is fantastic when it comes to making
large structural changes to files. Acme's ability to "execute" any text in one of its
buffers or tags is really fun (even if I feel like it tempts you into leaving code
comments that are executable all over the place) but my main interest in it is how it
exposes its internal state as a set of files that can be interacted with easily from
any language that can read & write files. That and the ability to take control of a
window from an external process lets you define custom UI elements and things like
test runners, linters and custom menus.
