# TODO

- [ ] Support syntax highlighting in virtual buffers
  - Currently this is determined based on the _path_ for a buffer, which only
    exists for "real" buffers. Changing this to be based on the name should
    fix it I think.
- [ ] Rethink how UI works to allow for decoupling the main editor behaviour
  - Currently the editor has a concept of the "current buffer" that determines
    how many of the editors actions are resolved. Moving to a model where the
    buffer ID is always specified would allow for decoupling the UI layout
    behaviour from the core editor state.
  - There are several editor actions relating to the UI which would then
    probably need to be handled by the UI implementation itself?
  - The "current buffer" concept is also exposed through the filesystem
    interface where it is useful for checking to see which buffer the user is
    currently working with. Maintaining that is worthwhile and arguably, having
    a way of seeing _all_ currently visible buffers is also useful.


## Misc / Editor features
- [ ] support running an exec script from a file
- [ ] Add command & Edit history using minibuffer with hidden lines

## Structural regular expressions
- [ ] Storing the history of previous edit commands and allowing for cycling through
      them would be helpful (same for Command mode).
