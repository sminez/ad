# TODO

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

- [ ] Move focus state entirely to Layout.
  - Having this in Buffers was an artifact of the original design where only a single
    buffer was visible at a time really.
  - The logic involved in keeping focus in sync between the layout and buffer state
    can all disappear if Buffers moves to holding a simple BTreeMap of ID -> Buffer.

- [ ] Move plumbing out to its own crate and make the parser more robust
  - Parsing based on lines from String::lines messes with data that contains
    runs of newlines or trailing newlines.

- [ ] Move LSP and subprocess work to an async worker pool
  - Hopefully can get away with doing this with something lighter weight than tokio?
  - Might also want / be able to do the same thing with fsys but that will probably
    require some changes in ninep in order to support arbitrary async runtimes.

- [ ] Find a way of testing the regex engine against the main `regex` crate.
  - There are still a number of correctness issues in how the engine operates
  - We only strictly _need_ it for the streaming input case (which now that structex
    is its own crate, we could arguably drop?)
  - The only things that the `regex` / `regex-automata` crates don't provide is the
    "@" operator, but maybe that can be handled with a pre-processing step?
    - It'd be a shame to get rid of it as it was a lot of fun to implement, but it'd
      definitely be a better design choice to use a proper engine.

- [ ] Consolidate the various click position checks in the ui code
  - `border_at_coords`, `buffer_for_screen_coords`, `focus_buffer_for_screen_xy`
    and `scroll_view` all implement this internally rather than using a shared helper
  - looks like there are some slight differences in how they operate as well?

## Misc / Editor features
- [ ] support running an exec script from a file
- [ ] Add command & Edit history using minibuffer with hidden lines

## Structural regular expressions
- [ ] Storing the history of previous edit commands and allowing for cycling through
      them would be helpful (same for Command mode).
