# TODO

- [ ] Support syntax highlighting in virtual buffers
  - Currently this is determined based on the _path_ for a buffer, which only
    exists for "real" buffers. Changing this to be based on the name should
    fix it I think.
- [ ] Rework minibuffer behaviour to handle all user input in the main loop
  - The current design has always felt hacky. It reads directly from input
    outside of the main editor event loop in order to allow for the minibuffer
    to be used to accept user input as part of other editor actions (such as
    providing a name for a file when saving to disk).
  - Getting rid of that direct read from input will require altering the design
    of the minibuffer to instead be held as optional state on the editor which
    handles processing input when present. That side of things isn't too bad,
    but reworking everything that uses this for "quick" user input is going to
    be a chunky refactor I suspect.
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


## MINIBUFFER USAGE WITHIN THE EDITOR

> This one is in a tail position of a match with no follow on code so it can be factored out reasonably simply
```
/home/sminez/repos/personal/ad/src/editor/actions.rs:239:40:                        let res = self.minibuffer_prompt("File changed on disk, reload? [y/n]: ");
```

> This one is for obtaining a buffer's save path, but we could change that to setting a name for the buffer and then recalling the parent save logic.
> That _should_ work but it'll require returning a tri-state and the control flow is a little funky...the save logic accepts an optional path to use
> so the output from the minibuffer can be injected there.
```
/home/sminez/repos/personal/ad/src/editor/actions.rs:405:47:            (None, Bk::Unnamed) => match self.minibuffer_prompt("Save As: ") {
```

> These ones are fairly simple: they're already just "once you have the input, call another method"
```
/home/sminez/repos/personal/ad/src/editor/actions.rs:510:30:        let selection = self.minibuffer_select_from("> ", numbered_lines);
/home/sminez/repos/personal/ad/src/editor/actions.rs:536:30:        let selection = self.minibuffer_select_from(prompt, lines);
/home/sminez/repos/personal/ad/src/editor/actions.rs:548:30:        let selection = self.minibuffer_select_from("> ", self.layout.as_buffer_list());
/home/sminez/repos/personal/ad/src/editor/actions.rs:855:35:        if let Some(input) = self.minibuffer_prompt(":") {
/home/sminez/repos/personal/ad/src/editor/actions.rs:865:35:        if let Some(input) = self.minibuffer_prompt("!") {
/home/sminez/repos/personal/ad/src/editor/actions.rs:876:35:        if let Some(input) = self.minibuffer_prompt("Edit> ") {
/home/sminez/repos/personal/ad/src/editor/actions.rs:892:35:        if let Some(input) = self.minibuffer_prompt("LSP Rename> ") {
/home/sminez/repos/personal/ad/src/editor/actions.rs:945:60:        if let MiniBufferSelection::Line { cy, .. } = self.minibuffer_select_from("kill", known) {
```

> These ones are a little odd, I'm using the minibuffer purely to display set of lines to the user but take no action when the minibuffer exits
```
/home/sminez/repos/personal/ad/src/editor/actions.rs:477:18:            self.minibuffer_select_from("No write since last change> ", dirty_buffers);
/home/sminez/repos/personal/ad/src/editor/actions.rs:563:14:        self.minibuffer_select_from(
/home/sminez/repos/personal/ad/src/editor/actions.rs:594:14:        self.minibuffer_select_from("<EDIT LOG> ", self.layout.active_buffer().debug_edit_log());
```


> These two helper methods are both only used in a single place within the codebase
> - They should just be inlined into their callsites as part of this work
```
/home/sminez/repos/personal/ad/src/editor/actions.rs:260:30:        let selection = self.minibuffer_select_from_command_output("> ", &cmd, d);
/home/sminez/repos/personal/ad/src/editor/actions.rs:416:26:                if !self.minibuffer_confirm("File already exists") {
```
