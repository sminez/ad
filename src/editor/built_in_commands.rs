pub fn built_in_commands() -> Vec<(Vec<&'static str>, &'static str)> {
    vec![
        (
            vec!["b", "buffer"],
            "switch to the buffer with the given ID ('buffer 5')",
        ),
        (
            vec!["bn", "buffer-next"],
            "switch to the next available open buffer in the buffer list",
        ),
        (
            vec!["bp", "buffer-prev"],
            "switch to the previous available open buffer in the buffer list",
        ),
        (
            vec!["cd", "change-directory"],
            "change ad's working directory ('cd ../src')",
        ),
        (
            vec!["db", "delete-buffer"],
            "delete the active buffer as long as there are no pending changes",
        ),
        (
            vec!["db!", "delete-buffer!"],
            "delete the active buffer discarding all pending changes",
        ),
        (
            vec!["dc", "delete-column"],
            "delete the active column so long as this would not exit the editor with unsaved changes",
        ),
        (
            vec!["dc!", "delete-column!"],
            "delete the active column discarding all pendings changes if this is the last column",
        ),
        (
            vec!["dw", "delete-window"],
            "delete the active window so long as this would not exit the editor with unsaved changes",
        ),
        (
            vec!["dw!", "delete-window!"],
            "delete the active window discarding all pendings changes if this is the last window",
        ),
        (vec!["new-window"], "clone the current window as a new window at the end of the focused column"),
        (vec!["new-column"], "clone the current window as a new column"),
        (vec!["next-window"], "move focus to the next window in the current column"),
        (vec!["next-column"], "move focus to the next column"),
        (vec!["prev-window"], "move focus to the previous window in the current column"),
        (vec!["prev-column"], "move focus to the previous column"),
        (
            vec!["echo"],
            "display the given string in the status line ('echo hello, world!')",
        ),
        (
            vec!["E", "Edit"],
            "run an Edit command (See 'Running Edit Commands')",
        ),
        (
            vec!["Execute"],
            "execute the contents of the current dot in the focused buffer",
        ),
        (
            vec!["expand-dot"],
            "smart expand the current cursor position into a range",
        ),
        (vec!["help"], "display this help file"),
        (
            vec!["Load"],
            "load the contents of the current dot in the focused buffer",
        ),
        (
            vec!["mark-clean"],
            "mark the current buffer as being clean to prevent saving changes",
        ),
        (
            vec!["o", "open"],
            "open the given file path in a new buffer ('open README.md')",
        ),
        (
            vec!["O", "open-in-new-window"],
            "open the given file path in a new buffer placed in a new window ('open-in-new-window README.md')",
        ),
        (vec!["pwd"], "print the current editor working directory"),
        (
            vec!["q", "quit"],
            "quit ad as long as there are no buffers with pending changes",
        ),
        (
            vec!["q!", "quit!"],
            "quit ad discarding all pending changes for open buffers",
        ),
        (
            vec!["reload-buffer", "Get"],
            "refresh the current buffer's content from the state of the file on disk",
        ),
        (
            vec!["reload-config"],
            "reload the editor config file located at ~/.ad/config.toml",
        ),
        (
            vec!["set"],
            "set a config property ('set bg-color=#ebdbb2')",
        ),
        (vec!["view-logs"], "open ad's internal logs in a new buffer"),
        (
            vec!["viewport-bottom"],
            "place the current line at the bottom of the window",
        ),
        (
            vec!["viewport-center"],
            "place the current line at the center of the window",
        ),
        (
            vec!["viewport-top"],
            "place the current line at the top of the window",
        ),
        (
            vec!["wq", "write-quit"],
            "save the current buffer to disk and exit, blocking if other buffers are dirty",
        ),
        (
            vec!["wq!", "write-quit!"],
            "save the current buffer to disk and exit, discarding other changes",
        ),
        (
            vec!["w", "write"],
            "save the current buffer to disk. (Blocked if the file has been modified on disk)",
        ),
        (
            vec!["w!", "write!"],
            "save the current buffer to disk ignoring external changes",
        ),
        (vec!["lsp-find-references"], "show references to the symbol under the current cursor in the mini-buffer (requires an attached LSP server)"),
        (vec!["lsp-goto-declaration"], "jump to the declaration of the symbol under the current cursor (requires an attached LSP server)"),
        (vec!["lsp-goto-definition"], "jump to the definition of the symbol under the current cursor (requires an attached LSP server)"),
        (vec!["lsp-goto-type-definition"], "jump to the definition of the type of symbol under the current cursor (requires an attached LSP server)"),
        (vec!["lsp-hover"], "display hover text for the symbol under the current cursor in a new window (requires an attached LSP server)"),
        (vec!["lsp-show-capabilities"], "display the JSON capabilities of the LSP server for the current buffer (requires an attached LSP server)"),
        (vec!["lsp-show-diagnostics"], "display all LSP diagnostics in the mini-buffer"),
        (vec!["lsp-start"], "attempt to start an LSP server associated with the current filetype"),
        (vec!["lsp-stop"], "stop the LSP server associated with the current filetype"),
    ]
}
