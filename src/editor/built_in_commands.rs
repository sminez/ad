// NOTE: this file can not include any imports as it is included in build.rs in order
//       to auto-generate the help page. The function below is used in a test case in
//       commands.rs in order to ensure that all of the commands included here parse
//       correctly.

#[allow(dead_code)]
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
            vec!["echo"],
            "display the given string in the status line ('echo hello, world!')",
        ),
        (
            vec!["E", "Edit"],
            "run an Edit command (See 'Running Edit Commands')",
        ),
        (
            vec!["expand-dot"],
            "smart expand the current cursor position into a range",
        ),
        (vec!["help"], "display this help file"),
        (
            vec!["mark-clean"],
            "mark the current buffer as being clean to prevent saving changes",
        ),
        (
            vec!["o", "open"],
            "open the given file path in a new buffer ('open README.md')",
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
            "reload the editor config file located at ~/.ad/init.conf",
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
    ]
}
