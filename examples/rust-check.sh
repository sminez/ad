#!/usr/bin/env bash
# A simple ad helper script to lint Rust files on change
# TODO:
#   - write an event listener that handles loads of 'error[errnum]' by showing
#     the help output from cargo --explain errnum

clearBuffer() {
    echo -n "," | 9p write "ad/buffers/$1/xaddr"
    echo -n "" | 9p write "ad/buffers/$1/xdot"
}

updateCargoOutput() {
    clearBuffer "$1"
    output="$(cargo check --message-format=short 2>&1)"
    echo -n "$output" | 9p write "ad/buffers/$1/body"
    echo -n "mark-clean $1" | 9p write ad/ctl
    if [[ "$output" =~ "error" ]]; then
        echo -n "buffer $1" | 9p write ad/ctl
        echo -n 'Edit x/.*/ s/(.+):(\d+):\d+:/$1:$2/' | 9p write ad/ctl
    fi
}

# Open the +cargo output buffer (would really need to track multiple?)
CURRENT_ID="$(9p read ad/buffers/current | cut -f1)"
echo -n "open +cargo" | 9p write ad/ctl
echo -n "buffer $CURRENT_ID" | 9p write ad/ctl
ID="$(9p read ad/buffers/index | grep "+cargo" | cut -f1)"

9p read ad/log | while read -r line; do
    action="$(echo "$line" | cut -d' ' -f2)"
    if [ "$action" = "save" ]; then
        updateCargoOutput "$ID"
    fi
done
