#!/usr/bin/env bash
# A simple ad helper script to format and format and lint rust files on change


clearBuffer() {
    echo -n "," | 9p write "ad/buffers/$1/xaddr"
    echo -n "" | 9p write "ad/buffers/$1/xdot"
}

updateCargoOutput() {
    clearBuffer "$1"
    cargo check --message-format=short 2>&1 | 9p write "ad/buffers/$1/body"
}

# Open the +cargo output buffer (would really need to track multiple?)
echo "open +cargo" | 9p write ad/ctl
ID="$(9p read ad/buffers/index | grep "+cargo" | cut -f1)"
