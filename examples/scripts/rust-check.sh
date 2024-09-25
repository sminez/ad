#!/usr/bin/env bash
# A simple ad helper script to lint Rust files on change and support checking
# details of errors that are reported.

clearBuffer() {
  echo -n "," | 9p write "ad/buffers/$1/xaddr"
  echo -n "" | 9p write "ad/buffers/$1/xdot"
}

updateCargoOutput() {
  clearBuffer "$1"
  output="$(cargo check --message-format=short 2>&1)"
  echo -e "$output\n>>>\n\n" | 9p write "ad/buffers/$1/body"
  if [[ "$output" =~ error|warning ]]; then
    echo -n "buffer $1" | 9p write ad/ctl
    echo -n 'Edit x/(.+):(\d+):(\d+):/ c/$1:$2:$3/' | 9p write ad/ctl
  fi
  echo -n "0" | 9p write "ad/buffers/$1/addr"
  echo -n "mark-clean $1" | 9p write ad/ctl
}

handleErrorLoads() {
  9p read "ad/buffers/$1/event" | while read -r line; do
    action="$(echo "$line" | cut -d' ' -f2)"
    target="$(echo "$line" | cut -d' ' -f7)"

    case "$action" in
      "L")
        if [[ "$target" =~ E.... ]]; then
          # Clear any previous error explanation output
          echo -n "0" | 9p write "ad/buffers/$1/addr"
          echo -n "Edit x/\n>>>@*/ d" | 9p write ad/ctl
          # Write the new output
          echo -e "\n>>> Explanation for error[$target]\n" | 9p write "ad/buffers/$1/body"
          cargo --explain "$target" | 9p write "ad/buffers/$1/body"
        else
          echo -n "$line" | 9p write "ad/buffers/$1/event"
        fi
      ;;
      "X") echo -n "$line" | 9p write "ad/buffers/$1/event" ;;
    esac
  done
}

# Open the +cargo output buffer (would really need to track multiple?)
CURRENT_ID="$(9p read ad/buffers/current | cut -f1)"
echo -n "open +cargo" | 9p write ad/ctl
ID="$(9p read ad/buffers/index | grep "+cargo" | cut -f1)"
updateCargoOutput "$ID"
handleErrorLoads "$ID" &
echo -n "buffer $CURRENT_ID" | 9p write ad/ctl

9p read ad/log | while read -r line; do
  action="$(echo "$line" | cut -d' ' -f2)"
  if [ "$action" = "save" ]; then
    updateCargoOutput "$ID"
  fi
done
