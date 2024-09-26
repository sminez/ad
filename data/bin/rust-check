#!/usr/bin/env bash
# A simple ad helper script to lint Rust files on change and support checking
# details of errors that are reported.

. "$HOME/.ad/lib/ad.sh"

updateCargoOutput() {
  clearBuffer "$1"
  output="$(cargo check --message-format=short 2>&1)"
  bufWrite "$1" body "$output\n\n>>>"
  if [[ "$output" =~ error|warning ]]; then
    focusBuffer "$1"
    adEdit 'x/(.+):(\d+):(\d+):/ c/$1:$2:$3/'
  fi
  curToBof "$1"
  markClean "$1"
}

handleErrorLoads() {
  bufRead "$1" event | while read -r line; do
    action="$(echo "$line" | cut -d' ' -f2)"
    target="$(echo "$line" | cut -d' ' -f7)"

    case "$action" in
      "L")
        if [[ "$target" = "Clear" ]]; then
          adEdit "x/\n>>>@*/ c/\n>>>/"
        elif [[ "$target" =~ E.... ]]; then
          output="$(cargo --explain "$target")"
          curToBof "$1"
          adEdit "x/\n>>>@*/ d"
          bufWrite "$1" body "\n>>> Explanation for error[$target] (Clear)\n$output"
          markClean "$1"
        else
          bufWrite "$1" event "$line"
        fi
      ;;
      "X") bufWrite "$1" event "$line" ;;
    esac
  done
}

CURRENT_ID="$(currentBufferId)"
adCtl "open +cargo"
ID="$(adIndex | grep "+cargo" | cut -f1)"
focusBuffer "$CURRENT_ID"
updateCargoOutput "$ID"
handleErrorLoads "$ID" &

adLog | while read -r line; do
  action="$(echo "$line" | cut -d' ' -f2)"
  if [ "$action" = "save" ]; then
    updateCargoOutput "$ID"
  fi
done
