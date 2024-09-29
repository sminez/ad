#!/usr/bin/env bash

. "$HOME/.ad/lib/ad.sh"

selected="$(
  echo -en "this is an example\nof how the minibuffer works\nyou can select from these lines" |
    minibufferSelect "demo> "
)"

adCtl "echo selection was '$selected'"
