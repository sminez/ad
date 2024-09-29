#!/usr/bin/env bash
# Helper functions for writing scripts to interact with ad

[ -e "$HOME/.profile" ] && source ~/.profile

adCtl() { echo -n "$*" | 9p write ad/ctl; }
adEdit() { adCtl "Edit $*"; }
adIndex() { 9p read ad/buffers/index; }

adError() {
  adCtl "echo $*"
  exit 1
}

requireAd() {
  [[ -z "$bufid" ]] && adError "need to be run from inside of ad"
}

bufRead() { 9p read "ad/buffers/$1/$2"; }
bufWrite() {
  _path="ad/buffers/$1/$2"
  shift
  shift
  echo -en "$*" | 9p write "$_path"
}
bufWriteNoEscapes() {
  _path="ad/buffers/$1/$2"
  shift
  shift
  echo -En "$*" | 9p write "$_path"
}

adLog() { 9p read ad/log; }
currentBufferId() { 9p read ad/buffers/current | cut -f1; }

focusBuffer() { adCtl "buffer $1"; }
clearBuffer() {
  bufWrite "$1" xaddr ","
  bufWrite "$1" xdot ""
}
markClean() { adCtl "mark-clean $1"; }
curToBof() { bufWrite "$1" addr 0; }
curToEof() { bufWrite "$1" addr '$'; }
