#!/usr/bin/env bash
# A best effort line count of Rust source code using ad's structural
# regular expressions.

srcDir="$1"
cd $srcDir

files="$(fd -t f -e rs)"
sourceLines=$(ad -e 'y:#\[cfg\(test\)\]@*^}: x:.*\n: g:\S: v:^\s*//: P' $files | wc -l)
sourceComment=$(ad -e 'y:#\[cfg\(test\)\]@*^}: x:.*\n: g:^\s*//: P' $files | wc -l)
testLines=$(ad -e 'x:#\[cfg\(test\)\]@*^}: x:.*\n: g:\S: v:^\s*//: P' $files | wc -l)
testComment=$(ad -e 'x:#\[cfg\(test\)\]@*^}: x:.*\n: g:^\s*//: P' $files | wc -l)

echo "Rust Source lines in '$srcDir'"
echo -e "  FILES       $(echo $files | wc -w)"
echo -e "  SOURCE      code: $sourceLines\tcomment: $sourceComment"
echo -e "  TESTS       code: $testLines\tcomment: $testComment"
