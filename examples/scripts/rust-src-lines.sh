#!/usr/bin/env sh
# A best effort line count of Rust source code using ad's structural
# regular expressions.

srcDir="$1"
cd $srcDir

# The 'y' and 'x' prefixes used below set the initial loop construct to either be
# "blocks not matching..." or "blocks matching..." respectively
loop=':#\[cfg\(test\)\]@*^}: X'

files="$(fd -t f)"
sourceLines=$(ad -e "y$loop g:\\S: v:^\\s*//: P" $files | wc -l)
sourceComment=$(ad -e "y$loop g:^\\s*//: P" $files | wc -l)
testLines=$(ad -e "x$loop g:\\S: v:^\\s*//: P" $files | wc -l)
testComment=$(ad -e "x$loop g:^\\s*//: P" $files | wc -l)

echo "Rust Source lines in '$srcDir'"
echo -e "  FILES       $(echo $files | wc -w)"
echo -e "  SOURCE      code: $sourceLines\tcomment: $sourceComment"
echo -e "  TESTS       code: $testLines\tcomment: $testComment"
