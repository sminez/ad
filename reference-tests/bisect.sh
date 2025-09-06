#!/usr/bin/env sh
# Compare our output to the reference JS implementation to locate where output diverges

set -eu

TEST_DATA="$1"

diff_one() {
  node check.js "$1" "$2"
  # ./target/release/reference-tests "$1" "$2"
  ./target/debug/reference-tests "$1" "$2"

  echo ">> Last transaction run: $(gunzip -c "$1" | jq ".txns[$(( $2 - 1 ))]")"
  echo ">> Diff:"
  diff --color=always -y output/js_output.txt output/rust_output.txt
}


echo "Building Rust binary..."
# cargo build --release
cargo build

echo "Clearing previous output directory..."
rm -rf output
mkdir output

N_TRANSACTIONS="$(gunzip -c "$TEST_DATA" | jq '.txns | length')"
echo "Test file contains $N_TRANSACTIONS transactions"

# If a number of transactions was given then run that number only and diff
N="${2:-""}"
if [ -n "$N" ]; then
  echo ">> Running the first $N transactions"
  diff_one "$TEST_DATA" "$N"
  exit 0
fi


echo ">> Running all transactions from $TEST_DATA..."
echo "Running reference JS implementation (this can take a while)..."
node check.js "$TEST_DATA" "$N_TRANSACTIONS"
echo "Running Rust implementation..."
# ./target/release/reference-tests "$TEST_DATA" "$N_TRANSACTIONS"
./target/debug/reference-tests "$TEST_DATA" "$N_TRANSACTIONS"

if diff --color=always -y output/js_output.txt output/rust_output.txt > /dev/null; then
  echo "Output matches"
  exit 0
fi

# Otherwise...
echo ">> OUTPUT DOES NOT MATCH"
echo "   Bisecting to locate where we diverge..."

# lo is always matching, hi is always non-matching
lo=0
hi="$N_TRANSACTIONS"

while [ $(( hi - lo )) -ne 1 ]; do
  mid=$(( lo + (hi - lo) / 2 ))

  echo ">> lo=$lo mid=$mid hi=$hi"
  echo "   Checking first $mid transactions of $TEST_DATA..."

  echo "   Running reference JS implementation..."
  node check.js "$TEST_DATA" "$mid"
  echo "   Running Rust implementation..."
  # ./target/release/reference-tests "$TEST_DATA" "$mid"
  ./target/debug/reference-tests "$TEST_DATA" "$mid"

  if diff --color=always -y output/js_output.txt output/rust_output.txt > /dev/null; then
    echo "   First $mid steps match"
    lo="$mid"
  else
    echo "   Outputs differ after $mid steps"
    hi="$mid"
  fi
  echo ""
done

echo ""
echo ">> Fist failing step is $hi"
# We're not guaranteed that the failing transaction was the last one we ran so re-run here
diff_one "$TEST_DATA" "$hi"
