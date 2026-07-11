#!/bin/sh
# Compare the batch checker with the compiler's own recorded verdicts.
#
#   compiler/test/compare.sh <corpus> [<jasmin-lsp>]
#
# <corpus> is the `compiler/tests` directory of the Jasmin release tagged
# v2026.09.0, or the release root that contains it. The corpus is not part of
# this repository, so the script says what it needs and skips when the
# directory is absent. <jasmin-lsp> is the checker; it defaults to the
# executable this workspace builds,
# `_build/default/lsp-server/native/main.exe`.
#
# Ten positive files must come out `ok` with exit status 0. Ten negative files
# must fail with exit status 1 and give, as their first diagnostic, the file,
# line, column and message that `negative.expected` records for the part of
# the compiler up to the end of preprocessing. The recorded column is
# zero-based and the checker prints one-based columns, so the script adds one
# on the expected side. The message is compared with its line breaks written
# as `\n`, which is the single-line form the checker prints.
#
# One line is printed per file and one count per group. Exit status: 0 when
# all twenty files match, 1 when any does not, 2 when the corpus or the
# checker cannot be used, and 0 again when the corpus is absent, which is a
# skip rather than a failure.

set -u

if [ $# -lt 1 ]; then
  printf '%s\n' \
    "compare.sh: no corpus given, nothing to compare; skipping." \
    "usage: compiler/test/compare.sh <corpus> [<jasmin-lsp>]" \
    "<corpus> is the compiler/tests directory of the Jasmin release v2026.09.0."
  exit
fi

corpus=$1
# A caller may name the release root instead of the tests directory, which is
# one level down.
if [ -d "$corpus/compiler/tests" ]; then
  corpus=$corpus/compiler/tests
fi
if [ ! -d "$corpus" ]; then
  printf '%s\n' \
    "compare.sh: no corpus at $1; skipping." \
    "The Jasmin test corpus is not part of this repository. Pass the" \
    "compiler/tests directory of the Jasmin release v2026.09.0 to run it."
  exit
fi
if [ ! -f "$corpus/negative.expected" ]; then
  printf '%s\n' "compare.sh: $corpus has no negative.expected" >&2
  exit 2
fi

here=$(unset CDPATH; cd -- "$(dirname -- "$0")" && pwd)
root=$(unset CDPATH; cd -- "$here/../.." && pwd)
binary=${2:-$root/_build/default/lsp-server/native/main.exe}

if [ ! -x "$binary" ]; then
  printf '%s\n' "compare.sh: no checker at $binary; build it first" >&2
  exit 2
fi

# Positive files: the compiler accepts them, so the checker must too. The list
# covers a multi-line annotation, globals of every width, a parameter used as
# an array bound, a stack array with a for loop, several variables in one
# declaration, two namespaces with a shared include, two require lines (one
# with ..), and calls between functions.
positives='
success/common/annotations.jazz
success/common/array_sizes.jazz
success/common/global.jazz
success/common/param.jazz
success/x86-64/array_access.jazz
success/x86-64/bug_895.jazz
success/namespaces/common/basic.jazz
success/namespaces/common/map.jazz
success/require/common/file2.jazz
success/subroutines/x86-64/exec.jazz
'

# Negative files with the architecture each one is recorded for. Every entry's
# first block in negative.expected is an error the compiler raises before the
# end of preprocessing, so the checker has to produce it: an annotation
# attribute, a return-storage mismatch, three type-alias errors (duplicate, not
# found, used as an annotation parameter), an undeclared variable, a duplicate
# namespace entry, a global that is not a word, an illegal string escape, and,
# for the second architecture, an assignment with the wrong number of lvalues.
negatives='
fail/annotation/x86-64/returnaddress.jazz x86-64
fail/common/storage_signature_mismatch.jazz x86-64
fail/common/typealias_duplicate.jazz x86-64
fail/common/typealias_annotation_param.jazz x86-64
fail/common/typealias_notfound.jazz x86-64
fail/common/var_initialize_undecl.jazz x86-64
fail/namespaces/common/duplicate.jazz x86-64
fail/typing/x86-64/global_int.jazz x86-64
fail/x86-64/string.jazz x86-64
fail/arm-m4/tst_imm_shifted.jazz arm-m4
'

cd "$corpus" || exit 2

# The first block `negative.expected` records for one file, as three lines: the
# line, the one-based column and the message with its line breaks written as
# \n. Prints nothing when the file has no block.
expected() {
  awk -v target="$1" '
    function escape(s,   out, i, c) {
      out = ""
      for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1)
        if (c == "\\") out = out "\\\\"
        else if (c == "\t") out = out "\\t"
        else if (c == "\r") out = out "\\r"
        else out = out c
      }
      return out
    }
    BEGIN { want = target ":"; state = 0; message = ""; more = 0 }
    state == 0 { if ($0 == want) state = 1; next }
    state == 1 { if ($0 ~ /^[ \t]*$/) next; state = 2 }
    state == 2 {
      text = $0
      cut = index(text, "):")
      if (cut == 0) { exit }
      head = substr(text, 1, cut + 1)
      rest = substr(text, cut + 2)
      sub(/^[ \t]+/, "", rest)
      if (!match(head, /line [0-9]+/)) { exit }
      number = substr(head, RSTART + 5, RLENGTH - 5)
      if (!match(head, /\([0-9]+/)) { exit }
      column = substr(head, RSTART + 1, RLENGTH - 1) + 1
      if (rest != "") { message = escape(rest) }
      else { more = 1 }
      state = 3
      next
    }
    state == 3 {
      if ($0 ~ /^[ \t]*$/) { state = 4; exit }
      if (more) {
        message = message (message == "" ? "" : "\\n") escape($0)
      }
      next
    }
    state == 4 { exit }
    END {
      if (state < 3) exit
      printf "%s\n%d\n%s\n", number, column, message
    }
  ' negative.expected
}

ok_positive=0
ok_negative=0
failed=0

pass() {
  printf 'ok   %s\n' "$1"
}

fail() {
  failed=$((failed + 1))
  printf 'FAIL %s: %s\n' "$1" "$2"
}

while read -r path; do
  [ -n "$path" ] || continue
  if [ ! -f "$path" ]; then
    fail "$path" "the corpus has no such file"
    continue
  fi
  output=$("$binary" check "$path" 2>&1)
  status=$?
  if [ "$status" -ne 0 ]; then
    fail "$path" "verdict $status, the compiler accepts it (expected 0)"
    continue
  fi
  last=$(printf '%s\n' "$output" | tail -n 1)
  if [ "$last" != "$path: ok" ]; then
    fail "$path" "last line \"$last\", expected \"$path: ok\""
    continue
  fi
  ok_positive=$((ok_positive + 1))
  pass "$path"
done <<EOF
$positives
EOF

while read -r path arch; do
  [ -n "$path" ] || continue
  if [ ! -f "$path" ]; then
    fail "$path" "the corpus has no such file"
    continue
  fi
  want=$(expected "$path")
  if [ -z "$want" ]; then
    fail "$path" "negative.expected records no block for it"
    continue
  fi
  exp_line=$(printf '%s\n' "$want" | sed -n 1p)
  exp_column=$(printf '%s\n' "$want" | sed -n 2p)
  exp_message=$(printf '%s\n' "$want" | sed -n '3,$p')

  output=$("$binary" check --arch "$arch" "$path" 2>&1)
  status=$?
  if [ "$status" -ne 1 ]; then
    fail "$path" "verdict $status, the compiler rejects it (expected 1)"
    continue
  fi
  first=$(printf '%s\n' "$output" | sed -n 1p)
  case $first in
    "$path":*) rest=${first#"$path":} ;;
    *)
      fail "$path" "first line does not start with the file name: $first"
      continue
      ;;
  esac
  got_line=${rest%%:*}
  rest=${rest#*:}
  got_column=${rest%%:*}
  rest=${rest#*: }
  got_kind=${rest%%:*}
  got_message=${rest#*: }

  problem=''
  if [ "$got_line" != "$exp_line" ]; then
    problem="line $got_line, expected $exp_line ($got_message)"
  elif [ "$got_column" != "$exp_column" ]; then
    problem="column $got_column, expected $exp_column ($got_message)"
  elif [ "$got_message" != "$exp_message" ]; then
    problem="message \"$got_message\", expected \"$exp_message\""
  fi
  if [ -n "$problem" ]; then
    fail "$path" "$problem"
  else
    ok_negative=$((ok_negative + 1))
    pass "$path ($got_kind at $exp_line:$exp_column)"
  fi
done <<EOF
$negatives
EOF

total=20
matched=$((ok_positive + ok_negative))
printf '\n'
printf 'positive: %d of 10 matched\n' "$ok_positive"
printf 'negative: %d of 10 matched\n' "$ok_negative"
printf 'total:    %d of %d matched, %d did not\n' "$matched" "$total" "$failed"
[ "$failed" -eq 0 ] || exit 1
