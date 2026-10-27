#!/bin/bash
# Check the Zed query files against the grammar they are written for.
#
# usage: test/queries.sh [grammar-directory]
#
# Zed disables a query file it cannot compile, and says nothing: one node
# name the grammar does not have and highlighting, brackets, the outline,
# indentation or text objects stop working for that whole file. This job
# runs every query file through the tree-sitter CLI against a sample, so
# a wrong node name fails here instead of quietly doing nothing.
#
# The grammar is the first argument, else $JASMIN_GRAMMAR_DIR, else a
# sibling checkout of the grammar the extension pins. TREE_SITTER names the
# CLI and defaults to `tree-sitter` on PATH. The job only reads the grammar.
#
# Zed loads the grammar from the commit `extension.toml` pins, so that commit
# is the grammar the query files have to compile against, and a checkout at
# the pin passes. A checkout with grammar fixes not yet upstream is ahead of
# the pin, so a work tree at another commit is a warning, not a failure: the run proves the queries against the checkout it
# was given, and both revisions are printed so the line cannot be mistaken for
# a run at the pin. A directory that cannot be asked its revision (an unpacked
# archive, say) warns as well.
#
# Two statements the pinned grammar lacks, `#spill(x);` (`intrinsic_instr`)
# and `assert("message", condition);` (`assert_statement`), cannot be named in
# a file Zed reads: Zed drops the whole file, and every highlight with it. Their
# patterns are kept in `highlights.local.scm`, which Zed does not read, and
# this job runs that file against `fixtures/sample-local.jazz` only when the
# grammar's `src/node-types.json` has both nodes. Against the pin it says the
# file is skipped.
#
# For each file the job prints the capture names it produced, and it checks
# every capture name written in a query against the names Zed gives a style
# to, so `@variable.builtin` fails here rather than doing nothing. A file that
# compiles and matches something can still hold a pattern the sample never
# exercises, so a short list of constructs each named file must capture is
# checked too.

set -u

# Error text from the CLI goes into logs; colours do not belong there.
export NO_COLOR=1

here=$(cd "$(dirname "$0")" && pwd)
extension=$(dirname "$here")
query_dir="$extension/languages/jasmin"
fixture="$here/fixtures/sample.jazz"
local_fixture="$here/fixtures/sample-local.jazz"
files=(highlights brackets outline indents textobjects)
# The patterns for the two nodes the pin lacks, in a file Zed does not read,
# and the nodes it names.
local_file=highlights.local.scm
local_name=local
local_nodes="assert_statement intrinsic_instr"

# The capture names Zed documents, across the five query files, plus the one
# it does not: `module`. A namespace name and a `from` module carry the pair
# `@type @module`, so a theme that knows `module` uses it and every other
# theme still colours the name as a type. It is the one deliberate extra, not
# a documented name; the list below is the allowlist the job checks against.
named="
attribute boolean comment comment.doc constant constant.builtin constructor
embedded emphasis emphasis.strong enum function hint keyword label link_text
link_uri module number operator predictive preproc primary property punctuation
punctuation.bracket punctuation.delimiter punctuation.list_marker
punctuation.special string string.escape string.regex string.special
string.special.symbol tag tag.doctype text.literal title type type.builtin
variable variable.special variable.parameter variant
open close item name context context.extra annotation
indent start end outdent
function.around function.inside class.around class.inside comment.around
comment.inside
"

ts=${TREE_SITTER:-tree-sitter}
if ! command -v "$ts" >/dev/null 2>&1; then
  printf 'no tree-sitter CLI: put it on PATH or set TREE_SITTER\n' >&2
  exit 2
fi

grammar=${1:-${JASMIN_GRAMMAR_DIR:-}}
if [ -z "$grammar" ]; then
  for candidate in \
    "$extension/../../grammar/tree-sitter-jasmin" \
    "$extension/../../../grammar/tree-sitter-jasmin"
  do
    if [ -f "$candidate/src/parser.c" ]; then
      grammar=$candidate
      break
    fi
  done
fi
if [ -z "$grammar" ] || [ ! -f "$grammar/src/parser.c" ]; then
  printf 'usage: %s <grammar-directory>   (or set JASMIN_GRAMMAR_DIR)\n' "$0" >&2
  printf 'the grammar is the tree-sitter checkout the extension pins\n' >&2
  exit 2
fi
grammar=$(cd "$grammar" && pwd)

# The commit the extension pins, read from the manifest rather than written
# here twice: Zed compiles the grammar from this commit, and the queries are
# written for it.
pin=$(sed -n 's/^rev = "\([0-9a-f]\{40\}\)"$/\1/p' "$extension/extension.toml" | head -n 1)

# A grammar directory that is its own git work tree reports its HEAD, so the
# job can compare it with the pin. One that is not (an archive, or a directory
# inside another checkout) cannot be asked, and the job says so rather than
# passing silently.
revision=
worktree=no
if [ -e "$grammar/.git" ] && revision=$(git -C "$grammar" rev-parse HEAD 2>/dev/null); then
  worktree=yes
fi

if [ -z "$pin" ]; then
  printf 'warn extension.toml names no commit: the grammar is not checked against the pin\n' >&2
elif [ "$worktree" = yes ] && [ "$revision" = "$pin" ]; then
  : # the grammar Zed compiles: nothing to warn about
elif [ "$worktree" = yes ]; then
  printf 'warn %s is at %s, the extension pins %s\n' "$grammar" "$revision" "$pin" >&2
  printf '  the queries are checked against this checkout, not the grammar Zed would load\n' >&2
elif [ "$worktree" = no ]; then
  printf 'warn %s is not a git work tree: its revision cannot be compared with the pin %s\n' \
    "$grammar" "$pin" >&2
fi

# The parser's own list of node types answers whether the two newer nodes
# exist here without compiling a query to find out. A grammar without the list
# is asked nothing, and the local file is skipped as it is against the pin.
missing=
for node in $local_nodes; do
  grep -qF "\"$node\"" "$grammar/src/node-types.json" 2>/dev/null || missing="$missing $node"
done

tmp=$(mktemp -d) || exit 2
trap 'rm -rf "$tmp"' EXIT

printf '%s\n' $named | sort -u >"$tmp/named"

printf 'grammar %s\n' "$grammar"
[ "$worktree" = yes ] && printf 'commit  %s\n' "$revision"
[ -n "$pin" ] && printf 'pin     %s\n' "$pin"
if [ -z "$missing" ]; then
  printf 'local   %s checked too: this grammar has %s\n' "$local_file" "$local_nodes"
else
  printf 'local   %s skipped: this grammar has no%s\n' "$local_file" "$missing"
fi
printf 'sample  %s\n\n' "$fixture"

# Every call names the grammar directory and so rebuilds the parser from it.
# The CLI would otherwise keep a compiled parser in its cache under the
# language's name and hand the next run that one, so a run against a second
# checkout of the same grammar could pass on the first checkout's parser.
samples=("$fixture")
[ -z "$missing" ] && samples+=("$local_fixture")
for sample in "${samples[@]}"; do
  if ! "$ts" parse -q -p "$grammar" "$sample" >"$tmp/parse.log" 2>&1; then
    printf 'fail sample %s: it does not parse without errors\n' "${sample#"$here"/}" >&2
    sed 's/^/  /' "$tmp/parse.log" >&2
    exit 1
  fi
done

failures=0
checked=0
attempted=0

check() {
  local stem=$1 file=$2 sample=$3 label
  label=$(basename "$file")
  attempted=$((attempted + 1))

  if [ ! -f "$file" ]; then
    printf 'fail %-20s missing\n' "$label"
    failures=$((failures + 1))
    return
  fi

  local unknown
  unknown=$(grep -ohE '@[a-zA-Z_][a-zA-Z0-9_.]*' "$file" | sed 's/^@//' | sort -u | comm -23 - "$tmp/named")
  if [ -n "$unknown" ]; then
    printf 'fail %-20s names a capture Zed does not have: %s\n' "$label" "$(printf '%s' "$unknown" | tr '\n' ' ')"
    failures=$((failures + 1))
    return
  fi

  local out
  if ! out=$("$ts" query -p "$grammar" "$file" "$sample" 2>&1); then
    printf 'fail %-20s does not compile against the grammar\n' "$label"
    printf '%s\n' "$out" | sed 's/^/  /'
    failures=$((failures + 1))
    return
  fi

  local produced count
  produced=$(printf '%s\n' "$out" | sed -nE 's/.*capture: ([0-9]+ - )?([^,]*),.*/\2/p' | sort -u)
  count=$(printf '%s\n' "$produced" | grep -c '[^[:space:]]')
  if [ "$count" -eq 0 ]; then
    printf 'fail %-20s matched nothing in the sample\n' "$label"
    failures=$((failures + 1))
    return
  fi

  printf '%s\n' "$out" >"$tmp/$stem.out"
  printf 'ok   %-20s %2d captures: %s\n' \
    "$label" "$count" "$(printf '%s\n' "$produced" | tr '\n' ' ' | sed 's/ $//')"
  checked=$((checked + 1))
}

for name in "${files[@]}"; do
  check "$name" "$query_dir/$name.scm" "$fixture"
done
# The patterns the pin cannot take, against the sample only a newer grammar parses.
if [ -z "$missing" ]; then
  check "$local_name" "$query_dir/$local_file" "$local_fixture"
fi

# A file that matches something is not the same as every pattern in it being
# exercised: a pattern for a node the sample never uses compiles and then
# matches nothing, so nobody notices it is wrong. Each entry below is
# `<query file>:<text the sample must capture through it>`, covering the six
# things the outline lists and the two statements the pin lacks; the second pair has no output file against the pin, and is skipped.
required=(
  "outline:lane"
  "outline:CRYPTO"
  "outline:ROUNDS"
  "outline:mask"
  "outline:keep"
  "outline:mix"
  "$local_name:#spill"
  "$local_name:assert"
)
for entry in "${required[@]}"; do
  name=${entry%%:*}
  text=${entry#*:}
  [ -f "$tmp/$name.out" ] || continue
  # The capture's own text, not any larger capture that happens to contain
  # it: an outline item's text is its whole declaration, so a name shows up
  # in it only through the pattern that captured the name.
  needle='text: `'"$text"'`'
  if ! grep -qF -- "$needle" "$tmp/$name.out"; then
    label=$name.scm
    [ "$name" = "$local_name" ] && label=$local_file
    printf 'fail %-20s matched no %s in the sample\n' "$label" "$text"
    failures=$((failures + 1))
  fi
done

if [ "$failures" -ne 0 ]; then
  printf '\n%d failure(s) across %d query files\n' "$failures" "$attempted" >&2
  exit 1
fi
printf '\nall %d query files compile, match the sample and capture what it must\n' "$checked"
