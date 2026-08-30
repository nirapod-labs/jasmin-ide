#!/bin/bash
# Check the Zed query files against the grammar they are written for.
#
# usage: test/queries.sh [grammar-directory]
#
# Zed disables a query file it cannot compile, and says nothing: one node
# name the grammar does not have and highlighting, brackets, the outline,
# indentation or text objects stop working for that whole file. This job
# runs every query file through the tree-sitter CLI against one sample, so
# a wrong node name fails here instead of quietly doing nothing.
#
# The grammar is the first argument, else $JASMIN_GRAMMAR_DIR, else a
# sibling checkout of the grammar the extension pins. TREE_SITTER names the
# CLI and defaults to `tree-sitter` on PATH. The job only reads the grammar.
#
# Zed loads the grammar from the commit `extension.toml` pins, so a check
# against any other checkout says nothing about the grammar the queries will
# meet. A grammar directory that is its own git work tree is therefore
# compared with that commit and a different HEAD fails the job; a directory
# that cannot be asked (an unpacked archive, say) gets a warning instead.
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
files=(highlights brackets outline indents textobjects)

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
# job can hold it to the pin. One that is not (an archive, or a directory
# inside another checkout) cannot be asked, and the job says so rather than
# passing silently.
revision=
worktree=no
if [ -e "$grammar/.git" ] && revision=$(git -C "$grammar" rev-parse HEAD 2>/dev/null); then
  worktree=yes
fi

if [ -z "$pin" ]; then
  printf 'warn extension.toml names no commit: the grammar is not checked against the pin\n' >&2
elif [ "$worktree" = yes ] && [ "$revision" != "$pin" ]; then
  printf 'fail grammar %s is at %s, the extension pins %s\n' "$grammar" "$revision" "$pin" >&2
  printf '  check the grammar out at the pinned commit, or move the pin, before trusting these queries\n' >&2
  exit 1
elif [ "$worktree" = no ]; then
  printf 'warn %s is not a git work tree: its revision cannot be compared with the pin %s\n' \
    "$grammar" "$pin" >&2
fi

tmp=$(mktemp -d) || exit 2
trap 'rm -rf "$tmp"' EXIT

printf '%s\n' $named | sort -u >"$tmp/named"

printf 'grammar %s\n' "$grammar"
[ "$worktree" = yes ] && printf 'commit  %s\n' "$revision"
[ -n "$pin" ] && printf 'pin     %s\n' "$pin"
printf 'sample  %s\n\n' "$fixture"

# Every call names the grammar directory and so rebuilds the parser from it.
# The CLI would otherwise keep a compiled parser in its cache under the
# language's name and hand the next run that one, so a run against a second
# checkout of the same grammar could pass on the first checkout's parser.
if ! "$ts" parse -q -p "$grammar" "$fixture" >"$tmp/parse.log" 2>&1; then
  printf 'fail sample: it does not parse without errors\n' >&2
  sed 's/^/  /' "$tmp/parse.log" >&2
  exit 1
fi

failures=0
for name in "${files[@]}"; do
  file="$query_dir/$name.scm"
  if [ ! -f "$file" ]; then
    printf 'fail %-15s missing\n' "$name.scm"
    failures=$((failures + 1))
    continue
  fi

  unknown=$(grep -ohE '@[a-zA-Z_][a-zA-Z0-9_.]*' "$file" | sed 's/^@//' | sort -u | comm -23 - "$tmp/named")
  if [ -n "$unknown" ]; then
    printf 'fail %-15s names a capture Zed does not have: %s\n' "$name.scm" "$(printf '%s' "$unknown" | tr '\n' ' ')"
    failures=$((failures + 1))
    continue
  fi

  if ! out=$("$ts" query -p "$grammar" "$file" "$fixture" 2>&1); then
    printf 'fail %-15s does not compile against the grammar\n' "$name.scm"
    printf '%s\n' "$out" | sed 's/^/  /'
    failures=$((failures + 1))
    continue
  fi

  produced=$(printf '%s\n' "$out" | sed -nE 's/.*capture: ([0-9]+ - )?([^,]*),.*/\2/p' | sort -u)
  count=$(printf '%s\n' "$produced" | grep -c '[^[:space:]]')
  if [ "$count" -eq 0 ]; then
    printf 'fail %-15s matched nothing in the sample\n' "$name.scm"
    failures=$((failures + 1))
    continue
  fi

  printf '%s\n' "$out" >"$tmp/$name.out"
  printf 'ok   %-15s %2d captures: %s\n' \
    "$name.scm" "$count" "$(printf '%s\n' "$produced" | tr '\n' ' ' | sed 's/ $//')"
done

# A file that matches something is not the same as every pattern in it being
# exercised: a pattern for a node the sample never uses compiles and then
# matches nothing, so nobody notices it is wrong. Each entry below is
# `<query file>:<text the sample must capture through it>`, covering the six
# things the outline lists and the two statements the grammar gained last.
required=(
  "outline:lane"
  "outline:CRYPTO"
  "outline:ROUNDS"
  "outline:mask"
  "outline:keep"
  "outline:mix"
  "highlights:#spill"
  "highlights:assert"
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
    printf 'fail %-15s matched no %s in the sample\n' "$name.scm" "$text"
    failures=$((failures + 1))
  fi
done

if [ "$failures" -ne 0 ]; then
  printf '\n%d failure(s) across %d query files\n' "$failures" "${#files[@]}" >&2
  exit 1
fi
printf '\nall %d query files compile, match the sample and capture what it must\n' "${#files[@]}"
