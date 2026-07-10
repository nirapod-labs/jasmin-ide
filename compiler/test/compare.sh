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

