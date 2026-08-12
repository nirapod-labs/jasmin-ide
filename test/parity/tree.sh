#!/bin/sh
# The tree half of the parser parity gate: the tree our driver returns for a
# file against the tree the compiler's own parser returns for the same bytes.
#
#   sh test/parity/tree.sh <corpus root>
#
# <corpus root> is the `compiler` directory of a Jasmin release (the one that
# holds `tests/` and `examples/`) or its `tests` directory. Every `.jazz` file
# under `tests/success` and, when the root has one, `examples` is parsed once
# by `parity_tree`, which answers, on stderr:
#
#   driver: <n> diagnostics   what the recovering driver reported for the file;
#   same-tree: yes|no|n/a     whether the driver's item list is the compiler
#                             parser's own, field for field, locations and
#                             annotations included;
#   same-recovery: yes|no|n/a the same question for the tree the recovery
#                             engine returns when it is asked directly.
#
# `n/a` means the compiler's parser rejects the file, so there is no compiler
# tree to compare with. The file is then outside the positive corpus the gate
# names, and the run counts it apart instead of passing it.
#
# A file passes only when the driver reports no diagnostic and both trees are
# the compiler parser's own.
#
# Normalisation: none. The comparison is the item lists themselves, compared
# with `Marshal` and `No_sharing`, which writes every field of every node:
# source locations, annotations and item order all count, and nothing is
# stripped, rewritten or renumbered on either side. The release's own printer
# elides locations (every located node it prints is read through
# `Location.unloc`), so the printing below is a cross-check and not the
# comparison; it cannot hide a location difference because it does not take
# part in the structural answer.
#
# When JASMIN2TEX names the release's parse-and-print, its printing of the
# same bytes is compared with ours byte for byte. Both sides are run from the
# file's own directory with the file's own name, so a `require` beside the
# file resolves, and the two see the same bytes under the same name.
#
# Environment:
#   PARITY_TREE  the tree printer. Default: the `parity_tree` this tree
#                builds.
#   JASMIN2TEX   the release's parse-and-print (`jasmin2tex`). Optional:
#                without it the structural answers are still measured and the
#                run says the printing was not compared.
#   PARITY_JOBS  how many files are parsed at once, default 4.
#   PARITY_TSV   a file to write one tab-separated line per file to.
#
# Exit status: 0 when every file the compiler parses passes, 1 when one does
# not or when the corpus holds a file the compiler's parser rejects, 2 when
# the script cannot run (bad arguments, no corpus, no printer, a named
# parse-and-print that does not work).

set -u

name=tree
tab=$(printf '\t')

say() { printf '%s: %s\n' "$name" "$*"; }
warn() { printf '%s: %s\n' "$name" "$*" >&2; }
die() {
	warn "$*"
	exit 2
}

usage() {
	cat >&2 <<'USAGE'
usage: sh test/parity/tree.sh <corpus root>

  <corpus root>   a Jasmin release's `compiler` directory (holding `tests`
                  and, usually, `examples`), or its `tests` directory

environment: PARITY_TREE (the tree printer), JASMIN2TEX (the release's
parse-and-print; without it the printing is not compared), PARITY_JOBS
(default 4), PARITY_TSV (write the per-file lines here)
USAGE
	exit 2
}

if [ $# -ne 1 ]; then usage; fi
root=${1:-}
[ -n "$root" ] || usage
[ -d "$root" ] || die "no corpus directory: $root"
root=$(cd -- "$root" && pwd)

if [ -d "$root/tests/success" ]; then
	tests=$root/tests
	examples=$root/examples
elif [ -d "$root/success" ]; then
	tests=$root
	examples=
else
	die "no tests/success under $root: name the release's compiler directory"
fi

here=$(cd -- "$(dirname -- "$0")" && pwd)
printer=${PARITY_TREE:-$here/../../_build/default/test/parity/parity_tree.exe}
[ -x "$printer" ] || die "no tree printer at $printer (build the tree, or set PARITY_TREE)"

compiler=${JASMIN2TEX:-}
if [ -n "$compiler" ]; then
	case $compiler in
	*/*) [ -x "$compiler" ] || die "JASMIN2TEX is not executable: $compiler" ;;
	*) command -v "$compiler" >/dev/null 2>&1 || die "JASMIN2TEX is not on PATH: $compiler" ;;
	esac
fi

jobs=${PARITY_JOBS:-4}
case $jobs in
'' | *[!0-9]*) die "PARITY_JOBS is not a number: $jobs" ;;
esac
[ "$jobs" -ge 1 ] || die "PARITY_JOBS is below one: $jobs"

work=$(mktemp -d "${TMPDIR:-/tmp}/tree.XXXXXX") || die "cannot make a work directory"
trap 'rm -rf "$work"' EXIT
trap 'exit 2' HUP INT TERM

# arch_for <relative path>: the architecture the folder names, as the parity
# harness reads it. The tree does not depend on the target, but the release's
# parse-and-print runs pretyping before it prints, and pretyping does: a file
# under `arm-m4` is a typing error when it is read as x86-64.
arch_for() {
	old_ifs=$IFS
	IFS=/
	set -- $1
	IFS=$old_ifs
	deepest=
	for component in "$@"; do
		case $component in
		arm-m4) deepest=arm-m4 ;;
		riscv | risc-v) deepest=riscv ;;
		armv8a) deepest=armv8a ;;
		x86-64) deepest=x86-64 ;;
		esac
	done
	printf '%s\n' "${deepest:-x86-64}"
}

# The files, one path relative to the corpus root per line, in the corpus's
# own order: the release's positive folder first, then its examples.
list=$work/files.tsv
: >"$list"
find "$tests/success" -type f -name '*.jazz' -print | LC_ALL=C sort >>"$list"
if [ -n "$examples" ] && [ -d "$examples" ]; then
	find "$examples" -type f -name '*.jazz' -print | LC_ALL=C sort >>"$list"
fi
total=$(wc -l <"$list" | tr -d ' ')
[ "$total" -gt 0 ] || die "no files: the corpus holds no .jazz under its success folder"

# parse_one <index> <absolute path>
#
# Writes one result line to $work/result.<index>:
#
#   index relative diagnostics same-tree same-recovery printed status
#
# `printed` is `yes`, `no`, `unprintable` (the release's parse-and-print
# refused the file: it runs pretyping before it prints, so a file that fails
# typing has no print) or `-` when no parse-and-print was named.
#
# status is `pass`, `rejected` (the compiler's parser rejects a file the
# corpus calls positive), `error` (the driver reports a diagnostic on a file
# the compiler parses), `tree` (a tree differs) or `print` (the trees agree
# but the two printings do not, which points at the printer or the build).
parse_one() {
	index=$1
	path=$2
	relative=${path#"$root"/}
	directory=$(dirname "$path")
	base=$(basename "$path")
	out=$work/t.$index
	(cd "$directory" && "$printer" "$base") >"$out" 2>"$work/t.$index.err" </dev/null
	diagnostics=$(sed -n 's/^driver: \([0-9]*\) diagnostics$/\1/p' "$work/t.$index.err")
	same_tree=$(sed -n 's/^same-tree: //p' "$work/t.$index.err")
	same_recovery=$(sed -n 's/^same-recovery: //p' "$work/t.$index.err")
	difference=$(sed -n 's/^first-difference: //p' "$work/t.$index.err")
	[ -n "$diagnostics" ] || diagnostics=unknown
	[ -n "$same_tree" ] || same_tree=unknown
	[ -n "$same_recovery" ] || same_recovery=unknown
	printed=-
	if [ -n "$compiler" ]; then
		(cd "$directory" && "$compiler" --arch "$(arch_for "$relative")" "$base") \
			>"$work/c.$index" 2>"$work/c.$index.err" </dev/null
		if [ $? -ne 0 ]; then
			printed=unprintable
		elif cmp -s "$out" "$work/c.$index"; then
			printed=yes
		else
			printed=no
		fi
	fi
	status=tree
	case $same_tree/$same_recovery in
	n/a/n/a) status=rejected ;;
	yes/yes)
		if [ "$diagnostics" != 0 ]; then status=error
		elif [ "$printed" = no ]; then status=print
		else status=pass
		fi
		;;
	*) status=tree ;;
	esac
	{
		printf '%s\t%s\t%s\t%s\t%s\t%s\t%s' \
			"$index" "$relative" "$diagnostics" "$same_tree" "$same_recovery" \
			"$printed" "$status"
		if [ -n "$difference" ]; then printf '\t%s' "$difference"; fi
		printf '\n'
	} >"$work/result.$index"
}

index=0
running=0
while IFS= read -r path; do
	[ -n "$path" ] || continue
	index=$((index + 1))
	parse_one "$index" "$path" &
	running=$((running + 1))
	if [ "$running" -ge "$jobs" ]; then
		wait
		running=0
	fi
done <"$list"
wait

i=1
missing=0
while [ "$i" -le "$index" ]; do
	[ -f "$work/result.$i" ] || missing=$((missing + 1))
	i=$((i + 1))
done
[ "$missing" -eq 0 ] || die "$missing of $index files wrote no result"

results=$work/results.tsv
: >"$results"
i=1
while [ "$i" -le "$index" ]; do
	cat "$work/result.$i"
	i=$((i + 1))
done >"$results"

if [ -n "${PARITY_TSV:-}" ]; then
	cp "$results" "$PARITY_TSV" || die "cannot write PARITY_TSV=$PARITY_TSV"
fi

# The first line the two printings disagree on, ours then the compiler's.
first_difference() {
	diff -u "$1" "$2" 2>/dev/null |
		awk 'BEGIN { seen = 0 }
		/^---|^\+\+\+|^@@/ { next }
		/^-/ && !seen { print "ours: " substr($0, 2); seen = 1; next }
		/^\+/ && seen { print "compiler: " substr($0, 2); exit }'
}

passed=0
rejected=0
errored=0
differing=0
print_only=0
clean=0
tree_differs=0
recovery_differs=0
printed_yes=0
printed_no=0
unprintable=0
mismatches=$work/mismatches
: >"$mismatches"
while IFS=$tab read -r index relative diagnostics same_tree same_recovery printed status difference; do
	if [ "$diagnostics" = 0 ]; then clean=$((clean + 1)); fi
	if [ "$same_tree" = no ]; then tree_differs=$((tree_differs + 1)); fi
	if [ "$same_recovery" = no ]; then recovery_differs=$((recovery_differs + 1)); fi
	case $printed in
	yes) printed_yes=$((printed_yes + 1)) ;;
	no) printed_no=$((printed_no + 1)) ;;
	unprintable) unprintable=$((unprintable + 1)) ;;
	esac
	case $status in
	pass) passed=$((passed + 1)) ;;
	rejected)
		rejected=$((rejected + 1))
		printf '%s\t%s\n' "$relative" \
			"the compiler's parser rejects a file the corpus calls positive (driver diagnostics: $diagnostics)" \
			>>"$mismatches"
		;;
	error)
		errored=$((errored + 1))
		printf '%s\t%s\n' "$relative" \
			"the driver reports $diagnostics diagnostic(s) on a file the compiler parses" \
			>>"$mismatches"
		;;
	tree)
		differing=$((differing + 1))
		printf '%s\t%s\n' "$relative" "${difference:-a tree differs}" >>"$mismatches"
		;;
	print)
		print_only=$((print_only + 1))
		detail=$(first_difference "$work/t.$index" "$work/c.$index")
		printf '%s\t%s\n' "$relative" "printing: ${detail:-the two printings differ}" \
			>>"$mismatches"
		;;
	esac
done <"$results"

comparable=$((passed + errored + differing + print_only))
say "$total files; the compiler's parser accepts $comparable, rejects $rejected"
say "no error from the driver: $clean of $total"
say "the same tree as the compiler: $((comparable - tree_differs)) of $comparable"
say "the same recovery tree as the compiler: $((comparable - recovery_differs)) of $comparable"
if [ -n "$compiler" ]; then
	say "the same printing as the compiler: $printed_yes of $((printed_yes + printed_no)) compared; $unprintable the release's parse-and-print refuses"
else
	say "no JASMIN2TEX named: the printing was not compared"
fi

if [ -s "$mismatches" ]; then
	while IFS=$tab read -r relative detail; do
		say "different $relative: $detail"
	done <"$mismatches"
fi

if [ "$rejected" -eq 0 ] && [ "$errored" -eq 0 ] && [ "$differing" -eq 0 ] && [ "$print_only" -eq 0 ]; then
	say "every file the compiler parses passes"
	exit 0
fi
say "the corpus does not agree"
exit 1
