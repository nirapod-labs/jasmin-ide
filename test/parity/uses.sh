#!/bin/sh
# The typed-use half of the resolver parity gate: for every variable
# occurrence the compiler typed in a corpus file, the declaration the resolver
# answers with against the declaration the compiler's own `v_dloc` names.
#
#   sh test/parity/uses.sh <corpus root> [<check list>]
#
# <corpus root> is a Jasmin release root, its `compiler` directory, or its
# `tests` directory. Without a check list every `.jazz` file under
# `success` is compared once per architecture its folder names, the rule
# `parity.sh` uses: the deepest component named `x86-64`, `arm-m4`, `riscv`,
# `risc-v` or `armv8a` picks the target, a file under none is checked on
# x86-64, and a file under `common` on x86-64, arm-m4 and riscv. With a check
# list the corpus is that list instead: one `<label> <path> <architecture>`
# line per check, tab separated, paths relative to `<corpus root>` — the same
# file `parity.sh` writes as PARITY_TSV and `tree.sh` takes as its second
# argument, so the three halves of the gate can compare one set.
#
# The comparison itself is `test/parity/typed_uses.ml`: it runs the check, and
# for every use the typed facts carry asks the resolver what that byte names.
# The gate's question is one sentence — 100% of typed variable uses resolve to
# the compiler's declaration — so this script passes only when every use of
# every accepted file is that declaration. A use is one of three failures:
#
#   elsewhere     the resolver resolved it to another declaration;
#   unresolved    the resolver answered no declaration at all;
#   other-name    the byte the compiler typed a name at holds another name in
#                 the index, so the two sides are not speaking about one name.
#
# A fourth kind the tool counts is not a failure and never fails a run: an
# `unlocated` use is one the compiler placed where no file the resolution
# reached writes its name (a flag it made up, an intrinsic's expansion), so
# there is no occurrence to ask about. Those lines are listed apart, and the
# run prints the five counts so they reconcile: typed uses = same + elsewhere
# + unresolved + other-name + unlocated.
#
# A file the check does not accept is counted apart and fails the run: the
# corpus is the release's positive folder, so a file it rejects has no
# compiler-side answer to compare with, and counting its uses as agreement
# would measure nothing.
#
# Environment:
#   TYPED_USES  the comparison. Default: the `typed_uses` this tree builds.
#   PARITY_JOBS how many files are checked at once, default 4.
#   USES_TSV    a file to write one tab-separated line per check to.
#   USES_KEEP   a directory to copy the raw answer of a failing check to.
#
# Exit status: 0 when every accepted file's every typed use is the compiler's
# declaration and the run measured at least one use, 1 when one is not or a
# file was not accepted, 2 when the script cannot run (bad arguments, no
# corpus, no tool, or the tool answering nothing at all).

set -u

name=uses
tab=$(printf '\t')

say() { printf '%s: %s\n' "$name" "$*"; }
warn() { printf '%s: %s\n' "$name" "$*" >&2; }
die() {
	warn "$*"
	exit 2
}

usage() {
	cat >&2 <<'USAGE'
usage: sh test/parity/uses.sh <corpus root> [<check list>]

  <corpus root>   a Jasmin release root, its `compiler` directory, or its
                  `tests` directory
  <check list>    optional; one "<label>\t<path>\t<architecture>" per check,
                  paths relative to <corpus root>

environment: TYPED_USES (the comparison), PARITY_JOBS (default 4),
USES_TSV (write the per-check lines here), USES_KEEP (copy a failing check's
raw answer here)
USAGE
	exit 2
}

if [ $# -lt 1 ] || [ $# -gt 2 ]; then usage; fi
root=${1:-}
checks=${2:-}
[ -n "$root" ] || usage
[ -d "$root" ] || die "no corpus directory: $root"
root=$(cd -- "$root" && pwd)
# A release root holds its corpus at compiler/tests, and any of the three
# spellings is a corpus root: the folder below is looked for from here.
if [ -d "$root/compiler/tests/success" ]; then
	root=$root/compiler/tests
fi
if [ ! -d "$root/success" ]; then
	die "no success folder under $root: name a release root, its compiler directory or its tests directory"
fi
if [ -n "$checks" ]; then
	[ -f "$checks" ] || die "no check list: $checks"
	checks=$(cd -- "$(dirname -- "$checks")" && pwd)/$(basename -- "$checks")
fi

here=$(cd -- "$(dirname -- "$0")" && pwd)
tool=${TYPED_USES:-$here/../../_build/default/test/parity/typed_uses.exe}
[ -x "$tool" ] || die "no comparison at $tool (build test/parity/typed_uses.exe, or set TYPED_USES)"

jobs=${PARITY_JOBS:-4}
case $jobs in
'' | *[!0-9]*) die "PARITY_JOBS is not a number: $jobs" ;;
esac
[ "$jobs" -ge 1 ] || die "PARITY_JOBS is below one: $jobs"

work=$(mktemp -d "${TMPDIR:-/tmp}/uses.XXXXXX") || die "cannot make a work directory"
trap 'rm -rf "$work"' EXIT
trap 'exit 2' HUP INT TERM

# arches_for <relative path>: the architectures the folder names, one per
# line. The deepest named component wins, and `common` is every architecture
# the release checks. The rule is `parity.sh`'s, so the two halves of the gate
# never disagree about what a folder means.
arches_for() {
	old_ifs=$IFS
	IFS=/
	set -- $1
	IFS=$old_ifs
	deepest=
	for component in "$@"; do
		case $component in
		x86-64) deepest=x86-64 ;;
		arm-m4) deepest=arm-m4 ;;
		risc-v | riscv) deepest=riscv ;;
		armv8a) deepest=armv8a ;;
		common) deepest=common ;;
		esac
	done
	case $deepest in
	'') printf 'x86-64\n' ;;
	common) printf 'x86-64\narm-m4\nriscv\n' ;;
	*) printf '%s\n' "$deepest" ;;
	esac
}

# The checks, one "<label>\t<path>\t<architecture>" per line.
list=$work/checks.tsv
if [ -n "$checks" ]; then
	awk -F'\t' 'NF >= 2 && $2 != "" { printf "%s\t%s\t%s\n", ($1 == "" ? "accept" : $1), $2, ($3 == "" ? "x86-64" : $3) }' \
		"$checks" >"$list"
else
	: >"$list"
	find "$root/success" -type f -name '*.jazz' -print | LC_ALL=C sort |
		while IFS= read -r path; do
			relative=${path#"$root"/}
			arches_for "$relative" | while IFS= read -r arch; do
				printf 'accept\t%s\t%s\n' "$relative" "$arch"
			done
		done >>"$list"
fi
total=$(wc -l <"$list" | tr -d ' ')
[ "$total" -gt 0 ] || die "no checks: the corpus holds no .jazz under its success folder"
# A file the corpus does not hold is a fault of the caller, not an answer.
while IFS=$tab read -r label relative arch; do
	[ -f "$root/$relative" ] || die "no such file under $root: $relative"
done <"$list"

# check_one <index> <label> <relative path> <architecture>
#
# Writes one result line to $work/result.<index>:
#
#   index label relative arch tool-exit accepted uses same elsewhere unresolved
#   other-name unlocated
#
# `accepted` is `yes` or `no`; the five counts are the tool's own. The tool's
# stderr is kept in $work/answer.<index> so a difference can be quoted with
# the check's own name in front of it.
check_one() {
	index=$1
	label=$2
	relative=$3
	arch=$4
	answer=$work/answer.$index
	"$tool" "$root/$relative" "$arch" >"$work/stdout.$index" 2>"$answer" </dev/null
	status=$?
	accepted=$(sed -n 's/^accepted: //p' "$answer")
	uses=$(sed -n 's/^typed-uses: //p' "$answer")
	same=$(sed -n 's/^same-declaration: //p' "$answer")
	elsewhere=$(sed -n 's/^elsewhere: //p' "$answer")
	unresolved=$(sed -n 's/^unresolved: //p' "$answer")
	othername=$(sed -n 's/^other-name: //p' "$answer")
	unlocated=$(sed -n 's/^unlocated: //p' "$answer")
	[ -n "$accepted" ] || accepted=unknown
	[ -n "$uses" ] || uses=unknown
	[ -n "$same" ] || same=unknown
	[ -n "$elsewhere" ] || elsewhere=unknown
	[ -n "$unresolved" ] || unresolved=unknown
	[ -n "$othername" ] || othername=unknown
	[ -n "$unlocated" ] || unlocated=unknown
	printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
		"$index" "$label" "$relative" "$arch" "$status" "$accepted" \
		"$uses" "$same" "$elsewhere" "$unresolved" "$othername" \
		"$unlocated" >"$work/result.$index"
}

index=0
running=0
while IFS=$tab read -r label relative arch; do
	[ -n "$relative" ] || continue
	index=$((index + 1))
	check_one "$index" "$label" "$relative" "${arch:-x86-64}" &
	running=$((running + 1))
	if [ "$running" -ge "$jobs" ]; then
		wait
		running=0
	fi
done <"$list"
wait

# A check that wrote no result is a fault of the harness, not a difference.
i=1
missing=0
while [ "$i" -le "$index" ]; do
	[ -f "$work/result.$i" ] || missing=$((missing + 1))
	i=$((i + 1))
done
[ "$missing" -eq 0 ] || die "$missing of $index checks wrote no result"

results=$work/results.tsv
: >"$results"
i=1
while [ "$i" -le "$index" ]; do
	cat "$work/result.$i"
	i=$((i + 1))
done >"$results"

if [ -n "${USES_TSV:-}" ]; then
	cp "$results" "$USES_TSV" || die "cannot write USES_TSV=$USES_TSV"
fi

# The tally, and one line per check that is not clean. The first line of each
# chain is the check's own coordinates, so a reader can find the file, the
# target and the use without opening the raw answer.
accepted_checks=0
rejected=0
typed_uses=0
same=0
elsewhere=0
unresolved=0
othername=0
unlocated=0
noanswer=0
broken=0
mismatches=$work/mismatches
unlocated_list=$work/unlocated
: >"$mismatches"
: >"$unlocated_list"
while IFS=$tab read -r ordinal label relative arch status accepted uses check_same \
	check_elsewhere check_unresolved check_othername check_unlocated ||
	[ -n "${relative:-}" ]; do
	[ -n "${relative:-}" ] || continue
	answer=$work/answer.$ordinal
	if [ "$uses" = unknown ] || [ "$check_same" = unknown ]; then
		noanswer=$((noanswer + 1))
		printf '%s (%s): the comparison answered nothing (exit %s)\n' \
			"$relative" "$arch" "$status" >>"$mismatches"
		continue
	fi
	if [ "$status" -eq 2 ]; then
		broken=$((broken + 1))
		printf '%s (%s): the comparison could not run (exit 2)\n' \
			"$relative" "$arch" >>"$mismatches"
		continue
	fi
	if [ "$accepted" = yes ]; then
		accepted_checks=$((accepted_checks + 1))
	else
		rejected=$((rejected + 1))
		first=$(sed -n 's/^first-error: //p' "$answer")
		printf '%s (%s): the check does not accept it: %s\n' \
			"$relative" "$arch" "${first:-no error printed}" >>"$mismatches"
	fi
	typed_uses=$((typed_uses + uses))
	same=$((same + check_same))
	elsewhere=$((elsewhere + check_elsewhere))
	unresolved=$((unresolved + check_unresolved))
	othername=$((othername + check_othername))
	unlocated=$((unlocated + check_unlocated))
	# Every line the tool wrote about a use that is not the compiler's
	# declaration, quoted with this check's coordinates. A use the compiler
	# placed where no source writes its name is not a difference: it has no
	# occurrence to compare, so it is listed apart.
	sed -n 's/^use-\([a-z-]*\): /\1 /p' "$answer" |
		while IFS= read -r line; do
			case $line in
			unlocated\ *)
				printf '%s (%s) %s\n' "$relative" "$arch" "$line" \
					>>"$unlocated_list"
				;;
			*)
				printf '%s (%s) %s\n' "$relative" "$arch" "$line" \
					>>"$mismatches"
				;;
			esac
		done
done <"$results"

