#!/bin/sh
# Parity: the checker against a reference Jasmin compiler, or against the
# release's own record of what that compiler says.
#
#   sh test/parity/parity.sh <corpus root> [<record list>]
#
# <corpus root> is the `compiler/tests` directory of a Jasmin release, or a
# release root that holds it. Every `.jazz` file under the corpus folders
# below is checked once per architecture the folder names, and the check's
# verdict and first diagnostic are compared with the reference:
#
#   JASMINC set     `jasminc -arch <architecture> -until_cstexp`, run here;
#   JASMINC unset   the release's `negative.expected`, which records the first
#                   diagnostic of every negative case. A case it does not
#                   name is one the release expects to be accepted at the end
#                   of preprocessing. A record of a pass after the checkpoint
#                   (stack allocation and the rest) says the case passed the
#                   boundary, so it is accepted there; a record inside the
#                   boundary is a rejection, and a record with no kind is
#                   read as one. No compiler runs.
#
# With a record list the walk is replaced by that list, one
# `<label> <path> <architecture>` line per check, tab separated; a path is
# relative to the corpus root or absolute.
#
# Environment:
#   JASMINC      the reference compiler. Optional: without it the run compares
#                with the release's record and runs no compiler.
#   JASMIN_LSP   the checker. Default: the `jasmin-lsp` this tree builds.
#   PARITY_TSV   a file to write one tab-separated line per check to.
#   PARITY_KEEP  a directory to copy the raw output of a differing check to.
#   PARITY_JOBS  how many checks run at once, default 4.
#
# Exit status: 0 when every comparison inside the checkpoint agrees, 1 when one
# differs, 2 when the
# script cannot run (bad arguments, no corpus, no checker, neither an oracle
# nor a record) or is interrupted before it prints a verdict.
#
# The report keeps its totals and adds a split by the reference's verdict at
# the checkpoint: the checks the reference rejects there, where the gate asks
# for the same verdict and the same first diagnostic, and the checks it
# accepts there, where it rejected nothing and the gate asks only that the
# checker accepts too. With JASMINC the checkpoint is the one named below;
# without it there is no checkpoint and the split says so.

set -u

name=parity
tab=$(printf '\t')
us=$(printf '\037')

say() { printf '%s: %s\n' "$name" "$*"; }
warn() { printf '%s: %s\n' "$name" "$*" >&2; }
die() {
	warn "$*"
	exit 2
}

usage() {
	cat >&2 <<'USAGE'
usage: sh test/parity/parity.sh <corpus root> [<record list>]

  <corpus root>   a Jasmin release's compiler/tests directory, or the
                  release root that holds it
  <record list>   optional; one "<label>\t<path>\t<architecture>" per check

environment: JASMINC (the reference compiler; when unset the run compares
with the release's negative.expected and runs no compiler), JASMIN_LSP (the
checker), PARITY_TSV (write the per-check lines here), PARITY_KEEP (copy the
raw output of a differing check here), PARITY_JOBS (default 4)
USAGE
	exit 2
}

if [ $# -lt 1 ] || [ $# -gt 2 ]; then usage; fi
corpus=${1:-}
[ -n "$corpus" ] || usage
records=${2:-}
[ -d "$corpus" ] || die "no corpus directory: $corpus"
corpus=$(cd -- "$corpus" && pwd)
# A release root holds its corpus at compiler/tests, and either path is a
# corpus root: the folders below are looked for from here.
if [ -d "$corpus/compiler/tests" ]; then corpus=$corpus/compiler/tests; fi

here=$(cd -- "$(dirname -- "$0")" && pwd)
checker=${JASMIN_LSP:-$here/../../_build/install/default/bin/jasmin-lsp}
[ -x "$checker" ] || die "no checker at $checker (build the tree, or set JASMIN_LSP)"
checker=$(cd -- "$(dirname -- "$checker")" && pwd)/$(basename -- "$checker")

record=$corpus/negative.expected
oracle=${JASMINC:-}
recorded=0
if [ -n "$oracle" ]; then
	case $oracle in
	*/*) [ -x "$oracle" ] || die "JASMINC is not executable: $oracle" ;;
	*) command -v "$oracle" >/dev/null 2>&1 || die "JASMINC is not on PATH: $oracle" ;;
	esac
	version=$("$oracle" -version 2>/dev/null) || die "$oracle -version failed"
	[ -n "$version" ] || die "$oracle printed no version"
	reference=$oracle
else
	recorded=1
	[ -f "$record" ] || die "no JASMINC, and no $record: nothing to compare"
	version='the release record, negative.expected'
	reference=negative.expected
fi

jobs=${PARITY_JOBS:-4}
case $jobs in
'' | *[!0-9]*) die "PARITY_JOBS is not a number: $jobs" ;;
esac
[ "$jobs" -ge 1 ] || die "PARITY_JOBS is below one: $jobs"

# The checkpoint the reference is stopped at: the first one the compiler
# reaches after `Compile.preprocess`, that is after parameter substitution,
# the copy-and-fix-length pass and `Typing.check_prog`. `main_compiler.ml`
# prints the `Typing` checkpoint before `Compile.preprocess` runs, so
# `-until_typing` stops one pass early, at the end of pretyping, and the whole
# main typing pass falls outside it. `-until_cstexp` is the next checkpoint
# and names the step whose message is "param expansion".
stop_after=-until_cstexp

# The split compare.awk prints is by the reference's verdict at that
# checkpoint, so it names one only when a compiler runs. Without a compiler
# the reference is the release's record, which is not stopped anywhere, and
# the split says that instead of calling the record's verdict a boundary.
if [ "$recorded" -eq 1 ]; then checkpoint=; else checkpoint=$stop_after; fi

work=$(mktemp -d "${TMPDIR:-/tmp}/parity.XXXXXX") || die "cannot make a work directory"
trap 'rm -rf "$work"' EXIT
# An interrupted run prints no verdict, so it exits the way an unarguable
# invocation does instead of carrying on with a work directory it has lost.
trap 'exit 2' HUP INT TERM

# The corpus folders and the verdict their name implies for a check that
# stops at the end of preprocessing. Only `fail` holds programs the compiler
# is expected to reject there; the safety and sct folders fail in a later
# pass, and `pending` is the release's own list of cases it does not run, so
# it is left out and counted in the header instead.
folders="success fail sct-checker safety exec warning"

# group_of <relative path>: the folder a check is reported under.
group_of() {
	case $1 in
	sct-checker/*/* | safety/*/*) printf '%s\n' "$(printf '%s' "$1" | cut -d/ -f1-2)" ;;
	*) printf '%s\n' "${1%%/*}" ;;
	esac
}

# arches_for <relative path>: the architectures the folder names, one per
# line. A component names at most one, the deepest wins, and a file under
# none is checked on the compiler's default. `common` is the release's own
# spelling for "every architecture it checks": x86-64, arm-m4 and risc-v.
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

# The checks, one "<label>\t<path>\t<architecture>\t<group>" per line.
list=$work/checks.tsv
: >"$list"
if [ -n "$records" ]; then
	[ -f "$records" ] || die "no record list: $records"
	awk -F'\t' 'NF >= 3 { printf "%s\t%s\t%s\n", $1, $2, $3 }' "$records" >>"$list"
else
	for folder in $folders; do
		[ -d "$corpus/$folder" ] || continue
		find "$corpus/$folder" -type f -name '*.jazz' -print | LC_ALL=C sort |
			while IFS= read -r path; do
				relative=${path#"$corpus"/}
				group=$(group_of "$relative")
				label=accept
				[ "$group" = fail ] && label=reject
				arches_for "$relative" | while IFS= read -r arch; do
					printf '%s\t%s\t%s\t%s\n' "$label" "$relative" "$arch" "$group"
				done
			done >>"$list"
	done
fi
total=$(wc -l <"$list" | tr -d ' ')
[ "$total" -gt 0 ] || die "no checks: the corpus has none of $folders"

# The cases the release does not run, counted so the gap is visible.
pending=0
if [ -d "$corpus/pending" ]; then
	pending=$(find "$corpus/pending" -type f -name '*.jazz' -print | wc -l | tr -d ' ')
fi

# The release's own record of the first diagnostic per negative case.
expected=$work/expected.tsv
if [ -f "$record" ]; then
	awk -v corpus="$corpus/" -f "$here/expected.awk" "$record" >"$expected"
else
	: >"$expected"
fi

# The plan: the checks, and, when no compiler is named, the expectation each
# one carries from the release's record in the four columns after the group.
# An empty record means the release expects the case to be accepted; a record
# that holds a compilation error is read as accepted at the end of
# preprocessing too, which is a rule of thumb rather than the boundary: that
# kind covers parameter substitution, a pass inside it, as well as the passes
# after it.
plan=$work/plan.tsv
if [ "$recorded" -eq 1 ]; then
	awk -F'\t' -v OFS="$us" -v expect_file="$expected" '
		FILENAME == expect_file {
			if (NF >= 9) {
				ordinals[$1]++
				block[$1, ordinals[$1]] = $3 "\t" $4 "\t" $5 "\t" $6 "\t" $7 "\t" $8 "\t" $9
			}
			next
		}
		{
			record = record_for($2, $3)
			if (record == "") {
				print $1, $2, $3, $4, 0, 0, 0, "", ""
				next
			}
			split(record, part, "\t")
			# A record of a pass after the checkpoint says the program passed
			# the boundary, so the release accepts the check there.
			if (part[7] == "beyond") {
				print $1, $2, $3, $4, 0, 0, 0, "", ""
				next
			}
			print $1, $2, $3, $4, 1, part[1], part[2], part[3], part[4]
		}
		function record_for(wanted, wanted_arch,   many) {
			many = ordinals[wanted]
			if (many < 1) return ""
			if (many == 1) return block[wanted, 1]
			if (wanted_arch == "x86-64") return block[wanted, 1]
			if (wanted_arch == "arm-m4" && many >= 2) return block[wanted, 2]
			if (wanted_arch == "riscv" && many >= 3) return block[wanted, 3]
			return block[wanted, 1]
		}' "$expected" "$list" >"$plan"
else
	awk -F'\t' -v OFS="$us" '{ print $1, $2, $3, $4, "", "", "", "", "" }' "$list" >"$plan"
fi

# run_check <index> <label> <relative path> <arch> <group> [<status> <line>
# <column> <message>]
#
# Writes one result line to $work/result.<index>:
#
#   index group path arch label checker-status checker-line checker-column
#   checker-kind checker-message reference-status reference-line
#   reference-column reference-message
#
# Lines are one-based and columns zero-based on both sides, as the two tools
# print them; the checker prints one-based columns and this subtracts one.
# With no oracle named, the reference columns are the record's, given by the
# caller.
run_check() {
	index=$1
	label=$2
	relative=$3
	arch=$4
	group=$5
	out=$work/result.$index

	(cd "$corpus" && "$checker" check --arch "$arch" -- "$relative" \
		>"$work/c.$index" 2>"$work/c.$index.err")
	checker_status=$?
	checker_first=$(awk -f "$here/checker.awk" "$work/c.$index")

	if [ "$recorded" -eq 1 ]; then
		reference_status=$6
		reference_first=$(printf '%s\t%s\t%s\t%s' "$7" "$8" "$9" "${10}")
	else
		"$oracle" -arch "$arch" "$stop_after" "$corpus/$relative" \
			>"$work/o.$index" 2>&1
		reference_status=$?
		reference_first=$(awk -v corpus="$corpus/" -f "$here/oracle.awk" "$work/o.$index")
	fi

	printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
		"$index" "$group" "$relative" "$arch" "$label" \
		"$checker_status" "$checker_first" "$reference_status" "$reference_first" \
		>"$out"
}

index=0
running=0
while IFS=$us read -r label relative arch group reference_status reference_line \
	reference_column reference_message reference_kind ||
	[ -n "${relative:-}" ]; do
	[ -n "${relative:-}" ] || continue
	index=$((index + 1))
	[ -n "${arch:-}" ] || arch=x86-64
	[ -n "${group:-}" ] || group=$(group_of "${relative#"$corpus"/}")
	if [ "$recorded" -eq 1 ]; then
		run_check "$index" "$label" "$relative" "$arch" "$group" \
			"$reference_status" "$reference_line" "$reference_column" \
			"${reference_message:-}" "${reference_kind:-}" </dev/null &
	else
		run_check "$index" "$label" "$relative" "$arch" "$group" </dev/null &
	fi
	running=$((running + 1))
	if [ "$running" -ge "$jobs" ]; then
		wait
		running=0
	fi
done <"$plan"
wait

# A check that wrote no result is a fault of the harness, not a difference.
i=1
missing=0
while [ "$i" -le "$index" ]; do
	[ -f "$work/result.$i" ] || missing=$((missing + 1))
	i=$((i + 1))
done
[ "$missing" -eq 0 ] || die "$missing of $index checks wrote no result"

# One line per check, in plan order.
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

# The tables and the split by the reference's verdict. compare.awk reads the
# results and the expectations, prints them, and writes the harness's own exit
# status to $work/status.
report=$work/report.txt
awk -v corpus="$corpus" -v reference="$reference" -v version="$version" \
	-v checker="$checker" -v expect_file="$expected" -v pending="$pending" \
	-v from_record="$recorded" -v differ="$work/differ" \
	-v status_file="$work/status" -v checkpoint="$checkpoint" \
	-f "$here/compare.awk" "$expected" "$results" >"$report" ||
	die "the comparison did not run"

cat "$report"

# The raw output of a differing check, kept when a caller asks for it.
if [ -s "$work/differ" ] && [ -n "${PARITY_KEEP:-}" ]; then
	mkdir -p "$PARITY_KEEP" || die "cannot make $PARITY_KEEP"
	while IFS=$tab read -r index rest; do
		[ -n "$index" ] || continue
		for suffix in c c.err o; do
			[ -f "$work/$suffix.$index" ] || continue
			cp "$work/$suffix.$index" "$PARITY_KEEP/check-$index.$suffix"
		done
	done <"$work/differ"
fi

status=2
if [ -f "$work/status" ]; then status=$(cat "$work/status"); fi
case $status in
0)
	say "every check agrees with $reference"
	exit 0
	;;
1)
	say "the corpus does not agree with $reference"
	exit 1
	;;
*)
	die "the comparison left no verdict"
	;;
esac
