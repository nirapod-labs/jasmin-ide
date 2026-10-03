#!/bin/sh
# The typed-use half of the resolver parity gate: for every variable
# occurrence the compiler typed in a corpus file, the declaration the resolver
# answers with against the declaration the compiler's own `v_dloc` names.
#
#   sh test/parity/uses.sh <corpus root> [<check list>]
#
# <corpus root> is a Jasmin release root, its `compiler` directory, or its
# `tests` directory. Without a check list every `.jazz` file under `success` is
# compared once per architecture its folder names, the rule `parity.sh` uses:
# the deepest component named `x86-64`, `arm-m4`, `riscv`, `risc-v` or
# `armv8a` picks the target, a file under none is checked on x86-64, and a
# file under `common` on x86-64, arm-m4 and riscv. With a check list the
# corpus is that list instead: one `<label> <path> <architecture>` line per
# check, tab separated, paths relative to `<corpus root>`, the same file
# `parity.sh` writes as PARITY_TSV and `tree.sh` takes as its second argument.
#
# The comparison itself is `test/parity/typed_uses.ml`. Per check it reports
# every typed use of the file and sorts it into one class:
#
#   same-declaration  the resolver answers the declaration the compiler bound;
#   elsewhere         the resolver answers another declaration;
#   unresolved        the resolver answers no declaration at that byte, or
#                     the resolution never reached the file;
#   other-name        the occurrence at the byte holds another name;
#   unlocated         the source writes no identifier of the name inside the
#                     compiler's span, so the use was made up on an
#                     instruction (a flag such as `_zf_`, a temporary). It is
#                     listed, not measured.
#   unchecked         the check refused the file, so its uses have no
#                     compiler-side answer to compare with.
#
# The gate's clause is 100% of typed variable uses resolve to the compiler's
# declaration. The percentage is over the measured uses, the four classes
# that the source writes: same, elsewhere, unresolved and other-name. Every
# typed use falls in exactly one class, and the run checks that the counts
# add up to the typed uses. This script passes only when no measured use is
# a difference and no file was refused.
#
# Environment:
#   TYPED_USES              the comparison. Default: the one this tree builds.
#   PARITY_JOBS             how many files are checked at once, default 4.
#   USES_TSV                a file to write one tab-separated line per check to.
#   USES_KEEP               a directory to copy the raw answer of every check to.
#   USES_NEGATIVE_CONTROL   when 1, the comparison plants a wrong binding (the
#                           declaration of the first measured use moved) in
#                           every accepted check that has one. The run then
#                           passes only when every such check reports the
#                           planted difference, which shows the comparison
#                           fails when a binding is wrong.
#
# Exit status: 0 when every measured use is the compiler's declaration and
# every file was accepted (in negative-control mode: when the planted wrong
# binding was reported in every accepted check that has a measured use); 1
# when a measured use is not, or a file was refused (in negative-control mode:
# when one planted binding went unreported); 2 when the script cannot run (bad
# arguments, no corpus, no tool, the tool answering nothing, or counts that do
# not add up).

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
USES_TSV (write the per-check lines here), USES_KEEP (copy every check's
raw answer here), USES_NEGATIVE_CONTROL=1 (plant a wrong binding)
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

negative=${USES_NEGATIVE_CONTROL:-0}
case $negative in
0 | 1) ;;
*) die "USES_NEGATIVE_CONTROL is not 0 or 1: $negative" ;;
esac

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
# the release checks. The rule is `parity.sh`'s, so the halves of the gate
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

# check_one <index> <relative path> <architecture>
#
# Writes one result line to $work/result.<index>:
#
#   index relative arch tool-exit accepted typed measured same elsewhere
#   unresolved other-name unlocated unchecked
#
# The counts are the tool's own. The tool's stderr is kept in
# $work/answer.<index> so a difference can be quoted with the check's name in
# front of it.
check_one() {
	index=$1
	relative=$2
	arch=$3
	answer=$work/answer.$index
	if [ "$negative" = 1 ]; then
		"$tool" --negative-control "$root/$relative" "$arch" >"$work/stdout.$index" 2>"$answer" </dev/null
	else
		"$tool" "$root/$relative" "$arch" >"$work/stdout.$index" 2>"$answer" </dev/null
	fi
	status=$?
	field() { sed -n "s/^$1: //p" "$answer" | head -n 1; }
	accepted=$(field accepted)
	typed=$(field typed-uses)
	measured=$(field source-written)
	same=$(field same-declaration)
	elsewhere=$(field elsewhere)
	unresolved=$(field unresolved)
	othername=$(field other-name)
	unlocated=$(field unlocated)
	unchecked=$(field unchecked)
	[ -n "$accepted" ] || accepted=unknown
	for value in typed measured same elsewhere unresolved othername unlocated unchecked; do
		eval "current=\${$value}"
		[ -n "$current" ] || eval "$value=unknown"
	done
	printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
		"$index" "$relative" "$arch" "$status" "$accepted" \
		"$typed" "$measured" "$same" "$elsewhere" "$unresolved" \
		"$othername" "$unlocated" "$unchecked" >"$work/result.$index"
}

index=0
running=0
while IFS=$tab read -r label relative arch; do
	[ -n "$relative" ] || continue
	index=$((index + 1))
	check_one "$index" "$relative" "${arch:-x86-64}" &
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
if [ -n "${USES_KEEP:-}" ]; then
	mkdir -p "$USES_KEEP" || die "cannot make USES_KEEP=$USES_KEEP"
	i=1
	while [ "$i" -le "$index" ]; do
		cp "$work/answer.$i" "$USES_KEEP/answer.$i.txt" ||
			die "cannot write USES_KEEP=$USES_KEEP"
		i=$((i + 1))
	done
fi

# The tally, and one line per check that is not clean. Each line names the
# check's file, architecture and use, so a reader can find it without opening
# the raw answer.
accepted_checks=0
measurable=0
rejected=0
typed_uses=0
measured=0
same=0
elsewhere=0
unresolved=0
othername=0
unlocated=0
unchecked=0
noanswer=0
broken=0
detected=0
mismatches=$work/mismatches
unlocated_list=$work/unlocated
: >"$mismatches"
: >"$unlocated_list"
while IFS=$tab read -r ordinal relative arch status accepted t m s e u o l c; do
	[ -n "${relative:-}" ] || continue
	answer=$work/answer.$ordinal
	if [ "$t" = unknown ] || [ "$s" = unknown ]; then
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
		if [ "$m" -gt 0 ]; then
			measurable=$((measurable + 1))
		fi
		if [ $((e + u + o)) -gt 0 ]; then
			detected=$((detected + 1))
		fi
	else
		rejected=$((rejected + 1))
		first=$(sed -n 's/^first-error: //p' "$answer")
		printf '%s (%s): the check does not accept it: %s\n' \
			"$relative" "$arch" "${first:-no error printed}" >>"$mismatches"
	fi
	typed_uses=$((typed_uses + t))
	measured=$((measured + m))
	same=$((same + s))
	elsewhere=$((elsewhere + e))
	unresolved=$((unresolved + u))
	othername=$((othername + o))
	unlocated=$((unlocated + l))
	unchecked=$((unchecked + c))
	# Every line the tool wrote about a use that is not the compiler's
	# declaration, quoted with this check's coordinates. A use the source does
	# not write is listed apart: it is not measured.
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

# Rows that are not the planted difference are a difference of the corpus; in
# negative-control mode the planted one is expected, so only the count of
# detections matters there.
files=$(awk -F'\t' '!seen[$2]++' "$results" | wc -l | tr -d ' ')
groups=$work/groups.tsv
awk -F'\t' '
	function group(rel,   parts, n) {
		n = split(rel, parts, "/")
		if (n >= 2) return parts[2]
		return rel
	}
	function num(value) { return value == "unknown" ? 0 : value }
	{
		g = group($2)
		if (!(g in seen)) { seen[g] = 1; order[++count] = g }
		files[g]++
		if ($5 == "yes") accepted[g]++
		typed[g] += num($6)
		measured[g] += num($7)
		same[g] += num($8)
		elsewhere[g] += num($9)
		unresolved[g] += num($10)
		othername[g] += num($11)
		unlocated[g] += num($12)
		unchecked[g] += num($13)
	}
	END {
		for (i = 1; i <= count; i++) {
			g = order[i]
			printf "%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", g, files[g],
				accepted[g], typed[g], measured[g], same[g], elsewhere[g],
				unresolved[g], othername[g], unlocated[g]
		}
	}' "$results" >"$groups"

if [ -n "$checks" ]; then
	say "the check list names $total checks"
fi
say "$files files, $index checks; the check accepts $accepted_checks, refuses $rejected"
say "typed variable uses: $typed_uses; source-written (measured): $measured; the compiler's declaration: $same; elsewhere: $elsewhere; unresolved: $unresolved; other-name: $othername; unlocated: $unlocated; unchecked: $unchecked"
sum=$((same + elsewhere + unresolved + othername + unlocated + unchecked))
if [ "$sum" -ne "$typed_uses" ]; then
	die "the counts do not add up: $same + $elsewhere + $unresolved + $othername + $unlocated + $unchecked = $sum, not $typed_uses"
fi
if [ "$measured" -gt 0 ]; then
	pct=$(awk -v a="$same" -v b="$measured" 'BEGIN { printf "%.4f", 100 * a / b }')
	say "the gate's measure: $same of $measured measured uses are the compiler's declaration ($pct%)"
fi
if [ -s "$groups" ]; then
	say "by folder: files, accepted, typed uses, measured, same, elsewhere, unresolved, other-name, unlocated"
	while IFS=$tab read -r group group_files group_accepted group_typed group_measured \
		group_same group_elsewhere group_unresolved group_othername group_unlocated; do
		say "  $group: $group_files files, $group_accepted accepted, $group_typed typed, $group_measured measured, $group_same same, $group_elsewhere elsewhere, $group_unresolved unresolved, $group_othername other-name, $group_unlocated unlocated"
	done <"$groups"
fi

if [ -s "$unlocated_list" ]; then
	say "unlocated uses: the source writes no identifier of the name in the compiler's span; they are listed, not measured"
	while IFS= read -r line; do
		say "unlocated $line"
	done <"$unlocated_list"
fi

if [ -s "$mismatches" ] && [ "$negative" != 1 ]; then
	while IFS= read -r line; do
		say "different $line"
	done <"$mismatches"
fi

# A run that measured nothing is not a pass, and it is not a corpus difference
# either: it is the harness or the tool failing to run.
if [ "$noanswer" -gt 0 ] || [ "$broken" -gt 0 ]; then
	say "$noanswer check(s) answered nothing and $broken could not run: the comparison is not working"
	exit 2
fi
if [ "$typed_uses" -eq 0 ] || [ "$measured" -eq 0 ]; then
	say "the corpus holds no measured typed variable use: nothing was measured"
	exit 2
fi

if [ "$negative" = 1 ]; then
	# A check with no measured use has nothing to plant a wrong binding on,
	# so the control expects one report from each check that has one.
	if [ "$measurable" -gt 0 ] && [ "$detected" -eq "$measurable" ]; then
		say "negative control: a wrong binding was planted in each of the $measurable accepted checks that have a measured use, and the comparison reported it in $detected"
		exit 0
	fi
	say "negative control failed: the planted wrong binding was reported in $detected of $measurable accepted checks with a measured use"
	exit 1
fi

if [ "$rejected" -eq 0 ] && [ "$elsewhere" -eq 0 ] && [ "$unresolved" -eq 0 ] &&
	[ "$othername" -eq 0 ] && [ "$unchecked" -eq 0 ]; then
	say "every measured typed use of every accepted file is the compiler's declaration"
	exit 0
fi
say "the corpus does not agree"
exit 1
