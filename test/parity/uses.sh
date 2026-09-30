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

files=$(awk -F'\t' '!seen[$3]++' "$results" | wc -l | tr -d ' ')
groups=$work/groups.tsv
awk -F'\t' '
	function group(rel,   parts, n) {
		n = split(rel, parts, "/")
		if (n >= 2) return parts[2]
		return rel
	}
	{
		g = group($3)
		if (!(g in seen)) { seen[g] = 1; order[++count] = g }
		files[g]++
		if ($6 == "yes") accepted[g]++
		uses[g] += ($7 == "unknown" ? 0 : $7)
		same[g] += ($8 == "unknown" ? 0 : $8)
		elsewhere[g] += ($9 == "unknown" ? 0 : $9)
		unresolved[g] += ($10 == "unknown" ? 0 : $10)
		othername[g] += ($11 == "unknown" ? 0 : $11)
		unlocated[g] += ($12 == "unknown" ? 0 : $12)
	}
	END {
		for (i = 1; i <= count; i++) {
			g = order[i]
			printf "%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", g, files[g],
				accepted[g], uses[g], same[g], elsewhere[g], unresolved[g],
				othername[g], unlocated[g]
		}
	}' "$results" >"$groups"

if [ -n "$checks" ]; then
	say "the check list names $total checks"
fi
say "$files files, $index checks; the check accepts $accepted_checks, refuses $rejected"
say "typed variable uses: $typed_uses; the compiler's declaration: $same; resolved elsewhere: $elsewhere; unresolved: $unresolved; other-name: $othername; unlocated: $unlocated"
say "the counts reconcile: $same + $elsewhere + $unresolved + $othername + $unlocated = $((same + elsewhere + unresolved + othername + unlocated)) of $typed_uses"
if [ -s "$groups" ]; then
	say "by folder: files, accepted, typed uses, the compiler's declaration, elsewhere, unresolved, other-name, unlocated"
	while IFS=$tab read -r group group_files group_accepted group_uses group_same \
		group_elsewhere group_unresolved group_othername group_unlocated; do
		say "  $group: $group_files files, $group_accepted accepted, $group_uses uses, $group_same same, $group_elsewhere elsewhere, $group_unresolved unresolved, $group_othername other-name, $group_unlocated unlocated"
	done <"$groups"
fi

if [ -s "$unlocated_list" ]; then
	say "unlocated uses: the compiler placed them where no file the resolution reached writes their name; they are listed, not counted as differences"
	while IFS= read -r line; do
		say "unlocated $line"
	done <"$unlocated_list"
fi

if [ -s "$mismatches" ]; then
	if [ -n "${USES_KEEP:-}" ]; then
		mkdir -p "$USES_KEEP" || die "cannot make USES_KEEP=$USES_KEEP"
		i=1
		while [ "$i" -le "$index" ]; do
			cp "$work/answer.$i" "$USES_KEEP/answer.$i.txt" ||
				die "cannot write USES_KEEP=$USES_KEEP"
			i=$((i + 1))
		done
	fi
	while IFS= read -r line; do
		say "different $line"
	done <"$mismatches"
fi

# A run that measured nothing is not a pass, and it is not a corpus difference
# either: it is the harness or the tool failing to run.
if [ "$noanswer" -gt 0 ] || [ "$broken" -gt 0 ]; then
	say "$noanswer check(s) answered nothing and $broken could not run: $tool is not a working comparison"
	exit 2
fi
if [ "$typed_uses" -eq 0 ]; then
	say "the corpus holds no typed variable use in any file the check accepts: nothing was measured"
	exit 2
fi
if [ "$rejected" -eq 0 ] && [ "$elsewhere" -eq 0 ] && [ "$unresolved" -eq 0 ] &&
	[ "$othername" -eq 0 ]; then
	say "every typed use of every accepted file is the compiler's declaration"
	exit 0
fi
say "the corpus does not agree"
exit 1
