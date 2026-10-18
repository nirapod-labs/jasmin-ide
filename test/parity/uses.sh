#!/bin/sh
# The typed-use half of the resolver parity gate: for every variable occurrence
# the compiler typed in a corpus file, the declaration the resolver answers in
# the namespace the compiler typed the occurrence in, against the declaration the
# compiler's own `v_dloc` names.
#
#   sh test/parity/uses.sh <corpus root> [<check list>]
#
# <corpus root> is a Jasmin release root, its `compiler` directory, or its
# `tests` directory. Without a check list every `.jazz` file under `success` is
# compared once per architecture its folder names, the rule `parity.sh` uses:
# the deepest component named `x86-64`, `arm-m4`, `riscv`, `risc-v` or `armv8a`
# picks the target, a file under none is checked on x86-64, and a file under
# `common` on x86-64, arm-m4 and riscv. With a check list the corpus is that list
# instead: one `<label> <path> <architecture>` line per check, tab separated,
# paths relative to `<corpus root>`, the same file `parity.sh` writes as
# PARITY_TSV and `tree.sh` takes as its second argument.
#
# The comparison itself is `test/parity/typed_uses.ml`. Its facts hold the typed
# uses of the checked file and of every file a top-level `require` reaches, so a
# check reports one line per use. A use is one occurrence of an architecture: a
# required file reached by several checks of one architecture is the same uses
# each time, and it is counted once. Each use falls in exactly one class:
#
#   same-declaration  the resolver answers the declaration the compiler bound,
#                     in the namespace the compiler typed the use in;
#   elsewhere         the resolver answers another declaration there;
#   unresolved        the resolver answers no declaration there, or never read
#                     the file in that namespace;
#   other-name        the occurrence at the byte holds another name;
#   misplaced         the compiler's span holds no identifier of the name, but
#                     the file writes the name elsewhere: a failure;
#   excluded          the name is not an identifier token anywhere in the file,
#                     so the compiler made the use up on an instruction. Two
#                     reasons: `excluded-flag` (a flag name such as `_zf_`) and
#                     `excluded-temporary` (any other name, such as `bytes`);
#   unchecked         the check refused the file, so its uses have no
#                     compiler-side answer to compare with.
#
# The gate's clause is 100% of typed variable uses resolve to the compiler's
# declaration. The percentage is over the measured uses: same-declaration,
# elsewhere, unresolved, other-name and misplaced. The run checks that the
# classes add up to the distinct typed uses, and that no two checks judge the
# same use differently. The script passes only when no measured use is a
# difference, no file was refused, and the checks agree.
#
# Environment:
#   TYPED_USES              the comparison. Default: the one this tree builds.
#   PARITY_JOBS             how many files are checked at once, default 4.
#   USES_TSV                a file to write one tab-separated line per check to.
#   USES_KEEP               a directory to copy the raw answer of every check to.
#   USES_NEGATIVE_CONTROL   when 1, the comparison plants a wrong binding (the
#                           declaration of the first measured use of each accepted
#                           check moved, in the checked file or in a file it
#                           requires). The planted uses are differences like any
#                           other, so the run must fail: it exits 1 when every
#                           accepted check with a measured use reported its
#                           planted binding, and 2 when one went unreported, since
#                           a comparison that misses a planted binding cannot be
#                           trusted. A planted use of a file two checks reach is
#                           judged differently by the two, so in this mode such a
#                           conflict is counted and printed instead of stopping
#                           the run.
#
# Exit status: 0 when every measured use is the compiler's declaration in its own
# namespace, every file was accepted and the checks agree; 1 when a measured use
# is not, or a file was refused (negative-control mode: when every planted
# binding was reported, which is the result the control must give); 2 when the
# script cannot run (bad arguments, no corpus, no tool, the tool answering
# nothing, counts that do not add up, two checks judging one use differently, or,
# in negative-control mode, a planted binding that went unreported).

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

# arches_for <relative path>: the architectures the folder names, one per line.
# The deepest named component wins, and `common` is every architecture the
# release checks. The rule is `parity.sh`'s, so the halves of the gate never
# disagree about what a folder means.
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
#   unresolved other-name misplaced excluded-flag excluded-temporary unchecked
#   planted
#
# The counts are the tool's own for this check. Its ledger, one line per use,
# goes to $work/ledger.<index> with the architecture in front, and its stderr to
# $work/answer.<index>.
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
	awk -v a="$arch" -F'\t' 'BEGIN { OFS = "\t" } NF > 0 { print a, $0 }' \
		"$work/stdout.$index" >"$work/ledger.$index"
	field() { sed -n "s/^$1: //p" "$answer" | head -n 1; }
	accepted=$(field accepted)
	typed=$(field typed-uses)
	measured=$(field measured)
	same=$(field same-declaration)
	elsewhere=$(field elsewhere)
	unresolved=$(field unresolved)
	othername=$(field other-name)
	misplaced=$(field misplaced)
	xflag=$(field excluded-flag)
	xtemp=$(field excluded-temporary)
	unchecked=$(field unchecked)
	planted=$(field planted)
	[ -n "$accepted" ] || accepted=unknown
	[ -n "$planted" ] || planted=unknown
	for value in typed measured same elsewhere unresolved othername misplaced xflag xtemp unchecked; do
		eval "current=\${$value}"
		[ -n "$current" ] || eval "$value=unknown"
	done
	printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
		"$index" "$relative" "$arch" "$status" "$accepted" \
		"$typed" "$measured" "$same" "$elsewhere" "$unresolved" \
		"$othername" "$misplaced" "$xflag" "$xtemp" "$unchecked" \
		"$planted" >"$work/result.$index"
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

# The documents, one absolute path per check, and the ledger of every check.
awk -F'\t' -v root="$root" '{ print root "/" $2 }' "$results" | sort -u >"$work/documents"
i=1
: >"$work/ledger.all"
while [ "$i" -le "$index" ]; do
	cat "$work/ledger.$i" >>"$work/ledger.all"
	i=$((i + 1))
done

# The distinct uses: one per architecture, file, byte, namespace reading and
# occurrence index. The first check to judge a use fixes its class, and another
# check that judges it differently is a conflict: a fault of the harness. A file is
# reached through a require when some check's ledger marks one of its uses
# required, whichever check the file is checked in: the required file's own check
# judges the same uses as its own, so the origin is read over every check.
awk -F'\t' '$3 == "required" { print $4 }' "$work/ledger.all" | sort -u >"$work/required.txt"
awk -F'\t' -v root="$root" '
	FILENAME == ARGV[1] { document[$0] = 1; next }
	FILENAME == ARGV[2] { reached[$0] = 1; next }
	{
		key = $1 SUBSEP $4 SUBSEP $5 SUBSEP $6 SUBSEP $7
		if (key in cls) {
			if (cls[key] != $2) conflicts++
			next
		}
		cls[key] = $2
		total++
		count[$2]++
		rel = substr($4, length(root) + 2)
		split(rel, parts, "/")
		group = parts[2]
		groups[group] = 1
		gcount[group, "typed"]++
		gcount[group, $2]++
		if ($4 in reached) {
			required_uses++
			if (!($4 in document)) {
				only_uses++
				only[$4] = 1
			}
		}
	}
	END {
		for (c in count) {
			name = c
			gsub(/-/, "_", name)
			printf "class_%s=%d\n", name, count[c]
		}
		printf "typed=%d\n", total
		printf "conflicts=%d\n", conflicts + 0
		printf "required_uses=%d\n", required_uses + 0
		for (f in reached) files++
		printf "required_files=%d\n", files + 0
		for (f in only) only_files++
		printf "required_only_files=%d\n", only_files + 0
		printf "required_only_uses=%d\n", only_uses + 0
		for (g in groups) {
			printf "group\t%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", g, gcount[g, "typed"],
				gcount[g, "same"] + 0, gcount[g, "elsewhere"] + 0,
				gcount[g, "unresolved"] + 0, gcount[g, "other-name"] + 0,
				gcount[g, "misplaced"] + 0,
				gcount[g, "excluded-flag"] + gcount[g, "excluded-temporary"] + 0,
				gcount[g, "unchecked"] + 0
		}
	}' "$work/documents" "$work/required.txt" "$work/ledger.all" >"$work/distinct.txt"

# Per-check results: how many checks were accepted, measured, or detected a planted
# binding, and which checks could not run.
accepted_checks=0
measurable=0
rejected=0
noanswer=0
broken=0
detected=0
undetected=0
mismatches=$work/mismatches
undetected_list=$work/undetected
excluded_list=$work/excluded
: >"$mismatches"
: >"$excluded_list"
: >"$undetected_list"
while IFS=$tab read -r ordinal relative arch status accepted t m s e u o p xf xt c planted; do
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
			# Under the control a check with a measured use must have a planted
			# binding, and must report a difference for it.
			if [ "$planted" = yes ] && [ $((e + u + o + p)) -gt 0 ]; then
				detected=$((detected + 1))
			else
				undetected=$((undetected + 1))
				printf '%s (%s): planted %s, %s difference(s) reported\n' \
					"$relative" "$arch" "$planted" $((e + u + o + p)) >>"$undetected_list"
			fi
		fi
	else
		rejected=$((rejected + 1))
		first=$(sed -n 's/^first-error: //p' "$answer")
		printf '%s (%s): the check does not accept it: %s\n' \
			"$relative" "$arch" "${first:-no error printed}" >>"$mismatches"
	fi
	# Each line the tool wrote about a use that is a difference or an exclusion,
	# quoted with this check's coordinates.
	sed -n 's/^use-\([a-z-]*\): /\1 /p' "$answer" |
		while IFS= read -r line; do
			case $line in
			excluded-*\ *)
				printf '%s (%s) %s\n' "$relative" "$arch" "$line" \
					>>"$excluded_list"
				;;
			*)
				printf '%s (%s) %s\n' "$relative" "$arch" "$line" \
					>>"$mismatches"
				;;
			esac
		done
done <"$results"

# Each distinct message once: a use two checks reach is named once.
awk '!seen[$0]++' "$mismatches" >"$mismatches.once" && mv "$mismatches.once" "$mismatches"
awk '!seen[$0]++' "$excluded_list" >"$excluded_list.once" && mv "$excluded_list.once" "$excluded_list"

# The distinct totals, read from the awk output.
class_elsewhere=0
class_unresolved=0
class_other_name=0
class_misplaced=0
class_same=0
class_excluded_flag=0
class_excluded_temporary=0
class_unchecked=0
typed=0
conflicts=0
required_uses=0
required_files=0
required_only_files=0
required_only_uses=0
eval "$(grep '^[a-z_]*=[0-9]*$' "$work/distinct.txt")"
measured=$((class_same + class_elsewhere + class_unresolved + class_other_name + class_misplaced))
excluded=$((class_excluded_flag + class_excluded_temporary))
sum=$((measured + excluded + class_unchecked))

files=$(awk -F'\t' '!seen[$2]++' "$results" | wc -l | tr -d ' ')
if [ -n "$checks" ]; then
	say "the check list names $total checks"
fi
say "$files files, $index checks; the check accepts $accepted_checks, refuses $rejected"
say "distinct typed variable uses per architecture: $typed; measured: $measured; the compiler's declaration in its namespace: $class_same; elsewhere: $class_elsewhere; unresolved: $class_unresolved; other-name: $class_other_name; misplaced: $class_misplaced; excluded: $excluded (flag $class_excluded_flag, temporary $class_excluded_temporary); unchecked: $class_unchecked"
say "uses of files a top-level require reaches: $required_uses (in $required_files distinct files; $required_only_files of them are not checked directly, with $required_only_uses uses)"
if [ "$sum" -ne "$typed" ]; then
	die "the counts do not add up: $measured measured + $excluded excluded + $class_unchecked unchecked = $sum, not $typed"
fi
if [ "$conflicts" -gt 0 ]; then
	if [ "$negative" = 1 ]; then
		say "negative control: $conflicts use(s) are judged differently by two checks of one architecture, since a planted use of a file is reached unplanted from another check"
	else
		die "$conflicts use(s) are judged differently by two checks of one architecture: the comparison is not consistent"
	fi
fi
if [ "$measured" -gt 0 ]; then
	pct=$(awk -v a="$class_same" -v b="$measured" 'BEGIN { printf "%.4f", 100 * a / b }')
	say "the gate's measure: $class_same of $measured measured uses are the compiler's declaration in their own namespace ($pct%)"
fi
if [ -s "$work/distinct.txt" ]; then
	say "by folder: typed, measured, same, elsewhere, unresolved, other-name, misplaced, excluded, unchecked"
	grep "^group$tab" "$work/distinct.txt" | sort | while IFS=$tab read -r kind group g_typed g_same g_elsewhere g_unresolved g_other g_misplaced g_excluded g_unchecked; do
		g_measured=$((g_same + g_elsewhere + g_unresolved + g_other + g_misplaced))
		say "  $group: $g_typed typed, $g_measured measured, $g_same same, $g_elsewhere elsewhere, $g_unresolved unresolved, $g_other other-name, $g_misplaced misplaced, $g_excluded excluded, $g_unchecked unchecked"
	done
fi

if [ -s "$excluded_list" ]; then
	say "excluded uses: the name is not an identifier token anywhere in the file, so the compiler made the use up; they are listed, not measured"
	while IFS= read -r line; do
		say "$line"
	done <"$excluded_list"
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
if [ "$typed" -eq 0 ] || [ "$measured" -eq 0 ]; then
	say "the corpus holds no measured typed variable use: nothing was measured"
	exit 2
fi

if [ "$negative" = 1 ]; then
	# A check with no measured use has nothing to plant a wrong binding on, so the
	# control expects one report from each check that has one. The planted uses
	# are differences, so the run fails, as it must.
	if [ "$measurable" -gt 0 ] && [ "$undetected" -eq 0 ]; then
		say "negative control: a wrong binding was planted in each of the $measurable accepted checks that have a measured use, and the comparison reported it in $detected; the run fails, as it must"
		exit 1
	fi
	while IFS= read -r line; do
		say "unreported $line"
	done <"$undetected_list"
	say "negative control broken: the planted wrong binding was reported in $detected of $measurable accepted checks with a measured use"
	exit 2
fi

if [ "$rejected" -eq 0 ] && [ "$class_elsewhere" -eq 0 ] && [ "$class_unresolved" -eq 0 ] &&
	[ "$class_other_name" -eq 0 ] && [ "$class_misplaced" -eq 0 ] && [ "$class_unchecked" -eq 0 ]; then
	say "every measured typed use of every accepted file is the compiler's declaration in its own namespace"
	exit 0
fi
say "the corpus does not agree"
exit 1
