#!/bin/sh
# The mutation half of the first-error parity gate: one single-token mutant
# per corpus file per family, given to both the checker and a real compiler,
# and their first diagnostics compared by line and column.
#
#   sh test/parity/mutate.sh <corpus root>
#
# <corpus root> is the `compiler/tests` directory of a Jasmin release, or a
# release root that holds it. The mutants are the single-token families
# `test/mutation` defines — [Delete], [Duplicate] and [Swap], one site per
# file per family — emitted by the `mutants` binary of this tree. [Truncate]
# is not a single-token mutation and is not part of this run.
#
# A case agrees when both tools accept the mutant, or when both reject it at
# the same line and column. The compiler prints zero-based columns and the
# checker one-based ones; the checker's column is reduced by one before the
# comparison, as the parity harness does. The compiler is run at
# `-until_cstexp`, the first checkpoint at or after the end of its typing
# pass, because that is the part of the compiler the checker models.
#
# The corpus is copied into a work directory, and each worker mutates its own
# copy: a mutant is written over its file, the two tools run, and the
# original text is put back before the next mutant, so every check sees
# exactly one mutated file and the corpus a caller named is never touched.
#
# Environment:
#   JASMINC      the reference compiler. Required: without it there is
#                nothing to compare with. It may be a shim that forwards to a
#                compiler on another host; it is run once per mutant with
#                `-arch <arch> -until_cstexp <absolute path>`, and the path
#                has to be one that host can read.
#   JASMIN_LSP   the checker. Default: the `jasmin-lsp` this tree builds.
#   MUTANTS      the emitter. Default: the `mutants` this tree builds.
#   PARITY_SEED  the text that fixes which site each mutant touches.
#                Default: 0.
#   PARITY_FILES how many files to mutate, spread evenly over the sorted
#                file list. Default: 0, every file in the corpus.
#   PARITY_JOBS  how many mutants run at once, default 4.
#   PARITY_WORK  a directory to make the run in, default under TMPDIR. A
#                compiler on another host has to see this path, so it must
#                be somewhere the two hosts share; it is removed on exit.
#   PARITY_TSV   a file to write one tab-separated line per mutant to.
#   PARITY_KEEP  a directory to copy the raw output of a differing mutant to.
#
# The PARITY_TSV columns are, in order: the mutant's ordinal, its family, its
# path below the corpus, the architecture, the checker's exit status and
# first diagnostic as `<line> <column> <kind> <message>`, the compiler's exit
# status and first diagnostic in the same four fields, whether the compiler's
# location appears anywhere among the checker's diagnostics, and the
# mutation's label.
#
# A run that compared nothing must not pass. The checks before the comparison
# hold that line: no mutant is an error, a mutant that is its own source text
# is an error, a mutant that wrote no result is an error, and a set in which
# neither tool rejected a single mutant has no positive control and is an
# error too.
#
# Exit status: 0 when every mutant agrees, 1 when one does not, 2 when the
# script cannot run (bad arguments, no corpus, no checker, no compiler, no
# emitter, no mutant, a mutant that is its own source, a mutant with no
# result, or no mutant rejected at all) or is interrupted before it prints a
# verdict.

set -u

name=mutate
tab=$(printf '\t')
unit=$(printf '\037')

say() { printf '%s: %s\n' "$name" "$*"; }
warn() { printf '%s: %s\n' "$name" "$*" >&2; }
die() {
	warn "$*"
	exit 2
}

usage() {
	cat >&2 <<'USAGE'
usage: sh test/parity/mutate.sh <corpus root>

  <corpus root>   a Jasmin release's compiler/tests directory, or the
                  release root that holds it

environment: JASMINC (the reference compiler; required), JASMIN_LSP (the
checker), MUTANTS (the emitter), PARITY_SEED (the site seed, default 0),
PARITY_FILES (files to mutate, default all), PARITY_JOBS (default 4),
PARITY_WORK (a directory the run is made in), PARITY_TSV (write the per-mutant
lines here), PARITY_KEEP (copy the raw output of a differing mutant here)
USAGE
	exit 2
}

if [ $# -ne 1 ]; then usage; fi
corpus=${1:-}
[ -d "$corpus" ] || die "no corpus directory: $corpus"
corpus=$(cd -- "$corpus" && pwd)
if [ -d "$corpus/compiler/tests" ]; then corpus=$corpus/compiler/tests; fi

here=$(cd -- "$(dirname -- "$0")" && pwd)
root=$(cd -- "$here/../.." && pwd)
checker=${JASMIN_LSP:-$root/_build/install/default/bin/jasmin-lsp}
[ -x "$checker" ] || die "no checker at $checker (build the tree, or set JASMIN_LSP)"
emitter=${MUTANTS:-$root/_build/default/test/mutation/mutants.exe}
[ -x "$emitter" ] || die "no mutant emitter at $emitter (build the tree, or set MUTANTS)"

oracle=${JASMINC:-}
[ -n "$oracle" ] || die "JASMINC names no compiler: there is nothing to compare with"
case $oracle in
*/*) [ -x "$oracle" ] || die "JASMINC is not executable: $oracle" ;;
*) command -v "$oracle" >/dev/null 2>&1 || die "JASMINC is not on PATH: $oracle" ;;
esac
version=$("$oracle" -version 2>/dev/null) || die "$oracle -version failed"
[ -n "$version" ] || die "$oracle printed no version"

jobs=${PARITY_JOBS:-4}
case $jobs in
'' | *[!0-9]*) die "PARITY_JOBS is not a number: $jobs" ;;
esac
[ "$jobs" -ge 1 ] || die "PARITY_JOBS is below one: $jobs"

seed=${PARITY_SEED:-0}
files=${PARITY_FILES:-0}
case $files in
'' | *[!0-9]*) die "PARITY_FILES is not a number: $files" ;;
esac

# The work directory holds the copies, the mutants and one file per check, so
# a caller may put it somewhere with room; it is removed on exit either way.
# PARITY_WORK matters when the compiler is not on this host: the mutants have
# to be somewhere that host can see.
if [ -n "${PARITY_WORK:-}" ]; then
	mkdir -p "$PARITY_WORK" || die "cannot make PARITY_WORK=$PARITY_WORK"
	work=$(mktemp -d "$PARITY_WORK/mutate.XXXXXX") || die "cannot make a work directory"
else
	work=$(mktemp -d "${TMPDIR:-/tmp}/mutate.XXXXXX") || die "cannot make a work directory"
fi
work=$(cd -- "$work" && pwd)
trap 'rm -rf "$work"' EXIT
trap 'exit 2' HUP INT TERM

# The folders the parity harness measures, copied so that a mutation is a
# write into a copy and never into the corpus a caller named. `pending` is
# the release's own list of cases it does not run, so nothing in it is
# mutated; it joins the copy below, once the set is fixed.
folders="success fail sct-checker safety exec warning"
master=$work/master
mkdir -p "$master" || die "cannot make $master"
copied=0
for folder in $folders; do
	[ -d "$corpus/$folder" ] || continue
	cp -R "$corpus/$folder" "$master/$folder" || die "cannot copy $folder"
	copied=$((copied + 1))
done
[ "$copied" -gt 0 ] || die "no corpus: $corpus holds none of $folders"
# The files this run mutates, counted before the tree gains the folder that is
# in it only to be read.
candidates=$(find "$master" -type f -name '*.jazz' | wc -l | tr -d ' ')

# The compiler has to be able to read the work directory. A compiler on
# another host sees only what that host shares, and a mutant it cannot read
# would read as a rejection rather than as a route that is wrong; one file
# the compiler accepts here proves the route before the run measures it.
probe=$work/probe.jazz
printf 'export fn probe() {\n  return;\n}\n' >"$probe" || die "cannot write $probe"
"$oracle" -arch x86-64 -until_cstexp "$probe" >"$work/probe.out" 2>&1
probe_status=$?
if [ "$probe_status" -ne 0 ]; then
	cat "$work/probe.out" >&2
	die "the compiler cannot check $probe (exit $probe_status): is PARITY_WORK somewhere it can see?"
fi

# The mutants, one per file per single-token family, at the site the seed
# names.
"$emitter" --output "$work/mutants" --seed "$seed" --files "$files" "$master" \
	>"$work/emit.log" 2>&1 || {
	cat "$work/emit.log" >&2
	die "the emitter failed"
}
cat "$work/emit.log"
manifest=$work/mutants/manifest.tsv
[ -f "$manifest" ] || die "the emitter wrote no manifest"
total=$(wc -l <"$manifest" | tr -d ' ')
[ "$total" -gt 0 ] || die "the emitter wrote no mutant"

# `pending` is copied in after the mutants are emitted, so it cannot move the
# site a seed names, and before any worker runs, so a file the run does mutate
# can `require` one of its files as the release's own tree allows. A copy
# without it makes the checker report a missing required file at the top of a
# file whose only fault is the mutant: a fault of the copy, at a position
# neither tool's parse reached.
if [ -d "$corpus/pending" ]; then
	cp -R "$corpus/pending" "$master/pending" || die "cannot copy pending"
fi

# A mutant that is its source text again is a file with no mutation in it, and
# a set of those agrees with the compiler without measuring anything. The
# emitter is written not to make one; this proves it for the set this run
# compares, before any compiler time is spent on it.
unchanged=0
while IFS=$tab read -r index family relative label; do
	[ -n "$index" ] || continue
	if cmp -s "$work/mutants/$index.jazz" "$master/$relative"; then
		unchanged=$((unchanged + 1))
		warn "mutant $index is $relative unchanged"
	fi
done <"$manifest"
[ "$unchanged" -eq 0 ] ||
	die "$unchanged of $total mutants are the corpus text; nothing would be compared"

# The architecture a check runs on: the deepest component of the path that
# names one, `common` on the compiler's default, and x86-64 otherwise, as
# `parity.sh` reads it. One architecture per file keeps the run one check per
# mutant.
arch_for() {
	old_ifs=$IFS
	IFS=/
	set -f
	set -- $1
	set +f
	IFS=$old_ifs
	deepest=
	for component in "$@"; do
		case $component in
		x86-64) deepest=x86-64 ;;
		arm-m4) deepest=arm-m4 ;;
		riscv | risc-v) deepest=riscv ;;
		armv8a) deepest=armv8a ;;
		esac
	done
	[ -n "$deepest" ] || deepest=x86-64
	printf '%s\n' "$deepest"
}

# [split_diagnostic <fields> <text>] leaves a first diagnostic in line,
# column, kind and message. The fields are separated by a byte that is not
# whitespace, so an empty field is kept: the tab of the awk output would be
# collapsed by the shell's field splitting and every field after it would move.
# The checker prints four fields (line, column, kind, message); the compiler
# prints three (line, column, message), and its kind is the prefix of that
# message, which `oracle.awk` strips, so there is no kind to read there.
split_diagnostic() {
	count=$1
	old_ifs=$IFS
	IFS=$unit
	set -f
	set -- $2
	set +f
	IFS=$old_ifs
	line=${1:-0}
	column=${2:-0}
	if [ "$count" -eq 4 ]; then
		kind=${3:-}
		message=${4:-}
	else
		kind=-
		message=${3:-}
	fi
	[ -n "$kind" ] || kind=-
	[ -n "$message" ] || message=-
}

# One worker, one private copy of the corpus. Its plan is the manifest
# filtered to the files it owns, in corpus order.
worker() {
	number=$1
	tree=$work/tree.$number
	mkdir -p "$tree" || return 2
	cp -R "$master/." "$tree/" || return 2
	ran=0
	while IFS=$tab read -r index family relative label; do
		[ -n "$index" ] || continue
		cp "$work/mutants/$index.jazz" "$tree/$relative" || return 2
		arch=$(arch_for "$relative")
		(cd "$tree" && "$checker" check --arch "$arch" -- "$relative" \
			>"$work/c.$index" 2>"$work/c.$index.err")
		checker_status=$?
		split_diagnostic 4 \
			"$(awk -f "$here/checker.awk" "$work/c.$index" | tr '\t' "$unit")"
		c_line=$line
		c_column=$column
		c_kind=$kind
		c_message=$message
		"$oracle" -arch "$arch" -until_cstexp "$tree/$relative" \
			>"$work/o.$index" 2>&1 </dev/null
		reference_status=$?
		split_diagnostic 3 \
			"$(awk -v corpus="$tree/" -f "$here/oracle.awk" "$work/o.$index" | tr '\t' "$unit")"
		r_line=$line
		r_column=$column
		r_message=$message
		# Whether the compiler's location is anywhere among the checker's
		# diagnostics, not only first: the checker orders what it prints by
		# position, and a recovered item's marker can come before the error
		# the compiler stopped on.
		contains=na
		if [ "$reference_status" -ne 0 ] && [ "$r_line" -ne 0 ]; then
			contains=no
			pattern=":$r_line:$((r_column + 1)): "
			if grep -qF -- "$pattern" "$work/c.$index"; then contains=yes; fi
		fi
		printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' \
			"$index" "$family" "$relative" "$arch" \
			"$checker_status" "$c_line" "$c_column" "$c_kind" "$c_message" \
			"$reference_status" "$r_line" "$r_column" "$r_message" \
			"$contains" "$label" >"$work/result.$index"
		ran=$((ran + 1))
		cp "$master/$relative" "$tree/$relative" || return 2
	done <"$work/plan.$number"
	warn "worker $number: $ran mutants over $(wc -l <"$work/plan.$number" | tr -d ' ') results"
}

# Each file belongs to exactly one worker: a mutant is written over the file
# it came from, so two workers on one file would race. Files are dealt
# round-robin, in corpus order, so a bounded worker count still covers the
# whole corpus.
number=1
while [ "$number" -le "$jobs" ]; do
	awk -F'\t' -v workers="$jobs" -v worker="$number" '
		$3 != previous { ordinal++; previous = $3 }
		ordinal % workers == worker - 1' "$manifest" >"$work/plan.$number" ||
		die "cannot make the plan for worker $number"
	number=$((number + 1))
done

want=$(wc -l <"$manifest" | tr -d ' ')
covered=$(cat "$work"/plan.* | wc -l | tr -d ' ')
[ "$covered" -eq "$want" ] ||
	die "$covered of $want mutants were given to no worker"

number=1
while [ "$number" -le "$jobs" ]; do
	worker "$number" &
	number=$((number + 1))
done
wait

# A mutant that wrote no result is a fault of the harness, not a difference.
results=$work/results.tsv
: >"$results"
missing=0
i=0
while [ "$i" -lt "$total" ]; do
	if [ -f "$work/result.$i" ]; then
		cat "$work/result.$i"
	else
		missing=$((missing + 1))
	fi
	i=$((i + 1))
done >"$results"
[ "$missing" -eq 0 ] || die "$missing of $total mutants wrote no result"

if [ -n "${PARITY_TSV:-}" ]; then
	cp "$results" "$PARITY_TSV" || die "cannot write PARITY_TSV=$PARITY_TSV"
fi

# The comparison. Both accept is agreement; both reject is agreement only
# when the line and the column are equal. A tool that exits above one did not
# reach a verdict at all, so that mutant is a difference whatever it printed.
same=0
differ=0
accepted=0
rejected=0
agreed=0
faults=0
contains=0
preceded=0
mismatches=$work/mismatches
differing=$work/differing
: >"$mismatches"
: >"$differing"
while IFS=$tab read -r index family relative arch checker_status c_line \
	c_column c_kind c_message reference_status r_line r_column r_message \
	found label; do
	[ -n "$index" ] || continue
	expected=accept
	observed=accept
	[ "$reference_status" -ne 0 ] && expected="reject at $r_line:$r_column"
	[ "$checker_status" -ne 0 ] && observed="reject at $c_line:$c_column"
	# What each side printed first, in its own words: both texts of a
	# mismatch belong in the log, not only the two locations.
	checker_text=accept
	[ "$checker_status" -eq 0 ] || checker_text=$c_kind": "$c_message
	compiler_text=accept
	[ "$reference_status" -eq 0 ] || compiler_text=$r_message
	if [ "$checker_status" -gt 1 ] || [ "$reference_status" -gt 1 ]; then
		faults=$((faults + 1))
		printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$relative" "$index" "$family" \
			"$label" "the compiler exits $reference_status" \
			"the checker exits $checker_status" "$checker_text" "$compiler_text" \
			>>"$mismatches"
		printf '%s\n' "$index" >>"$differing"
	elif [ "$checker_status" -eq 0 ] && [ "$reference_status" -eq 0 ]; then
		agreed=$((agreed + 1))
	elif [ "$checker_status" -eq 0 ]; then
		accepted=$((accepted + 1))
		printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$relative" "$index" "$family" \
			"$label" "$expected" "$observed" "$checker_text" "$compiler_text" \
			>>"$mismatches"
		printf '%s\n' "$index" >>"$differing"
	elif [ "$reference_status" -eq 0 ]; then
		rejected=$((rejected + 1))
		printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$relative" "$index" "$family" \
			"$label" "$expected" "$observed" "$checker_text" "$compiler_text" \
			>>"$mismatches"
		printf '%s\n' "$index" >>"$differing"
	elif [ "$c_line" -eq "$r_line" ] && [ "$c_column" -eq "$r_column" ]; then
		same=$((same + 1))
		[ "$found" = yes ] && contains=$((contains + 1))
	else
		differ=$((differ + 1))
		[ "$found" = yes ] && contains=$((contains + 1))
		[ "$found" = yes ] && preceded=$((preceded + 1))
		printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$relative" "$index" "$family" \
			"$label" "$expected" "$observed" "$checker_text" "$compiler_text" \
			>>"$mismatches"
		printf '%s\n' "$index" >>"$differing"
	fi
done <"$results"

# The raw output of a differing mutant, kept when a caller asks for it.
if [ -s "$differing" ] && [ -n "${PARITY_KEEP:-}" ]; then
	mkdir -p "$PARITY_KEEP" || die "cannot make $PARITY_KEEP"
	while IFS= read -r index; do
		[ -n "$index" ] || continue
		for suffix in c c.err o; do
			[ -f "$work/$suffix.$index" ] || continue
			cp "$work/$suffix.$index" "$PARITY_KEEP/mutant-$index.$suffix"
		done
	done <"$differing"
fi

# The tables: per group, and per family of mutation.
tables=$work/tables
awk -F'\t' -v version="$version" -v files="$files" -v seed="$seed" \
	-v total="$total" '
	function group_of(path,   part, parts, at) {
		parts = split(path, part, "/")
		if (parts >= 3 && (part[1] == "sct-checker" || part[1] == "safety"))
			return part[1] "/" part[2]
		at = index(path, "/")
		if (at > 0) path = substr(path, 1, at - 1)
		return path
	}
	BEGIN {
		groups = 0
	}
	{
		g = group_of($3)
		if (!(g in seen_group)) { seen_group[g] = 1; order[++groups] = g }
		cases[g]++
		if ($5 > 1 || $10 > 1) fault[g]++
		else if ($5 == 0 && $10 == 0) accept[g]++
		else if ($5 == 0) accepted[g]++
		else if ($10 == 0) rejected[g]++
		else if ($6 == $11 && $7 == $12) same[g]++
		else differ[g]++
		total_fault += ($5 > 1 || $10 > 1)
		total_same += ($5 == 1 && $10 == 1 && $6 == $11 && $7 == $12)
		total_differ += ($5 == 1 && $10 == 1 && ($6 != $11 || $7 != $12))
		total_accepted += ($5 == 0 && $10 != 0)
		total_rejected += ($5 != 0 && $10 == 0)
		total_agreed += ($5 == 0 && $10 == 0)

		f = $2
		if (!(f in seen_family)) { seen_family[f] = 1; family_order[++families] = f }
		family[f]++
		family_fault[f] += ($5 > 1 || $10 > 1)
		family_same[f] += ($5 == 1 && $10 == 1 && $6 == $11 && $7 == $12)
		family_accept[f] += ($5 != 0 && $10 == 0)
		family_reject[f] += ($5 == 0 && $10 != 0)
		family_differ[f] += ($5 == 1 && $10 == 1 && ($6 != $11 || $7 != $12))
		family_agree[f] += ($5 == 0 && $10 == 0)
	}
	END {
		printf "%-32s %6s %6s %6s %6s %8s %6s\n", "group", "cases", "same", "differ", "accept", "rejected", "faults"
		for (i = 1; i <= groups; i++) {
			g = order[i]
			printf "%-32s %6d %6d %6d %6d %8d %6d\n", g, cases[g], same[g], \
				differ[g], accepted[g], rejected[g], fault[g]
		}
		printf "%-32s %6d %6d %6d %6d %8d %6d\n", "total", total, total_same, \
			total_differ, total_accepted, total_rejected, total_fault
		printf "%-32s %6s %6s %6s %6s %8s %6s\n", "family", "cases", "same", "differ", "accept", "rejected", "faults"
		for (i = 1; i <= families; i++) {
			f = family_order[i]
			printf "%-32s %6d %6d %6d %6d %8d %6d\n", f, family[f], \
				family_same[f], family_differ[f], family_accept[f], family_reject[f], \
				family_fault[f]
		}
	}' "$results" >"$tables"

say "$version"
if [ "$files" -eq 0 ]; then
	say "seed $seed; every corpus file mutated"
else
	say "seed $seed; $files files to mutate"
fi
say "files mutated: $(awk -F'\t' 'BEGIN { n = 0 } $3 != "" { if ($3 != previous) { n++; previous = $3 } } END { print n }' "$manifest") of $candidates"
say "cases compared: $total; both accept: $agreed; both reject: $((same + differ))"
say "the same line and column: $same; a different location: $differ"
say "accepted where the compiler rejects: $accepted; rejected where it accepts: $rejected"
say "did not finish: $faults"
say "agreement: $((agreed + same)) of $total; difference: $((differ + accepted + rejected + faults))"
say "the compiler's location is somewhere in the checker's output: $contains of $((same + differ)) rejected mutants"
say "of the $differ at a different location, the compiler's is in the checker's output too: $preceded"
sed 's/^/'"$name"': /' "$tables"
if [ -s "$mismatches" ]; then
	while IFS=$tab read -r relative index family label expected observed \
		checker_text compiler_text; do
		say "mismatch $relative mutant $index ($family, $label): expected $expected, observed $observed"
		say "  the checker's first diagnostic: $checker_text"
		say "  the compiler's first diagnostic: $compiler_text"
	done <"$mismatches"
fi

# A set every member of which both tools accept has no first error to compare:
# the mutation did not break anything, or the compiler on the other end of the
# route is not the one this run thinks it is. That is a harness fault, not an
# agreement, and it exits 2 rather than 0.
if [ $((same + differ)) -eq 0 ] && [ "$accepted" -eq 0 ] && [ "$rejected" -eq 0 ]
then
	say "no mutant changed either verdict: the run compared no first error"
	exit 2
fi

if [ -s "$mismatches" ]; then
	say "the mutants do not agree"
	exit 1
fi
say "every mutant agrees"
exit 0
