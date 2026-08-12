#!/bin/sh
# The tree half of the parser parity gate: the tree our driver returns for a
# file against the tree the compiler's own parser returns for the same bytes.
#
#   sh test/parity/tree.sh <corpus root> [<check list>]
#
# <corpus root> is the `compiler` directory of a Jasmin release (the one that
# holds `tests/` and `examples/`) or its `tests` directory. Without a check
# list, every `.jazz` file under `tests/success` and, when the root has one,
# `examples` is parsed once by `parity_tree`. With a check list the corpus is
# that list instead: `<label> <path> <architecture>` per check, tab separated,
# paths relative to <corpus root> — the same file `parity.sh` takes as its
# second argument. That is how the two halves of the gate share one corpus:
# the diagnostic run writes one line per check it counted positive, and this
# one compares the tree of every file those checks name. A file the list names
# once per architecture is one tree comparison, because the tree does not
# depend on the target; the run reports the checks and the files apart.
#
# `parity_tree` answers, on stderr:
#
#   driver: <n> diagnostics   what the recovering driver reported for the file;
#   same-tree: yes|no|n/a     whether the driver's item list is the compiler
#                             parser's own, field for field, locations and
#                             annotations included;
#   same-recovery: yes|no|n/a the same question for the tree the recovery
#                             engine returns when it is asked directly.
#
# `n/a` means the compiler's parser rejects the file, so there is no compiler
# tree to compare with: the file cannot be compared, and the run counts it
# apart instead of passing it. Without a check list it fails the run, because
# that walk calls every file it names positive. With one it fails the run too:
# the list is the set the diagnostic half counted positive, so a file in it
# the compiler cannot parse is a contradiction between the two halves, and it
# is named as one.
#
# A file passes only when the driver reports no diagnostic, both trees are the
# compiler parser's own, and — when JASMIN2TEX is named — the release's
# parse-and-print produced a tree for it that prints the same.
#
# The two structural answers are true by construction, and the script says so
# on every run: the driver's fast path is `Jasmin.Parseio.parse_program` on the
# same bytes under the same name, returning that call's item list unchanged
# (`jasmin/parse.ml`), so `same-tree` asks whether that list equals itself. It
# is a regression guard on the driver, not a second observer. The only
# external comparison here is the printing, and it is the one measured against
# a separate binary.
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
# The oracle has to prove it answered. A file whose parse-and-print exited
# non-zero (`refused`), or exited zero and wrote nothing (`silent`), was not
# compared: it is left out of both the numerator and the denominator of the
# printing count, it is named in the run's output, and it fails the run. A run
# in which the parse-and-print produced a tree for none of the files is a
# broken configuration and not a pass: it exits 2 after a line that says so. A
# file the tree printer answered nothing for is counted as `unknown`, is not
# agreement, and fails the run too.
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
# Exit status: 0 when every file the compiler parses passes and every question
# was answered, 1 when a file does not (a tree differs, a printing differs, the
# driver reports a diagnostic, the compiler rejects a file the corpus calls
# positive, the parse-and-print produced no tree for it, or the tree printer
# answered nothing), 2 when the script cannot run (bad arguments, no corpus, no
# printer, or a named parse-and-print or tree printer that answered nothing at
# all).

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
usage: sh test/parity/tree.sh <corpus root> [<check list>]

  <corpus root>   a Jasmin release's `compiler` directory (holding `tests`
                  and, usually, `examples`), or its `tests` directory
  <check list>    optional; one "<label>\t<path>\t<architecture>" per check,
                  paths relative to <corpus root>

environment: PARITY_TREE (the tree printer), JASMIN2TEX (the release's
parse-and-print; without it the printing is not compared), PARITY_JOBS
(default 4), PARITY_TSV (write the per-file lines here)
USAGE
	exit 2
}

if [ $# -lt 1 ] || [ $# -gt 2 ]; then usage; fi
root=${1:-}
checks=${2:-}
[ -n "$root" ] || usage
[ -d "$root" ] || die "no corpus directory: $root"
root=$(cd -- "$root" && pwd)
if [ -n "$checks" ]; then
	[ -f "$checks" ] || die "no check list: $checks"
	checks=$(cd -- "$(dirname -- "$checks")" && pwd)/$(basename -- "$checks")
fi

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

# The files, one `<path relative to the corpus root>\t<architecture>` per line.
# Without a check list they are the release's positive folder and its examples,
# in the corpus's own order, under the architecture the folder names. With one
# they are its files, in its order, under the architecture its own line names:
# the release's parse-and-print runs pretyping before it prints, so the check's
# architecture is the one to print under.
list=$work/files.tsv
: >"$list"
checklist=$list
checks_total=0
if [ -n "$checks" ]; then
	awk -F'\t' 'NF >= 2 && $2 != "" { printf "%s\t%s\n", $2, ($3 == "" ? "x86-64" : $3) }' \
		"$checks" >"$work/checks.tsv"
	checklist=$work/checks.tsv
	checks_total=$(wc -l <"$checklist" | tr -d ' ')
	[ "$checks_total" -gt 0 ] || die "no checks in $checks"
	# One line per file: the tree is one comparison however many architectures
	# the check list names for it, and the first is the one to print under.
	awk -F'\t' '!seen[$1]++' "$checklist" >>"$list"
else
	for folder in "$tests/success" "$examples"; do
		[ -n "$folder" ] && [ -d "$folder" ] || continue
		find "$folder" -type f -name '*.jazz' -print | LC_ALL=C sort |
			while IFS= read -r path; do
				relative=${path#"$root"/}
				printf '%s\t%s\n' "$relative" "$(arch_for "$relative")"
			done >>"$list"
	done
fi
total=$(wc -l <"$list" | tr -d ' ')
[ "$total" -gt 0 ] || die "no files: the corpus holds no .jazz under its success folder"
# A file the corpus does not hold is a fault of the caller, not an answer.
while IFS=$tab read -r relative arch; do
	[ -f "$root/$relative" ] || die "no such file under $root: $relative"
done <"$list"

# parse_one <index> <relative path> <architecture>
#
# Writes one result line to $work/result.<index>:
#
#   index relative diagnostics same-tree same-recovery printed status
#   printer-exit [difference]
#
# `printed` is `yes` (the parse-and-print wrote a tree and it is byte for byte
# ours), `no` (it wrote a tree and the two differ), `refused` (it exited
# non-zero, so it wrote no tree), `silent` (it exited zero and wrote nothing,
# which is no tree either) or `-` when no parse-and-print was named. Only
# `yes` and `no` are comparisons: `refused` and `silent` are files whose
# printing was not measured, and the summary counts them apart.
#
# `printer-exit` is the tree printer's own exit status, so a run in which the
# harness itself failed says so instead of being left to guess.
#
# status is `pass`, `rejected` (the compiler's parser rejects a file the
# corpus calls positive), `error` (the driver reports a diagnostic on a file
# the compiler parses), `tree` (a tree differs), `print` (the trees agree but
# the two printings do not, which points at the printer or the build),
# `unprinted` (the trees agree but the parse-and-print produced no tree for
# the file, so its printing was not compared) or `noanswer` (the tree printer
# answered nothing for the file: a `driver:`, `same-tree:` or `same-recovery:`
# line is missing).
parse_one() {
	index=$1
	relative=$2
	arch=$3
	path=$root/$relative
	directory=$(dirname "$path")
	base=$(basename "$path")
	out=$work/t.$index
	(cd "$directory" && "$printer" "$base") >"$out" 2>"$work/t.$index.err" </dev/null
	printer_exit=$?
	diagnostics=$(sed -n 's/^driver: \([0-9]*\) diagnostics$/\1/p' "$work/t.$index.err")
	same_tree=$(sed -n 's/^same-tree: //p' "$work/t.$index.err")
	same_recovery=$(sed -n 's/^same-recovery: //p' "$work/t.$index.err")
	difference=$(sed -n 's/^first-difference: //p' "$work/t.$index.err")
	[ -n "$diagnostics" ] || diagnostics=unknown
	[ -n "$same_tree" ] || same_tree=unknown
	[ -n "$same_recovery" ] || same_recovery=unknown
	printed=-
	if [ -n "$compiler" ]; then
		(cd "$directory" && "$compiler" --arch "$arch" "$base") \
			>"$work/c.$index" 2>"$work/c.$index.err" </dev/null
		oracle_exit=$?
		if [ "$oracle_exit" -ne 0 ]; then
			printed=refused
		elif [ ! -s "$work/c.$index" ]; then
			printed=silent
		elif cmp -s "$out" "$work/c.$index"; then
			printed=yes
		else
			printed=no
		fi
	fi
	status=tree
	if [ "$diagnostics" = unknown ] || [ "$same_tree" = unknown ] ||
		[ "$same_recovery" = unknown ]; then
		status=noanswer
	else
		case $same_tree/$same_recovery in
		n/a/n/a) status=rejected ;;
		yes/yes)
			if [ "$diagnostics" != 0 ]; then status=error
			elif [ "$printed" = no ]; then status=print
			elif [ "$printed" = refused ] || [ "$printed" = silent ]; then
				status=unprinted
			else status=pass
			fi
			;;
		*) status=tree ;;
		esac
	fi
	{
		printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s' \
			"$index" "$relative" "$diagnostics" "$same_tree" "$same_recovery" \
			"$printed" "$status" "$printer_exit"
		if [ -n "$difference" ]; then printf '\t%s' "$difference"; fi
		printf '\n'
	} >"$work/result.$index"
}

index=0
running=0
while IFS=$tab read -r relative arch; do
	[ -n "$relative" ] || continue
	index=$((index + 1))
	parse_one "$index" "$relative" "$arch" &
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

rejected=0
errored=0
differing=0
print_only=0
unprinted=0
noanswer=0
clean=0
tree_yes=0
tree_no=0
tree_na=0
tree_unknown=0
recovery_yes=0
recovery_no=0
recovery_unknown=0
printed_yes=0
printed_no=0
printed_refused=0
printed_silent=0
mismatches=$work/mismatches
: >"$mismatches"
while IFS=$tab read -r index relative diagnostics same_tree same_recovery printed status printer_exit difference; do
	if [ "$diagnostics" = 0 ]; then clean=$((clean + 1)); fi
	case $same_tree in
	yes) tree_yes=$((tree_yes + 1)) ;;
	no) tree_no=$((tree_no + 1)) ;;
	n/a) tree_na=$((tree_na + 1)) ;;
	*) tree_unknown=$((tree_unknown + 1)) ;;
	esac
	case $same_recovery in
	yes) recovery_yes=$((recovery_yes + 1)) ;;
	no) recovery_no=$((recovery_no + 1)) ;;
	n/a) ;;
	*) recovery_unknown=$((recovery_unknown + 1)) ;;
	esac
	case $printed in
	yes) printed_yes=$((printed_yes + 1)) ;;
	no) printed_no=$((printed_no + 1)) ;;
	refused) printed_refused=$((printed_refused + 1)) ;;
	silent) printed_silent=$((printed_silent + 1)) ;;
	esac
	case $status in
	pass) ;;
	rejected)
		rejected=$((rejected + 1))
		printf '%s\t%s\n' "$relative" \
			"the compiler rejects it, so there is no compiler tree to compare with (driver diagnostics: $diagnostics)" \
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
	unprinted)
		unprinted=$((unprinted + 1))
		reason=$(sed -n '1p' "$work/c.$index.err" 2>/dev/null)
		printf '%s\t%s\n' "$relative" \
			"the parse-and-print produced no tree ($printed${reason:+: $reason}); the printing was not compared" \
			>>"$mismatches"
		;;
	noanswer)
		noanswer=$((noanswer + 1))
		printf '%s\t%s\n' "$relative" \
			"the tree printer's answer is incomplete (exit $printer_exit): a driver:, same-tree: or same-recovery: line is missing" \
			>>"$mismatches"
		;;
	esac
done <"$results"

# The per-group tally: how many checks the corpus names, how many files they
# sit on, how many the compiler's parser answered, how many it rejects (so
# there is no tree to compare with) and how many trees differ. The check list
# carries the checks and the results carry the answers, so one awk reads both.
awk -F'\t' -v OFS='\t' -v checks_file="$checklist" '
	function group(rel,   parts, n) {
		sub(/^tests\//, "", rel)
		n = split(rel, parts, "/")
		if (n >= 3 && (parts[1] == "sct-checker" || parts[1] == "safety"))
			return parts[1] "/" parts[2]
		return parts[1]
	}
	FILENAME == checks_file {
		g = group($1)
		checks[g]++
		if (!(g in seen)) { seen[g] = 1; order[++groups] = g }
		next
	}
	{
		g = group($2)
		files[g]++
		if ($4 == "yes" || $4 == "no") answered[g]++
		else if ($4 == "n/a") notree[g]++
		if ($4 == "no") unequal[g]++
	}
	END {
		for (i = 1; i <= groups; i++) {
			g = order[i]
			printf "%s\t%d\t%d\t%d\t%d\t%d\n", g, checks[g], files[g],
				answered[g], notree[g], unequal[g]
		}
	}' "$checklist" "$results" >"$work/groups.tsv"

# The two structural questions are answered per file and counted where they
# were answered. A file the tree printer said nothing about is neither
# agreement nor disagreement: it is its own count, and it fails the run below.
tree_answered=$((tree_yes + tree_no))
recovery_answered=$((recovery_yes + recovery_no))
if [ -n "$checks" ]; then
	say "the check list names $checks_total checks over $total files; the tree does not depend on the target, so one comparison per file covers them"
fi
say "$total files; the compiler's parser accepts $tree_answered, rejects $tree_na, and the tree printer did not answer for $tree_unknown"
say "no error from the driver: $clean of $total"
say "the same tree as the compiler, by construction: $tree_yes of $tree_answered answered; $tree_unknown no answer; $tree_na the compiler's parser rejects the file"
say "the same recovery tree as the compiler, by construction: $recovery_yes of $recovery_answered answered; $recovery_unknown no answer"
if [ -n "$compiler" ]; then
	printing_compared=$((printed_yes + printed_no))
	printing_not=$((printed_refused + printed_silent))
	say "the same printing as the compiler, external, byte for byte: $printed_yes of $printing_compared compared; $printing_not not compared ($printed_refused refused, $printed_silent answered nothing)"
else
	printing_compared=0
	say "no JASMIN2TEX named: the printing was not compared"
fi

if [ -s "$work/groups.tsv" ]; then
	say "by group: checks, files, the compiler's parser answered, no tree, a different tree"
	while IFS=$tab read -r group group_checks group_files group_answered group_notree group_unequal; do
		say "  $group: $group_checks checks, $group_files files, $group_answered answered, $group_notree no tree, $group_unequal unequal"
	done <"$work/groups.tsv"
fi

if [ -s "$mismatches" ]; then
	while IFS=$tab read -r relative detail; do
		say "different $relative: $detail"
	done <"$mismatches"
fi

# A run that answered nothing is not a pass, and it is not a corpus
# disagreement either: it is the harness or the oracle failing to run.
if [ "$tree_unknown" -gt 0 ] && [ "$tree_answered" -eq 0 ]; then
	say "the tree printer answered with a tree for none of the $total files: $printer is not a working printer"
	exit 2
fi
if [ -n "$compiler" ] && [ "$printing_compared" -eq 0 ] && [ "$rejected" -ne "$total" ]; then
	say "the parse-and-print produced a tree for none of the $total files: JASMIN2TEX=$compiler is not a working oracle ($printed_refused refused, $printed_silent answered nothing)"
	exit 2
fi
if [ "$rejected" -eq 0 ] && [ "$errored" -eq 0 ] && [ "$differing" -eq 0 ] &&
	[ "$print_only" -eq 0 ] && [ "$unprinted" -eq 0 ] && [ "$noanswer" -eq 0 ]; then
	say "every file the compiler parses passes"
	exit 0
fi
say "the corpus does not agree"
exit 1
