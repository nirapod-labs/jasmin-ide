# The three tables: the positive corpus, the negative corpus, and the
# verdicts with the release's own record of each negative case.
#
#   awk -v expect_file=<expected.tsv> -f compare.awk <expected.tsv> <results.tsv>
#
# The reference is the compiler when the harness named one, and the release's
# own record when it did not; `from_record` says which, and the messages say
# "the record" instead of "the reference" in the second case.
#
# <expected.tsv> is expected.awk's output; <results.tsv> holds one line per
# check, fifteen tab-separated fields:
#
#   1 index            8  checker column (zero-based)
#   2 group            9  checker kind
#   3 path            10  checker message
#   4 architecture    11  reference status
#   5 the folder's    12  reference line
#     own label       13  reference column (zero-based)
#   6 checker status  14  reference message
#   7 checker line    15  reference kind
#
# A record of the release is compared only where it lies inside the boundary:
# where the reference, stopped at the checkpoint, prints the same location,
# the same kind and, when the record names one, the same pass. Any other
# record is a record of a pass after the checkpoint, or one the reference
# does not give there; it is listed apart and is not a difference. A record
# with a message is compared by its message, and a bare kind ("parse error")
# is compared by its kind with the kind the reference prints.
#
# It prints the report on stdout, writes "<index> <path>" for every differing
# check to the file named by the variable differ, and writes the exit status
# the harness should use to the file named by status_file.
#
# The variable checkpoint names the checkpoint the reference is stopped at,
# or is empty when the reference is the release's record and no checkpoint
# exists. The report prints a split by the reference's own verdict beside the
# verdict table: a check the reference rejects at the checkpoint is one the
# boundary decides, so the gate asks for the same verdict and the same first
# diagnostic on it, and a check it accepts there is one it rejected nothing
# for, so the gate asks only that the checker accepts it too. The split names
# the checks that fail either row, never only the count, because a count is
# not something a reviewer can check. It is printed beside the totals, never
# instead of them.

BEGIN {
	FS = "\t"
	if (differ != "") printf "" >differ
	from_record = from_record + 0
	ref = from_record ? "the record" : "the reference"
}

FILENAME == expect_file {
	if (NF >= 9) {
		blocks[$1]++
		block[$1, blocks[$1]] = $3 "\t" $4 "\t" $5 "\t" $6 "\t" $7 "\t" $8 "\t" $9
	}
	next
}

{
	row++
	group[row] = $2
	path[row] = $3
	arch[row] = $4
	label[row] = $5
	status_c[row] = $6 + 0
	line_c[row] = $7 + 0
	column_c[row] = $8 + 0
	kind_c[row] = $9
	message_c[row] = normalise($10)
	status_o[row] = $11 + 0
	line_o[row] = $12 + 0
	column_o[row] = $13 + 0
	message_o[row] = normalise($14)
	kind_o[row] = $15

	if (!($2 in known)) {
		known[$2] = 1
		groups[++count_groups] = $2
	}
	if (!($3 in file_seen)) {
		file_seen[$3] = 1
		files++
	}
}

# normalise <message>: one spacing, no kind prefix. The compiler prefixes a
# diagnostic with its kind and the release's record leaves the prefix off,
# so both sides drop it.
function normalise(text,   bare) {
	gsub(/[ \t]+/, " ", text)
	sub(/^ /, "", text)
	sub(/ $/, "", text)
	# The release records a bare kind for a diagnostic it has no text for
	# ("parse error"), and dropping the prefix would leave nothing to compare,
	# so a message that is nothing but the prefix is kept as it stands.
	bare = text
	sub(/^(parse|typing|annotation|pretyping|compilation|internal) error:?[ ]*/, "", bare)
	if (bare != "") text = bare
	return text
}

# word <kind>: a kind without its " error", so the compiler's "parse error"
# and the checker's "parse" name one kind.
function word(kind,   w) {
	w = kind
	sub(/ error$/, "", w)
	return w
}

# sub_of <message>: the pass a message names, when it names one: the word
# before the first colon, after an "in function f:" that comes first.
function sub_of(text,   t) {
	t = text
	sub(/^in function [^:]*: /, "", t)
	if (match(t, /^[a-zA-Z ]+:/)) return substr(t, 1, RLENGTH - 1)
	return ""
}

# record_for <path> <architecture>: the release's block for this check.
# A file that the release checks on several architectures has one block per
# architecture, in the release's own order.
function record_for(wanted, wanted_arch,   many) {
	many = blocks[wanted]
	if (many < 1) return ""
	if (many == 1) return block[wanted, 1]
	if (wanted_arch == "x86-64") return block[wanted, 1]
	if (wanted_arch == "arm-m4" && many >= 2) return block[wanted, 2]
	if (wanted_arch == "riscv" && many >= 3) return block[wanted, 3]
	return block[wanted, 1]
}

# note_difference <row> <group> <text>: once per check, whatever number of
# tables it turns up in.
function note_difference(at, at_group, text) {
	if (noted[at]++) return
	notes[at_group, ++group_notes[at_group]] = text
	if (differ != "") printf "%s\t%s\n", at, path[at] >differ
	differences++
}

END {
	for (r = 1; r <= row; r++) {
		g = group[r]
		if (status_o[r] != 0 && status_o[r] != 1) {
			broken[g]++
			broken_row[r] = 1
			broken_lines[++broken_count] = sprintf("%s (%s): the reference exited %d", path[r], arch[r], status_o[r])
			continue
		}
		# Both tools answer 0 for "no error" and 1 for "an error"; anything
		# else is a check that did not run as asked, not a verdict.
		if (status_c[r] != 0 && status_c[r] != 1) {
			broken[g]++
			broken_row[r] = 1
			broken_lines[++broken_count] = sprintf("%s (%s): the checker exited %d", path[r], arch[r], status_c[r])
			continue
		}
		verdict[g, "checks"]++
		if ((status_c[r] == 0) == (status_o[r] == 0)) verdict[g, "agree"]++
		else verdict[g, "differ"]++

		# The split: the reference's own verdict says which side of the
		# boundary the check stands on. A check it rejects it decided at or
		# before the checkpoint, so a difference in the verdict or in the
		# first diagnostic is a difference the boundary sees. A check it
		# accepts it rejected nothing for, so the only difference that side
		# can hold is the checker rejecting a file the reference accepted.
		side = (status_o[r] == 0 ? "accepts" : "rejects")
		boundary[side, "checks"]++
		if ((status_c[r] == 0) == (status_o[r] == 0)) {
			boundary[side, "verdict"]++
			if (status_c[r] != 0 && line_c[r] == line_o[r] &&
			    column_c[r] == column_o[r] && message_c[r] == message_o[r])
				boundary[side, "diagnostic"]++
		}
		same = ((status_c[r] == 0) == (status_o[r] == 0))
		if (status_o[r] == 0) differs = !same
		else differs = (!same || line_c[r] != line_o[r] ||
			column_c[r] != column_o[r] || message_c[r] != message_o[r])
		if (differs) boundary_break[side, ++boundary_breaks[side]] = r

		if (status_o[r] == 0) {
			positive[g, "checks"]++
			if (status_c[r] == 0) positive[g, "accepted"]++
			else {
				positive[g, "errored"]++
				note_difference(r, g, sprintf("%s (%s): %s accepts; the checker reports %s at %d:%d: %s",
					path[r], arch[r], ref, kind_c[r], line_c[r], column_c[r] + 1,
					message_c[r]))
			}
			continue
		}

		negative[g, "checks"]++
		if (status_c[r] != 0) {
			if (line_o[r] == 0) {
				negative[g, "unlocated"]++
				note_difference(r, g, sprintf("%s (%s): %s's first error carries no location; the checker reports %s at %d:%d",
					path[r], arch[r], ref, kind_c[r], line_c[r], column_c[r] + 1))
			} else if (line_c[r] == line_o[r] && column_c[r] == column_o[r]) {
				negative[g, "same"]++
			} else {
				negative[g, "different"]++
				note_difference(r, g, sprintf("%s (%s): %s's first error is at %d:%d; the checker's is at %d:%d",
					path[r], arch[r], ref, line_o[r], column_o[r] + 1,
					line_c[r], column_c[r] + 1))
			}
		} else {
			negative[g, "accepted"]++
			note_difference(r, g, sprintf("%s (%s): %s rejects at %d:%d (%s); the checker accepts",
				path[r], arch[r], ref, line_o[r], column_o[r] + 1, message_o[r]))
		}
	}

	# The release's record of each check that names one. The reference decides
	# which records are in scope, not the record's own words: a record is
	# compared only where the reference, stopped at the checkpoint, prints the
	# same location, the same kind and, when the record names one, the same
	# pass. Any other record is listed apart and does not count, because the
	# gate's boundary is the reference's, and a record of a later pass says
	# nothing about it.
	for (r = 1; r <= row; r++) {
		if (broken_row[r]) continue
		record = record_for(path[r], arch[r])
		if (record == "") {
			if (status_o[r] == 1 && group[r] == "fail") {
				unrecorded++
				unrecorded_lines[++unrecorded_count] = sprintf("%s (%s): the release records no first diagnostic for it",
					path[r], arch[r])
			}
			continue
		}
		split(record, parts, "\t")
		located = (line_o[r] == parts[1] + 0 && column_o[r] == parts[2] + 0)
		if (parts[4] == "") kind_ok = (message_o[r] == normalise(parts[3]))
		else kind_ok = (word(parts[4]) == word(kind_o[r]) && (parts[5] == "" || parts[5] == sub_of(message_o[r])))
		if (!(status_o[r] == 1 && located && kind_ok)) {
			record_apart++
			if (from_record && status_o[r] == 0) record_named++
			if (status_o[r] == 0) told = "the reference accepts the check at the checkpoint"
			else told = sprintf("the reference gives %d:%d, %s", line_o[r], column_o[r] + 1, message_o[r])
			record_apart_lines[++record_apart_count] = sprintf("%s (%s): the release records %s at %d:%d; %s",
				path[r], arch[r], parts[3], parts[1], parts[2] + 1, told)
			continue
		}
		recorded++
		if (line_c[r] == parts[1] + 0 && column_c[r] == parts[2] + 0) recorded_location++
		else {
			recorded_location_fail[++recorded_location_count] = sprintf("%s (%s): recorded at %d:%d, the checker gives %s",
				path[r], arch[r], parts[1], parts[2] + 1,
				(status_c[r] == 0 ? "no error" : sprintf("%d:%d", line_c[r], column_c[r] + 1)))
			note_difference(r, group[r], sprintf("%s (%s): the release's recorded location is not reproduced",
				path[r], arch[r]))
		}
		if (parts[4] != "" && parts[6] == 1) {
			if (status_c[r] != 0 && word(kind_c[r]) == word(kind_o[r])) recorded_kind++
			else {
				recorded_kind_fail[++recorded_kind_count] = sprintf("%s (%s): the release records %s; the reference prints %s, the checker prints %s",
					path[r], arch[r], parts[4], kind_o[r], (status_c[r] == 0 ? "no error" : kind_c[r]))
				note_difference(r, group[r], sprintf("%s (%s): the checker's kind is not the reference's",
					path[r], arch[r]))
			}
		} else if (message_c[r] == normalise(parts[3])) recorded_message++
		else {
			recorded_message_fail[++recorded_message_count] = sprintf("%s (%s): recorded %s | the checker gives %s",
				path[r], arch[r], normalise(parts[3]), message_c[r])
			note_difference(r, group[r], sprintf("%s (%s): the release's recorded message is not reproduced",
				path[r], arch[r]))
		}
	}

	printf "# parity\n"
	printf "corpus    %s\n", corpus
	printf "checker   %s\n", checker
	printf "oracle    %s\n", (from_record ? "none; the release's negative.expected stands in" : reference)
	printf "version   %s\n", version
	printf "checks    %d over %d files\n", row, files
	printf "excluded  %d .jazz files under pending/, which the release does not run\n", pending
	printf "\n"

	printf "== the positive corpus: every check %s accepts ==\n", ref
	printf "%-20s %8s %10s %9s\n", "group", "checks", "accepted", "errored"
	for (i = 1; i <= count_groups; i++) {
		g = groups[i]
		if (positive[g, "checks"] == 0) continue
		printf "%-20s %8d %10d %9d\n", g, positive[g, "checks"],
			positive[g, "accepted"], positive[g, "errored"]
	}
	printf "positive checks: %d; the checker accepts %d; with an error %d\n",
		total_positive("checks"), total_positive("accepted"),
		total_positive("errored")
	printf "\n"

	printf "== the negative corpus: the first diagnostic's location ==\n"
	printf "%-20s %8s %8s %10s %9s %10s\n", "group", "checks", "same",
		"different", "accepted", "unlocated"
	for (i = 1; i <= count_groups; i++) {
		g = groups[i]
		if (negative[g, "checks"] == 0) continue
		printf "%-20s %8d %8d %10d %9d %10d\n", g, negative[g, "checks"],
			negative[g, "same"], negative[g, "different"],
			negative[g, "accepted"], negative[g, "unlocated"]
	}
	printf "negative checks: %d; the same line and column %d; a different one %d; the checker accepts %d\n",
		total_negative("checks"), total_negative("same"),
		total_negative("different"), total_negative("accepted")
	printf "\n"

	printf "== verdicts up to the end of preprocessing, and the release's record ==\n"
	printf "%-20s %8s %8s %8s\n", "group", "checks", "agree", "differ"
	for (i = 1; i <= count_groups; i++) {
		g = groups[i]
		if (verdict[g, "checks"] == 0) continue
		printf "%-20s %8d %8d %8d\n", g, verdict[g, "checks"],
			verdict[g, "agree"], verdict[g, "differ"]
	}
	printf "verdicts: %d checks; the same verdict %d; a different one %d\n",
		total_verdict("checks"), total_verdict("agree"), total_verdict("differ")
	printf "recorded first diagnostic: %d compared, inside the checkpoint; by the checker: location %d, message %d, kind %d\n",
		recorded, recorded_location, recorded_message, recorded_kind
	printf "records of a pass after the checkpoint, or not given there, not compared: %d\n",
		record_apart
	printf "\n"

	# The split by the reference's verdict, printed beside the totals above
	# and never instead of them: the exit status still turns on every
	# difference. What the split adds is which of the gate's clauses a
	# difference falls under.
	printf "== the split by the reference's verdict ==\n"
	printf "%-24s %8s %20s %20s\n", "", "checks", "same verdict", "same first diagnostic"
	printf "%-24s %8d %20s %20s\n", "the reference rejects",
		boundary["rejects", "checks"],
		share(boundary["rejects", "verdict"], boundary["rejects", "checks"]),
		share(boundary["rejects", "diagnostic"], boundary["rejects", "checks"])
	printf "%-24s %8d %20s %20s\n", "the reference accepts",
		boundary["accepts", "checks"],
		share(boundary["accepts", "verdict"], boundary["accepts", "checks"]), "-"
	printf "\n"
	if (checkpoint != "") {
		printf "The reference is stopped at %s, so a check in the rejects row\n", checkpoint
		printf "is one the boundary decides. The gate asks for the verdict on every\n"
		printf "check; this split asks more of that row, the first diagnostic too --\n"
		printf "its line, column and message -- so a differing text is named here.\n"
		printf "Same verdict: %s, %s.\n",
			share(boundary["rejects", "verdict"], boundary["rejects", "checks"]),
			verdict_of(boundary["rejects", "verdict"], boundary["rejects", "checks"])
		printf "Same first diagnostic: %s, %s.\n",
			share(boundary["rejects", "diagnostic"], boundary["rejects", "checks"]),
			verdict_of(boundary["rejects", "diagnostic"], boundary["rejects", "checks"])
		printf "A check in the accepts row is one the reference rejects nothing in at or\n"
		printf "before that checkpoint, so the gate asks that the checker accepts it too:\n"
		printf "%s, %s.\n",
			share(boundary["accepts", "verdict"], boundary["accepts", "checks"]),
			verdict_of(boundary["accepts", "verdict"], boundary["accepts", "checks"])
		# Why the accepts row cannot hide a difference from after the
		# boundary: the checker's own passes are a prefix of the reference's
		# at this checkpoint, so whatever it reports it reports inside the
		# boundary. A checker that grows a later pass has to change this
		# sentence with it.
		printf "The checker parses, expands requires, checks annotations, pretypes and runs\n"
		printf "the compiler's preprocessing; each of those runs at or before the checkpoint,\n"
		printf "so a difference on either row is one the boundary sees, not one beyond it.\n"
	} else {
		printf "No checkpoint: the reference is the release's whole-compilation record,\n"
		printf "not a stop, so neither row says where a difference stands. Its rejects row\n"
		printf "holds every case it names with a first error; its accepts row holds the\n"
		printf "The release names %d of the accepted checks with a record of a pass after the\n",
			record_named
		printf "checkpoint. Its records are read by their kinds alone here, and a record with no\n"
		printf "kind is taken as a boundary one. Run with JASMINC to measure the boundary itself.\n"
	}
	printf "\n"

	if (boundary_breaks["rejects"] > 0) {
		printf "== checks %s rejects that do not match ==\n", ref
		for (k = 1; k <= boundary_breaks["rejects"]; k++)
			printf "  %s\n", boundary_line(boundary_break["rejects", k])
		printf "\n"
	}
	if (boundary_breaks["accepts"] > 0) {
		if (checkpoint != "")
			printf "== checks %s accepts at the boundary that the checker rejects ==\n", ref
		else
			printf "== checks %s accepts that the checker rejects ==\n", ref
		for (k = 1; k <= boundary_breaks["accepts"]; k++)
			printf "  %s\n", boundary_line(boundary_break["accepts", k])
		printf "\n"
	}

	printf "== differences ==\n"
	if (differences == 0) printf "none\n"
	else {
		for (i = 1; i <= count_groups; i++) {
			g = groups[i]
			for (k = 1; k <= group_notes[g]; k++)
				printf "  %s\n", notes[g, k]
		}
	}
	if (recorded_location_count > 0) {
		printf "\n== recorded locations the checker does not give ==\n"
		for (k = 1; k <= recorded_location_count; k++) printf "  %s\n", recorded_location_fail[k]
	}
	if (recorded_kind_count > 0) {
		printf "\n== recorded kinds the checker does not give ==\n"
		for (k = 1; k <= recorded_kind_count; k++) printf "  %s\n", recorded_kind_fail[k]
	}
	if (recorded_message_count > 0) {
		printf "\n== recorded messages the checker does not give ==\n"
		for (k = 1; k <= recorded_message_count; k++) printf "  %s\n", recorded_message_fail[k]
	}
	if (record_apart_count > 0) {
		printf "\n== records not compared: of a pass after the checkpoint, or not given there ==\n"
		for (k = 1; k <= record_apart_count; k++) printf "  %s\n", record_apart_lines[k]
	}
	if (unrecorded_count > 0) {
		printf "\n== negative cases the release records no first diagnostic for ==\n"
		for (k = 1; k <= unrecorded_count; k++) printf "  %s\n", unrecorded_lines[k]
	}
	if (broken_count > 0) {
		printf "\n== checks that did not complete ==\n"
		for (k = 1; k <= broken_count; k++) printf "  %s\n", broken_lines[k]
	}
	printf "\n"
	if (differences == 0 && broken_count == 0) {
		printf "parity: every check agrees\n"
		if (status_file != "") printf "0\n" >status_file
	} else {
		printf "parity: %d differing checks, %d that did not complete\n",
			differences, broken_count
		if (status_file != "") printf "1\n" >status_file
	}
}

function total_positive(key,   i, sum) {
	sum = 0
	for (i = 1; i <= count_groups; i++) sum += positive[groups[i], key]
	return sum
}
function total_negative(key,   i, sum) {
	sum = 0
	for (i = 1; i <= count_groups; i++) sum += negative[groups[i], key]
	return sum
}
function total_verdict(key,   i, sum) {
	sum = 0
	for (i = 1; i <= count_groups; i++) sum += verdict[groups[i], key]
	return sum
}

# share <part> <whole>: a count with its share of <whole>, so the split says
# how far the gate's 100% is from the number the run gives.
function share(part, whole) {
	if (whole <= 0) return "n/a"
	return sprintf("%d of %d (%.1f%%)", part, whole, 100 * part / whole)
}

# verdict_of <part> <whole>: whether that share is all of them, as the gate's
# own word for it.
function verdict_of(part, whole) {
	return (part == whole ? "met" : "not met")
}

# boundary_line <row>: one differing check of the split, as the side it is on
# and both sides' first diagnostics.
function boundary_line(r) {
	if (status_o[r] == 0)
		return sprintf("%s (%s): %s accepts; the checker reports %s at %d:%d: %s",
			path[r], arch[r], ref, kind_c[r], line_c[r], column_c[r] + 1,
			message_c[r])
	if (status_c[r] == 0)
		return sprintf("%s (%s): %s rejects at %d:%d (%s); the checker accepts",
			path[r], arch[r], ref, line_o[r], column_o[r] + 1, message_o[r])
	return sprintf("%s (%s): %s rejects at %d:%d (%s); the checker rejects at %d:%d (%s)",
		path[r], arch[r], ref, line_o[r], column_o[r] + 1, message_o[r],
		line_c[r], column_c[r] + 1, message_c[r])
}
