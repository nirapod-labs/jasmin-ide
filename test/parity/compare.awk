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
# check, fourteen tab-separated fields:
#
#   1 index            8  checker column (zero-based)
#   2 group            9  checker kind
#   3 path            10  checker message
#   4 architecture    11  reference status
#   5 the folder's    12  reference line
#     own label       13  reference column (zero-based)
#   6 checker status  14  reference message
#   7 checker line
#
# It prints the report on stdout, writes "<index> <path>" for every differing
# check to the file named by the variable differ, and writes the exit status
# the harness should use to the file named by status_file.

BEGIN {
	FS = "\t"
	if (differ != "") printf "" >differ
	from_record = from_record + 0
	ref = from_record ? "the record" : "the reference"
}

FILENAME == expect_file {
	if (NF >= 5) {
		blocks[$1]++
		block[$1, blocks[$1]] = $3 "\t" $4 "\t" $5
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

	# The release's own record, for the negative cases the reference still
	# rejects at the end of preprocessing: only they fall inside its scope.
	# A check that did not complete has no first diagnostic to compare.
	for (r = 1; r <= row; r++) {
		if (broken_row[r] || status_o[r] != 1 || group[r] != "fail") continue
		record = record_for(path[r], arch[r])
		if (record == "") {
			unrecorded++
			unrecorded_lines[++unrecorded_count] = sprintf("%s (%s): the release records no first diagnostic for it",
				path[r], arch[r])
			continue
		}
		split(record, parts, "\t")
		recorded++
		if (line_o[r] == parts[1] + 0 && column_o[r] == parts[2] + 0)
			recorded_reference++
		else
			recorded_reference_fail[++recorded_reference_count] = sprintf("%s (%s): recorded at %d:%d, the reference gives %d:%d",
				path[r], arch[r], parts[1], parts[2] + 1,
				line_o[r], column_o[r] + 1)
		if (line_c[r] == parts[1] + 0 && column_c[r] == parts[2] + 0)
			recorded_location++
		else {
			recorded_location_fail[++recorded_location_count] = sprintf("%s (%s): recorded at %d:%d, the checker gives %s",
				path[r], arch[r], parts[1], parts[2] + 1,
				(status_c[r] == 0 ? "no error" : sprintf("%d:%d", line_c[r], column_c[r] + 1)))
			note_difference(r, group[r], "the release's recorded location is not reproduced")
		}
		if (message_c[r] == normalise(parts[3])) recorded_message++
		else
			recorded_message_fail[++recorded_message_count] = sprintf("%s (%s): recorded %s | the checker gives %s",
				path[r], arch[r], normalise(parts[3]), message_c[r])
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
	printf "recorded first diagnostic: %d compared; by the checker: location %d, message %d\n",
		recorded, recorded_location, recorded_message
	if (!from_record)
		printf "                           by the reference: location %d\n",
			recorded_reference
	printf "\n"

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
	if (recorded_reference_count > 0) {
		printf "\n== recorded locations the reference does not give ==\n"
		for (k = 1; k <= recorded_reference_count; k++) printf "  %s\n", recorded_reference_fail[k]
	}
	if (recorded_message_count > 0) {
		printf "\n== recorded messages the checker does not give ==\n"
		for (k = 1; k <= recorded_message_count; k++) printf "  %s\n", recorded_message_fail[k]
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
