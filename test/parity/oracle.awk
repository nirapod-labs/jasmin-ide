# The first error in a `jasminc` log, as the compiler prints it.
#
# A diagnostic starts with a location line and carries its message on that
# line and the lines that follow it, up to a blank line:
#
#   "/path/to/file.jazz", line 3 (2-15):
#   annotation error: attribute for "returnaddress" should be one of ...
#
# The location may name a range of lines, `line 1 (0) to line 4 (1)`, and
# the columns are zero-based bytes. Warnings have the same shape and are
# skipped. The output is one line:
#
#   <line> <column> <message>
#
# with the tab as the separator and a zero pair when the log holds no error.
# The message is the compiler's text with its own kind prefix removed (the
# checker prints the kind in its own column) and with every occurrence of
# the corpus root removed, so both sides name a file the same way.
function close_group() {
	if (!open) return
	open = 0
	if (found) return
	if (message ~ /^warning/) return
	if (message ~ /^[ \t]*$/) return
	found = 1
	gsub(/[ \t]+/, " ", message)
	sub(/^ /, "", message)
	sub(/ $/, "", message)
	while ((at = index(message, corpus)) > 0)
		message = substr(message, 1, at - 1) substr(message, at + length(corpus))
	sub(/^(parse|typing|annotation|pretyping|compilation|internal) error:?[ ]*/, "", message)
	result = sprintf("%d\t%d\t%s", start_line, start_column, message)
}
BEGIN {
	open = 0
	found = 0
	other = ""
}
{
	if ($0 ~ /^"[^"]*", line [0-9]+ \([0-9]+([-][0-9]+)?\)( to line [0-9]+ \([0-9]+([-][0-9]+)?\))?:/) {
		close_group()
		open = 1
		at = index($0, "):")
		head = substr($0, 1, at)
		message = substr($0, at + 2)
		sub(/^[ \t]+/, "", message)
		match(head, /line [0-9]+/)
		start_line = substr(head, RSTART + 5, RLENGTH - 5) + 0
		match(head, /\([0-9]+/)
		start_column = substr(head, RSTART + 1, RLENGTH - 1) + 0
	} else if (open) {
		if ($0 ~ /^[ \t]*$/) close_group()
		else message = message " " $0
	} else if (other == "" && $0 ~ /[^ \t]/) {
		other = $0
	}
}
END {
	close_group()
	if (found) print result
	else if (other != "") {
		gsub(/[ \t]+/, " ", other)
		printf "0\t0\t%s\n", other
	} else printf "0\t0\t\n"
}
