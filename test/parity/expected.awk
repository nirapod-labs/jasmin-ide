# The release's record of the first diagnostic of each negative case.
#
# `negative.expected` holds one block per check, in the order the release
# runs them: a bare file name, a blank line, the location line, then the
# diagnostic's own lines up to a blank line:
#
#   fail/x86-64/string.jazz:
#
#   "fail/x86-64/string.jazz", line 1 (10-16): ill-formed string (...)
#
# A file the release checks on several architectures has one block per
# architecture, in that order. This prints one line per block:
#
#   <path> <ordinal> <line> <column> <message>
#
# with the tab as the separator and zero-based columns, as the compiler
# prints them.
/^[^"].*\.jazz:$/ {
	finish()
	path = $0
	sub(/:$/, "", path)
	state = 1
	message = ""
	line = 0
	column = 0
	next
}
state == 1 {
	if ($0 ~ /^[ \t]*$/) next
	if ($0 ~ /^"/ && $0 ~ /line [0-9]+ \([0-9]+/) {
		at = index($0, "):")
		head = substr($0, 1, at)
		message = substr($0, at + 2)
		match(head, /line [0-9]+/)
		line = substr(head, RSTART + 5, RLENGTH - 5) + 0
		match(head, /\([0-9]+/)
		column = substr(head, RSTART + 1, RLENGTH - 1) + 0
		state = 2
		next
	}
	state = 0
	next
}
state == 2 {
	if ($0 ~ /^[ \t]*$/) {
		finish()
		next
	}
	message = message " " $0
}
function finish() {
	if (state != 2) {
		state = 0
		return
	}
	ordinal[path]++
	gsub(/[ \t]+/, " ", message)
	sub(/^ /, "", message)
	sub(/ $/, "", message)
	printf "%s\t%d\t%d\t%d\t%s\n", path, ordinal[path], line, column, message
	state = 0
}
END { finish() }
