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
# architecture, in that order. This prints one line per block, with the tab
# as the separator and zero-based columns, as the compiler prints them:
#
#   1 path      5 kind     8 bare   1 when the record is only its kind
#   2 ordinal   6 sub-kind 9 static scope, from the record's text alone:
#   3 line        the pass named after the kind (stack allocation), or ""
#   4 column    7 message  inside (up to the checkpoint) or beyond it
#
# The message is the release's text with its lines joined and "in function f"
# left in. The kind is the word the record prints first (parse error,
# compilation error, typing error and so on), and the sub-kind is the pass a
# compilation error names. A record without a kind is the front end's text.
#
# The static scope is what the record alone says, and it is used only when
# no compiler is named (record mode). Passes after preprocessing are named by
# their sub-kind or by the internal kind; everything else is inside. The
# gate's own rule does not use it: there the compiler, stopped at the
# checkpoint, decides which records it produces (see compare.awk).
function plain(text,   t) {
	t = text
	sub(/ in function [^:]*:/, ":", t)
	return t
}
function parse_head(text,   t, rest) {
	t = plain(text)
	kind = ""
	sub_kind = ""
	rest = t
	if (match(t, /^(internal )?compilation error/)) {
		kind = substr(t, RSTART, RLENGTH)
		rest = substr(t, RLENGTH + 1)
		sub(/^:[ ]*/, "", rest)
		if (match(rest, /^[a-zA-Z ]+:/)) {
			sub_kind = substr(rest, 1, RLENGTH - 1)
			rest = substr(rest, RLENGTH + 1)
			sub(/^[ ]*/, "", rest)
		}
	} else if (match(t, /^(parse|annotation|typing|pretyping) error|^typing:|^unexpected annotation/)) {
		kind = substr(t, RSTART, RLENGTH)
		sub(/ $/, "", kind)
		rest = substr(t, RLENGTH + 1)
		sub(/^:?[ ]*/, "", rest)
	}
	bare = (kind != "" && rest ~ /^[ ]*$/) ? 1 : 0
}
function static_scope(   s) {
	if (kind == "internal compilation error") return "beyond"
	if (kind == "compilation error" && sub_kind != "" && sub_kind != "param expansion")
		return "beyond"
	return "inside"
}
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
	parse_head(message)
	printf "%s\t%d\t%d\t%d\t%s\t%s\t%s\t%d\t%s\n", path, ordinal[path], line, column,
		message, kind, sub_kind, bare, static_scope()
	state = 0
}
END { finish() }
