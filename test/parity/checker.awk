# The first error in `jasmin-lsp check` output.
#
# A check prints one line per diagnostic, then one summary line:
#
#   <path>:<line>:<column>: <kind>: <message>
#   <path>: ok
#   <path>: 2 errors
#
# The columns are one-based and the message carries the same escapes as the
# JSON diagnostics ('\n' for a newline), so this prints the first error as
#
#   <line> <column minus one> <kind> <message>
#
# with the tab as the separator and a zero pair when the check found no
# error. Warnings come after errors in the same output, and are not errors.
{
	if (found) next
	if (!match($0, /:[0-9]+:[0-9]+: /)) next
	spec = substr($0, RSTART + 1, RLENGTH - 3)
	rest = substr($0, RSTART + RLENGTH)
	split(spec, parts, ":")
	kind = rest
	sub(/:.*$/, "", kind)
	if (kind == "warning") next
	found = 1
	message = substr(rest, length(kind) + 3)
	gsub(/\\n/, " ", message)
	gsub(/\\t/, " ", message)
	gsub(/\\\\/, "\\", message)
	gsub(/[ \t]+/, " ", message)
	sub(/^ /, "", message)
	sub(/ $/, "", message)
	printf "%d\t%d\t%s\t%s\n", parts[1] + 0, parts[2] - 1, kind, message
}
END {
	if (!found) printf "0\t0\t\t\n"
}
