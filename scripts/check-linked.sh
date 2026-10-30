#!/bin/sh
# Check that a release binary links nothing outside its platform's system
# libraries, and print its size.
#
#   scripts/check-linked.sh <binary> [macos|linux]
#
# The dependencies come from the platform's own tool, `otool -L` or `ldd`.
# macOS allows /usr/lib/libSystem.B.dylib alone; Linux allows a static binary
# or musl's loader and libc. Exit status: 0 allowed, 1 not allowed or nothing
# listed, 2 the check could not be made.

set -eu
name=check-linked.sh
say() { echo "$name: $*"; }
die() { echo "$name: error: $*" >&2; exit 2; }

[ $# -ge 1 ] && [ $# -le 2 ] || { echo "usage: $name <binary> [macos|linux]" >&2; exit 2; }
binary=$1
case ${2:-$(uname -s)} in
macos | Darwin) platform=macos tool=otool ;;
linux | Linux) platform=linux tool=ldd ;;
*) die "unknown platform: ${2:-$(uname -s)} (macos or linux)" ;;
esac
case $binary in -*) die "a path that starts with a dash is refused: $binary" ;; esac
[ -f "$binary" ] || die "no such file: $binary"

say "$binary"
bytes=$(wc -c <"$binary" | tr -d ' ')
say "size $bytes bytes ($(awk -v b="$bytes" 'BEGIN { printf "%.1f", b / 1048576 }') MiB)"

# The first four bytes, so a file that is not this platform's kind of program
# is refused before its listing is read.
magic=$(od -An -tx1 -N4 "$binary" | tr -d ' \n')
case $platform:$magic in
macos:feedface | macos:cefaedfe | macos:feedfacf | macos:cffaedfe | macos:cafebabe | macos:bebafeca | \
	macos:cafebabf | macos:bfbafeca | linux:7f454c46) ;;
*) die "not a $platform executable: the first four bytes are $magic" ;;
esac
command -v $tool >/dev/null 2>&1 || die "no $tool on PATH, so the dependencies of $binary cannot be read"

if [ $platform = macos ]; then
	listing=$(otool -L "$binary") || die "otool -L failed on $binary"
	# The first line names the file; a line ending in `:` names an architecture.
	deps=$(echo "$listing" | tail -n +2 | awk 'substr($0, length($0)) != ":" { print $1 }')
else
	status=0
	listing=$(ldd "$binary" 2>&1) || status=$?
	case $listing in
	*"not a dynamic executable"* | *"statically linked"*)
		say "static: the binary names no dynamic dependency"
		say "ok"
		exit 0
		;;
	esac
	[ $status -eq 0 ] || { echo "$listing" | head -n 1 >&2; die "ldd failed on $binary"; }
	# The file name of each library, from `name => path (address)` or `path (address)`.
	deps=$(echo "$listing" | awk '{
		sub(/\(0x[0-9a-fA-F]+\)/, "")
		if ($0 ~ /^[ \t]*$/) next
		path = (index($0, "=>") > 0) ? $0 : $1
		sub(/^[^>]*=>[ \t]*/, "", path)
		sub(/[ \t]+$/, "", path)
		n = split(path, parts, "/")
		print parts[n]
	}')
fi

if [ -z "$deps" ]; then
	say "the listing is empty, which is not a static binary and not a linked one"
	say "fail: nothing was measured"
	exit 1
fi
bad=0 seen=0
for dep in $deps; do
	seen=$((seen + 1))
	case $platform:$dep in
	macos:/usr/lib/libSystem.B.dylib | linux:libc.so.6 | linux:libc.musl-*.so.1 | linux:ld-musl-*.so.1 | \
		linux:linux-vdso.so.1 | linux:linux-gate.so.1)
		say "  allowed: $dep"
		;;
	*)
		say "  not allowed: $dep"
		bad=$((bad + 1))
		case $platform:$dep in
		linux:ld-linux*.so.*) say "  this is a glibc dynamic build; the release binary is static against musl" ;;
		esac
		;;
	esac
done
if [ $bad -ne 0 ]; then
	say "fail: $bad of $seen dependencies are not allowed on $platform"
	exit 1
fi
say "ok: all $seen dependencies are allowed on $platform"
