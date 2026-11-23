#!/bin/sh
set -eu

dir=$1
shift
mkdir -p "$dir"
cd "$dir"

fetch() {
	curl -sSfL --retry 3 -o download "$1"
	echo "$2  download" | sha256sum -c --quiet
}

for tool in "$@"; do
	case $tool in
	actionlint)
		fetch https://github.com/rhysd/actionlint/releases/download/v1.7.12/actionlint_1.7.12_linux_amd64.tar.gz \
			8aca8db96f1b94770f1b0d72b6dddcb1ebb8123cb3712530b08cc387b349a3d8
		tar xzf download actionlint
		;;
	zizmor)
		fetch https://github.com/zizmorcore/zizmor/releases/download/v1.29.0/zizmor-x86_64-unknown-linux-gnu.tar.gz \
			dd96df044a6e8538d5f423790f453bdd03d49e5b2bcc38214acc41a2f1297839
		tar xzf download zizmor
		;;
	tree-sitter)
		fetch https://github.com/tree-sitter/tree-sitter/releases/download/v0.25.10/tree-sitter-linux-x64.gz \
			8283ddba69253c698f6e987ba0e2f9285e079c8db4d36ebe1394b5bb3a0ebdfd
		gunzip -c download >tree-sitter
		chmod +x tree-sitter
		;;
	*)
		echo "ci-tools: unknown tool $tool" >&2
		exit 2
		;;
	esac
	rm download
	"./$tool" --version | head -1
done
