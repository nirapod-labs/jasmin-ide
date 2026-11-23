#!/bin/sh
set -eu

actionlint=1.7.12
zizmor=1.29.0

mkdir -p "$1"
cd "$1"

fetch() {
	curl -sSfL --retry 3 -o tool.tar.gz "$1"
	echo "$2  tool.tar.gz" | sha256sum -c --quiet
	tar xzf tool.tar.gz "$3"
	rm tool.tar.gz
}

fetch "https://github.com/rhysd/actionlint/releases/download/v$actionlint/actionlint_${actionlint}_linux_amd64.tar.gz" \
	8aca8db96f1b94770f1b0d72b6dddcb1ebb8123cb3712530b08cc387b349a3d8 actionlint
fetch "https://github.com/zizmorcore/zizmor/releases/download/v$zizmor/zizmor-x86_64-unknown-linux-gnu.tar.gz" \
	dd96df044a6e8538d5f423790f453bdd03d49e5b2bcc38214acc41a2f1297839 zizmor

./actionlint -version | head -1
./zizmor --version
