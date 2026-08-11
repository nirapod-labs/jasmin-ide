#!/bin/sh
# The server, with its pid written down before it becomes the server.
#
# The client starts its server as `<path>` with no arguments, so this script
# stands in for the executable: it records its own pid, then replaces itself
# with the server, which keeps that pid. The test can then ask whether the
# process the client started is still there once the client has returned.
#
# usage: the client runs it as `--server <this script>`; the pid is written to
# `server.pid` in the directory the client runs in.

printf '%s\n' "$$" >server.pid
exec ../../../lsp-server/native/main.exe
