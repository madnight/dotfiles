#!/bin/bash
set -e
(
# find all shell scripts and run `shellcheck`
for f in $(find . -regex ".*\.\(zsh\|sh\|bash\|zshrc\)"); do
	shellcheck $f && echo -e "---\nSucessfully linted $f\n---"
done
) || true
