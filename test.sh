#!/bin/bash
set -e
(
# find all shell scripts and run `shellcheck`
find . -regex ".*\.\(zsh\|sh\|bash\|zshrc\)" -print0 \
| while IFS= read -r -d $'\0' script; do
	shellcheck "$script"
done
) || true
