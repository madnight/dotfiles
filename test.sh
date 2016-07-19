#!/bin/bash
set -e
(
# find all shell scripts and run `shellcheck`
for f in $(find . -name '*.zsh' -o -name '*.sh' -o -name '.zshrc'); do
	shellcheck $f && echo -e "---\nSucessfully linted $f\n---"
done
) || true
