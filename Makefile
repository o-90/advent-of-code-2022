SHELL := /bin/bash

stylish-haskell:
	@find . -type f -name "*.hs" -not -path '.git' -print0 | xargs -0 stylish-haskell -i
