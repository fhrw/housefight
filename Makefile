.PHONY: b
b:
	elm make src/Main.elm

.PHONY: r
r:
	elm make src/Main.elm
	firefox index.html
