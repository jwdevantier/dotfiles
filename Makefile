XSTOW_BIN := $(shell command -v xstow 2> /dev/null)
XSTOW_PROFILES := i3
XSTOW_TARGET := ~
XSTOW_CMD := $(XSTOW_BIN) -v -t $(XSTOW_TARGET)/

.PHONY: xstow-dep
xstow-dep:
ifndef XSTOW_BIN
    $(error "xstow binary not available, please 'apt install xstow'")
endif

install: xstow-dep
	# installs each profile directory into the target
	sh -c '$(XSTOW_CMD) $(XSTOW_PROFILES)'

uninstall: xstow-dep
	# removes eaceh profile from target
	sh -c '$(XSTOW_CMD) -D $(XSTOW_PROFILES)'

i3deps:
	apt install xfonts-terminus i3 i3lock lxterminal suckless-tools
