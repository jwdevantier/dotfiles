XSTOW_BIN := $(shell command -v xstow 2> /dev/null)
XSTOW_PROFILES := i3
XSTOW_TARGET := ~
XSTOW_CMD := $(XSTOW_BIN) -v -t $(XSTOW_TARGET)/

.PHONY: xstow-dep
xstow-dep:
ifndef XSTOW_BIN
    $(error "xstow binary not available, please 'apt install xstow'")
endif

.PHONY: install
install: xstow-dep
	# installs each profile directory into the target
	sh -c '$(XSTOW_CMD) $(XSTOW_PROFILES)'

.PHONY: uninstall
uninstall: xstow-dep
	# removes eaceh profile from target
	sh -c '$(XSTOW_CMD) -D $(XSTOW_PROFILES)'

.PHONY: i3deps
i3deps:
	sudo apt install xstow xfonts-terminus i3 i3lock lxterminal suckless-tools python3-venv

.PHONY: i3setup
i3setup: i3deps install
	cd $(XSTOW_TARGET)/.config/i3 ;\
	  test -d venv || python3 -m venv venv ;\
	  . venv/bin/activate ;\
	  pwd ;\
	  pip install -Ur requirements.txt
