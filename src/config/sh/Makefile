PROFILE=$(HOME)/.profile
ZSHRC=$(HOME)/.zshrc
BASHRC=$(HOME)/.bashrc

install: link_zsh link_bash link_profile 

link_profile: 
	[ -e "$(PROFILE)" ] && rm $(PROFILE); \
	echo "source \"$(ZSHRC)\" skip" >> $(PROFILE)

link_zsh:
	[ -e "$(ZSHRC)" -o -L "$(ZSHRC)" ] && rm $(ZSHRC); \
	ln init.sh $(ZSHRC)

link_bash:
	[ -e "$(BASHRC)" -o -L "$(BASHRC)" ] && rm $(BASHRC); \
	ln init.sh $(BASHRC)
