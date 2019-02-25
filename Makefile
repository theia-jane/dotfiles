install: setup_home setup_config setup_sh

setup_home: 
	./scripts/setup_home.sh

setup_config: 
	./scripts/setup_config.sh

setup_sh:
	cd ./src/config/sh && make
