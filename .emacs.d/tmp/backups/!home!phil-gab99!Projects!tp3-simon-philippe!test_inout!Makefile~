clean:
	sudo rm -rf tests_build valgrind.log

install: clean
	mkdir tests_build || true
	pip3 install pyyaml numpy
	sudo apt-get install valgrind -y
	sudo apt-get install check -y
	(git clone https://github.com/WolfgangSt/libelf.git -q && cd libelf && sudo ./configure && make -s && sudo make install -s) || true
	cp -r ../src/* ./tests_build
	cp ./parse.y ./tests_build/parse.y
	cp ./report_fixer.py ./tests_build/report_fixer.py
	cp ./TP3_sol ./tests_build/TP3_sol
	cd tests_build && cmake . && make

printnames:
	cd .. && python3 name_validator.py

singletest: printnames clean install
	./tests_build/check_tests $(FOO)

alltests: printnames clean install
	python3 runall.py