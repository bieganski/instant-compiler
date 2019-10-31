all: compiler done tests

done:
	@echo "\n################\n" \
	"Buildowanie aplikacji przebiegło pomyślnie!\n"
tests:
	@echo "TESTERKA - uruchom poleceniem ./test.sh llvm jvm\n"\
	"################\n"

compiler:
	make -iC src
	stack install --local-bin-path=$(shell pwd) || stack install --local-bin-path=$(shell pwd) || /home/students/inf/PUBLIC/MRJP/Stack/stack install --local-bin-path=$(shell pwd)

clean:
	stack clean || stack clean || /home/students/inf/PUBLIC/MRJP/Stack/stack clean
	rm insc_llvm -f
	rm insc_jvm -f

