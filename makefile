all	: chat.ml
	ocamlc -g unix.cma graphics.cma chat.ml -o chat
	ocamlopt -g unix.cmxa graphics.cmxa chat.ml -o chat.opt

install il install_linux	:
	install chat chat.opt linux

iw install_windows	:
	install chat.exe windows

clean	:
	rm -f *.cm* *.o

distclean dclean	: clean
	rm -f chat chat.exe chat.opt
