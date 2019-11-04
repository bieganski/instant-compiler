# Opis rozwiązania:

rozwiązanie w Haskellu, korzysta z raczej standardowych funkcji języka,
oba programy korzystają z monady State do pamiętania potrzebnych informacji.

korzystam z bibliotek Data.Text (gwoli optymalizacji, + printf), 
System.FilePath (wygodna manipulacja ścieżkami) i System.Process do poleceń basha (jasmin).

## JVM

* optymalizacja: wyrażenia są obliczane w kolejności optymalnej dla
rozmiaru użytego stosu, w razie potrzeby na stosie robimy operację "swap".

* wiemy w jakiej kolejności liczyć wyrażenie bo posiadamy tzw. stack mapę,
czyli mapę Exp -> Int, z konieczną głębokością stosu dla poddrzewa


## LLVM

* implementacja raczej standardowa, żadnych niuansów tutaj nie ma :-)
