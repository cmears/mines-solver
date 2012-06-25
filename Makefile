.PHONY : all
all : Solve mines-solve

Solve : Solve.hs Logic.hs TimeAccounting.hs MinesImage.hs
	ghc --make Solve

mines-solve : minesweeper.cpp
	g++ -W -Wall -g -O3 minesweeper.cpp -lgecodedriver -o mines-solve
