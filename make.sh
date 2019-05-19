#!/bin/bash

cd src
alex Tokens.x
ghc Parser.hs

rm *.hi *.o

#mkdir ../bin
#mv Parser ../bin

