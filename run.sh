#!/bin/bash

alex src/Fine/Lexer.x -o src/Fine/Lexer.hs --ghc
happy src/Fine/Parser.y -o src/Fine/Parser.hs --ghc --info=happy-rules.txt
cabal run fine-compiler -- $1 $2
