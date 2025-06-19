#!/bin/bash

alex src/Fine/Lexer.x -o src/Fine/Lexer.hs --ghc
happy src/Fine/Parser.y -o src/Fine/Parser.hs --ghc
cabal run fine-compiler -- $1 $2
