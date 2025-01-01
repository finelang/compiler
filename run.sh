#!/bin/bash

alex src/Fine/Lexer.x -o src/Fine/Lexer.hs
happy src/Fine/Parser.y -o src/Fine/Parser.hs --ghc
stack run --verbosity error -- $1 $2
