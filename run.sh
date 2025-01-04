#!/bin/bash

alex src/Fine/Lexer.x -o src/Fine/Lexer.hs
happy src/Fine/Parser.y -o src/Fine/Parser.hs --ghc
stack run -- $1 $2
