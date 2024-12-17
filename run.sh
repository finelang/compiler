#!/bin/bash

alex src/Lexer.x -o src/Lexer.hs
happy src/Parser.y -o src/Parser.hs --ghc
stack run --verbosity error -- $1
