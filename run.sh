#!/bin/bash

alex src/Lexer.x -o src/Lexer.hs
happy src/Parser.y -o src/Parser.hs
stack run -- $1
