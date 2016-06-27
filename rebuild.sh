#!/bin/bash
set -e
clear
echo ---
cabal build
cabal test
