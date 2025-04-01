#!/bin/bash

cabal clean
cabal build
cabal run AirplaneGraphProjectTests

$SHELL