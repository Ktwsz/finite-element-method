#!/bin/bash

n=100
cabal run exes -- $n && python plot.py

