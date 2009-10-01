#!/bin/sh

awk '/^-- > / {sub(/-- > /, ""); print; }' Dedukti/Parser.hs
