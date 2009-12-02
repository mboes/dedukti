#!/bin/sh

awk '/^-- > / {sub(/-- > /, ""); print; }' Dedukti/Parser/External.hs
