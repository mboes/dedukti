#!/bin/sh

awk '/^-- > / {sub(/-- > /, ""); print; }' Europa/Parser.hs
