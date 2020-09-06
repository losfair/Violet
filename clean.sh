#!/bin/sh

find . -name '*.hi' -exec rm '{}' ';'
find . -name '*.o' -exec rm '{}' ';'