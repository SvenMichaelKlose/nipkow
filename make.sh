#!/bin/sh

rm -r *.ppm obj; mkdir obj
rm -r compiled; mkdir compiled
sbcl --noinform --core bender/bender make.lisp
