#!/bin/sh
cvs -d :pserver:anonymous@www.itk.org:/cvsroot/Insight status $1 | sed -n 's/^[ \]*Working revision:[ \t]*\([0-9][0-9\.]*\).*/\1/p'

