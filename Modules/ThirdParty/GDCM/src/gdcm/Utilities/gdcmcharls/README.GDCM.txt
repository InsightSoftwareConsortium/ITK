This directory contains a subset of the CharLS project (https://github.com/team-charls/charls)

It was retrieved on Thu Sep 15 09:35:25 CEST 2016
URL:
https://github.com/team-charls/charls/archive/1.1.0.tar.gz
This is the 1.1.0 Release

Project Description
An optimized implementation of the JPEG-LS standard for lossless and
near-lossless image compression. JPEG-LS is a low-complexity standard that
matches JPEG 2000 compression ratios. In terms of speed, CharLS outperforms
open source and commercial JPEG LS implementations.

About JPEG-LS
JPEG-LS (ISO-14495-1/ITU-T.87) is a standard derived from the Hewlett Packard
LOCO algorithm. JPEG LS has low complexity (meaning fast compression) and high
compression ratios, similar to JPEG 2000. JPEG-LS is more similar to the old
Lossless JPEG than to JPEG 2000, but interestingly the two different techniques
result in vastly different performance characteristics.
Wikipedia on lossless JPEG and JPEG-LS: http://en.wikipedia.org/wiki/Lossless_JPEG

Legal
The code in this project is available through a BSD style license, allowing use
of the code in commercial closed source applications if you wish. All the code
in this project is written from scratch, and not based on other JPEG-LS
implementations.


We only include enough of distribution to build the charls library.


Modifications
-------------

- only include the src/ subdir
- keep our original cmakelists.txt
