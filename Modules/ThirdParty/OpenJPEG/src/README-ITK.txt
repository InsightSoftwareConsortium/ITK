This directory contains a subset of the OpenJPEG project (http://www.openjpeg.org/)
The OpenJPEG library is an open-source JPEG 2000 codec written in C language. It
has been developed in order to promote the use of JPEG 2000, the new still-image
compression standard from the Joint Photographic Experts Group (JPEG). In
addition to the basic codec, various other features are under development, among
them the  JP2 and MJ2 (Motion JPEG 2000) file formats, an indexing tool useful
for the JPIP  protocol, JPWL-tools for error-resilience, a Java-viewer for
j2k-images, ...

The library is developed by the Communications and Remote Sensing Lab  (TELE),
in the Université Catholique de Louvain (UCL).

We only include enough of distribution to build libopenjpeg. For instance we do not
include the codec subdirs.
Furthermore, the standard OpenJPEG build process is replaced with a CMake build
process.

We'd like to thank the OpenJPEG project for releasing an open source
implementation of the JPEG2000 codec. In particular, thanks to Antonin Descampe
for fixing the 'well known' 16bits j2k issue.

Modifications
-------------

- modification were made so that compilation with gcc -Wall flags passes without warnings
- remove all explicit tabs and replace by proper amount of spaces (2)
- remove all Makefile, *.dsp files...
