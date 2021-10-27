# NIFTI C Libraries

`Nifti_clib` is a set of i/o libraries for reading and writing files in the nifti-1,
nifti-2, and (to some degree) cifti file formats.  These libraries provide api's
for binary file format for storing medical image data, e.g. magnetic resonance
image (MRI) and functional MRI (fMRI) brain images.

This repository contains the C implementations. ( See other repositories at
[github](https://github.com/NIFTI-Imaging) for Java, MATLAB, and Python libraries).

`Nifti_clib` has been developed by members of the NIFTI DFWG and volunteers in the
neuroimaging community and serves as a reference implementation of the nifti-1
and nifti-2 file formats. In addition to being a reference implementation, we
hope it is also a useful i/o library. 

`Nifti_clib` code is released into the public domain,
developers are encouraged to incorporate niftilib code into their applications,
and, to contribute changes and enhancements to niftilib. Please contact us if
you would like to contribute additional functionality to the i/o library.

The main webpage for this project is [hosted on github](https://nifti-imaging.github.io/).
This web site provde historical information.  Additional information from the [NIFTI DFWG](http://nifti.nimh.nih.gov)

The testing dashboard for monitoring the health of the libraries is at
[my.cdash.org](https://my.cdash.org/index.php?project=nifti_clib).


## Nifti-2 C libraries
coming soon.

## Cifti C libraries
Introductory, coming soon.

## Nifti-1 C libraries

* Version 2.0.0 beta release Jul  2010
* Version 1.1.0 beta release Aug  2008
* Version 1.0.0 beta release Dec  2007
* Version 0.6 beta release Aug  2007
* Version 0.5 beta release May  2007
* Version 0.4 beta release Sept. 2006
* Version 0.3 beta release April 2006
* Version 0.2 beta release August 12, 2005
* Version 0.1 beta release March 11, 2005
 
niftilib code is released into the public domain.


## Library directories

directory |  description
----------|-------------
znzlib    | low level library for handling read/write of compressed files.
niftilib  | core i/o routines for reading and writing nifti-1 format files.  Primarily routines to read/write and manipulate the header field information, including orientation matrices.  Volume-wise, timecourse-wise, access to image data.
nifti2    | core i/o routines for reading and writing nifti-2 format files.
nifticdf  | functions to compute cumulative distributions and their inverses
fsliolib  | i/o routines for reading and writing nifti-1 format files, higher level than niftilib, includes routines for reading the data blob by volume, timecourse, etc., and, addresses image orientation issues.  `work in progress, subject to significant revision.....`
cifti     | very basic routines for reading cifti format files

## Destination directories

directory | description
----------|------------
bin       | destination directory for installed programs
include   | destination directory for library header files
lib       | destination directory for compiled libraries
docs      | destination directory Doxygen html (created via "make doc")


## Example directories

directory   | description
------------|-------------
`real_easy` | simple code snippets, some using ref. libs., some not


## Other directories

directory   | description
------------|------------
Testing     | directory containing code to test the libraries
packaging   | spec file for building RPMs, and template package description for Dev-Cpp (http://www.bloodshed.net/devcpp.html)



## Instructions to build

command     |  description
------------|-------------
"make all"  | results will be left in the directories: bin/ include/ lib/
"make help" | will show more build options

![NIFTI ICON](https://avatars0.githubusercontent.com/u/45666806?s=200&v=4)

