This is a distribution of SGI's experimental IOSTREAMS library (dated
6/8/200).  It includes a complete version of STL. The original version
of this code can be found at http://www.sgi.com/tech/stl

We distribute this code with ITK because it allows the new C++
IOSTREAMS from the standard library with thread safe versions of the
STL containers.  The version of STL distributed with verion 7.3 of the
MIPspro C++ compiler cannot use the new C++ IOSTREAMS when STL is
built to use thread safe containers. This version of the experimental
IOSTREAMS library fixes this problem.
