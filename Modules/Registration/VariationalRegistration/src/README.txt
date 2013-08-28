

This directory contains a template for a submission to the Insight Journal.

You may want to build it first the way it is, in order to get familiar with the
build process.  Then you may want to modify some of the files and add new files.

The files that you SHOULD NOT modify are

            IJMacros.txt
            ImageCompare.cxx
            CMakeTemplate.txt

The files that you will NOT need for a submission are

            README.txt (the file that you are reading).


The files that you will need to modify are


            CMakeLists.txt
            ImageCopy.cxx
            img1.png

The CMakeLists.txt file is actually built from the CMakeTemplate.txt above. You
want to edit this file and put the details of your own project. In particular,
add the names of the .cxx files that must be compiled in order to build your
source code.

The ImageCopy.cxx is a trivial example of a filter that don't do anything. It
is just a bogus example of how to write the code. You will not need this file
of a submission.

The img1.png is an input file for the test. You don't need this file for a
submission. It must be replaced with the normal input of your algoritm, as well
as the output images that will allow the testing system to verify that your
algorithm is working as advertised in your paper.

Post any questions to the ITK users mailing-list.
