ITK Examples Directory
----------------------

This directory contains simple, companion examples to the Insight
Segmentation and Registration Toolkit (ITK). These examples are designed to
demonstrate features of the system; they are not meant to be significant
applications of the software.

Specifically, the purpose of these examples is as follows:

   * Provide simple, minimalist examples of important features of ITK and how
     to use them. The examples have minimal dependencies on outside packages;
     and if they do, there should be CMake flags to turn them off.

   * Provide a consistent set of examples that will work in conjunctions with
     the ITK Software Guide. (The ITK Software Guide is found in the
     separate checkout ITKSoftwareGuide/SoftwareGuide.)

   * Provide a consistent set of examples that will work with ITK tutorials
     and courses.

   * Make sure that the code is well documented, of consistent style, and
     always up-to-date with the current ITK code.

The following is a list of subdirectories with a description of the code
found in them.

   * Installation - a very simple example to demonstrate compiling against
     the ITK libraries and configuring CMake.

   * DataRepresentation - creating images and meshes; shows the basics of
     creating and executing the pipeline

   * Iterators - iterating over images and meshes.

   * Infrastructure - events, observers, factories, smart pointers,
     namespaces, transforms, etc.

   * Numerics - working with VNL; a focus on interface with ITK classes

   * IO - the basics of reading/writing data

   * Filtering - examples of image processing and mesh filters.

   * Segmentation - a suite of basic segmentation examples.

   * Registration - a suite of basic registration methods.

   * GUI - some outside package interface: wxWindows, Qt, FLTK, Tk, VTK. The
     absolute minimal interface.

   * Wrapping - simple examples demonstrating the use of wrapped C++ code
     (e.q., Tcl and Python code)

To learn the software from these examples, you may wish to refer to the
"ITK Software Guide" found in the ITKSoftwareGuide/SoftwareGuide
directory. (Note: ITKSoftwareGuide is a separate Git clone found at
https://itk.org/ITKSoftwareGuide.git).
