This directory contains a subset of the CREATIS Dicom library
gdcm version 1.0.  We only include enough of distribution to build
gdcm.  We do not include the standard executables that come with
gdcm (PrintHeader, vtkviewer, or any python sample). 

We'd like to thank the Creatis lab for distributing a public GDCM 
IO library, and modifying the license to be BSD compatible
                                                                                                                                                             
Modifications
-------------

We reuse the jpeg library from ITK instead of building the one shipped
with gdcm. Therefore all gdcmjpeg* name were changed to itkjpeg*.
Same thing for include directory
