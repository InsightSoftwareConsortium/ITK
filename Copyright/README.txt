The main portions of the Insight Toolkit are copyrighted 
by the Insight Software Consortium and distributed freely
under the terms listed in the file:

                   ITKCopyright.txt

The Toolkit contains code from third parties that have different
copyrights.

They are:



 1) DICOMParser:  We are testing an experimental build in which
                  this files are removed. This is a deprecated
                  set of files that can be removed before ITK 3.0

 2) AnalyzeDB.h:  Vincent is requesting permission to the Mayo
                  Clinic for transfering the copyright to ITK

 3) PixelData:    GE Copyrights,
                  Vincent is contacting the providers of this
                  code too.

 4) OctreeNode:   Vincent will transfer copyright to the ISC.

 5) ThresholdMaximumConnectedComponents: Ken Urish is sending
                  the copyright transfer form to Josh Cates.
                  The copyfight has already been fixed in the
                  CVS repository.


 6) Netlib:       More on netlib to be added here...


 7) The triangle.c/h and showme.c files have been removed.
                  Their copyright/license was incompatible with
                  ITK and their functionality was not needed.
                  These removed the "itknetlib" library.


 8) itk_hash_map:  It has copyrights by SGI and HP, but the license
                  is consistent with BSD-like. So we simply need
                  to acknowledge those in our general copyright.

 9) itkMersenneTwisterRandomVariateGenerator:
                Copyright: Makoto Matsumoto and Takuji Nishimura,
                Copyright: Richard J. Wagner
                under a BSD-Like license.

 10) Code/Numerics/FEM/dsrc2c.c :

                Copyright: 1977 IMSL Inc, no license !!


 11) Code/Numerics/cvsrmvend

                Copyright:  Greg A. Woods
                "Freely distributable"
                I guess that counts as BSD...
                Easy to remove if it becomes a problem,
                This is just a script for making easier
                the CVS syncs of VXL.

 12) Testing/Code/IO/itkIOHeaderTest.cxx:

                Copyright: National Library of Medicine !!!
                Actually this is created by the Tcl script

          Insight/Utilities/Dart/BuildHeadrTests.tcl

                A fix has been commited for this file...

  13) Testing/Code/Numerics/NeuralNetworks:

                Copyright: CADDLab at UNC
                Fixed now in CVS --> ISC



Utilities:


  A) JPEG      :  Thomas G Lane ( BSD-Like)
               :  DCMTK : OFFIS... and many others... but BSD-Like


  B) OpenJPEG  :  Universite Catholique de Louvain:  BSD license


  C) NrrdIO      :  Gordon Kindlmann and  University of Utah
                  BSD-Like license. Clarifies that the original
                  package "Teem" (not in ITK) is still under GPL license


  D) NIFTI     :  No copyright ?
                  There is no mention of licensing nor copyright
                  at the NIFTI Web site:

                      http://nifti.nimh.nih.gov/nifti-1

                  However at http://niftilib.sourceforge.net it
                  appears as "public domain" license!!, but in
                  its subdirectory matlibs it has a GPL license.
                  its subdirectory jlibs is public domain
                  its subdirecotry clibs is public domain


     in ITK it has a subdir:
     fsliolib  :  From the FSL package:
                  University of Oxford: NO FOR COMMERCIAL USE

                       This code has been removed !


  E) EXPAT     : Thai Open Source Software Center Ltd and Clark Cooper.
                 with BSD-Like license


  F) PNG       : Copyright: Glen Randers-Pehrson
                            Andreas Dilger
                            Guy Erc Schalnat


  G) TIFF      : Copyright Sam Leffler and Silicon Graphics


  H) GDCM      : Copyright: CREATIS:

  I) ZLIB      : Copyright:  Mark Adler

  J) kwsys     : Copyright: Kitware : BSD-Like License 



