The main portions of the Insight Toolkit are copyrighted 
by the Insight Software Consortium and distributed freely
under the terms listed in the file:

                   ITKCopyright.txt

The Toolkit contains code from third parties that have different
copyrights.

They are:


 1) DICOMParser:  Copyright by Matt Turek.
                  Distributed under a BSD-Like license.
                  [This work was funded by NLM so its copyright
                   should probably be transfered to the ISC.]

 2) AnalyzeDB.h:  Vincent is requesting permission to the Mayo
                  Clinic for transfering the copyright to ITK.
                  Otherwise, a license statement is needed, to
                  make clear that ITK can distribute it.

 3) PixelData:    GE Copyrights,
                  Vincent is contacting the providers of this
                  code too.

 4) OctreeNode:   Vincent will transfer copyright to the ISC.

 5) ThresholdMaximumConnectedComponents: Ken Urish is sending
                  the copyright transfer form to Josh Cates.
                  The copyright has already been fixed in the
                  CVS repository.


 6) Netlib:       Netlib is a conglomerate of packages. Each 
                  one having different copyrights and licenses.
                  Here is a listing of what is included inside
                  Utilities/vxl/v3p/netlib

    blas:         From www.netlib.org/blas :
    
    FAQ: The reference BLAS is a freely-available software package. 
    It is available from netlib via anonymous ftp and the World Wide Web. 
    Thus, it can be included in commercial software packages (and has been). 
    We only ask that proper credit be given to the authors.


    datapac:   Written by James Filliben, National Bureau of Standars.

           Our understanding is that the US Government cannot hold
           copyrights, therefore we assume that this code is in the 
           public domain...

    eispack:

    lapack:

    laso:

    libf2c:

    linalg:

    linpack:

    mathews:

    minpack:

    napack:

    opt:

    temperton:

    tests:

    toms:



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


     in ITK it had a subdir:
     
        fsliolib  :  From the FSL package:
                     University of Oxford: 
                     NO FOR COMMERCIAL USE

                     Therefore this code has been removed !


  E) EXPAT     : Thai Open Source Software Center Ltd and Clark Cooper.
                 with BSD-Like license


  F) PNG       : Copyright: Glen Randers-Pehrson
                            Andreas Dilger
                            Guy Erc Schalnat


  G) TIFF      : Copyright Sam Leffler and Silicon Graphics

  H) GDCM      : Copyright: CREATIS: BSD-Like License

  I) ZLIB      : Copyright:  Mark Adler

  J) kwsys     : Copyright: Kitware : BSD-Like License 


