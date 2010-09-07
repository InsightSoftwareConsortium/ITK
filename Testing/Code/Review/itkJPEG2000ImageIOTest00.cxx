/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: ImageReadRegionOfInterestWrite.cxx,v $
  Language:  C++
  Date:      $Date: 2005/08/27 01:46:11 $
  Version:   $Revision: 1.12 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include "itkJPEG2000ImageIO.h"


int itkJPEG2000ImageIOTest00( int /*argc */, char * /*argv*/[] )
{
  itk::JPEG2000ImageIO::Pointer imageIO = itk::JPEG2000ImageIO::New();

  std::cout << "ClassName = " << imageIO->GetNameOfClass() << std::endl;

  imageIO->Print( std::cout );

  return EXIT_SUCCESS;
}
