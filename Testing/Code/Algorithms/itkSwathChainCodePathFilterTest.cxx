/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSwathChainCodePathFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkChainCodePath2D.h"
#include "itkImage.h"
#include "itkSwathChainCodePathFilter.h"

int itkSwathChainCodePathFilterTest(int, char*[])
{
  typedef itk::ChainCodePath2D                                PathType;  
  typedef itk::Image<double, 2>                               ImageType; 
  typedef itk::SwathChainCodePathFilter <PathType,ImageType>  FilterType;
  
  // Setup the inputs
  PathType::Pointer   inPath  = PathType::New();
  ImageType::Pointer  inImage = ImageType::New();
  
  // Setup the filter
  FilterType::Pointer filter = FilterType::New();
  filter->SetPathInput(inPath);
  filter->SetImageInput(inImage);
  
  // Setup the output
  PathType::Pointer outPath = filter->GetOutput();
  
  return EXIT_SUCCESS;
}


