/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrthogonalSwath2DPathFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkOrthogonallyCorrected2DParametricPath.h"
#include "itkImage.h"
#include "itkPolyLineParametricPath.h"
#include "itkOrthogonalSwath2DPathFilter.h"

int itkOrthogonalSwath2DPathFilterTest(int, char*[])
{
  typedef itk::PolyLineParametricPath<2>                    InputPathType;  
  typedef itk::Image<double, 2>                             InputImageType; 
  typedef itk::OrthogonallyCorrected2DParametricPath        OutputPathType; 

  typedef itk::OrthogonalSwath2DPathFilter
                        <InputPathType,InputImageType>     FilterType;
  
  // Setup the inputs
  InputPathType::Pointer   inPath  = InputPathType::New();
  InputImageType::Pointer  inImage = InputImageType::New();
  
  // Setup the filter
  FilterType::Pointer filter = FilterType::New();
  filter->SetPathInput(inPath);
  filter->SetImageInput(inImage);
  
  // Setup the output
  OutputPathType::Pointer            outPath;
  outPath=filter->GetOutput();
  
  return EXIT_SUCCESS;
}


