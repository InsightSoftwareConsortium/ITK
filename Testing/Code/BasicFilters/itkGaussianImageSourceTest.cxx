/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianImageSourceTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImage.h"
#include "itkGaussianImageSource.h"

int main()
{
  // This can be changed!
  const unsigned int dim = 3;

  // Image typedef
  typedef itk::Image< unsigned char, dim > TImageType;

  // Create a gaussian image source
  typedef itk::GaussianImageSource< TImageType > TGaussianSource;
  TGaussianSource::Pointer pSource = TGaussianSource::New();

  // Get the output of the source
  TImageType::Pointer pImage = pSource->GetOutput();
  
  // Run the pipeline
  pSource->Update();

  return EXIT_SUCCESS;
}
