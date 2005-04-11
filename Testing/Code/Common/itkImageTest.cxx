/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageTest.cxx
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

#include <iostream>
#include "itkImage.h"

int itkImageTest(int, char* [] )
{

  typedef itk::Image<float,2> Image;
  Image::Pointer image = Image::New();
  image->DebugOn();
  image->GetSource();
  image->DisconnectPipeline();

  Image::SpacingType spacing; spacing.Fill(1.0);
  Image::PointType origin; origin.Fill(1.0);
  Image::DirectionType direction;
  direction[0][0] = .5;
  direction[0][1] = .7;
  direction[1][0] = .7;
  direction[1][1] = .5;
  image->SetSpacing (spacing);
  image->SetOrigin (origin);
  image->SetDirection (direction);

  double dspacing[Image::ImageDimension] = {2.0, 2.0};
  double dorigin[Image::ImageDimension] = {2.0, 2.0};
  image->SetSpacing (dspacing);
  image->SetOrigin (dorigin);

  float fspacing[Image::ImageDimension] = {3.0, 3.0};
  float forigin[Image::ImageDimension] = {3.0, 3.0};
  image->SetSpacing (fspacing);
  image->SetOrigin (forigin);

  image->Print(std::cout);

  return (EXIT_SUCCESS);
}
