/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRayCastInterpolateImageFunctionTest.cxx
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
#include "itkRayCastInterpolateImageFunction.h"


int 
itkRayCastInterpolateImageFunctionTest(
    int itkNotUsed(argc),
    char * itkNotUsed(argv) [] )
{
    std::cout << "Testing RayCastInterpolateImageFunction:\n";

    typedef unsigned char PixelType;
    const unsigned int ImageDimension = 3;

    typedef itk::Image< PixelType, ImageDimension > ImageType;

    typedef ImageType::IndexType    IndexType;
    typedef ImageType::PointType    PointType;
    typedef ImageType::SpacingType  SpacingType;
    typedef ImageType::SizeType     SizeType;
    typedef ImageType::RegionType   RegionType;
    
    /* Allocate a simple test image */
    ImageType::Pointer image = ImageType::New();
    IndexType start;
    start.Fill(0);
    SizeType size;
    size[0] = 30;
    size[1] = 30;
    size[2] = 30;

    RegionType region;
    region.SetIndex(start);
    region.SetSize(size);
    image->SetRegions(region);
    image->Allocate();

    PointType origin;
    origin.Fill(0.0);

    SpacingType spacing;
    spacing.Fill(1.0);

    /* Set origin and spacing of physical coordinates */
    image->SetOrigin(origin);
    image->SetSpacing(spacing);

    /* Initialize the image contents */
    IndexType index;
    for (int slice = 0; slice < 80; slice++) {
        index[2] = slice;
        for (int row = 0; row < 40; row++) {
            index[1] = row;
            for (int col = 0; col < 20; col++) {
                index[0] = col;
                image->SetPixel(index, slice+row+col);
            }
        }
    }

    typedef itk::RayCastInterpolateImageFunction<
                      ImageType,double> InterpolatorType;

    /* Create and initialize the interpolator */
    InterpolatorType::Pointer interp = InterpolatorType::New();
    interp->SetInputImage(image);
    interp->Print( std::cout );

   
    return EXIT_SUCCESS;
}











