/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInterpolateTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <iostream>

#include "itkImage.h"
#include "itkIndex.h"
#include "itkSize.h"

#include "itkLinearInterpolateImageFunction.h"


typedef itk::Index<3>                              IndexType;
typedef itk::Size<3>                               SizeType;
typedef itk::Image<unsigned short, 3>              ImageType;
typedef itk::LinearInterpolateImageFunction<ImageType>  InterpolatorType;

int 
main(
    int argc,
    char *argv[])
{
    int flag = 0;           /* Did this test program work? */

    std::cout << "Testing image interpolation methods:\n";

    /* Define the image size and physical coordinates */
    SizeType size = {{20, 40, 80}};
    double origin [3] = { 0.5,   0.5,   0.5};
    double spacing[3] = { 0.1,   0.05 , 0.025};

    /* Allocate a simple test image */
    ImageType::Pointer image = ImageType::New();
    ImageType::RegionType region;
    region.SetSize(size);
    image->SetLargestPossibleRegion(region);
    image->SetBufferedRegion(region);
    image->Allocate();

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

    /* Create and initialize the interpolator */
    InterpolatorType::Pointer interp = InterpolatorType::New();
    interp->SetInputImage(image);

    // FIXME: Add trial evaluations near the border and outside the image

    /* Test evaluation at integer coordinates */
    std::cout << "Evaluate at integer coordinates: ";
    IndexType idx = {{10, 20,40}};
    double value1 = interp->Evaluate(idx);
    std::cout << value1 << std::endl;
    if (value1 != 70)  {
        std::cout << "*** Error: correct value is 70" << std::endl;
        flag = 1;
    }

    /* Test evaluation at non-integer coordinates */
    std::cout << "Evaluate at non-integer coordinates: ";
    double point[3] = {5.25, 12.5, 42.0};
    double value2 = interp->Evaluate(point);
    std::cout << value2 << std::endl;
    if (value2 != 59.75) {
        std::cout << "*** Error: correct value is 59.75" << std::endl;
        flag = 1;
    }

    /* Return results of test */
    if (flag != 0) {
        std::cout << "*** Some test failed" << std::endl;
        return flag; }
    else {
        std::cout << "All tests successfully passed" << std::endl;
        return 0; }
}

