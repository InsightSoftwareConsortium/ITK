/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumConditionalImageCalculatorTest.cxx
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
#include "itkImage.h"
#include "itkMinimumMaximumConditionalImageCalculator.h"
#include "itkSize.h"
#include "itkImageRegionIterator.h"

int 
itkMinimumMaximumConditionalImageCalculatorTest(int ,char *[] )
{

    typedef itk::Size<3>                                SizeType;
    typedef itk::Image<short, 3>                        ImageType;

    typedef itk::Image<unsigned char, 3>                        MaskImageType;

    typedef itk::MinimumMaximumConditionalImageCalculator<
                           ImageType,MaskImageType>   MinMaxCalculatorType;

    /* Define the image size and physical coordinates */
    SizeType size;
    size[0] = 20;
    size[1] = 20;
    size[2] = 20;
   
    ImageType::PointType origin;
    origin[0] = 0.0;
    origin[1] = 0.0;
    origin[2] = 0.0;

    ImageType::SpacingType spacing;
    spacing[0] = 1.0;
    spacing[1] = 1.0;
    spacing[2] = 1.0;

    int flag = 0;           /* Did this test program work? */

    std::cout << "Testing Minimum and Maximum Image Calulator:\n";

    // Allocate a simple test image
    ImageType::Pointer image = ImageType::New();
    ImageType::RegionType region;
    region.SetSize(size);
    image->SetRegions(region);
    image->Allocate();

    // Set origin and spacing of physical coordinates
    image->SetOrigin(origin);
    image->SetSpacing(spacing);

    // Allocate a simple test mask
    MaskImageType::Pointer mask = MaskImageType::New();
    mask->SetRegions(region);
    mask->Allocate();

    // Set origin and spacing of physical coordinates
    mask->SetOrigin(origin);
    mask->SetSpacing(spacing);


    short minimum = -52;
    short maximum = 103;
    short outside = 500;


    // Initialize the image contents with the minimum value
    image->FillBuffer( outside );
    mask->FillBuffer( 0 );

    // Set the patch in the mask
    MaskImageType::IndexType patchStart;
    patchStart[0] = 4;
    patchStart[1] = 4;
    patchStart[2] = 4;

    MaskImageType::SizeType patchSize;
    patchSize[0] = 8;
    patchSize[1] = 8;
    patchSize[2] = 8;

    MaskImageType::RegionType patchRegion;
    patchRegion.SetSize( patchSize );
    patchRegion.SetIndex( patchStart );

    MaskImageType::PixelType maskValue = 127;

    // Set voxel (10,10,10) to maximum value
    ImageType::IndexType index;
    index[0] = 10;
    index[1] = 10;
    index[2] = 10;
    image->SetPixel(index, maximum);
    mask->SetPixel(index, maskValue);

    // Set voxel (6,5,4) to minimum value
    index[0] = 6;
    index[1] = 5;
    index[2] = 4;
    image->SetPixel(index, minimum);
    mask->SetPixel(index, maskValue);

    // Create and initialize the calculator
    MinMaxCalculatorType::Pointer calculator = MinMaxCalculatorType::New();
    calculator->SetImage( image );
    calculator->SetMaskImage( mask );
    calculator->SetMaskValue( maskValue );
    calculator->Compute();

    calculator->Print(std::cout);

    // Return minimum of intensity
    short minimumResult = calculator->GetMinimum();
    std::cout << "The Minimum intensity value is : " << minimumResult << std::endl;
    std::cout << "Its index position is : " << calculator->GetIndexOfMinimum() << std::endl;

    if(minimumResult != minimum)
      {
      std::cout << "Minimum Value is wrong : " << minimumResult ;
      std::cout << " != " << minimum << std::endl;
      flag = 1;
      }
  
    // Return maximum of intensity
    short maximumResult = calculator->GetMaximum();
    std::cout << "The Maximum intensity value is : " << maximumResult << std::endl;
    std::cout << "Its index position is : " << calculator->GetIndexOfMaximum() << std::endl;

    if(maximumResult != maximum)
      {
      std::cout << "Maximum Value is wrong : " << maximumResult ;
      std::cout << " != " << maximum << std::endl;
      flag = 2;
      }


    // Return results of test 
    if (flag != 0) 
      {
      std::cout << "*** Some tests failed" << std::endl;
      return flag; 
      }
    else 
      {
      std::cout << "All tests successfully passed" << std::endl;
      return 0; 
      }

}

