/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOtsuThresholdImageCalculatorTest.cxx
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
#include "itkOtsuThresholdImageCalculator.h"
#include "itkSize.h"
#include "itkImageRegionIterator.h"

typedef itk::Size<3>                          SizeType;
typedef itk::Image<short, 3>                  ImageType;
typedef itk::OtsuThresholdImageCalculator<ImageType>  CalculatorType;
typedef itk::ImageRegionIterator<ImageType> Iterator;
namespace
{
  
/* Define the image size and physical coordinates */
SizeType size = {{20, 20, 20}};
double origin [3] = { 0.0, 0.0, 0.0};
  double spacing[3] = { 1, 1 , 1};
}

int itkOtsuThresholdImageCalculatorTest(int, char* [] )
{
    int flag = 0;           /* Did this test program work? */

    std::cout << "Testing Minimum and Maximum Image Calulator:\n";

    /* Allocate a simple test image */
    ImageType::Pointer image = ImageType::New();
    ImageType::RegionType region;
    region.SetSize(size);
    image->SetLargestPossibleRegion(region);
    image->SetRequestedRegion(region);
    image->SetBufferedRegion(region);
    image->Allocate();

    /* Set origin and spacing of physical coordinates */
    image->SetOrigin(origin);
    image->SetSpacing(spacing);

    unsigned long numPixels = region.GetNumberOfPixels();

    Iterator iter ( image, image->GetBufferedRegion() );

    short value1 = 10;
    short value2 = 50;
    short range = 5;
    short r2 = range * 2 + 1;

    /* Fill one half of with values of Value1 +- 2 */
    unsigned long i;

    for ( i = 0; i < numPixels / 2; i++ )
      {
      iter.Set( ( i % r2 ) + value1 - range );
      ++iter;
      }

    /* Fill the other half with values of Value2 +- 2 */
    for ( i = numPixels / 2; i < numPixels; i++ )
      {
      iter.Set( ( i % r2 ) + value2 - range );
      ++iter;
      }

    /* Create and initialize the calculator */
    CalculatorType::Pointer calculator = CalculatorType::New();
    calculator->SetImage(image);
    calculator->SetNumberOfHistogramBins( 64);

    calculator->Compute();

    std::cout << "calculator: " << calculator;

    /* Return minimum of intensity */
    short thresholdResult = calculator->GetThreshold();
    std::cout << "The threshold intensity value is : " << thresholdResult << std::endl;

    if ( thresholdResult < value1 || thresholdResult > value2 )
      {
      std::cout << "Threshold Value is wrong : " << thresholdResult << std::endl;
      std::cout << "Should be between " << value1 << " and " << value2 << std::endl;
      flag = 1;
      }  

    /* Return results of test */
    if (flag != 0) 
      {
      std::cout << "*** Some tests failed" << std::endl;
      return EXIT_FAILURE; 
      }
    else 
     {
     std::cout << "All tests successfully passed" << std::endl;
     return EXIT_SUCCESS; 
     }

}
