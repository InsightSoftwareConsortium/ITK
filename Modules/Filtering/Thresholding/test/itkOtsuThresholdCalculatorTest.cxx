/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkOtsuThresholdCalculator.h"
#include "itkImageRegionIterator.h"
#include "itkImageToHistogramFilter.h"

typedef itk::Size<3>                                        SizeType;
typedef itk::Image<short, 3>                                ImageType;
typedef itk::Statistics::ImageToHistogramFilter<ImageType>  HistogramGeneratorType;
typedef HistogramGeneratorType::HistogramType               HistogramType;
typedef itk::OtsuThresholdCalculator<HistogramType>         CalculatorType;
namespace
{

/* Define the image size and physical coordinates */
SizeType size = {{20, 20, 20}};
double origin [3] = { 0.0, 0.0, 0.0};
  double spacing[3] = { 1, 1 , 1};
}

int itkOtsuThresholdCalculatorTest(int, char* [] )
{
    typedef itk::ImageRegionIterator<ImageType> Iterator;

    int flag = 0;           /* Did this test program work? */

    std::cout << "Testing Minimum and Maximum Image Calculator:\n";

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


    HistogramGeneratorType::Pointer histGenerator = HistogramGeneratorType::New();
    histGenerator->SetInput(image);
    HistogramGeneratorType::HistogramSizeType hsize(1);
    hsize[0] = 64;
    histGenerator->SetHistogramSize( hsize );
    histGenerator->SetAutoMinimumMaximum( true );

    /* Create and initialize the calculator */
    CalculatorType::Pointer calculator = CalculatorType::New();
    calculator->SetInput( histGenerator->GetOutput() );

    calculator->Update();

    std::cout << "calculator: " << calculator;
    std::cout << "NumberOfHistogramBins: " << histGenerator->GetOutput()->GetSize();
    std::cout << std::endl;

    /* Return minimum of intensity */
    double thresholdResult = calculator->GetThreshold();
    std::cout << "The threshold intensity value is : " << thresholdResult << std::endl;

    if ( thresholdResult < static_cast<double>(value1) || thresholdResult > static_cast<double>(value2) )
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
