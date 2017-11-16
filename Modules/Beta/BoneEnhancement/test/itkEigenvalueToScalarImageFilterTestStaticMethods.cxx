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

#include "itkEigenvalueToScalarImageFilter.h"
#include "itkTestingMacros.h"

int itkEigenvalueToScalarImageFilterTestStaticMethods( int, char * [] )
{
  const unsigned int                         Dimension = 2;
  typedef int                                PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;
  
  typedef itk::EigenvalueToScalarImageFilter<ImageType, ImageType> EigenvalueToScalarImageFilterType;

  /* Test that the default is order by magnitude */
  TEST_EXPECT_EQUAL(EigenvalueToScalarImageFilterType::EigenValueOrder, EigenvalueToScalarImageFilterType::OrderByMagnitude);

  return EXIT_SUCCESS;
}
