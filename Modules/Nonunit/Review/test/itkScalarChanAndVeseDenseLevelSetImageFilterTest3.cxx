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

#include "itkScalarChanAndVeseDenseLevelSetImageFilter.h"
#include "itkUnconstrainedRegionBasedLevelSetFunctionSharedData.h"

int itkScalarChanAndVeseDenseLevelSetImageFilterTest3( int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef double                                  PixelType;
  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::Image< float, Dimension >          FeatureImageType;
  typedef ImageType                               OutputImageType;

  typedef itk::ScalarChanAndVeseLevelSetFunctionData< ImageType, FeatureImageType >  DataHelperType;

  typedef itk::UnconstrainedRegionBasedLevelSetFunctionSharedData< ImageType, FeatureImageType, DataHelperType >
    SharedDataHelperType;

  typedef itk::ScalarChanAndVeseLevelSetFunction<
    ImageType, FeatureImageType, SharedDataHelperType >     RegionBasedLevelSetFunctionType;

  RegionBasedLevelSetFunctionType::Pointer function = RegionBasedLevelSetFunctionType::New();
  if( function.IsNull() )
    {
    return EXIT_FAILURE;
    }

  typedef itk::ScalarChanAndVeseDenseLevelSetImageFilter<
    ImageType, FeatureImageType, OutputImageType,
    RegionBasedLevelSetFunctionType, SharedDataHelperType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  std::cout << "GetNameOfClass() = " << filter->GetNameOfClass() << std::endl;
  filter->Print( std::cout );

  return EXIT_SUCCESS;
}
