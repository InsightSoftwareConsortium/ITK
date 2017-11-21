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

#include "itkKrcahEigenToScalarFunctorImageFilter.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkTestingMacros.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"

int itkKrcahEigenToScalarFunctorImageFilterTest( int argc, char * argv[] )
{
  /* typedefs, instantiate filter */
  const unsigned int                              Dimension = 3;
  typedef double                                  ImagePixelType;
  typedef itk::Image< ImagePixelType, Dimension > ImageType;

  typedef double                                        EigenValueType;
  typedef itk::FixedArray< EigenValueType, Dimension >  EigenValueArrayType;
  typedef itk::Image< EigenValueArrayType, Dimension >  EigenValueImageType;

  typedef itk::KrcahEigenToScalarFunctorImageFilter< EigenValueImageType, ImageType> FilterType;
  FilterType::Pointer krcahFilter = FilterType::New();

  /* Basic tests */
  EXERCISE_BASIC_OBJECT_METHODS( krcahFilter, KrcahEigenToScalarFunctorImageFilter, UnaryFunctorImageFilter );

  /* Exercise basic set/get methods */
  krcahFilter->SetAlpha(0.5);
  TEST_SET_GET_VALUE(0.5, krcahFilter->GetAlpha());
  krcahFilter->SetBeta(0.5);
  TEST_SET_GET_VALUE(0.5, krcahFilter->GetBeta());
  krcahFilter->SetGamma(0.25);
  TEST_SET_GET_VALUE(0.25, krcahFilter->GetGamma());
  // Default should be -1
  TEST_SET_GET_VALUE(-1.0, krcahFilter->GetEnhanceType());
  krcahFilter->SetEnhanceDarkObjects();
  TEST_SET_GET_VALUE(1.0, krcahFilter->GetEnhanceType());
  krcahFilter->SetEnhanceBrightObjects();
  TEST_SET_GET_VALUE(-1.0, krcahFilter->GetEnhanceType());

  /* Create some test data which is computable */
  EigenValueArrayType simpleEigenPixel;
  for (unsigned int i = 0; i < Dimension; ++i) {
    simpleEigenPixel.SetElement(i, 0);
  }

  EigenValueImageType::RegionType region;
  EigenValueImageType::IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;
 
  EigenValueImageType::SizeType size;
  size[0] = 10;
  size[1] = 10;
  size[2] = 10;
 
  region.SetSize(size);
  region.SetIndex(start);
 
  EigenValueImageType::Pointer image = EigenValueImageType::New();
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(simpleEigenPixel);

  krcahFilter->SetInput(image);
  TRY_EXPECT_NO_EXCEPTION(krcahFilter->Update());

  itk::ImageRegionIteratorWithIndex< ImageType > input(krcahFilter->GetOutput(), region);

  input.GoToBegin();
  while ( !input.IsAtEnd() )
  {
    TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( input.Get(), 0.0, 6, 0.000001));
    ++input;
  }

  return EXIT_SUCCESS;
}
