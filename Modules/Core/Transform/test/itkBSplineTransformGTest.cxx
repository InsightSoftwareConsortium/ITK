/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkGTest.h"
#include "itkBSplineTransform.h"

#include "itkImageRegionConstIterator.h"

namespace
{


// Method which checks that two BSpline are exactly equal
template <typename TParametersValueType, unsigned int NDimensions, unsigned int VSplineOrder>
void
bspline_eq(const itk::BSplineTransform<TParametersValueType, NDimensions, VSplineOrder> * bspline1,
           const itk::BSplineTransform<TParametersValueType, NDimensions, VSplineOrder> * bspline2,
           const std::string &                                                            description = "",
           double                                                                         tolerance = 1e-15)
{
  using BSplineType = itk::BSplineTransform<TParametersValueType, NDimensions, VSplineOrder>;
  // Compare Transform Domain interface

  EXPECT_EQ(bspline1->GetNumberOfFixedParameters(), bspline2->GetNumberOfFixedParameters()) << description;
  ITK_EXPECT_VECTOR_NEAR(bspline1->GetTransformDomainOrigin(), bspline2->GetTransformDomainOrigin(), tolerance)
    << description;
  EXPECT_EQ(bspline1->GetTransformDomainDirection(), bspline2->GetTransformDomainDirection()) << description;
  EXPECT_EQ(bspline1->GetTransformDomainMeshSize(), bspline2->GetTransformDomainMeshSize()) << description;
  ITK_EXPECT_VECTOR_NEAR(
    bspline1->GetTransformDomainPhysicalDimensions(), bspline2->GetTransformDomainPhysicalDimensions(), tolerance)
    << description;

  // check transform parameters interface
  ITK_EXPECT_VECTOR_NEAR(bspline1->GetParameters(), bspline2->GetParameters(), tolerance) << description;
  ITK_EXPECT_VECTOR_NEAR(bspline1->GetFixedParameters(), bspline2->GetFixedParameters(), tolerance) << description;

  ASSERT_EQ(bspline1->GetCoefficientImages().Size(), NDimensions) << description;
  ASSERT_EQ(bspline2->GetCoefficientImages().Size(), NDimensions) << description;

  for (unsigned int i = 0; i < NDimensions; ++i)
  {
    using ImageType = typename BSplineType::ImageType;
    using ImageConstIterator = typename itk::ImageRegionConstIterator<ImageType>;

    typename ImageType::Pointer coeffImage1 = bspline1->GetCoefficientImages()[i];
    typename ImageType::Pointer coeffImage2 = bspline2->GetCoefficientImages()[i];

    ITK_EXPECT_VECTOR_NEAR(coeffImage1->GetOrigin(), coeffImage2->GetOrigin(), tolerance) << description;
    EXPECT_EQ(coeffImage1->GetDirection(), coeffImage2->GetDirection()) << description;
    ITK_EXPECT_VECTOR_NEAR(coeffImage1->GetSpacing(), coeffImage2->GetSpacing(), tolerance) << description;
    EXPECT_EQ(coeffImage1->GetLargestPossibleRegion(), coeffImage2->GetLargestPossibleRegion()) << description;
    EXPECT_EQ(coeffImage1->GetBufferedRegion(), coeffImage2->GetBufferedRegion()) << description;

    ImageConstIterator iter1(coeffImage1, coeffImage1->GetBufferedRegion());
    ImageConstIterator iter2(coeffImage2, coeffImage2->GetBufferedRegion());


    while (!iter1.IsAtEnd() && iter2.IsAtEnd())
    {
      EXPECT_EQ(iter1.Get(), iter2.Get()) << description << " Expected value at: " << iter1.GetIndex();
      ++iter1;
      ++iter2;
    }
  }
}

} // namespace

TEST(ITKBSplineTransform, Construction)
{

  using BSplineType = itk::BSplineTransform<double, 3, 3>;

  BSplineType::Pointer bspline1 = BSplineType::New();
  BSplineType::Pointer bspline2 = BSplineType::New();

  bspline_eq(bspline1.GetPointer(), bspline1.GetPointer());
  bspline_eq(bspline2.GetPointer(), bspline2.GetPointer());
  bspline_eq(bspline1.GetPointer(), bspline2.GetPointer());
  bspline_eq(bspline2.GetPointer(), bspline1.GetPointer());
}

TEST(ITKBSplineTransform, Copying_Clone)
{
  // This test sets up an non-trivial bspline transform then,
  // test various interface to set the parameters to the a new
  // transform. It validates that the result it the same as the initial

  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  using BSplineType = itk::BSplineTransform<double, 2, 3>;
  using ImageType = typename BSplineType::ImageType;
  using ImageIterator = typename itk::ImageRegionIterator<ImageType>;

  typename BSplineType::CoefficientImageArray coeffImageArray;

  ASSERT_EQ(coeffImageArray.Size(), 2);

  SizeType      imageSize = MakeSize(10, 10);
  DirectionType imageDirection; // filled with zeros
  imageDirection(0, 1) = -1;
  imageDirection(1, 0) = 1;

  VectorType imageSpacing = MakeVector(1.1, 1.2);

  PointType imageOrigin = MakePoint(0.9, 0.8);

  coeffImageArray[0] = ImageType::New();

  unsigned short px_value = 3;

  ImageType::Pointer coeffImage;
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    coeffImageArray[i] = ImageType::New();
    coeffImage = coeffImageArray[i];

    coeffImage->SetRegions(RegionType(imageSize));

    coeffImage->SetSpacing(imageSpacing);
    coeffImage->SetOrigin(imageOrigin);
    coeffImage->SetDirection(imageDirection);

    coeffImage->Allocate();

    ImageIterator iter(coeffImage, coeffImage->GetBufferedRegion());


    while (!iter.IsAtEnd())
    {
      iter.Set(px_value++);
      ++iter;
    }
  }

  BSplineType::Pointer bspline1 = BSplineType::New();
  bspline1->SetCoefficientImages(coeffImageArray);

  bspline_eq(bspline1.GetPointer(), bspline1.GetPointer(), "Check after initialization by coefficient images.");

  ITK_EXPECT_VECTOR_NEAR(bspline1->GetTransformDomainOrigin(), MakePoint(-0.3, 1.9), 1e-15);
  EXPECT_EQ(bspline1->GetTransformDomainDirection(), imageDirection);
  EXPECT_EQ(bspline1->GetTransformDomainMeshSize(), MakeSize(7, 7));
  ITK_EXPECT_VECTOR_NEAR(bspline1->GetTransformDomainPhysicalDimensions(), MakeVector(7.7, 8.4), 1e-15);

  BSplineType::Pointer bspline2 = BSplineType::New();
  bspline2->SetFixedParameters(bspline1->GetFixedParameters());
  bspline2->SetParameters(bspline1->GetParameters());

  bspline_eq(bspline1.GetPointer(), bspline2.GetPointer(), "Copying by parameters.");


  bspline2 = BSplineType::New();
  bspline2->SetTransformDomainOrigin(bspline1->GetTransformDomainOrigin());
  bspline2->SetTransformDomainPhysicalDimensions(bspline1->GetTransformDomainPhysicalDimensions());
  bspline2->SetTransformDomainDirection(bspline1->GetTransformDomainDirection());
  bspline2->SetTransformDomainMeshSize(bspline1->GetTransformDomainMeshSize());
  bspline2->SetParameters(bspline1->GetParameters());

  bspline_eq(bspline1.GetPointer(), bspline2.GetPointer(), "Set by transform domain then by parameters.");


  bspline2 = bspline1->Clone();
  bspline_eq(bspline1.GetPointer(), bspline2.GetPointer(), "Clone");
}
