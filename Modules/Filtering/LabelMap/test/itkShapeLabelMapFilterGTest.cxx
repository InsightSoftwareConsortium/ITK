/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkGTest.h"

#include "itkImage.h"
#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkTestingMacros.h"

#include <cmath>


namespace Math = itk::Math;

namespace
{

class ShapeLabelMapFixture : public ::testing::Test
{
public:
  ShapeLabelMapFixture() = default;
  ~ShapeLabelMapFixture() override = default;

protected:
  void
  SetUp() override
  {}
  void
  TearDown() override
  {}

  template <unsigned int D, typename TPixelType = unsigned short>
  struct FixtureUtilities
  {
    static const unsigned int Dimension = D;

    using PixelType = TPixelType;
    using ImageType = itk::Image<PixelType, Dimension>;

    using LabelObjectType = itk::ShapeLabelObject<PixelType, Dimension>;
    using ShapeLabelMapType = itk::LabelMap<LabelObjectType>;


    static typename ImageType::Pointer
    CreateImage()
    {
      auto image = ImageType::New();

      auto imageSize = ImageType::SizeType::Filled(25);
      image->SetRegions(typename ImageType::RegionType(imageSize));
      image->AllocateInitialized();

      return image;
    }


    static typename LabelObjectType::ConstPointer
    ComputeLabelObject(const ImageType * image, PixelType label = 1)
    {

      using L2SType = itk::LabelImageToShapeLabelMapFilter<ImageType>;
      auto l2s = L2SType::New();
      l2s->SetInput(image);
      l2s->ComputeFeretDiameterOn();
      l2s->ComputePerimeterOn();
      l2s->ComputeOrientedBoundingBoxOn();
      l2s->Update();
      return l2s->GetOutput()->GetLabelObject(label);
    }

    static bool
    TestListHasPoint(const typename LabelObjectType::OrientedBoundingBoxVerticesType & obbList,
                     const typename LabelObjectType::OrientedBoundingBoxPointType &    pt,
                     double                                                            tolerance = 1e-8)
    {
      for (auto & v : obbList)
      {
        if (pt.EuclideanDistanceTo(v) < tolerance)
        {
          return true;
        }
      }
      return false;
    }

    static int
    TestBasicObjectProperties()
    {
      using L2SType = itk::LabelImageToShapeLabelMapFilter<ImageType>;
      auto l2s = L2SType::New();

      ITK_EXERCISE_BASIC_OBJECT_METHODS(l2s, LabelImageToShapeLabelMapFilter, ImageToImageFilter);


      auto computeFeretDiameter = true;
      ITK_TEST_SET_GET_BOOLEAN(l2s, ComputeFeretDiameter, computeFeretDiameter);

      auto computePerimeter = true;
      ITK_TEST_SET_GET_BOOLEAN(l2s, ComputePerimeter, computePerimeter);

      auto computeOrientedBoundingBox = true;
      ITK_TEST_SET_GET_BOOLEAN(l2s, ComputeOrientedBoundingBox, computeOrientedBoundingBox);

      return EXIT_SUCCESS;
    }
  };
};
} // namespace

// The expected results were verified for these tests cases, unless
// the test is marked with the "resulting value" comment. In which
// case the baseline value was just what was computed by the method.


TEST_F(ShapeLabelMapFixture, BasicObjectProperties)
{
  FixtureUtilities<2>::TestBasicObjectProperties();
  FixtureUtilities<3>::TestBasicObjectProperties();
}

TEST_F(ShapeLabelMapFixture, 3D_T1x1x1)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  using Utils = FixtureUtilities<3>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  image->SetPixel(itk::MakeIndex(5, 7, 9), 1);

  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  ITK_EXPECT_VECTOR_NEAR(itk::MakeIndex(5, 7, 9), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(itk::MakeSize(1, 1, 1), labelObject->GetBoundingBox().GetSize(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(5.0, 7.0, 9.0), labelObject->GetCentroid(), 1e-10);
  EXPECT_EQ(0.0, labelObject->GetElongation());
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(0.0, 0.0, 0.0), labelObject->GetEquivalentEllipsoidDiameter(), 1e-10);
  EXPECT_NEAR(4.8359758620494109, labelObject->GetEquivalentSphericalPerimeter(), 1e-5); // resulting value
  EXPECT_NEAR(0.62035049089940009, labelObject->GetEquivalentSphericalRadius(), 1e-5);   // resulting value
  EXPECT_EQ(0.0, labelObject->GetFeretDiameter());
  EXPECT_EQ(0.0, labelObject->GetFlatness());
  EXPECT_EQ(1u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(1u, 1u, 1u), labelObject->GetOrientedBoundingBoxSize(), 1e-10);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(4.5, 6.5, 8.5), labelObject->GetOrientedBoundingBoxOrigin(), 1e-10);
  EXPECT_NEAR(3.0040803078963907, labelObject->GetPerimeter(), 1e-5); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_EQ(1.0, labelObject->GetPhysicalSize());
  // labelObject->GetPrincipalAxes();  degenerate case
  EXPECT_EQ(itk::MakeVector(0.0, 0.0, 0.0), labelObject->GetPrincipalMoments());
  EXPECT_NEAR(1.6098024574568734, labelObject->GetRoundness(), 1e-5); // resulting value

  EXPECT_EQ(labelObject->GetBoundingBox(), labelObject->GetRegion());

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}

TEST_F(ShapeLabelMapFixture, 3D_T3x2x1)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  using Utils = FixtureUtilities<3>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  for (unsigned int i = 5; i < 8; ++i)
  {
    image->SetPixel(itk::MakeIndex(i, 9, 11), 1);
    image->SetPixel(itk::MakeIndex(i, 10, 11), 1);
  }

  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  ITK_EXPECT_VECTOR_NEAR(itk::MakeIndex(5, 9, 11), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(itk::MakeSize(3, 2, 1), labelObject->GetBoundingBox().GetSize(), 1e-99);
  EXPECT_EQ(itk::MakePoint(6.0, 9.5, 11.0), labelObject->GetCentroid());
  EXPECT_NEAR(1.63299, labelObject->GetElongation(), 1e-4);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(0.0, 0.0, 0.0), labelObject->GetEquivalentEllipsoidDiameter(), 1e-10);
  EXPECT_NEAR(15.96804047389762, labelObject->GetEquivalentSphericalPerimeter(), 1e-5); // resulting value
  EXPECT_NEAR(1.1272516517868265, labelObject->GetEquivalentSphericalRadius(), 1e-5);   // resulting value
  EXPECT_NEAR(2.23606, labelObject->GetFeretDiameter(), 1e-4);
  EXPECT_EQ(0.0, labelObject->GetFlatness());
  EXPECT_EQ(6u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(1u, 2u, 3u), labelObject->GetOrientedBoundingBoxSize(), 1e-10);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(7.5, 8.5, 10.5), labelObject->GetOrientedBoundingBoxOrigin(), 1e-10);
  EXPECT_NEAR(14.624143852112988, labelObject->GetPerimeter(), 1e-5); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_EQ(6.0, labelObject->GetPhysicalSize());
  // labelObject->GetPrincipalAxes(); omitted
  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(0.0, 0.25, 0.666667), labelObject->GetPrincipalMoments(), 1e-4);
  EXPECT_NEAR(1.0918957468809676, labelObject->GetRoundness(), 1e-5); // resulting value

  EXPECT_EQ(labelObject->GetBoundingBox(), labelObject->GetRegion());

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(ShapeLabelMapFixture, 3D_T3x2x1_Direction)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  using Utils = FixtureUtilities<3>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  for (unsigned int i = 5; i < 8; ++i)
  {
    image->SetPixel(itk::MakeIndex(i, 9, 11), 1);
    image->SetPixel(itk::MakeIndex(i, 10, 11), 1);
  }

  constexpr itk::SpacePrecisionType d[9] = { 0.7950707161543119,     -0.44533237368675166, 0.41175433605536305,
                                             -0.6065167008084678,    -0.5840224148057925,  0.5394954222649374,
                                             0.00021898465942798317, -0.6786728931900383,  -0.7344406416415056 };

  const DirectionType direction = DirectionType::InternalMatrixType(d);

  image->SetDirection(direction);

  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  ITK_EXPECT_VECTOR_NEAR(itk::MakeIndex(5, 9, 11), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(itk::MakeSize(3, 2, 1), labelObject->GetBoundingBox().GetSize(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(
    itk::MakePoint(5.0690644435107242, -3.2528635005915247, -14.524925635405358), labelObject->GetCentroid(), 1e-4);
  EXPECT_NEAR(1.63299, labelObject->GetElongation(), 1e-4);
  // ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(0.0,0.0,0.0), labelObject->GetEquivalentEllipsoidDiameter(), 1e-10);
  EXPECT_NEAR(15.96804047389762, labelObject->GetEquivalentSphericalPerimeter(), 1e-5); // resulting value
  EXPECT_NEAR(1.1272516517868265, labelObject->GetEquivalentSphericalRadius(), 1e-5);   // resulting value
  EXPECT_NEAR(2.23606, labelObject->GetFeretDiameter(), 1e-4);
  // EXPECT_EQ(0.0, labelObject->GetFlatness()); unstable due to division near zero
  EXPECT_EQ(6u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(1u, 2u, 3u), labelObject->GetOrientedBoundingBoxSize(), 1e-10);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(6.0222153120831488, -4.4769132554776352, -15.570490372427054),
                         labelObject->GetOrientedBoundingBoxOrigin(),
                         1e-4);
  EXPECT_NEAR(14.624143852112988, labelObject->GetPerimeter(), 1e-5); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_EQ(6.0, labelObject->GetPhysicalSize());
  // labelObject->GetPrincipalAxes(); omitted
  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(0.0, 0.25, 0.666667), labelObject->GetPrincipalMoments(), 1e-4);
  EXPECT_NEAR(1.0918957468809676, labelObject->GetRoundness(), 1e-5); // resulting value

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(ShapeLabelMapFixture, 3D_T2x2x2_Spacing)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  using Utils = FixtureUtilities<3>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  for (unsigned int i = 0; i < 2; ++i)
  {
    for (unsigned int j = 0; j < 2; ++j)
    {
      image->SetPixel(itk::MakeIndex(5 + j, 9 + i, 11), 1);
      image->SetPixel(itk::MakeIndex(5 + j, 9 + i, 12), 1);
    }
  }

  image->SetSpacing(itk::MakeVector(1.0, 1.1, 2.2));

  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  ITK_EXPECT_VECTOR_NEAR(itk::MakeIndex(5, 9, 11), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(itk::MakeSize(2, 2, 2), labelObject->GetBoundingBox().GetSize(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(5.5, 10.45, 25.3), labelObject->GetCentroid(), 1e-4);
  EXPECT_NEAR(2.0, labelObject->GetElongation(), 1e-4);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(2.4814019635976252, 2.7295421599572678, 5.4590843199148482),
                         labelObject->GetEquivalentEllipsoidDiameter(),
                         1e-5);                                                          // resulting value
  EXPECT_NEAR(34.867517413417708, labelObject->GetEquivalentSphericalPerimeter(), 1e-5); // resulting value
  EXPECT_NEAR(1.6657337346779293, labelObject->GetEquivalentSphericalRadius(), 1e-5);    // resulting value
  EXPECT_NEAR(2.65518, labelObject->GetFeretDiameter(), 1e-4);
  EXPECT_NEAR(1.1, labelObject->GetFlatness(), 1e-4);
  EXPECT_EQ(8u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(2.0, 2.2, 4.4), labelObject->GetOrientedBoundingBoxSize(), 1e-10);
  EXPECT_NEAR(28.391854811919647, labelObject->GetPerimeter(), 1e-5); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_NEAR(19.36, labelObject->GetPhysicalSize(), 1e-10);
  // Because the sign of the Eigen vectors is not
  // unique, therefore the axes may not always point in the same
  // direction making origin not unique. Therefore we check the expected origin the list of vertices.
  EXPECT_TRUE(Utils::TestListHasPoint(labelObject->GetOrientedBoundingBoxVertices(), itk::MakePoint(4.5, 9.35, 23.1)));
  // labelObject->GetPrincipalAxes(); omitted
  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(0.25, 0.3025, 1.21), labelObject->GetPrincipalMoments(), 1e-4);
  EXPECT_NEAR(1.2280817031643669, labelObject->GetRoundness(), 1e-5); // resulting value

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(ShapeLabelMapFixture, 3D_T2x2x2_Spacing_Direction)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  using Utils = FixtureUtilities<3>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  DirectionType direction;

  constexpr itk::SpacePrecisionType d[9] = { 0.7950707161543119,     -0.44533237368675166, 0.41175433605536305,
                                             -0.6065167008084678,    -0.5840224148057925,  0.5394954222649374,
                                             0.00021898465942798317, -0.6786728931900383,  -0.7344406416415056 };

  direction = DirectionType::InternalMatrixType(d);

  image->SetDirection(direction);

  for (unsigned int i = 0; i < 2; ++i)
  {
    for (unsigned int j = 0; j < 2; ++j)
    {
      image->SetPixel(itk::MakeIndex(5 + j, 9 + i, 11), 1);
      image->SetPixel(itk::MakeIndex(5 + j, 9 + i, 12), 1);
    }
  }

  image->SetSpacing(itk::MakeVector(1.0, 1.1, 2.2));

  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  ITK_EXPECT_VECTOR_NEAR(itk::MakeIndex(5, 9, 11), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(itk::MakeSize(2, 2, 2), labelObject->GetBoundingBox().GetSize(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(10.136550336022847, 4.2103580941358141, -25.672275551739141),
                         labelObject->GetCentroid(),
                         1e-4); // resulting value
  EXPECT_NEAR(2.0, labelObject->GetElongation(), 1e-4);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(2.4814019635976252, 2.7295421599572678, 5.4590843199148482),
                         labelObject->GetEquivalentEllipsoidDiameter(),
                         1e-5);                                                          // resulting value
  EXPECT_NEAR(34.867517413417708, labelObject->GetEquivalentSphericalPerimeter(), 1e-5); // resulting value
  EXPECT_NEAR(1.6657337346779293, labelObject->GetEquivalentSphericalRadius(), 1e-5);    // resulting value
  EXPECT_NEAR(2.65518, labelObject->GetFeretDiameter(), 1e-4);
  EXPECT_NEAR(1.1, labelObject->GetFlatness(), 1e-4);
  EXPECT_EQ(8u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(2.0, 2.2, 4.4), labelObject->GetOrientedBoundingBoxSize(), 1e-10);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(9.7574735481346888, 5.3613400676410672, -28.034804130519035),
                         labelObject->GetOrientedBoundingBoxOrigin(),
                         1e-4);                                       // resulting value
  EXPECT_NEAR(28.391854811919647, labelObject->GetPerimeter(), 1e-5); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_NEAR(19.36, labelObject->GetPhysicalSize(), 1e-10);
  // labelObject->GetPrincipalAxes(); omitted
  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(0.25, 0.3025, 1.21), labelObject->GetPrincipalMoments(), 1e-4);
  EXPECT_NEAR(1.2280817031643669, labelObject->GetRoundness(), 1e-5); // resulting value

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(ShapeLabelMapFixture, 2D_T1x1)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  using Utils = FixtureUtilities<2>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  image->SetPixel(itk::MakeIndex(5, 7), 1);

  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);

  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(1.0, 1.0), labelObject->GetOrientedBoundingBoxSize(), 1e-10);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(4.5, 6.5), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(ShapeLabelMapFixture, 2D_T1_1)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  using Utils = FixtureUtilities<2>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  image->SetPixel(itk::MakeIndex(5, 7), 1);
  image->SetPixel(itk::MakeIndex(6, 8), 1);

  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);

  ITK_EXPECT_VECTOR_NEAR(itk::MakeSize(2, 2), labelObject->GetBoundingBox().GetSize(), 1e-99);
  ITK_EXPECT_VECTOR_NEAR(
    itk::MakeVector(Math::sqrt2, 2.0 * Math::sqrt2), labelObject->GetOrientedBoundingBoxSize(), 1e-4);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(4.0, 7.0), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(ShapeLabelMapFixture, 2D_T1_1_FlipDirection)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  using Utils = FixtureUtilities<2>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  image->SetPixel(itk::MakeIndex(5, 7), 1);
  image->SetPixel(itk::MakeIndex(6, 8), 1);

  constexpr itk::SpacePrecisionType d[4]{ 0, 1.0, 1.0, 0 };

  const DirectionType direction = DirectionType::InternalMatrixType(d);

  image->SetDirection(direction);


  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);

  ITK_EXPECT_VECTOR_NEAR(
    itk::MakeVector(Math::sqrt2, 2.0 * Math::sqrt2), labelObject->GetOrientedBoundingBoxSize(), 1e-4);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(6.0, 5.0), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);


  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(ShapeLabelMapFixture, 2D_T1_2_Direction)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  using Utils = FixtureUtilities<2>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  image->SetPixel(itk::MakeIndex(5, 7), 1);
  image->SetPixel(itk::MakeIndex(5, 8), 1);

  constexpr itk::SpacePrecisionType d[4]{ 0, 1.0, 1.0, 0 };

  const DirectionType direction = DirectionType::InternalMatrixType(d);

  image->SetDirection(direction);


  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  auto obbVertices = labelObject->GetOrientedBoundingBoxVertices();

  EXPECT_EQ(obbVertices.Size(), 4);

  EXPECT_TRUE(Utils::TestListHasPoint(obbVertices, itk::MakePoint(8.5, 4.5)));
  EXPECT_TRUE(Utils::TestListHasPoint(obbVertices, itk::MakePoint(8.5, 5.5)));
  EXPECT_TRUE(Utils::TestListHasPoint(obbVertices, itk::MakePoint(6.5, 4.5)));
  EXPECT_TRUE(Utils::TestListHasPoint(obbVertices, itk::MakePoint(6.5, 5.5)));


  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(ShapeLabelMapFixture, 2D_T2x4)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  using Utils = FixtureUtilities<2>;

  const Utils::ImageType::Pointer image(Utils::CreateImage());

  for (unsigned int i = 4; i < 6; ++i)
  {
    for (unsigned int j = 3; j < 7; ++j)
    {
      image->SetPixel(itk::MakeIndex(i, j), 1);
    }
  }

  const Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);

  ITK_EXPECT_VECTOR_NEAR(itk::MakeVector(2.0, 4.0), labelObject->GetOrientedBoundingBoxSize(), 1e-10);
  ITK_EXPECT_VECTOR_NEAR(itk::MakePoint(3.5, 2.5), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(ShapeLabelMapFixture, 3D_DegenerateFlatObject_NumericallyStableEllipsoidDiameter)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  using Utils = FixtureUtilities<3>;

  auto image = Utils::ImageType::New();

  Utils::ImageType::SizeType imageSize;
  imageSize.Fill(64);
  image->SetRegions(Utils::ImageType::RegionType(imageSize));
  image->AllocateInitialized();

  const itk::SpacePrecisionType d[9] = { 0.7950707161543119,     -0.44533237368675166, 0.41175433605536305,
                                         -0.6065167008084678,    -0.5840224148057925,  0.5394954222649374,
                                         0.00021898465942798317, -0.6786728931900383,  -0.7344406416415056 };
  DirectionType                 direction = DirectionType::InternalMatrixType(d);

  image->SetDirection(direction);
  image->SetSpacing(itk::MakeVector(0.9, 1.1, 1.7));
  image->SetOrigin(itk::MakePoint(1.0e8, -1.0e8, 0.5e8));

  // 1D line with large physical coordinates amplifies numerical cancellation in moment computation.
  for (unsigned int x = 8; x < 56; ++x)
  {
    image->SetPixel(itk::MakeIndex(static_cast<itk::IndexValueType>(x), 32, 31), 1);
  }

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image, 1);

  const auto & principalMoments = labelObject->GetPrincipalMoments();
  const auto & ellipsoidDiameter = labelObject->GetEquivalentEllipsoidDiameter();

  for (unsigned int i = 0; i < 3; ++i)
  {
    EXPECT_TRUE(std::isfinite(principalMoments[i])) << "Principal moment " << i << " is not finite";
    EXPECT_GE(principalMoments[i], 0.0) << "Principal moment " << i << " should be non-negative";
    EXPECT_TRUE(std::isfinite(ellipsoidDiameter[i])) << "Ellipsoid diameter[" << i << "] is not finite";
    EXPECT_GE(ellipsoidDiameter[i], 0.0) << "Ellipsoid diameter[" << i << "] should be non-negative";
  }

  const double maxAbsPrincipalMoment =
    std::max(std::max(std::abs(principalMoments[0]), std::abs(principalMoments[1])), std::abs(principalMoments[2]));

  EXPECT_GT(maxAbsPrincipalMoment, 0.0) << "At least one principal moment should be non-zero";

  const double rawDeterminant = principalMoments[0] * principalMoments[1] * principalMoments[2];
  EXPECT_TRUE(std::isfinite(rawDeterminant)) << "Determinant of principal moments should be finite";
  EXPECT_GE(rawDeterminant, 0.0) << "Product of non-negative principal moments should be non-negative";
}
