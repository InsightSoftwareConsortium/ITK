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

#include "itkGTest.h"

#include "itkImage.h"
#include "itkLabelImageToShapeLabelMapFilter.h"


namespace Math = itk::Math;

namespace
{

class ShapeLabelMapFixture
  : public ::testing::Test
{
public:
  ShapeLabelMapFixture() {}
  ~ShapeLabelMapFixture() {}

protected:
  void SetUp() {}
  void TearDown() {}

  template<unsigned int D, typename TPixelType = unsigned short>
  struct FixtureUtilities
  {
    static const unsigned int Dimension = D;

    typedef TPixelType                       PixelType;
    typedef itk::Image<PixelType, Dimension> ImageType;

    typedef itk::ShapeLabelObject<PixelType, Dimension> LabelObjectType;
    typedef itk::LabelMap<LabelObjectType>              ShapeLabelMapType;


    static typename ImageType::Pointer CreateImage(void)
      {
        typename ImageType::Pointer image = ImageType::New();

        typename ImageType::SizeType imageSize;
        imageSize.Fill(25);
        image->SetRegions(typename ImageType::RegionType(imageSize));
        image->Allocate();
        image->FillBuffer(0);

        return image;
      }


    static typename LabelObjectType::ConstPointer ComputeLabelObject(const ImageType* image,
                                                                     PixelType label = 1)
      {

        typedef itk::LabelImageToShapeLabelMapFilter<ImageType> L2SType;
        typename L2SType::Pointer l2s = L2SType::New();
        l2s->SetInput( image );
        l2s->ComputeFeretDiameterOn();
        l2s->ComputePerimeterOn();
        l2s->ComputeOrientedBoundingBoxOn();
        l2s->Update();
        return l2s->GetOutput()->GetLabelObject(label);
      }
  };


};
}

// The expected results were verified for these tests cases, unless
// the test is marked with the "resulting value" comment. In which
// case the baseline value was just what was computed by the method.


TEST_F(ShapeLabelMapFixture,3D_T1x1x1)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  typedef FixtureUtilities<3> Utils;

  Utils::ImageType::Pointer image( Utils::CreateImage() );

  image->SetPixel(MakeIndex(5,7,9), 1);

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  EXPECT_VECTOR_NEAR(MakeIndex(5,7,9), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  EXPECT_VECTOR_NEAR(MakeSize(1,1,1), labelObject->GetBoundingBox().GetSize(), 1e-99);
  EXPECT_VECTOR_NEAR(MakePoint(5.0,7.0,9.0), labelObject->GetCentroid(), 1e-10);
  EXPECT_EQ(0.0, labelObject->GetElongation());
  EXPECT_VECTOR_NEAR(MakePoint(0.0,0.0,0.0), labelObject->GetEquivalentEllipsoidDiameter(), 1e-10);
  EXPECT_NEAR(4.83598, labelObject->GetEquivalentSphericalPerimeter(), 1e-4); // resulting value
  EXPECT_NEAR(0.62035, labelObject->GetEquivalentSphericalRadius(), 1e-4); // resulting value
  EXPECT_EQ(0.0, labelObject->GetFeretDiameter());
  EXPECT_EQ(0.0, labelObject->GetFlatness());
  EXPECT_EQ(1u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  EXPECT_VECTOR_NEAR(MakeVector(1u,1u,1u), labelObject->GetOrientedBoundingBoxSize(), 1e-10);
  EXPECT_VECTOR_NEAR(MakePoint(4.5,6.5,8.5), labelObject->GetOrientedBoundingBoxOrigin(), 1e-10);
  EXPECT_NEAR(3.004, labelObject->GetPerimeter(), 1e-4); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_EQ(1.0, labelObject->GetPhysicalSize());
  // labelObject->GetPrincipalAxes();  degenerate case
  EXPECT_EQ(MakeVector(0.0,0.0,0.0), labelObject->GetPrincipalMoments());
  EXPECT_NEAR(1.6098, labelObject->GetRoundness(), 0.0001); // resulting value

  EXPECT_EQ(labelObject->GetBoundingBox(), labelObject->GetRegion());

  if (::testing::Test::HasFailure())
    {
    labelObject->Print(std::cout);
    }
}

TEST_F(ShapeLabelMapFixture,3D_T3x2x1)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  typedef FixtureUtilities<3> Utils;

  Utils::ImageType::Pointer image( Utils::CreateImage() );

  for (unsigned int i = 5; i < 8; ++i)
    {
    image->SetPixel(MakeIndex(i,9,11), 1);
    image->SetPixel(MakeIndex(i,10,11), 1);
    }

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  EXPECT_VECTOR_NEAR(MakeIndex(5,9,11), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  EXPECT_VECTOR_NEAR(MakeSize(3,2,1), labelObject->GetBoundingBox().GetSize(), 1e-99);
  EXPECT_EQ(MakePoint(6,9.5,11.0), labelObject->GetCentroid());
  EXPECT_NEAR(1.63299, labelObject->GetElongation(), 1e-4);
  EXPECT_VECTOR_NEAR(MakePoint(0.0,0.0,0.0), labelObject->GetEquivalentEllipsoidDiameter(), 1e-10);
  EXPECT_NEAR(15.96804, labelObject->GetEquivalentSphericalPerimeter(), 1e-4); // resulting value
  EXPECT_NEAR(1.12725, labelObject->GetEquivalentSphericalRadius(), 1e-4); // resulting value
  EXPECT_NEAR(2.23606, labelObject->GetFeretDiameter(), 1e-4);
  EXPECT_EQ(0.0, labelObject->GetFlatness());
  EXPECT_EQ(6u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  EXPECT_VECTOR_NEAR(MakeVector(1u,2u,3u), labelObject->GetOrientedBoundingBoxSize(),1e-10);
  EXPECT_VECTOR_NEAR(MakePoint(7.5, 8.5, 10.5), labelObject->GetOrientedBoundingBoxOrigin(), 1e-10);
  EXPECT_NEAR(14.62414, labelObject->GetPerimeter(), 1e-4); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_EQ(6.0, labelObject->GetPhysicalSize());
  // labelObject->GetPrincipalAxes(); omitted
  EXPECT_VECTOR_NEAR(MakeVector(0, 0.25, 0.666667), labelObject->GetPrincipalMoments(),1e-4);
  EXPECT_NEAR(1.09189, labelObject->GetRoundness(), 1e-4); // resulting value

  EXPECT_EQ(labelObject->GetBoundingBox(), labelObject->GetRegion());

  if (::testing::Test::HasFailure())
    {
    labelObject->Print(std::cout);
    }
}


TEST_F(ShapeLabelMapFixture,3D_T3x2x1_Direction)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  typedef FixtureUtilities<3> Utils;

  Utils::ImageType::Pointer image( Utils::CreateImage() );

  for (unsigned int i = 5; i < 8; ++i)
    {
    image->SetPixel(MakeIndex(i,9,11), 1);
    image->SetPixel(MakeIndex(i,10,11), 1);
    }

  DirectionType direction;

  const double d[9] = {0.7950707161543119, -0.44533237368675166, 0.41175433605536305,
                       -0.6065167008084678, -0.5840224148057925, 0.5394954222649374,
                       0.00021898465942798317, -0.6786728931900383, -0.7344406416415056};

  direction = DirectionType::InternalMatrixType(d);

  image->SetDirection(direction);

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  EXPECT_VECTOR_NEAR(MakeIndex(5,9,11), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  EXPECT_VECTOR_NEAR(MakeSize(3,2,1), labelObject->GetBoundingBox().GetSize(), 1e-99);
  EXPECT_VECTOR_NEAR(MakePoint(5.06906, -3.25286, -14.5249), labelObject->GetCentroid(), 1e-4);
  EXPECT_NEAR(1.63299, labelObject->GetElongation(), 1e-4);
  //EXPECT_VECTOR_NEAR(MakePoint(0.0,0.0,0.0), labelObject->GetEquivalentEllipsoidDiameter(), 1e-10);
  EXPECT_NEAR(15.96804, labelObject->GetEquivalentSphericalPerimeter(), 1e-4); // resulting value
  EXPECT_NEAR(1.12725, labelObject->GetEquivalentSphericalRadius(), 1e-4); // resulting value
  EXPECT_NEAR(2.23606, labelObject->GetFeretDiameter(), 1e-4);
  //EXPECT_EQ(0.0, labelObject->GetFlatness()); unstable due to division near zero
  EXPECT_EQ(6u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  EXPECT_VECTOR_NEAR(MakeVector(1u,2u,3u), labelObject->GetOrientedBoundingBoxSize(),1e-10);
  EXPECT_VECTOR_NEAR(MakePoint(3.22524, -3.19685, -14.83670), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);
  EXPECT_NEAR(14.62414, labelObject->GetPerimeter(), 1e-4); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_EQ(6.0, labelObject->GetPhysicalSize());
  //labelObject->GetPrincipalAxes(); omitted
  EXPECT_VECTOR_NEAR(MakeVector(0, 0.25, 0.666667), labelObject->GetPrincipalMoments(),1e-4);
  EXPECT_NEAR(1.09189, labelObject->GetRoundness(), 1e-4); // resulting value

  if (::testing::Test::HasFailure())
    {
    labelObject->Print(std::cout);
    }
}


TEST_F(ShapeLabelMapFixture,3D_T2x2x2_Spacing)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  typedef FixtureUtilities<3> Utils;

  Utils::ImageType::Pointer image( Utils::CreateImage() );

  for (unsigned int i = 0; i < 2; ++i)
    {
    for (unsigned int j = 0; j < 2; ++j)
      {
      image->SetPixel(MakeIndex(5+j,9+i,11), 1);
      image->SetPixel(MakeIndex(5+j,9+i,12), 1);
      }
    }

  image->SetSpacing(MakeVector(1.0,1.1,2.2));

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  EXPECT_VECTOR_NEAR(MakeIndex(5,9,11), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  EXPECT_VECTOR_NEAR(MakeSize(2,2,2), labelObject->GetBoundingBox().GetSize(), 1e-99);
  EXPECT_VECTOR_NEAR(MakePoint(5.5, 10.45, 25.3), labelObject->GetCentroid(), 1e-4);
  EXPECT_NEAR(2.0, labelObject->GetElongation(), 1e-4);
  EXPECT_VECTOR_NEAR(MakePoint(2.4814, 2.72954, 5.45908), labelObject->GetEquivalentEllipsoidDiameter(), 1e-4); // resulting value
  EXPECT_NEAR(34.86751, labelObject->GetEquivalentSphericalPerimeter(), 1e-4); // resulting value
  EXPECT_NEAR(1.66573, labelObject->GetEquivalentSphericalRadius(), 1e-4); // resulting value
  EXPECT_NEAR(2.65518, labelObject->GetFeretDiameter(), 1e-4);
  EXPECT_NEAR(1.1, labelObject->GetFlatness(), 1e-4);
  EXPECT_EQ(8u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  EXPECT_VECTOR_NEAR(MakeVector(2, 2.2, 4.4), labelObject->GetOrientedBoundingBoxSize(),1e-10);
  EXPECT_NEAR(28.3919, labelObject->GetPerimeter(), 1e-4); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_NEAR(19.36, labelObject->GetPhysicalSize(), 1e-10);
  // We are omitted these because the sign of the eigen vectors is not
  //unique, therefore the axes may not always point in the same
  //direction and the origin may not be the same corner
  //EXPECT_VECTOR_NEAR(MakePoint(4.5, 9.35, 23.1), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);
  //labelObject->GetPrincipalAxes(); omitted
  EXPECT_VECTOR_NEAR(MakeVector(0.25, 0.3025, 1.21), labelObject->GetPrincipalMoments(),1e-4);
  EXPECT_NEAR( 1.22808, labelObject->GetRoundness(), 1e-4); // resulting value

  if (::testing::Test::HasFailure())
    {
    labelObject->Print(std::cout);
    }
}


TEST_F(ShapeLabelMapFixture,3D_T2x2x2_Spacing_Direction)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  typedef FixtureUtilities<3> Utils;

  Utils::ImageType::Pointer image( Utils::CreateImage() );

  DirectionType direction;

  const double d[9] = {0.7950707161543119, -0.44533237368675166, 0.41175433605536305,
                       -0.6065167008084678, -0.5840224148057925, 0.5394954222649374,
                       0.00021898465942798317, -0.6786728931900383, -0.7344406416415056};

  direction = DirectionType::InternalMatrixType(d);

  image->SetDirection(direction);

  for (unsigned int i = 0; i < 2; ++i)
    {
    for (unsigned int j = 0; j < 2; ++j)
      {
      image->SetPixel(MakeIndex(5+j,9+i,11), 1);
      image->SetPixel(MakeIndex(5+j,9+i,12), 1);
      }
    }

  image->SetSpacing(MakeVector(1.0,1.1,2.2));

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);


  EXPECT_VECTOR_NEAR(MakeIndex(5,9,11), labelObject->GetBoundingBox().GetIndex(), 1e-99);
  EXPECT_VECTOR_NEAR(MakeSize(2,2,2), labelObject->GetBoundingBox().GetSize(), 1e-99);
  EXPECT_VECTOR_NEAR(MakePoint(10.13655, 4.21035, -25.67227), labelObject->GetCentroid(), 1e-4); // resulting value
  EXPECT_NEAR(2.0, labelObject->GetElongation(), 1e-4);
  EXPECT_VECTOR_NEAR(MakePoint(2.4814, 2.72954, 5.45908), labelObject->GetEquivalentEllipsoidDiameter(), 1e-4); // resulting value
  EXPECT_NEAR(34.86751, labelObject->GetEquivalentSphericalPerimeter(), 1e-4); // resulting value
  EXPECT_NEAR(1.66573, labelObject->GetEquivalentSphericalRadius(), 1e-4); // resulting value
  EXPECT_NEAR(2.65518, labelObject->GetFeretDiameter(), 1e-4);
  EXPECT_NEAR(1.1, labelObject->GetFlatness(), 1e-4);
  EXPECT_EQ(8u, labelObject->GetNumberOfPixels());
  EXPECT_EQ(0u, labelObject->GetNumberOfPixelsOnBorder());
  EXPECT_VECTOR_NEAR(MakeVector(2, 2.2, 4.4), labelObject->GetOrientedBoundingBoxSize(),1e-10);
  EXPECT_VECTOR_NEAR(MakePoint(8.92548, 4.27240, -23.31018), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4); // resulting value
  EXPECT_NEAR(28.3919, labelObject->GetPerimeter(), 1e-4); // resulting value
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorder());
  EXPECT_EQ(0.0, labelObject->GetPerimeterOnBorderRatio());
  EXPECT_NEAR(19.36, labelObject->GetPhysicalSize(), 1e-10);
  //labelObject->GetPrincipalAxes(); omitted
  EXPECT_VECTOR_NEAR(MakeVector(0.25, 0.3025, 1.21), labelObject->GetPrincipalMoments(),1e-4);
  EXPECT_NEAR( 1.22808, labelObject->GetRoundness(), 1e-4); // resulting value

  if (::testing::Test::HasFailure())
    {
    labelObject->Print(std::cout);
    }
}


TEST_F(ShapeLabelMapFixture,2D_T1x1)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  typedef FixtureUtilities<2> Utils;

  Utils::ImageType::Pointer image( Utils::CreateImage() );

  image->SetPixel(MakeIndex(5,7), 1);

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);

  EXPECT_VECTOR_NEAR(MakeVector(1.0,1.0), labelObject->GetOrientedBoundingBoxSize(),1e-10);
  EXPECT_VECTOR_NEAR(MakePoint(4.5, 6.5), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);

  if (::testing::Test::HasFailure())
    {
    labelObject->Print(std::cout);
    }
}


TEST_F(ShapeLabelMapFixture,2D_T1_1)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  typedef FixtureUtilities<2> Utils;

  Utils::ImageType::Pointer image( Utils::CreateImage() );

  image->SetPixel(MakeIndex(5,7), 1);
  image->SetPixel(MakeIndex(6,8), 1);

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);

  EXPECT_VECTOR_NEAR(MakeSize(2,2), labelObject->GetBoundingBox().GetSize(), 1e-99);
  EXPECT_VECTOR_NEAR(MakeVector(Math::sqrt2, 2.0*Math::sqrt2), labelObject->GetOrientedBoundingBoxSize(),1e-4);
  EXPECT_VECTOR_NEAR(MakePoint(4.0, 7.0), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);

  if (::testing::Test::HasFailure())
    {
    labelObject->Print(std::cout);
    }
}


TEST_F(ShapeLabelMapFixture,2D_T1_1_FlipDirection)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  typedef FixtureUtilities<2> Utils;

  Utils::ImageType::Pointer image( Utils::CreateImage() );

  image->SetPixel(MakeIndex(5,7), 1);
  image->SetPixel(MakeIndex(6,8), 1);

  DirectionType direction;

  const double d[4] = {0,1.0,
                       1.0, 0};

  direction = DirectionType::InternalMatrixType(d);

  image->SetDirection(direction);


  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);

  EXPECT_VECTOR_NEAR(MakeVector(Math::sqrt2, 2.0*Math::sqrt2), labelObject->GetOrientedBoundingBoxSize(),1e-4);
  EXPECT_VECTOR_NEAR(MakePoint(6.0, 5.0), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);


  if (::testing::Test::HasFailure())
    {
    labelObject->Print(std::cout);
    }
}


TEST_F(ShapeLabelMapFixture,2D_T2x4)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  typedef FixtureUtilities<2> Utils;

  Utils::ImageType::Pointer image( Utils::CreateImage() );

  for (unsigned int i = 4; i < 6; ++i)
    {
    for (unsigned int j = 3; j < 7; ++j)
      {
      image->SetPixel(MakeIndex(i,j), 1);
      }
    }

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(image);

  EXPECT_VECTOR_NEAR(MakeVector(2.0,4.0), labelObject->GetOrientedBoundingBoxSize(),1e-10);
  EXPECT_VECTOR_NEAR(MakePoint(3.5, 2.5), labelObject->GetOrientedBoundingBoxOrigin(), 1e-4);

  if (::testing::Test::HasFailure())
    {
    labelObject->Print(std::cout);
    }
}
