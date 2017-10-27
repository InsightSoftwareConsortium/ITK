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
#include "itkHessianImageFilter.h"
#include "itkImage.h"
#include "itkGaussianImageSource.h"
#include <sstream>


namespace Math = itk::Math;

namespace
{

class HessianImageFilterFixture
  : public ::testing::Test
{
public:
  HessianImageFilterFixture() {}
  ~HessianImageFilterFixture() {}

protected:
  void SetUp() {}
  void TearDown() {}

  template<unsigned int D, typename TPixelType = float >
  struct FixtureUtilities
  {
    static const unsigned int Dimension = D;

    typedef TPixelType                       PixelType;
    typedef itk::Image<PixelType, Dimension> ImageType;

    typedef itk::HessianImageFilter<ImageType> HessianFilterType;

    typedef typename HessianFilterType::OutputImageType HessianImageType;
    typedef typename HessianFilterType::OutputPixelType HessianType;


    static const unsigned int imageSize = 25;

    static typename ImageType::Pointer CreateImage(void)
      {
        typename ImageType::Pointer image = ImageType::New();

        typename ImageType::SizeType size;
        size.Fill(imageSize);

        image->SetRegions(typename ImageType::RegionType(size));
        image->Allocate();
        image->FillBuffer(0);

        return image;
      }

    static typename ImageType::Pointer CreateGaussianImage(void)
      {

        typedef itk::GaussianImageSource<ImageType> GaussianSourceType;

        typename GaussianSourceType::Pointer gaussianSource = GaussianSourceType::New();

        typename ImageType::SizeType size;
        size.Fill(imageSize);

        typename ImageType::SpacingType spacing(1.0);

        gaussianSource->SetSize( size );
        gaussianSource->SetSpacing( spacing );
        gaussianSource->SetMean( itk::FixedArray< double, Dimension>( (imageSize-1)/2.0 ) );
        gaussianSource->SetSigma( itk::FixedArray< double, Dimension>( 10.0 ) );
        gaussianSource->SetNormalized( true );

        gaussianSource->Print(std::cout);
        gaussianSource->Update();

        return gaussianSource->GetOutput();
      }

  };


};
}


TEST_F(HessianImageFilterFixture,BasicMethods)
{
  typedef FixtureUtilities<2> Utils;

  Utils::HessianFilterType::Pointer filter = Utils::HessianFilterType::New();


  std::ostringstream oss;
  filter->Print( oss );

  EXPECT_EQ( "HessianImageFilter", filter->GetNameOfClass() );
  EXPECT_EQ( "ImageToImageFilter", filter->Superclass::GetNameOfClass() );
}

namespace {

template<typename T>
itk::FixedArray<T,3> MakeFixedArray(T v1, T v2, T v3)
{
  const T a[] = {v1,v2,v3};
  return itk::FixedArray<T,3>(a);
}

template<typename T>
itk::FixedArray<T,6> MakeFixedArray(T v1, T v2, T v3,
                                    T v4, T v5,
                                    T v6)
{
  const T a[] = {v1,v2,v3,v4,v5,v6};
  return itk::FixedArray<T,6>(a);
}


}


TEST_F(HessianImageFilterFixture,ValueTest_3D)
{
  typedef FixtureUtilities<3> Utils;

  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;

  Utils::ImageType::Pointer image(Utils::CreateGaussianImage());

  Utils::HessianFilterType::Pointer filter = Utils::HessianFilterType::New();
  filter->SetInput(image);
  filter->Update();

  Utils::HessianImageType::Pointer output = filter->GetOutput();


  std::cout << "Value: " << image->GetPixel(MakeIndex(11,12,12)) << std::endl;
  std::cout << "Value: " << image->GetPixel(MakeIndex(12,12,12)) << std::endl;
  std::cout << "Value: " << image->GetPixel(MakeIndex(13,12,12)) << std::endl;
  std::cout << "Value: " << image->GetPixel(MakeIndex(14,12,12)) << std::endl;

  EXPECT_VECTOR_NEAR(MakeFixedArray(-0.0001615,0.0,0.0,
                                    -0.0001615,0.0,
                                    -0.0001615),
                      output->GetPixel(MakeIndex(12,12,12)),
                      1e-6);

  EXPECT_VECTOR_NEAR(MakeFixedArray(-0.00016,0.0,0.0,
                                    -0.00016,0.0,
                                    -0.00016),
                      output->GetPixel(MakeIndex(10,10,10)),
                      1e-7);

}


TEST_F(HessianImageFilterFixture,ValueTest_2D)
{
  typedef FixtureUtilities<2> Utils;

  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  Utils::ImageType::Pointer image(Utils::CreateImage());


  for(unsigned int i = 0; i < Utils::imageSize;++i )
    {
    image->SetPixel(MakeIndex(10,i), 1);
    }

  typedef FixtureUtilities<2> Utils;

  Utils::HessianFilterType::Pointer filter = Utils::HessianFilterType::New();
  filter->SetInput(image);
  filter->Update();

  Utils::HessianImageType::Pointer output = filter->GetOutput();

  EXPECT_VECTOR_NEAR(MakeFixedArray(-2.0,0.0,0.0),
                     output->GetPixel(MakeIndex(10,10)),
                     1e-6);

  EXPECT_VECTOR_NEAR(MakeFixedArray(-2.0,0.0,0.0),
                     output->GetPixel(MakeIndex(10,0)),
                     1e-6);

  image->SetSpacing(MakeVector(10.0,2.0));
  image->Modified();

  filter->Update();

  EXPECT_VECTOR_NEAR(MakeFixedArray(-.02,0.0,0.0),
                     output->GetPixel(MakeIndex(10,10)),
                     1e-6);

  EXPECT_VECTOR_NEAR(MakeFixedArray(-.02,0.0,0.0),
                     output->GetPixel(MakeIndex(10,0)),
                     1e-6);

}
