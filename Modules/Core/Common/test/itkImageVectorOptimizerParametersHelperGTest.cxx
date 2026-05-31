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

#include "itkImageVectorOptimizerParametersHelper.h"
#include "itkOptimizerParameters.h"
#include "itkGTest.h"

namespace
{

using ValueType = double;
constexpr itk::SizeValueType ImageDimension{ 2 };
constexpr itk::SizeValueType VectorDimension{ 4 };
using VectorPixelType = itk::Vector<ValueType, VectorDimension>;
using ImageVectorType = itk::Image<VectorPixelType, ImageDimension>;
using ImageVectorPointer = ImageVectorType::Pointer;
using RegionType = ImageVectorType::RegionType;
using SizeType = RegionType::SizeType;
using IndexType = ImageVectorType::IndexType;
using VectorPixelContainer = ImageVectorType::PixelContainer;
using OptimizerParametersType = itk::OptimizerParameters<ValueType>;
using ImageVectorOptimizerParametersHelperType =
  itk::ImageVectorOptimizerParametersHelper<ValueType, VectorDimension, ImageDimension>;

void
testMemoryAccess(OptimizerParametersType & params, ImageVectorPointer imageOfVectors, itk::SizeValueType dimLength)
{
  for (itk::SizeValueType y = 0; y < dimLength; ++y)
  {
    for (itk::SizeValueType x = 0; x < dimLength; ++x)
    {
      IndexType index;
      index[0] = x;
      index[1] = y;

      // The image index returns a N-dim vector, so have to check each
      // element against the values returned by parameter object.
      const itk::OffsetValueType offset = (x + y * dimLength) * VectorDimension;
      VectorPixelType            vectorpixel = imageOfVectors->GetPixel(index);
      for (itk::SizeValueType ind = 0; ind < VectorDimension; ++ind)
      {
        const ValueType paramsValue = params[offset + ind];
        EXPECT_EQ(vectorpixel[ind], paramsValue);
      }
    }
  }
}

} // namespace

/******************************************************/

TEST(ImageVectorOptimizerParametersHelper, ConvertedLegacyTest)
{
  constexpr int dimLength{ 3 };
  auto          size = itk::MakeFilled<SizeType>(dimLength);

  const RegionType region{ size };

  const ImageVectorPointer imageOfVectors = ImageVectorType::New();
  imageOfVectors->SetRegions(region);
  imageOfVectors->Allocate();

  ValueType             vectorinitvalues[VectorDimension] = { 0.0, 0.1, 0.2, 0.3 };
  const VectorPixelType vectorvalues(vectorinitvalues);

  //
  // Fill up the image values with the function
  //
  //   Intensity = f(x,y) = x + 3 * y
  //
  //
  for (int y = 0; y < dimLength; ++y)
  {
    for (int x = 0; x < dimLength; ++x)
    {
      IndexType index;
      index[0] = x;
      index[1] = y;

      const ValueType value = x + y * dimLength;

      VectorPixelType & vectorpixel = imageOfVectors->GetPixel(index);
      vectorpixel.Fill(value);
      vectorpixel += vectorvalues;
    }
  }

  // Create a parameter object and assign the ImageVector helper.
  OptimizerParametersType params;
  auto *                  imageVectorParamsHelper = new ImageVectorOptimizerParametersHelperType;
  // Assign the helper to the parameter object.
  // OptimizerParameters object will manage the helper once its been set.
  params.SetHelper(imageVectorParamsHelper);
  // Set the image in the helper. It will point the parameter data pointer
  // to the image data.
  params.SetParametersObject(imageOfVectors);

  testMemoryAccess(params, imageOfVectors, dimLength);

  // Test MoveDataPointer
  itk::Array<ValueType> array(imageOfVectors->GetPixelContainer()->Size(), 1.23);
  params.MoveDataPointer(array.data_block());

  // Test null image pointer
  params.SetParametersObject(nullptr);
  EXPECT_THROW(params.MoveDataPointer(array.data_block()), itk::ExceptionObject);

  // Test setting an image of wrong type
  using BadImageType = itk::Image<signed char, 2>;
  auto badImage = BadImageType::New();
  EXPECT_THROW(params.SetParametersObject(badImage), itk::ExceptionObject);
}
