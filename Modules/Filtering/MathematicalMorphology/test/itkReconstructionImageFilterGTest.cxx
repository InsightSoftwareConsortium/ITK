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

#include "itkReconstructionByDilationImageFilter.h"
#include "itkReconstructionByErosionImageFilter.h"
#include "itkGTest.h"

#include <string>

namespace
{
using ImageType = itk::Image<unsigned char, 2>;

ImageType::Pointer
MakeImage(unsigned char value)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(ImageType::SizeType::Filled(4)));
  image->Allocate();
  image->FillBuffer(value);
  return image;
}

template <typename TFilter>
std::string
GetPreconditionMessage(unsigned char marker, unsigned char mask)
{
  auto filter = TFilter::New();
  filter->SetMarkerImage(MakeImage(marker));
  filter->SetMaskImage(MakeImage(mask));
  try
  {
    filter->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    return e.GetDescription();
  }
  return {};
}
} // namespace

TEST(ReconstructionImageFilter, PreconditionMessageStatesRequiredOrder)
{
  EXPECT_EQ((GetPreconditionMessage<itk::ReconstructionByErosionImageFilter<ImageType, ImageType>>(1, 2)),
            "Marker pixels must be >= mask pixels.");
  EXPECT_EQ((GetPreconditionMessage<itk::ReconstructionByDilationImageFilter<ImageType, ImageType>>(2, 1)),
            "Marker pixels must be <= mask pixels.");
}
