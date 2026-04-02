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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkVoxBoCUBImageIOFactory.h"
#include "itkVoxBoCUBImageIO.h"
#include "itkNumberToString.h"
#include "itkTestingMacros.h"
#include "itksys/SystemTools.hxx"

int
itkVoxBoCUBImageIOTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage OutputImage" << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = unsigned short;
  using ImageType = itk::Image<PixelType, 3>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  itk::VoxBoCUBImageIOFactory::RegisterOneFactory();

  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  writer->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // Precision test: verify that voxel spacing round-trips without the
  // 6-digit truncation introduced by default stream precision.
  //
  // The VoxBoCUBImageIO write path serialises m_Spacing[] as text in the CUB
  // header.  Without the ConvertNumberToString fix the string has only 6
  // significant digits, so reading back produces a different double value.
  //
  // This sub-test FAILS against the unfixed VoxBoCUBImageIO.
  {
    // Choose a spacing value that needs more than 6 significant decimal digits.
    constexpr double hpSpacing = 0.123456789012345;

    auto                precImage = ImageType::New();
    ImageType::SizeType precSize;
    precSize.Fill(2);
    precImage->SetRegions(precSize);
    precImage->Allocate(true);

    ImageType::SpacingType spacing;
    spacing[0] = hpSpacing;
    spacing[1] = hpSpacing;
    spacing[2] = hpSpacing;
    precImage->SetSpacing(spacing);

    const std::string precFname = std::string(argv[2]) + "_precision.cub";

    auto precWriter = WriterType::New();
    precWriter->SetFileName(precFname.c_str());
    precWriter->SetImageIO(itk::VoxBoCUBImageIO::New());
    precWriter->SetInput(precImage);

    auto precReader = ReaderType::New();
    precReader->SetFileName(precFname.c_str());
    precReader->SetImageIO(itk::VoxBoCUBImageIO::New());

    ITK_TRY_EXPECT_NO_EXCEPTION(precWriter->Update());
    ITK_TRY_EXPECT_NO_EXCEPTION(precReader->Update());

    const double readSpacing = precReader->GetOutput()->GetSpacing()[0];
    itksys::SystemTools::RemoveFile(precFname);
    if (readSpacing != hpSpacing)
    {
      std::cerr << "VoxBoCUB spacing precision loss: wrote " << itk::ConvertNumberToString(hpSpacing)
                << " but read back " << itk::ConvertNumberToString(readSpacing) << '\n';
      return EXIT_FAILURE;
    }
  }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
