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

#include <set>
#include "itkJPEG2000ImageIO.h"


int
itkJPEG2000ImageIOTest00(int /*argc */, char * /*argv*/[])
{
  itk::JPEG2000ImageIO::Pointer imageIO = itk::JPEG2000ImageIO::New();

  std::cout << "ClassName = " << imageIO->GetNameOfClass() << std::endl;

  imageIO->Print(std::cout);

  // Test streaming enumeration for JPEG2000ImageIOInternalEnums::DecodingFormat elements
  const std::set<itk::JPEG2000ImageIOInternalEnums::DecodingFormat> allDecodingFormat{
    itk::JPEG2000ImageIOInternalEnums::DecodingFormat::J2K_CFMT,
    itk::JPEG2000ImageIOInternalEnums::DecodingFormat::JP2_CFMT,
    itk::JPEG2000ImageIOInternalEnums::DecodingFormat::JPT_CFMT,
    itk::JPEG2000ImageIOInternalEnums::DecodingFormat::MJ2_CFMT
  };
  for (const auto & ee : allDecodingFormat)
  {
    std::cout << "STREAMED ENUM VALUE JPEG2000ImageIOInternalEnums::DecodingFormat: " << ee << std::endl;
  }

  // Test streaming enumeration for JPEG2000ImageIOInternalEnums::DFMFormat elements
  const std::set<itk::JPEG2000ImageIOInternalEnums::DFMFormat> allDFMFormat{
    itk::JPEG2000ImageIOInternalEnums::DFMFormat::PXM_DFMT,
    itk::JPEG2000ImageIOInternalEnums::DFMFormat::PGX_DFMT,
    itk::JPEG2000ImageIOInternalEnums::DFMFormat::BMP_DFMT,
    itk::JPEG2000ImageIOInternalEnums::DFMFormat::YUV_DFMT
  };
  for (const auto & ee : allDFMFormat)
  {
    std::cout << "STREAMED ENUM VALUE JPEG2000ImageIOInternalEnums::DFMFormat: " << ee << std::endl;
  }

  return EXIT_SUCCESS;
}
