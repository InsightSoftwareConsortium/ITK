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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNrrdImageIO.h"
#include "itkTestingMacros.h"
#include "itkVectorImage.h"

// Specific ImageIO test

namespace
{

template <typename T>
void
checkField(itk::MetaDataDictionary & metadata, const std::string & key, const T & expectedValue)
{
  T value{};
  if (!itk::ExposeMetaData<T>(metadata, key, value))
  {
    std::cerr << "Metadata test failed: '" << key << "' is not found" << std::endl;
    exit(EXIT_FAILURE);
  }
  if (value != expectedValue)
  {
    std::cerr << "Metadata test failed: '" << key << "' value is expected to be '" << expectedValue << "', got '"
              << value << "' instead" << std::endl;
    exit(EXIT_FAILURE);
  }
}

} // namespace

int
itkNrrdVectorImageUnitLabelReadTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Input [Output]\n";
    return EXIT_FAILURE;
  }

  const char * filename = argv[1];

  // Create an NRRD image IO object
  using ImageIOType = itk::NrrdImageIO;
  ImageIOType::Pointer imageIO = ImageIOType::New();

  // Read metadata
  if (!imageIO->CanReadFile(filename))
  {
    std::cerr << "Cannot read file: " << filename << std::endl;
    return EXIT_FAILURE;
  }
  imageIO->SetFileName(filename);
  imageIO->ReadImageInformation(); // Read only the header information

  // Read the relevant header information
  itk::MetaDataDictionary  thisDic = imageIO->GetMetaDataDictionary();
  std::vector<std::string> keys = thisDic.GetKeys();

  std::cout << "Metadata: " << std::endl;
  for (const std::string & key : keys)
  {
    // Print all key/value pairs from metadata dictionary for debugging
    if (key == "NRRD_pixel_original_axis")
    {
      int value{ -1 };
      itk::ExposeMetaData<int>(thisDic, key, value);
      std::cout << "  " << key << "=" << value << std::endl;
    }
    else if (key == "NRRD_measurement frame")
    {
      std::vector<std::vector<double>> value;
      itk::ExposeMetaData<std::vector<std::vector<double>>>(thisDic, key, value);
      std::cout << "  " << key << "=";
      for (size_t i = 0; i < value.size(); ++i)
      {
        std::cout << "(";
        for (size_t j = 0; j < value[i].size(); ++j)
        {
          std::cout << value[i][j] << (j < value[i].size() - 1 ? "," : "");
        }
        std::cout << ")";
      }
      std::cout << std::endl;
    }
    else
    {
      std::string value;
      itk::ExposeMetaData<std::string>(thisDic, key, value);
      std::cout << "  " << key << "=" << value << std::endl;
    }
  }

  std::string pixelType = imageIO->GetPixelTypeAsString(imageIO->GetPixelType());
  std::cout << "Pixel type: " << pixelType << std::endl;
  if (pixelType != "vector")
  {
    std::cerr << "Metadata test failed: pixel type is expected to be 'vector', got '" << pixelType << "' instead"
              << std::endl;
    exit(EXIT_FAILURE);
  }

  // General test of axis types
  checkField<std::string>(thisDic, "NRRD_kinds[0]", "domain");
  checkField<std::string>(thisDic, "NRRD_kinds[1]", "domain");
  checkField<std::string>(thisDic, "NRRD_kinds[2]", "domain");

  // Main goal of this test: check that label and units information is available for the pixel component
  checkField<std::string>(thisDic, "NRRD_labels[pixel]", "time");
  checkField<std::string>(thisDic, "NRRD_units[pixel]", "s");

  checkField<int>(thisDic, "NRRD_pixel_original_axis", 0);

  // Test that additional metadata stored in the NRRD file is added to the metadata dictionary
  checkField<std::string>(thisDic, "axis 0 index type", "numeric");
  checkField<std::string>(thisDic, "axis 0 index values", "0.0 0.09 0.18 0.27 0.36 0.45 0.54 0.63 0.72 0.8");

  if (argc < 3)
  {
    return EXIT_SUCCESS;
  }

  // Write the image and read it back: label and units of the pixel component are preserved
  const char * outputFilename = argv[2];
  using ImageType = itk::VectorImage<float, 3>;
  ImageType::Pointer image;
  ITK_TRY_EXPECT_NO_EXCEPTION(image = itk::ReadImage<ImageType>(filename));
  auto writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(image);
  writer->SetFileName(outputFilename);
  writer->SetImageIO(ImageIOType::New());
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  auto writtenImageIO = ImageIOType::New();
  writtenImageIO->SetFileName(outputFilename);
  writtenImageIO->ReadImageInformation();
  itk::MetaDataDictionary writtenDic = writtenImageIO->GetMetaDataDictionary();
  checkField<std::string>(writtenDic, "NRRD_labels[pixel]", "time");
  checkField<std::string>(writtenDic, "NRRD_units[pixel]", "s");

  // The label and units are stored in the standard NRRD fields of the pixel axis, not as custom key/value pairs
  std::ifstream writtenFile(outputFilename, std::ios::binary);
  std::string   header;
  std::string   line;
  while (std::getline(writtenFile, line) && !line.empty())
  {
    header += line + "\n";
  }
  std::cout << "Written header:\n" << header << std::endl;
  if (header.find("labels: \"time\"") == std::string::npos || header.find("units: \"s\"") == std::string::npos)
  {
    std::cerr << "Label and units of the pixel axis are not written into the labels and units fields" << std::endl;
    return EXIT_FAILURE;
  }
  if (header.find("NRRD_labels[pixel]") != std::string::npos || header.find("NRRD_units[pixel]") != std::string::npos)
  {
    std::cerr << "Label and units of the pixel axis are written as custom key/value pairs" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
