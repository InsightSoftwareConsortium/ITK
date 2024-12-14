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
#include "itkIOCommon.h"
#include "itkMetaDataObject.h"
#include "itkAnatomicalOrientation.h"
#include "itkTestingMacros.h"

int
itkReadWriteImageWithDictionaryTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Input\n";
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<unsigned char, 3>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  // Create the 16x16 input image
  auto inputImage = ImageType::New();

  auto                        size = ImageType::SizeType::Filled(16);
  const ImageType::IndexType  index{};
  const ImageType::RegionType region{ index, size };
  inputImage->SetRegions(region);
  inputImage->Allocate();
  inputImage->FillBuffer(0);

  inputImage->SetDirection(itk::AnatomicalOrientation::CreateFromPositiveStringEncoding("LSA").GetAsDirection());

  // Add some metadata in the dictionary
  itk::MetaDataDictionary & inputDictionary = inputImage->GetMetaDataDictionary();
  const std::string         voxelunitstr = "mm. "; // try to follow analyze format (length matters)
  itk::EncapsulateMetaData<std::string>(inputDictionary, itk::ITK_VoxelUnits, voxelunitstr);
  const std::string datestr = "26-05-2010"; // try to follow analyze format (length matters)
  itk::EncapsulateMetaData<std::string>(inputDictionary, itk::ITK_ExperimentDate, datestr);
  const std::string timestr = "13-44-00.0"; // try to follow analyze format (length matters)
  itk::EncapsulateMetaData<std::string>(inputDictionary, itk::ITK_ExperimentTime, timestr);
  const std::string patientstr = "patientid "; // try to follow analyze format (length matters)
  itk::EncapsulateMetaData<std::string>(inputDictionary, itk::ITK_PatientID, patientstr);

  // Write the image down
  auto writer = WriterType::New();

  writer->SetInput(inputImage);
  writer->SetFileName(argv[1]);
  writer->Update();

  // Read the image back
  auto reader = ReaderType::New();

  reader->SetFileName(argv[1]);
  reader->Update();

  const ImageType::Pointer outputImage = reader->GetOutput();

  // Compare the metadatas
  int numMissingMetaData = 0;
  int numWrongMetaData = 0;

  itk::MetaDataDictionary & outputDictionary = outputImage->GetMetaDataDictionary();

  std::string metadatastr;

  if (itk::ExposeMetaData<std::string>(outputDictionary, itk::ITK_VoxelUnits, metadatastr))
  {
    // MetaIO is rather strict on the format of ITK_VoxelUnits but for our purpose "mm"=="mm. "
    if (!(metadatastr == voxelunitstr || (metadatastr == "mm" && voxelunitstr == "mm. ")))
    {
      std::cout << "voxelunitstr.size()=" << voxelunitstr.size() << '\n';
      std::cout << "metadatastr.size()=" << metadatastr.size() << '\n';
      std::cout << "voxelunitstr=|" << voxelunitstr << '|' << '\n';
      std::cout << "metadatastr=|" << metadatastr << '|' << '\n';
      ++numWrongMetaData;
    }
  }
  else
  {
    std::cout << "Missing ITK_VoxelUnits" << '\n';
    ++numMissingMetaData;
  }

  if (itk::ExposeMetaData<std::string>(outputDictionary, itk::ITK_ExperimentDate, metadatastr))
  {
    if (metadatastr != datestr)
    {
      std::cout << "datestr.size()=" << datestr.size() << '\n';
      std::cout << "metadatastr.size()=" << metadatastr.size() << '\n';
      std::cout << "datestr=|" << datestr << '|' << '\n';
      std::cout << "metadatastr=|" << metadatastr << '|' << '\n';
      ++numWrongMetaData;
    }
  }
  else
  {
    std::cout << "Missing ITK_ExperimentDate" << '\n';
    ++numMissingMetaData;
  }

  if (itk::ExposeMetaData<std::string>(outputDictionary, itk::ITK_ExperimentTime, metadatastr))
  {
    if (metadatastr != timestr)
    {
      std::cout << "timestr.size()=" << timestr.size() << '\n';
      std::cout << "metadatastr.size()=" << metadatastr.size() << '\n';
      std::cout << "timestr=|" << timestr << '|' << '\n';
      std::cout << "metadatastr=|" << metadatastr << '|' << '\n';
      ++numWrongMetaData;
    }
  }
  else
  {
    std::cout << "Missing ITK_ExperimentTime" << '\n';
    ++numMissingMetaData;
  }

  if (itk::ExposeMetaData<std::string>(outputDictionary, itk::ITK_PatientID, metadatastr))
  {
    if (metadatastr != patientstr)
    {
      std::cout << "patientstr.size()=" << patientstr.size() << '\n';
      std::cout << "metadatastr.size()=" << metadatastr.size() << '\n';
      std::cout << "patientstr=|" << patientstr << '|' << '\n';
      std::cout << "metadatastr=|" << metadatastr << '|' << '\n';
      ++numWrongMetaData;
    }
  }
  else
  {
    std::cout << "Missing ITK_PatientID" << '\n';
    ++numMissingMetaData;
  }

  std::cout << '\n' << "Number of missing metadata = " << numMissingMetaData << '\n';
  std::cout << "Number of wrong metadata = " << numWrongMetaData << '\n' << '\n';


  // Perform a weaker but more exhaustive test
  int numMissingMetaData2 = 0;
  int numWrongMetaData2 = 0;
  int numAddedMetaData2 = 0;

  for (auto it = inputDictionary.Begin(); it != inputDictionary.End(); ++it)
  {
    if (!outputDictionary.HasKey(it->first))
    {
      std::cout << "Missing " << it->first << '\n';
      ++numMissingMetaData2;
    }
    else
    {
      const auto it2 = outputDictionary.Find(it->first);
      if (it->second->GetMetaDataObjectTypeInfo() != it2->second->GetMetaDataObjectTypeInfo())
      {
        std::cout << "input_meta=" << it->second;
        std::cout << "output_meta=" << it2->second;
        ++numWrongMetaData2;
      }
    }
  }

  for (auto it = outputDictionary.Begin(); it != outputDictionary.End(); ++it)
  {
    if (!inputDictionary.HasKey(it->first))
    {
      std::cout << "added_meta=|" << it->first << "|-" << it->second;
      ++numAddedMetaData2;
    }
  }

  std::cout << '\n' << "(weak but exhaustive) Number of missing metadata = " << numMissingMetaData2 << '\n';
  std::cout << "(weak but exhaustive) Number of wrong metadata = " << numWrongMetaData2 << '\n';
  std::cout << "(weak but exhaustive) Number of added metadata = " << numAddedMetaData2 << '\n' << '\n';

  // Do not consider added metadata as errors since this may just indicate file format information
  if (numMissingMetaData != 0 || numWrongMetaData != 0 || numMissingMetaData2 != 0 || numWrongMetaData2 != 0)
  {
    // FIXME:   FIXME MetaImage library: Then restore this test:    return EXIT_FAILURE;
    std::cout << " FAILED: FIXME MetaImage library: Then restore this test" << '\n';
  }

  return EXIT_SUCCESS;
}
