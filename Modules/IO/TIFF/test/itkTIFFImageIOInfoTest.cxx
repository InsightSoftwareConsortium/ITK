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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkMetaDataObject.h"
#include "itkTestingMacros.h"
#include "itkTIFFImageIO.h"


int
itkTIFFImageIOInfoTest(int argc, char * argv[])
{

  if (argc != 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " input" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string filename = argv[1];

  auto tiffImageIO = itk::TIFFImageIO::New();
  tiffImageIO->SetFileName(filename);
  tiffImageIO->ReadImageInformation();


  std::cout << "Dimensions: ( ";
  for (unsigned int d = 0; d < tiffImageIO->GetNumberOfDimensions(); ++d)
  {
    std::cout << tiffImageIO->GetDimensions(d) << " ";
  }
  std::cout << ")" << std::endl;

  std::cout << "Origin: ( ";
  for (unsigned int d = 0; d < tiffImageIO->GetNumberOfDimensions(); ++d)
  {
    std::cout << tiffImageIO->GetOrigin(d) << " ";
  }
  std::cout << ")" << std::endl;

  std::cout << "Spacing: ( ";
  for (unsigned int d = 0; d < tiffImageIO->GetNumberOfDimensions(); ++d)
  {
    std::cout << tiffImageIO->GetSpacing(d) << " ";
  }
  std::cout << ")" << std::endl;

  using DictionaryType = itk::MetaDataDictionary;
  using MetaDataStringType = itk::MetaDataObject<std::string>;

  const DictionaryType & dictionary = tiffImageIO->GetMetaDataDictionary();
  auto                   itr = dictionary.Begin();
  auto                   end = dictionary.End();

  std::cout << "MetaDataDictionary" << std::endl;
  while (itr != end)
  {
    itk::MetaDataObjectBase::Pointer entry = itr->second;
    const std::string                tagkey = itr->first;

    MetaDataStringType::Pointer entryvalue = dynamic_cast<MetaDataStringType *>(entry.GetPointer());

    if (entryvalue)
    {
      std::cout << tagkey << ": " << entryvalue->GetMetaDataObjectValue() << std::endl;
    }
    else
    {
      std::cout << tagkey << ": " << entry << std::endl;
    }

    ++itr;
  }

  return EXIT_SUCCESS;
}
