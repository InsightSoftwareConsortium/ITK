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

//
//  This example illustrates how to read a DICOM series into a volume
//  and then save this volume into another DICOM series using the
//  exact same name.
//  It makes use of the DCMTK library
//

#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkDCMTKImageIO.h"
#include "itkDCMTKSeriesFileNames.h"
#include "itkTestingMacros.h"

int
itkDCMTKSeriesReadImageWrite(int argc, char * argv[])
{
  if (argc != 6)
  {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " DicomDirectory "
              << " outputFile"
              << " recursive"
              << " loadSequences"
              << " loadPrivateTags" << std::endl;
    return EXIT_FAILURE;
  }

  using ImageType = itk::Image<unsigned short, 3>;
  using ReaderType = itk::ImageSeriesReader<ImageType>;
  using ImageIOType = itk::DCMTKImageIO;
  using SeriesFileNames = itk::DCMTKSeriesFileNames;

  auto dcmtkIO = ImageIOType::New();
  auto it = SeriesFileNames::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(it, DCMTKSeriesFileNames, ProcessObject);


  // Test exceptions
  const char * pInputDirectory = nullptr;
  ITK_TRY_EXPECT_EXCEPTION(it->SetInputDirectory(pInputDirectory));

  // Exercise warnings
  std::string inputDirectory = "";
  it->SetInputDirectory(inputDirectory);

  inputDirectory = "NotADirectory";
  it->SetInputDirectory(inputDirectory);

  inputDirectory = argv[1];
  it->SetInputDirectory(inputDirectory);

  auto recursive = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(it, Recursive, recursive);

  auto loadSequences = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(it, LoadSequences, loadSequences);

  auto loadPrivateTags = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(it, LoadPrivateTags, loadPrivateTags);

  auto reader = ReaderType::New();

  const ReaderType::FileNamesContainer & fileNames = it->GetInputFileNames();
  const unsigned int                     numberOfFileNames = fileNames.size();
  std::cout << numberOfFileNames << std::endl;
  for (unsigned int fni = 0; fni < numberOfFileNames; ++fni)
  {
    std::cout << "filename # " << fni << " = ";
    std::cout << fileNames[fni] << std::endl;
  }

  reader->SetFileNames(fileNames);
  reader->SetImageIO(dcmtkIO);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();

  writer->SetFileName(argv[2]);
  writer->SetInput(reader->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
