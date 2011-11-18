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

#include "itkDICOMImageIO2.h"
#include "itkImageSeriesReader.h"
#include "itkDICOMSeriesFileNames.h"
#include "itkFilterWatcher.h"

int itkDICOMImageSeriesTest(int ac, char* av[])
{

  if( ac < 3 )
    {
    std::cerr << "Usage: " << av[0] << " DicomDirectory ReverseOrder(0/1)\n";
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned short,5>        ImageNDType;
  typedef itk::ImageSeriesReader<ImageNDType> ReaderType;

  itk::DICOMImageIO2::Pointer io = itk::DICOMImageIO2::New();

  // Get the DICOM filenames from the directory
  itk::DICOMSeriesFileNames::Pointer names = itk::DICOMSeriesFileNames::New();
  names->SetDirectory(av[1]);

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileNames(names->GetFileNames());
  reader->SetImageIO(io);
  std::cout << names;

  FilterWatcher watcher(reader);

  try
    {
    if (atoi(av[2]))
      {
      reader->ReverseOrderOn();
      }
    reader->Update();
    reader->GetOutput()->Print(std::cout);
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

}
