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

#include "itkImageSeriesReader.h"

int itkImageSeriesReaderVectorTest(int ac, char* av[])
{

  if(ac < 3)
  {
    std::cerr << "usage: itkIOTests itkImageSeriesReaderDimensionsTest inputFileName(s)" << std::endl;
    return EXIT_FAILURE;
  }

  typedef itk::VectorImage< unsigned short, 3>            VectorImageType;

  typedef itk::ImageSeriesReader<VectorImageType>   VectorImageSeriesReader;

  VectorImageSeriesReader::FileNamesContainer fnames;
  for (int i = 1; i < ac; ++i)
      fnames.push_back(av[i]);


  std::cout << "testing reading a image series into VecorImage" << std::endl;
   try
    {
    VectorImageSeriesReader::Pointer reader = VectorImageSeriesReader::New();
    reader->SetFileNames(fnames);
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  std::cout << "testing reading image series into ImageOfVectors " << std::endl;
  try
    {
    VectorImageSeriesReader::Pointer reader = VectorImageSeriesReader::New();
    reader->SetFileNames(fnames);
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
