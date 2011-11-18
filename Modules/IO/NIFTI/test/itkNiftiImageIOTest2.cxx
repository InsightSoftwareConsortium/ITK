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

#include "itkNiftiImageIOTest.h"

int itkNiftiImageIOTest2(int ac, char* av[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(ac > 1) {
    char *testdir = *++av;
    --ac;
    itksys::SystemTools::ChangeDirectory(testdir);
  }
  if(ac != 4)
    return EXIT_FAILURE;
  char *arg1 = av[1];
  char *arg2 = av[2];
  char *prefix = av[3];
  int test_success = 0;

  typedef itk::Image<signed short, 3> ImageType;
  typedef ImageType::Pointer          ImagePointer;

  if((strcmp(arg1, "true") == 0) && WriteNiftiTestFiles(prefix) == -1)
    {
      return EXIT_FAILURE;
    }

  ImagePointer input;
  try
    {
    typedef itk::ImageFileReader<ImageType> ImageReaderType;
    itk::NiftiImageIO::Pointer io = itk::NiftiImageIO::New();
    ImageReaderType::Pointer imageReader = ImageReaderType::New();
    imageReader->SetImageIO(io);
    imageReader->SetFileName(arg2);
    imageReader->Update();
    input = imageReader->GetOutput();
    input = itk::IOTestHelper::ReadImage<ImageType>(std::string(arg2));
    }
  catch (itk::ExceptionObject &)
    {
      test_success = 1;
    }

  if(strcmp(arg1, "true") == 0)
    {
      return test_success;
    }
  else
    {
      return !test_success;
    }

}
