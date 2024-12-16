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

#include "itkNiftiImageIOTest.h"
#include "itkTestingMacros.h"

// Specific ImageIO test

template <>
itk::ImageBase<1>::DirectionType
PreFillDirection<1>()
{
  itk::ImageBase<1>::DirectionType myDirection{};
  myDirection[0][0] = -1.0;
  return myDirection;
}

template <>
itk::ImageBase<2>::DirectionType
PreFillDirection<2>()
{
  itk::ImageBase<2>::DirectionType myDirection{};
  myDirection[0][1] = 1.0;
  myDirection[1][0] = -1.0;
  return myDirection;
}

template <>
itk::ImageBase<3>::DirectionType
PreFillDirection<3>()
{
  itk::ImageBase<3>::DirectionType myDirection{};
  myDirection[0][2] = 1.0;
  myDirection[1][0] = -1.0;
  myDirection[2][1] = 1.0;
  return myDirection;
}

template <>
itk::ImageBase<4>::DirectionType
PreFillDirection<4>()
{
  itk::ImageBase<4>::DirectionType myDirection{};
  myDirection[0][2] = 1.0;
  myDirection[1][0] = -1.0;
  myDirection[2][1] = 1.0;
  myDirection[3][3] = 1.0;
  return myDirection;
}

bool
Equal(const double a, const double b)
{
  // actual equality
  double diff = a - b;
  if (diff == 0.0)
  {
    return true;
  }
  // signs match?
  if ((a < 0.00 && b >= 0.0) || (b < 0.0 && a >= 0.0))
  {
    return false;
  }
  if (diff < 0.0)
  {
    diff = -diff;
  }
  double avg = (a + b) / 2.0;
  if (avg < 0.0)
  {
    avg = -avg;
  }
  if (diff > avg / 1000.0)
  {
    return false;
  }
  return true;
}

int
itkNiftiImageIOTest(int argc, char * argv[])
{
  itk::ObjectFactoryBase::UnRegisterAllFactories();
  itk::NiftiImageIOFactory::RegisterOneFactory();
  int rval = 0;
  if (argc < 2)
  {
    std::cerr << "testFileName required." << std::endl;
    return EXIT_FAILURE;
  }
  //
  // first argument is the test filepath to do all testing
  const char * testFileName = *++argv;
  --argc;

  std::string prefix = "";
  if (argc > 1)
  {
    prefix = *++argv;
    --argc;
  }
  static bool firstTime = true;
  if (firstTime)
  {
    itk::ObjectFactoryBase::RegisterFactory(itk::NiftiImageIOFactory::New());
    firstTime = false;
  }
  if (argc > 1) // This is a mechanism for reading unsigned char images for testing.
  {
    using ImageType = itk::Image<unsigned char, 3>;
    const itk::NiftiImageIO::Pointer imageIO = itk::NiftiImageIO::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(imageIO, NiftiImageIO, ImageIOBase);


    // Enable old behavior of NIFTI reader
    imageIO->SetLegacyAnalyze75Mode(itk::NiftiImageIOEnums::Analyze75Flavor::AnalyzeITK4);

    for (int imagenameindex = 1; imagenameindex < argc; ++imagenameindex)
    {
      auto fileName = std::string(argv[imagenameindex]);

      // The way the test is structured, we cannot know the expected file
      // type, so just print it
      const typename itk::NiftiImageIOEnums::NiftiFileEnum fileType = imageIO->DetermineFileType(fileName.c_str());
      std::cout << "File type: " << fileType << std::endl;

      try
      {
        const ImageType::Pointer input = itk::IOTestHelper::ReadImage<ImageType>(fileName, false, imageIO);
      }
      catch (const itk::ExceptionObject & e)
      {
        e.Print(std::cerr);
        rval = 1;
      }
    }
  }
  else // This is the mechanism for doing internal testing of all data types.
  {
    int cur_return = MakeNiftiImage<char>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type char" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned char>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned char" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<short>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type short" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned short>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned short" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<int>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type int" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned int>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned int" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<long>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type long" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned long>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned long" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<long long>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type long long" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<unsigned long long>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type unsigned long long" << std::endl;
      rval += cur_return;
    }
    cur_return = MakeNiftiImage<float>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type float" << std::endl;
      rval += cur_return;
    }
    // awaiting a double precision byte swapper
    cur_return = MakeNiftiImage<double>(testFileName);
    if (cur_return != 0)
    {
      std::cerr << "Error writing Nifti file type double" << std::endl;
      rval += cur_return;
    }
    std::cout << "Prefix:" << prefix << std::endl;
    rval += TestNiftiByteSwap(prefix);
  }
  // Tests added to increase code coverage.
  {
    const itk::NiftiImageIOFactory::Pointer MyFactoryTest = itk::NiftiImageIOFactory::New();
    if (MyFactoryTest.IsNull())
    {
      return EXIT_FAILURE;
    }
    // This was made a protected function.  MyFactoryTest->PrintSelf(std::cout,0);
  }

  TestEnumStreaming();

  return rval;
}
