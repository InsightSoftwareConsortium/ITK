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


int itkImageSeriesReaderDimensionsTest(int ac, char* av[])
{

  if(ac < 3)
  {
    std::cerr << "usage: itkIOTests itkImageSeriesReaderDimensionsTest inputFileName(s)" << std::endl;
    return EXIT_FAILURE;
  }


  typedef itk::Image<short, 2> Image2DType;
  typedef itk::Image<short, 3> Image3DType;
  typedef itk::Image<short, 4> Image4DType;
  typedef itk::Image<short, 5> Image5DType;

  typedef itk::ImageSeriesReader<Image2DType> Reader2DType;
  typedef itk::ImageSeriesReader<Image3DType> Reader3DType;
  typedef itk::ImageSeriesReader<Image4DType> Reader4DType;
  typedef itk::ImageSeriesReader<Image5DType> Reader5DType;

  Reader2DType::FileNamesContainer fname;
  fname.push_back(av[1]);

  Reader2DType::FileNamesContainer fnames;
  for (int i = 1; i < ac; ++i)
      fnames.push_back(av[i]);


  std::cout << "testing reading a single 2D image to 2D" << std::endl;
   try
    {
    Reader2DType::Pointer reader = Reader2DType::New();
    reader->SetFileNames(fname);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  std::cout << "testing reading a single 2D image to 3D" << std::endl;
  try
    {
    Reader3DType::Pointer reader = Reader3DType::New();
    reader->SetFileNames(fname);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  std::cout << "testing reading a single 2D image to 4D" << std::endl;
   try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileNames(fname);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

   //////////

   std::cout << "testing reading a series of 2D images to 2D" << std::endl;
   try
    {
    Reader2DType::Pointer reader = Reader2DType::New();
    reader->SetFileNames(fnames);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    //return EXIT_FAILURE;
    }

  std::cout << "testing reading a series of 2D images to 3D" << std::endl;
  try
    {
    Reader3DType::Pointer reader = Reader3DType::New();
    reader->SetFileNames(fnames);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  std::cout << "testing reading a series of 2D images to 4D" << std::endl;
   try
    {
    Reader4DType::Pointer reader = Reader4DType::New();
    reader->SetFileNames(fnames);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  std::cout << "testing reading a series of 2D images to 5D" << std::endl;
   try
    {
    Reader5DType::Pointer reader = Reader5DType::New();
    reader->SetFileNames(fnames);
    reader->Update();
    std::cout << "output image size:: " << reader->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << ex;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}
