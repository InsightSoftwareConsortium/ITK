/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesReaderDimensionsTest.cxx
  Language:  C++
  Date:      $Date$

  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageSeriesReader.h"


int itkImageSeriesReaderDimensionsTest(int ac, char* av[])
{

  if(ac < 3)
  {
    std::cerr << "usage: itkIOTests itkImageSeriesReaderDimensionsTest inputFileName(s)" << std::endl;
    return EXIT_FAILURE;
  }

  
  typedef itk::Image<short, 1> Image1DType;
  typedef itk::Image<short, 2> Image2DType;
  typedef itk::Image<short, 3> Image3DType;
  typedef itk::Image<short, 4> Image4DType;
  typedef itk::Image<short, 5> Image5DType;

  typedef itk::ImageSeriesReader<Image1DType> Reader1DType;
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
