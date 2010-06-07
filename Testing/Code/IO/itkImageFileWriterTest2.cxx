/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileWriterTest2.cxx
  Language:  C++
  Date:      $Date$xgoto-l

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
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"

int itkImageFileWriterTest2(int ac, char* av[])
{

  if (ac < 2)
    {
    std::cout << "usage: itkIOTests itkImageFileWriterTest2 outputFileName" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned char,2>       ImageNDType;
  typedef itk::ImageFileWriter<ImageNDType> WriterType;
  typedef itk::ImageFileReader<ImageNDType> ReaderType;

  ImageNDType::Pointer image = ImageNDType::New();
  ImageNDType::RegionType region;
  ImageNDType::IndexType index;
  ImageNDType::SizeType size;


  ImageNDType::PointType originalPoint;
  ImageNDType::PointType readPoint;

  size.Fill(5);
  index.Fill(1);
  region.SetSize(size);
  region.SetIndex(index);
  
  image->SetRegions(region);
  image->Allocate();

  image->FillBuffer(0);
  

  image->TransformIndexToPhysicalPoint(index, originalPoint);
  std::cout << "Original Starting Index: " << index << std::endl;
  std::cout << "Original Starting Point (physical cooridents) : " << originalPoint << std::endl;
  std::cout << "Original Origin: " << image->GetOrigin() << std::endl;
  
  WriterType::Pointer writer = WriterType::New();
  ReaderType::Pointer reader = ReaderType::New();
  try
    {
    writer->SetInput(image);
    writer->SetFileName(av[1]);
    writer->Update();


    reader->SetFileName(av[1]);
    reader->Update();
    index = reader->GetOutput()->GetLargestPossibleRegion().GetIndex();
    reader->GetOutput()->TransformIndexToPhysicalPoint(index, readPoint);
    std::cout << "Read Starting Index: " << index << std::endl;
    std::cout << "Original Starting Point (physical cooridents) : " << readPoint << std::endl;
    std::cout << "Read Origin: " << image->GetOrigin() << std::endl;
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << "caught exception!" << std::endl;
    std::cout << ex;
    return EXIT_FAILURE;
    }

  if (readPoint != originalPoint) 
    {
    std::cout << "Image locations changed!" << std::endl;
    return EXIT_FAILURE;
    }

  // execute the PrintSelf methods
  std::cout << writer;

  return EXIT_SUCCESS;

}
