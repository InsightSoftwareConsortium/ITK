/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>
#include "itksys/SystemTools.hxx"
#include "itkRandomImageSource.h"
#include "itkNumericSeriesFileIterator.h"
#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"
#include "itkImage.h"

int itkImageSeriesIOTest(int argc, char * argv[])
{
  //
  // first argument is passing in the writable directory to do all testing
  if(argc > 1) {
    char *testdir = *++argv;
    --argc;
    itksys::SystemTools::ChangeDirectory(testdir);
  }

  // Create a typedef to make the code more digestable
  //
  typedef itk::Image<unsigned char,3> Image3DType;

  unsigned long size[3];
  size[0]=32; size[1]=64; size[2]=12 ;
  
  // Create an image consisting of random numbers.
  itk::RandomImageSource<Image3DType>::Pointer random = 
    itk::RandomImageSource<Image3DType>::New();
  random->SetMin(0);
  random->SetMax(255);
  random->SetSize(size);

  // Write out the image. Control the formatting.
  itk::NumericSeriesFileIterator::Pointer fileIter =
    itk::NumericSeriesFileIterator::New();
  fileIter->SetSeriesFormat("junk.%d.png");

  itk::ImageSeriesWriter<Image3DType>::Pointer writer = 
     itk::ImageSeriesWriter<Image3DType>::New();
  writer->SetInput(random->GetOutput());
  writer->SetFileIterator(fileIter);
  writer->Write();

  // Now read back the image. From the filename the default iterator
  // will pick the right sequence of files.
  itk::ImageSeriesReader<Image3DType>::Pointer reader = 
     itk::ImageSeriesReader<Image3DType>::New();
  reader->SetFileIterator(fileIter);
  reader->Update();

  return EXIT_SUCCESS;
}
