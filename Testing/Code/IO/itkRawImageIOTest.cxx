/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIOTest.cxx
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
#include "itkRandomImageSource.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkRawImageIO.h"

int main()
{
  typedef itk::Image<unsigned short,2> ImageType;

  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  unsigned long size[2];
  size[0]=128; size[1]=64;
  
  itk::RandomImageSource<ImageType>::Pointer random;
  random = itk::RandomImageSource<ImageType>::New();
  random->SetMin(0.0);
  random->SetMax(1.0);
  random->SetSize(size);

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  itk::RawImageIO::Pointer vtkIO;
  vtkIO = itk::VTKImageIO::New();

  // Write out the image
  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(random->GetOutput());
  writer->SetFileName("junk.raw");
  writer->SetImageIO(vtkIO);
  writer->Write();

  if ( !vtkIO->CanReadFile("junk.raw") )
    {
    return 1;
    }

  // Create a source object (in this case a reader)
  itk::ImageFileReader<ImageType>::Pointer reader;
  reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(vtkIO);
  reader->SetFileName("junk.raw");
  reader->Update();

  writer->SetInput(reader->GetOutput());
  writer->SetFileName("junk2.raw");
  writer->Write();

  return EXIT_SUCCESS;
}



