/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <fstream>
#include <iostream>
#include "itkRandomImageSource.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkVTKImageIO.h"

int itkVTKImageIOTest(int, char* [] )
{
  typedef itk::Image<float,2> FloatImageType;

  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  unsigned long size[2];
  size[0]=128; size[1]=64;
  
  itk::RandomImageSource<FloatImageType>::Pointer random;
  random = itk::RandomImageSource<FloatImageType>::New();
  random->SetMin(0.0);
  random->SetMax(1.0);
  random->SetSize(size);

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  itk::VTKImageIO::Pointer vtkIO;
  vtkIO = itk::VTKImageIO::New();

  // Write out the image
  itk::ImageFileWriter<FloatImageType>::Pointer writer;
  writer = itk::ImageFileWriter<FloatImageType>::New();
  writer->SetInput(random->GetOutput());
  writer->SetFileName("junk.vtk");
  writer->SetImageIO(vtkIO);
  writer->Write();

  if ( !vtkIO->CanReadFile("junk.vtk") )
    {
    return 1;
    }

  // Create a source object (in this case a reader)
  itk::ImageFileReader<FloatImageType>::Pointer reader;
  reader = itk::ImageFileReader<FloatImageType>::New();
  reader->SetImageIO(vtkIO);
  reader->SetFileName("junk.vtk");
  reader->Update();

  writer->SetInput(reader->GetOutput());
  writer->SetFileName("junk2.vtk");
  writer->Write();

  return EXIT_SUCCESS;
}



