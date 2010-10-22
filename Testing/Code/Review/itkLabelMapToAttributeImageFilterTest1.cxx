/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelMapToAttributeImageFilterTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelObject.h"
#include "itkShapeLabelObject.h"
#include "itkShapeLabelObjectAccessors.h"
#include "itkLabelMap.h"

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkLabelMapToAttributeImageFilter.h"

#include "itkTestingMacros.h"

int itkLabelMapToAttributeImageFilterTest1(int argc, char * argv[])
{

  if( argc != 3)
    {
    std::cerr << "usage: " << argv[0] << " input output" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;

  typedef unsigned short PixelType;

  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ShapeLabelObject< PixelType, dim >           ShapeLabelObjectType;
  typedef itk::LabelMap< ShapeLabelObjectType >             LabelMapType;

  //Reading Image File
  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  //Converting LabelImage to ShapeLabelMap
  typedef itk::LabelImageToShapeLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  typedef itk::LabelMapToAttributeImageFilter< LabelMapType, ImageType,
     itk::Functor::NumberOfPixelsLabelObjectAccessor<LabelMapType::LabelObjectType> > L2ImageType;
  L2ImageType::Pointer l2i = L2ImageType::New();
  l2i->SetInput( i2l->GetOutput() );
  itk::SimpleFilterWatcher watcher(l2i, "filter");

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
