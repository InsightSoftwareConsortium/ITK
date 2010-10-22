/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryReconstructionLabelMapFilterTest1.cxx
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

#include "itkLabelMap.h"

#include "itkLabelImageToLabelMapFilter.h"
#include "itkBinaryReconstructionLabelMapFilter.h"
#include "itkAttributeSelectionLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int itkBinaryReconstructionLabelMapFilterTest(int argc, char * argv[])
{
  if( argc != 5 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input marker output";
    std::cerr << " fg";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 3;

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::AttributeLabelObject< PixelType, dim, bool >     AttributeLabelObjectType;
  typedef itk::LabelMap< AttributeLabelObjectType >             LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  typedef itk::BinaryReconstructionLabelMapFilter< LabelMapType, ImageType > LabelReconstructionType;
  LabelReconstructionType::Pointer reconstruction = LabelReconstructionType::New();

  //testing get and set macros for Lambda
  int fg = atoi( argv[4] );
  reconstruction->SetForegroundValue( fg );
  TEST_SET_GET_VALUE( fg , reconstruction->GetForegroundValue() );

  reconstruction->SetInput( i2l->GetOutput() );
  reconstruction->SetMarkerImage( reader2->GetOutput() );

  itk::SimpleFilterWatcher watcher(reconstruction, "filter");
  reconstruction->Update();
  reconstruction->GetOutput()->PrintLabelObjects();

  typedef itk::AttributeSelectionLabelMapFilter< LabelMapType > LabelOpeningType;
  LabelOpeningType::Pointer opening = LabelOpeningType::New();
  opening->SetInput( reconstruction->GetOutput() );
  opening->SetAttribute(true);

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( opening->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
