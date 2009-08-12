/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeUniqueLabelMapFilterTest1.cxx
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
#include "itkShapeUniqueLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int itkShapeUniqueLabelMapFilterTest1(int argc, char * argv[])
{
  if( argc != 5 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input output";
    std::cerr << " reverseOrdering(0/1) attribute";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 3;
 
  typedef unsigned char PixelType;
 
  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ShapeLabelObject< PixelType, dim >           ShapeLabelObjectType;
  typedef itk::LabelMap< ShapeLabelObjectType >             LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
 
  typedef itk::LabelImageToShapeLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  typedef itk::ShapeUniqueLabelMapFilter< LabelMapType > LabelUniqueType;
  LabelUniqueType::Pointer Unique = LabelUniqueType::New();

  //testing get and set macros for ReverseOrdering 
  bool reverseOrdering = atoi( argv[3] );
  Unique->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , Unique->GetReverseOrdering() );

  //testing boolean macro for ReverseOrdering
  Unique->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, Unique->GetReverseOrdering() );

  Unique->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, Unique->GetReverseOrdering() );

  //testing get and set macros for Attribute 
  LabelUniqueType::AttributeType attribute = atoi( argv[4] );
  Unique->SetAttribute( attribute );
  TEST_SET_GET_VALUE( attribute, Unique->GetAttribute() );

  Unique->SetInput( i2l->GetOutput() );

  itk::SimpleFilterWatcher watcher(Unique, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( Unique->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();
  
  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
