/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsUniqueLabelMapFilterTest1.cxx
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
#include "itkStatisticsLabelObject.h"
#include "itkStatisticsLabelObjectAccessors.h"
#include "itkLabelMap.h"

#include "itkLabelImageToStatisticsLabelMapFilter.h"
#include "itkStatisticsUniqueLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int itkStatisticsUniqueLabelMapFilterTest1(int argc, char * argv[])
{
  if( argc != 6 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input feature output";
    std::cerr << " reverseOrdering(0/1) attribute";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 3;
 
  typedef unsigned char PixelType;
 
  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::StatisticsLabelObject< PixelType, dim >           StatisticsLabelObjectType;
  typedef itk::LabelMap< StatisticsLabelObjectType >             LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
 
  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );
 
  typedef itk::LabelImageToStatisticsLabelMapFilter< ImageType, ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );
  i2l->SetFeatureImage( reader2->GetOutput() );

  typedef itk::StatisticsUniqueLabelMapFilter< LabelMapType > LabelUniqueType;
  LabelUniqueType::Pointer Unique = LabelUniqueType::New();

  //testing get and set macros for ReverseOrdering 
  bool reverseOrdering = atoi( argv[4] );
  Unique->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , Unique->GetReverseOrdering() );

  //testing boolean macro for ReverseOrdering
  Unique->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, Unique->GetReverseOrdering() );

  Unique->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, Unique->GetReverseOrdering() );

  //testing get and set macros for Attribute 
  LabelUniqueType::AttributeType attribute = atoi( argv[5] );
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
  writer->SetFileName( argv[3] );
  writer->UseCompressionOn();
  
  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
