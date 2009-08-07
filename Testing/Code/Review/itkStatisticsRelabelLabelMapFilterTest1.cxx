/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsRelabelLabelMapFilterTest1.cxx
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

#include "itkStatisticsRelabelLabelMapFilter.h"
#include "itkLabelImageToStatisticsLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int itkStatisticsRelabelLabelMapFilterTest1(int argc, char * argv[])
{

  if( argc != 6)
    {
    std::cerr << "usage: " << argv[0] << " input feature output";
    std::cerr << "background reverseOrdering attribute" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;

  typedef unsigned char PixelType;
  
  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::StatisticsLabelObject< PixelType, dim >           StatisticsLabelObjectType;
  typedef itk::LabelMap< StatisticsLabelObjectType >             LabelMapType;

  //Reading Image File
  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  //Converting LabelImage to StatisticsLabelMap
  typedef itk::LabelImageToStatisticsLabelMapFilter< ImageType, ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );
  i2l->SetFeatureImage( reader2->GetOutput() );
  
  typedef itk::StatisticsRelabelLabelMapFilter< LabelMapType > RelabelType;
  RelabelType::Pointer relabel = RelabelType::New();

  //testing get and set macros for ReverseOrdering 
  bool reverseOrdering = atoi( argv[4] );
  relabel->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , relabel->GetReverseOrdering() );

  //testing boolean macro for ReverseOrdering
  relabel->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, relabel->GetReverseOrdering() );
  
  relabel->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, relabel->GetReverseOrdering() );

  //testing get and set macros for Attribute 
  unsigned int attribute = atoi( argv[5] );
  relabel->SetAttribute( attribute );
  TEST_SET_GET_VALUE( attribute, relabel->GetAttribute() );

  std::string attributeName  = StatisticsLabelObjectType::GetNameFromAttribute( attribute );
  relabel->SetAttribute( attributeName );

  relabel->SetInput( i2l->GetOutput() );

  itk::SimpleFilterWatcher watcher(relabel, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2ImageType;
  L2ImageType::Pointer l2i = L2ImageType::New();
  l2i->SetInput( relabel->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
