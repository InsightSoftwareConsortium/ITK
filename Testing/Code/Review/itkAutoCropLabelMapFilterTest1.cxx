/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutoCropLabelMapFilterTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelObject.h"
#include "itkLabelMap.h"
#include "itkLabelImageToLabelMapFilter.h"
#include "itkAutoCropLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int itkAutoCropLabelMapFilterTest1( int argc, char * argv [] )
{

  if( argc != 6 )
    {
    std::cerr << "usage: " << argv[0];
    std::cerr << " inputLabelImage outputLabelImage inputBackgroundValue sizeX sizeY" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;
  typedef unsigned char   PixelType;
  
  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::LabelObject< PixelType, dim > LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >   LabelMapType;
  
  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  
  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  PixelType backgroundValue = atoi( argv[3] );

  i2l->SetBackgroundValue( backgroundValue );

  typedef itk::AutoCropLabelMapFilter< LabelMapType > ChangeType;
  ChangeType::Pointer change = ChangeType::New();
  change->SetInput( i2l->GetOutput() );

  ChangeType::SizeType size;
  size[0] = atoi( argv[4] );
  size[1] = atoi( argv[5] );
  change->SetCropBorder( size );
  TEST_SET_GET_VALUE( size, change->GetCropBorder() );
  
  itk::SimpleFilterWatcher watcher6(change, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( change->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  typedef ChangeType::IndexType             IndexType;
  typedef ChangeType::InputImageRegionType  InputImageRegionType;

  const IndexType & minIndex = change->GetMinIndex();
  const IndexType & maxIndex = change->GetMaxIndex();

  const InputImageRegionType & cropRegion = change->GetRegion();

  std::cout << "GetMinIndex() = " << minIndex << std::endl;
  std::cout << "GetMaxIndex() = " << maxIndex << std::endl;
  std::cout << "GetRegion() = " << cropRegion << std::endl;

  return EXIT_SUCCESS;
}
