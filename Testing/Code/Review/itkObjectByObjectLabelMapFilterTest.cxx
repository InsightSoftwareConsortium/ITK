/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeBasedKmeansEstimator.h
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
#include "itkLabelMap.h"
#include "itkLabelImageToLabelMapFilter.h"
#include "itkObjectByObjectLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkBinaryDilateImageFilter.h"
#include "itkFlatStructuringElement.h"


int itkObjectByObjectLabelMapFilterTest(int argc, char * argv[])
{

  if( argc != 4 )
    {
    std::cerr << "usage: " << argv[0] << " input output keepLabel" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  const int dim = 2;

  typedef itk::Image< unsigned char, dim > ImageType;

  typedef itk::LabelObject< unsigned char, dim > LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >       LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput( reader->GetOutput() );

  typedef itk::FlatStructuringElement< dim > KernelType;
  typedef itk::BinaryDilateImageFilter< ImageType, ImageType, KernelType > DilateType;
  DilateType::Pointer dilate = DilateType::New();
  KernelType::SizeType rad;
  rad.Fill( 3 );
  dilate->SetKernel( KernelType::Ball( rad ) );

  typedef itk::ObjectByObjectLabelMapFilter< LabelMapType > ObOType;
  ObOType::Pointer obo = ObOType::New();
  obo->SetInput( i2l->GetOutput() );
  obo->SetFilter( dilate );
  obo->SetPadSize( rad );
  obo->SetKeepLabels( atoi(argv[3]) );
//  obo->SetBinaryInternalOutput( false );
  itk::SimpleFilterWatcher watcher(obo, "filter");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( obo->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();

  return 0;
}
