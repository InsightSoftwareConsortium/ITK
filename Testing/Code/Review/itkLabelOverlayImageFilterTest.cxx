/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelOverlayImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"
#include "itkLabelOverlayImageFilter.h"


int itkLabelOverlayImageFilterTest(int argc, char * argv[])
{
  const int Dimension = 2;

  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage  LabelImage Opacity OutputImage" << std::endl;
    return 1;
    }
 
  typedef unsigned char                           PType;
  typedef itk::Image< PType, Dimension >          IType;
  typedef itk::RGBPixel<unsigned char>            CPType;
  typedef itk::Image< CPType, Dimension >         CIType;
  typedef itk::ImageFileReader< IType >           ReaderType;

  //Read in the input image
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  //Read in the label image
  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  //Instantiate the filter
  typedef itk::LabelOverlayImageFilter< IType, IType, CIType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  //Use the background
  filter->SetUseBackground( true );
  //Set the filter input and label images
  filter->SetInput( reader->GetOutput() );
  filter->SetLabelImage( reader2->GetOutput() );

  //Set opacity 
  filter->SetOpacity( atof(argv[3]) );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  //Instantiate output image
  typedef itk::ImageFileWriter< CIType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[4] );
  writer->Update();

  return EXIT_SUCCESS;
}
