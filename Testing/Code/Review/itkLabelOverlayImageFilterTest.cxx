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
 
  typedef unsigned char                               PixelType;
  typedef itk::Image< PixelType, Dimension >          ImageType;
  typedef itk::RGBPixel<unsigned char>                ColorPixelType;
  typedef itk::Image< ColorPixelType, Dimension >     ColorImageType;
  typedef itk::ImageFileReader< ImageType >           ReaderType;

  //Read in the input image
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  //Read in the label image
  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  //Instantiate the filter
  typedef itk::LabelOverlayImageFilter< 
    ImageType, ImageType, ColorImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();

  //Use the background
  filter->SetUseBackground( true );
  if( filter->GetUseBackground() != true )
    {
    std::cerr << "UseBackground Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }
   
  filter->SetUseBackground( false );
  if( filter->GetUseBackground() != false )
    {
    std::cerr << "UseBackground Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }
 
  filter->UseBackgroundOn();
  if( filter->GetUseBackground() != true )
    {
    std::cerr << "UseBackground Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }
 
  filter->UseBackgroundOff();
  if( filter->GetUseBackground() != false )
    {
    std::cerr << "UseBackground Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }



  filter->SetUseBackground( true );

  // Exercising Background Value methods
  filter->SetBackgroundValue( 200 );
  if( filter->GetUseBackground() != true )
    {
    std::cerr << "UseBackground Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }
 

  //Set the filter input and label images
  filter->SetInput( reader->GetOutput() );
  filter->SetLabelImage( reader2->GetOutput() );

  
  filter->SetOpacity( 2 );
  if( filter->GetOpacity() != 2 )
    {
    std::cerr << "Opacity Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }

 
  filter->SetOpacity( 3 );
  if( filter->GetOpacity() != 3 )
    {
    std::cerr << "Opacity Set/Get Problem" << std::endl;
    return EXIT_FAILURE;
    }


  //Set opacity 
  filter->SetOpacity( atof(argv[3]) );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  //Instantiate output image
  typedef itk::ImageFileWriter< ColorImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[4] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
