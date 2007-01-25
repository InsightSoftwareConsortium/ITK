/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryErodeImageFilterTest3.cxx
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
#include "itkSimpleFilterWatcher.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryBallStructuringElement.h"

int itkBinaryErodeImageFilterTest3(int argc, char * argv[])
{
  if( argc < 7 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage OutputImage Foreground Background BoundaryToForeground Radius" << std::endl;
    return EXIT_FAILURE;
    }
  const int dim = 2;
  
  typedef unsigned char PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::BinaryBallStructuringElement< PType, dim > SRType;
  SRType kernel;
  kernel.SetRadius( atoi(argv[6]) );
  kernel.CreateStructuringElement();

  typedef itk::BinaryErodeImageFilter< IType, IType, SRType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetKernel( kernel );

  // test default values
  if ( filter->GetBackgroundValue( ) != itk::NumericTraits< PType >::NonpositiveMin() )
    {
    std::cerr << "Wrong default background value." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetForegroundValue( ) != itk::NumericTraits< PType >::max() )
    {
    std::cerr << "Wrong default foreground value." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetErodeValue( ) != itk::NumericTraits< PType >::max() )
    {
    std::cerr << "Wrong default dilate value." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetBoundaryToForeground( ) != true )
    {
    std::cerr << "Wrong default BoundaryToForeground value." << std::endl;
    return EXIT_FAILURE;
    }

  //Exercise Set/Get methods for Background Value
  filter->SetForegroundValue( atoi(argv[3]) );
  if ( filter->GetForegroundValue( ) != atoi(argv[3]) )
    {
    std::cerr << "Set/Get Foreground value problem." << std::endl;
    return EXIT_FAILURE;
    }

  // the same with the alias
  filter->SetErodeValue( atoi(argv[3]) );
  if ( filter->GetErodeValue( ) != atoi(argv[3]) )
    {
    std::cerr << "Set/Get Erode value problem." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetBackgroundValue( atoi(argv[4]) );
  if ( filter->GetBackgroundValue( ) != atoi(argv[4]) )
    {
    std::cerr << "Set/Get Background value problem." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetBoundaryToForeground( atoi(argv[5]) );
  if ( filter->GetBoundaryToForeground( ) != atoi(argv[5]) )
    {
    std::cerr << "Set/Get BoundaryToForeground value problem." << std::endl;
    return EXIT_FAILURE;
    }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;

}




