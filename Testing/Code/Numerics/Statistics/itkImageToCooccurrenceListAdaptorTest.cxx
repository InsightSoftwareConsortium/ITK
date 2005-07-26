/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToCooccurrenceListAdaptorTest.cxx
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


#include "itkImage.h"

#include "itkImageFileReader.h"
#include "itkImageToCooccurrenceListAdaptor.h"

int itkImageToCooccurrenceListAdaptorTest( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Error: argument missing" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile " << std::endl;
    }

  typedef float               PixelType;
  const   unsigned int        Dimension = 2;

  typedef itk::Image< PixelType, Dimension >    ImageType;

  typedef itk::ImageFileReader< ImageType >     ReaderType;

  typedef itk::Statistics::ImageToCooccurrenceListAdaptor < 
                                  ImageType > CooccurrenceListType;

  ReaderType::Pointer reader = ReaderType::New();
                                                                                                                            
  reader->SetFileName( argv[1] );
                                                                                                                            
  try 
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception caught" << std::endl;
    std::cerr << exp << std::endl;
    }

  CooccurrenceListType::Pointer list = CooccurrenceListType::New();
  list->SetImage(reader->GetOutput());

  CooccurrenceListType::OffsetType offset0 = {{-1,0}};
  CooccurrenceListType::OffsetType offset1 = {{1,0}};
  CooccurrenceListType::OffsetType offset2 = {{0,-1}};
  CooccurrenceListType::OffsetType offset3 = {{0,1}};

  list->UseNeighbor(offset0);
  list->UseNeighbor(offset1);
  list->UseNeighbor(offset2);
  list->UseNeighbor(offset3);

  list->Compute();
  std::cout << "ImageToCooccurrenceListAdaptorTest [PASSED]" << std::endl;

  return EXIT_SUCCESS;
  
}


