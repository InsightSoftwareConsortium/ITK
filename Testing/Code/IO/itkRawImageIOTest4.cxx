/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIOTest4.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <fstream>
#include "itkRawImageIO.h"
#include "itkImageFileReader.h"
#include "itkImage.h"
#include "itkImageLinearIteratorWithIndex.h"


int itkRawImageIOTest4(int, char**)
{

  typedef unsigned short PixelType;
  const unsigned int ImageDimension = 2;

  typedef itk::RawImageIO<PixelType,ImageDimension> IOType;
  IOType::Pointer io = IOType::New();

  io->SetFileTypeToBinary();
  io->SetByteOrderToBigEndian();

  unsigned int dims[ImageDimension] = { 5, 5 };
  for( int j = 0; j < ImageDimension; j++ )
    {
    io->SetDimensions( j, dims[j] );
    }


  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;

  char filename[] = "test.raw";
  
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( filename );
  reader->SetImageIO( io );
  reader->Update();


  typedef itk::ImageLinearIteratorWithIndex<ImageType> Iterator;
  Iterator it( reader->GetOutput(), reader->GetOutput()->GetBufferedRegion() );

  it.GoToBegin();
  it.SetDirection( 0 ); 

  while ( !it.IsAtEnd() )
    {
    while ( !it.IsAtEndOfLine() )
      {
      std::cout << it.Get() << " ";
      ++it;
      }
    std::cout << std::endl;
    it.NextLine();
    }

  return EXIT_SUCCESS;

}

