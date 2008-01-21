/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrixImageWriteReadTest.cxx
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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"


int itkMatrixImageWriteReadTest( int ac, char* av[] )
{
  if(ac < 1)
    {
    std::cerr << "Usage: " << av[0] << " Input\n";
    return EXIT_FAILURE;
    }
  
  typedef itk::Matrix<float,3,3>                      MatrixPixelType;
  typedef itk::Image<MatrixPixelType, 3>              MatrixImageType;
 
  MatrixImageType::Pointer matrixImage1 = MatrixImageType::New();
   
  MatrixImageType::SizeType size;
  size.Fill(10);
  
  MatrixImageType::IndexType start;
  start.Fill(0);

  MatrixImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  matrixImage1->SetRegions( region );
  matrixImage1->Allocate();

  MatrixPixelType matrixPixel;

  matrixPixel[0][0] = 1; 
  matrixPixel[0][1] = 2; 
  matrixPixel[0][2] = 3; 

  matrixPixel[1][0] = 4; 
  matrixPixel[1][1] = 5; 
  matrixPixel[1][2] = 6; 

  matrixPixel[2][0] = 7; 
  matrixPixel[2][1] = 8; 
  matrixPixel[2][2] = 9; 

  itk::ImageRegionIterator< MatrixImageType > itr( matrixImage1, region );

  itr.GoToBegin();

  while( !itr.IsAtEnd() )
    {
    itr.Set( matrixPixel );
    for(unsigned int i=0; i<3; i++)
      {
      for(unsigned int j=0; j<3; j++)
        {
        matrixPixel[i][j]++;
        }
      }
    ++itr;
    }

  typedef itk::ImageFileWriter< MatrixImageType > MatrixWriterType;

  MatrixWriterType::Pointer matrixWriter = MatrixWriterType::New();

  matrixWriter->SetInput( matrixImage1 );
  matrixWriter->SetFileName( av[1] );

  try
    {
    matrixWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
 

  typedef itk::ImageFileReader<  MatrixImageType > MatrixReaderType;

  MatrixReaderType::Pointer matrixReader = MatrixReaderType::New();
 
  matrixReader->SetFileName( av[1] );

  try
    {
    matrixReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  MatrixImageType::ConstPointer matrixImage2 = matrixReader->GetOutput();

  // Compare the read values to the original values
  const float tolerance = 1e-5;

  itk::ImageRegionConstIterator< MatrixImageType > tItr( matrixImage2, region );
  itk::ImageRegionConstIterator< MatrixImageType > mItr( matrixImage1, region );
  
  tItr.GoToBegin();
  mItr.GoToBegin();

  while( !mItr.IsAtEnd() )
    {
    const MatrixPixelType matrixPixel1 = mItr.Get();
    const MatrixPixelType matrixPixel2 = tItr.Get();

    for(unsigned int i=0; i<3; i++)
      {
      for(unsigned int j=0; j<3; j++)
        {
        if( vcl_abs( matrixPixel1[i][j] - matrixPixel2[i][j] ) > tolerance )
          {
          std::cerr << "Matrix read does not match expected values " << std::endl;
          std::cerr << "Index " << tItr.GetIndex() << std::endl;
          std::cerr << "Matrix read     " << std::endl << matrixPixel1 << std::endl;
          std::cerr << "Matrix expected " << std::endl << matrixPixel2 << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    ++mItr;
    ++tItr;
    }
  

  return EXIT_SUCCESS;

}
