/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricSecondRankTensorImageWriteReadTest.cxx
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
#include "itkSymmetricSecondRankTensor.h"
#include "itkImageRegionIterator.h"


// Write a 2D SymmetricSecondRankTensor image to file and read it back again.
int itkSymmetricSecondRankTensorImageWriteReadTest( int ac, char* av[] )
{
  if(ac < 1)
    {
    std::cerr << "Usage: " << av[0] << " Input\n";
    return EXIT_FAILURE;
    }
  
  typedef itk::SymmetricSecondRankTensor<float, 2>    TensorPixelType;
  typedef itk::Image<TensorPixelType, 2>              TensorImageType;

  TensorImageType::Pointer tensorImageInput = TensorImageType::New();
   
  TensorImageType::SizeType size;
  size.Fill(10);
  
  TensorImageType::IndexType start;
  start.Fill(0);

  TensorImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  tensorImageInput->SetRegions( region );
  tensorImageInput->Allocate();

  TensorPixelType tensorPixelInput;

  tensorPixelInput(0,0) = 1; 
  tensorPixelInput(0,1) = 2; 
  tensorPixelInput(1,1) = 3; 

  itk::ImageRegionIterator< TensorImageType > itr( tensorImageInput, region );

  itr.GoToBegin();

  while( !itr.IsAtEnd() )
    {
    itr.Set( tensorPixelInput );
    for(unsigned int i=0; i<3; i++)
        tensorPixelInput[i]++;
    ++itr;
    }

  typedef itk::ImageFileWriter< TensorImageType > TensorWriterType;

  TensorWriterType::Pointer tensorWriter = TensorWriterType::New();

  tensorWriter->SetInput( tensorImageInput );
  tensorWriter->SetFileName( av[1] );

  try
    {
    tensorWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileReader< TensorImageType > TensorReaderType;

  TensorReaderType::Pointer tensorReader = TensorReaderType::New();
 
  tensorReader->SetFileName( av[1] );

  try
    {
    tensorReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  TensorImageType::ConstPointer tensorImageOutput = tensorReader->GetOutput();

  // Compare the read values to the original values
  const float tolerance = 1e-5;

  itk::ImageRegionConstIterator< TensorImageType > inIt( tensorImageInput, region );
  itk::ImageRegionConstIterator< TensorImageType > outIt( tensorImageOutput, region );
  
  inIt.GoToBegin();
  outIt.GoToBegin();

  while( !outIt.IsAtEnd() )
    {
    tensorPixelInput = inIt.Get();
    const TensorPixelType tensorPixelOutput = outIt.Get();

    for(unsigned int i=0; i<3; i++)
      {
      if( vcl_abs( tensorPixelInput[i] - tensorPixelOutput[i] ) > tolerance )
        {
        std::cerr << "Tensor read does not match expected values " << std::endl;
        std::cerr << "Index " << inIt.GetIndex() << std::endl;
        std::cerr << "Tensor input value "  << std::endl << tensorPixelInput << std::endl;
        std::cerr << "Tensor output value " << std::endl << tensorPixelOutput << std::endl;
        return EXIT_FAILURE;
        }
      }
    ++inIt;
    ++outIt;
    }

  return EXIT_SUCCESS;
}
