/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


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
      if( std::abs( tensorPixelInput[i] - tensorPixelOutput[i] ) > tolerance )
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
