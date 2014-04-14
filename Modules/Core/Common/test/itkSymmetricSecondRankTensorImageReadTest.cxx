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


int itkSymmetricSecondRankTensorImageReadTest( int ac, char* av[] )
{
  if(ac < 1)
    {
    std::cerr << "Usage: " << av[0] << " Input\n";
    return EXIT_FAILURE;
    }

  typedef itk::SymmetricSecondRankTensor<float, 3>    TensorPixelType;
  typedef itk::Image<TensorPixelType, 3>              TensorImageType;

  typedef itk::Matrix<float,3,3>                      MatrixPixelType;
  typedef itk::Image<MatrixPixelType, 3>              MatrixImageType;

  MatrixImageType::Pointer matrixImage = MatrixImageType::New();

  MatrixImageType::SizeType size;
  size.Fill(10);

  MatrixImageType::IndexType start;
  start.Fill(0);

  MatrixImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  matrixImage->SetRegions( region );
  matrixImage->Allocate();

  MatrixPixelType matrixPixel;

  matrixPixel[0][0] = 1;
  matrixPixel[0][1] = 2;
  matrixPixel[0][2] = 3;

  matrixPixel[1][0] = 2;
  matrixPixel[1][1] = 4;
  matrixPixel[1][2] = 5;

  matrixPixel[2][0] = 3;
  matrixPixel[2][1] = 5;
  matrixPixel[2][2] = 6;

  itk::ImageRegionIterator< MatrixImageType > itr( matrixImage, region );

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

  matrixWriter->SetInput( matrixImage );
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


  typedef itk::ImageFileReader<  TensorImageType > TensorReaderType;

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

  TensorImageType::ConstPointer tensorImage = tensorReader->GetOutput();

  // Compare the read values to the original values
  const float tolerance = 1e-5;

  itk::ImageRegionConstIterator< TensorImageType > tItr( tensorImage, region );
  itk::ImageRegionConstIterator< MatrixImageType > mItr( matrixImage, region );

  tItr.GoToBegin();
  mItr.GoToBegin();

  while( !mItr.IsAtEnd() )
    {
    matrixPixel = mItr.Get();
    const TensorPixelType tensorPixel = tItr.Get();

    for(unsigned int i=0; i<3; i++)
      {
      for(unsigned int j=0; j<3; j++)
        {
        if( std::abs( matrixPixel[i][j] - tensorPixel(i,j) ) > tolerance )
          {
          std::cerr << "Tensor read does not match expected values " << std::endl;
          std::cerr << "Index " << tItr.GetIndex() << std::endl;
          std::cerr << "Tensor value " << std::endl << tensorPixel << std::endl;
          std::cerr << "Matrix value " << std::endl << matrixPixel << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    ++mItr;
    ++tItr;
    }


  return EXIT_SUCCESS;

}
