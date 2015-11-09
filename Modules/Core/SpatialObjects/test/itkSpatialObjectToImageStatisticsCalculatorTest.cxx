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

#include <iostream>
#include "itkSpatialObjectToImageStatisticsCalculator.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkEllipseSpatialObject.h"
#include "itkImageSliceIteratorWithIndex.h"

int itkSpatialObjectToImageStatisticsCalculatorTest(int, char * [] )
{
  typedef unsigned char                PixelType;
  typedef itk::Image<PixelType,2>      ImageType;
  typedef itk::EllipseSpatialObject<2> EllipseType;

  // Image Definition
  ImageType::RegionType region;
  ImageType::SizeType size;
  size.Fill(50);

  // Circle definition
  EllipseType::Pointer ellipse = EllipseType::New();
  ellipse->SetRadius(10);

  EllipseType::VectorType offset;
  offset.Fill(25);
  ellipse->GetIndexToObjectTransform()->SetOffset(offset);
  ellipse->ComputeObjectToParentTransform();


  // Create a test image
  typedef itk::SpatialObjectToImageFilter<EllipseType,ImageType> ImageFilterType;
  ImageFilterType::Pointer filter = ImageFilterType::New();
  filter->SetInput(ellipse);
  filter->SetSize(size);
  filter->SetInsideValue(255);
  filter->Update();

  ImageType::Pointer image = filter->GetOutput();

  offset.Fill(25);
  ellipse->GetIndexToObjectTransform()->SetOffset(offset);
  ellipse->ComputeObjectToParentTransform();


  typedef itk::SpatialObjectToImageStatisticsCalculator<ImageType,EllipseType> CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();
  calculator->SetImage(image);
  calculator->SetSpatialObject(ellipse);
  calculator->Update();

  std::cout << " --- Ellipse and Image perfectly aligned --- " << std::endl;
  std::cout << "Sample mean = " << calculator->GetMean() << std::endl;
  std::cout << "Sample covariance = " << calculator->GetCovarianceMatrix();

  if(calculator->GetMean() != CalculatorType::VectorType(255)
    || itk::Math::NotAlmostEquals( calculator->GetCovarianceMatrix()[0][0], itk::NumericTraits< CalculatorType::MatrixType::ValueType >::ZeroValue() )
    )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  offset.Fill(20);
  ellipse->GetIndexToObjectTransform()->SetOffset(offset);
  ellipse->ComputeObjectToParentTransform();
  ellipse->Update();
  calculator->Update();

  std::cout << " --- Ellipse and Image mismatched left --- " << std::endl;
  std::cout << "Sample mean = " << calculator->GetMean() << std::endl;
  std::cout << "Sample covariance = " << calculator->GetCovarianceMatrix();

  if(  (std::fabs(calculator->GetMean()[0]-140.0)>1.0)
    || (std::fabs(calculator->GetCovarianceMatrix()[0][0]-16141.0)>1.0)
    )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  std::cout << " --- Ellipse and Image mismatched right --- " << std::endl;

  offset.Fill(30);
  ellipse->GetIndexToObjectTransform()->SetOffset(offset);
  ellipse->ComputeObjectToParentTransform();
  ellipse->Update();
  calculator->Update();

  std::cout << "Sample mean = " << calculator->GetMean() << std::endl;
  std::cout << "Sample covariance = " << calculator->GetCovarianceMatrix();

  if( (std::fabs(calculator->GetMean()[0]-140.0)>1.0)
    || (std::fabs(calculator->GetCovarianceMatrix()[0][0]-16141.0)>1.0)
    )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  std::cout << " --- Testing higher dimensionality --- " << std::endl;

  // Create a new 3D image
  typedef itk::Image<PixelType,3> Image3DType;
  Image3DType::Pointer image3D = Image3DType::New();

  typedef Image3DType::RegionType RegionType;
  typedef Image3DType::SizeType   SizeType;
  typedef Image3DType::IndexType  IndexType;

  SizeType size3D;
  size3D[0]=50;
  size3D[1]=50;
  size3D[2]=3;
  IndexType start;
  start.Fill( 0 );
  RegionType region3D;
  region3D.SetIndex( start );
  region3D.SetSize(  size3D  );
  image3D->SetRegions( region3D );
  image3D->Allocate();
  image3D->FillBuffer(255);

  // Fill the image
  std::cout << "Allocating image." << std::endl;
  typedef itk::ImageSliceIteratorWithIndex< Image3DType > SliceIteratorType;
  SliceIteratorType it( image3D, region3D );


  it.GoToBegin();
  it.SetFirstDirection( 0 );  // 0=x, 1=y, 2=z
  it.SetSecondDirection( 1 ); // 0=x, 1=y, 2=z

  unsigned int value = 0;
  while( !it.IsAtEnd() )
  {
    while( !it.IsAtEndOfSlice() )
    {
      while( !it.IsAtEndOfLine() )
      {
        it.Set( value );
        ++it;
      }
      it.NextLine();
    }
    it.NextSlice();
    value++;
  }

  std::cout << "Allocating spatial object." << std::endl;
  typedef itk::EllipseSpatialObject<3> Ellipse3DType;
  Ellipse3DType::Pointer ellipse3D = Ellipse3DType::New();
  double radius[3];
  radius[0] = 10;
  radius[1] = 10;
  radius[2] = 0;
  ellipse3D->SetRadius(radius);

  Ellipse3DType::VectorType offset3D;
  offset3D.Fill(25);
  offset3D[2]=0; // first slice
  ellipse3D->GetIndexToObjectTransform()->SetOffset(offset3D);
  ellipse3D->ComputeObjectToParentTransform();

  // Create a new calculator with a sample size of 3
  std::cout << "Updating calculator." << std::endl;
  typedef itk::SpatialObjectToImageStatisticsCalculator<Image3DType,Ellipse3DType,3> Calculator3DType;
  Calculator3DType::Pointer calculator3D = Calculator3DType::New();
  calculator3D->SetImage(image3D);
  calculator3D->SetSpatialObject(ellipse3D);
  calculator3D->Update();

  std::cout << "Sample mean = " << calculator3D->GetMean() << std::endl;
  std::cout << "Sample covariance = " << calculator3D->GetCovarianceMatrix();

  if(  (std::fabs(calculator3D->GetMean()[0]-0.0)>1.0)
    || (std::fabs(calculator3D->GetMean()[1]-1.0)>1.0)
    || (std::fabs(calculator3D->GetMean()[2]-2.0)>1.0)
    )
    {
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Number of pixels = " << calculator3D->GetNumberOfPixels() << std::endl;
  if(calculator3D->GetNumberOfPixels() != 305)
     {
     std::cout << "[FAILED]" << std::endl;
     return EXIT_FAILURE;
     }

  std::cout << "Sum = " << calculator3D->GetSum() << std::endl;
  if(calculator3D->GetSum() != 915)
     {
     std::cout << "[FAILED]" << std::endl;
     return EXIT_FAILURE;
     }

  std::cout << " [PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
