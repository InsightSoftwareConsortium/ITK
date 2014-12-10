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

// Software Guide : BeginLatex
//
// \index{itk::Spatial\-Object\-To\-Image\-Statistics\-Calculator}
// This example describes how to use the
// \doxygen{SpatialObjectToImageStatisticsCalculator} to compute statistics
// of an \doxygen{Image} only in a region defined inside a given \doxygen{SpatialObject}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkSpatialObjectToImageStatisticsCalculator.h"
// Software Guide : EndCodeSnippet

#include "itkImage.h"
#include "itkEllipseSpatialObject.h"
#include "itkRandomImageSource.h"

int main(int, char * [] )
{
// Software Guide : BeginLatex
//
// We first create a test image using the \doxygen{RandomImageSource}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::Image< unsigned char, 2 >      ImageType;
  typedef itk::RandomImageSource< ImageType > RandomImageSourceType;
  RandomImageSourceType::Pointer randomImageSource
                                               = RandomImageSourceType::New();
  ImageType::SizeValueType size[2];
  size[0] = 10;
  size[1] = 10;
  randomImageSource->SetSize(size);
  randomImageSource->Update();
  ImageType::Pointer image = randomImageSource->GetOutput();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Next we create an \doxygen{EllipseSpatialObject} with a radius of 2.
// We also move the ellipse to the center of the image by increasing
// the offset of the IndexToObjectTransform.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::EllipseSpatialObject<2> EllipseType;
  EllipseType::Pointer ellipse = EllipseType::New();
  ellipse->SetRadius(2);
  EllipseType::VectorType offset;
  offset.Fill(5);
  ellipse->GetIndexToObjectTransform()->SetOffset(offset);
  ellipse->ComputeObjectToParentTransform();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Then we can create the \doxygen{SpatialObjectToImageStatisticsCalculator}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::SpatialObjectToImageStatisticsCalculator<
    ImageType, EllipseType > CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We pass a pointer to the image to the calculator.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  calculator->SetImage(image);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We also pass the SpatialObject. The statistics will be computed inside the SpatialObject
// (Internally the calculator is using the \code{IsInside()} function).
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  calculator->SetSpatialObject(ellipse);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// At the end we trigger the computation via the \code{Update()} function and we
// can retrieve the mean and the covariance matrix using \code{GetMean()}
// and \code{GetCovarianceMatrix()}
// respectively.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  calculator->Update();
  std::cout << "Sample mean = " << calculator->GetMean() << std::endl;
  std::cout << "Sample covariance = " << calculator->GetCovarianceMatrix();
// Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
