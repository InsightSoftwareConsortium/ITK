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
// \index{itk::Statistics::EuclideanDistanceMetric}
//
// The Euclidean distance function (\subdoxygen{Statistics}{EuclideanDistanceMetric}
// requires as template parameter the type of the measurement vector. We can
// use this function for any subclass of the \doxygen{FixedArray}. As a
// subclass of the \subdoxygen{Statistics}{DistanceMetric}, it has two basic
// methods, the \code{SetOrigin(measurement vector)} and the
// \code{Evaluate(measurement vector)}. The \code{Evaluate()} method returns
// the distance between its argument (a measurement vector) and the measurement
// vector set by the \code{SetOrigin()} method.
//
// In addition to the two methods, EuclideanDistanceMetric has two more
// methods that return the distance of two measurements ---
// \code{Evaluate(measurement vector, measurement vector)} and the
// coordinate distance between two measurements (not vectors) ---
// \code{Evaluate(measurement, measurement)}. The argument type of the
// latter method is the type of the component of the measurement vector.
//
// We include the header files for the class and the \doxygen{Vector}.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkArray.h"
#include "itkEuclideanDistanceMetric.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We define the type of the measurement vector that will be input of
// the Euclidean distance function. As a result, the measurement type
// is \code{float}.
//
// Software Guide : EndLatex

int main(int, char*[])
{
  // Software Guide : BeginCodeSnippet
  typedef itk::Array< float > MeasurementVectorType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The instantiation of the function is done through the usual
  // \code{New()} method and a smart pointer.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::EuclideanDistanceMetric< MeasurementVectorType >
    DistanceMetricType;
  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We create three measurement vectors, the \code{originPoint},
  // the \code{queryPointA}, and the \code{queryPointB}. The type of the
  // \code{originPoint} is fixed in the
  // \subdoxygen{Statistics}{DistanceMetric} base class as
  // \code{itk::Vector< double, length of the measurement vector of the
  // each distance metric instance>}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginLatex
  //
  // The Distance metric does not know about the length of the measurement
  // vectors.  We must set it explicitly using the
  // \code{SetMeasurementVectorSize()} method.
  //
  // Software Guide : EndLatex
  distanceMetric->SetMeasurementVectorSize( 2 );

  // Software Guide : BeginCodeSnippet
  DistanceMetricType::OriginType originPoint( 2 );
  MeasurementVectorType queryPointA( 2 );
  MeasurementVectorType queryPointB( 2 );

  originPoint[0] = 0;
  originPoint[1] = 0;

  queryPointA[0] = 2;
  queryPointA[1] = 2;

  queryPointB[0] = 3;
  queryPointB[1] = 3;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // In the following code snippet, we show the uses of the three different
  // \code{Evaluate()} methods.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  distanceMetric->SetOrigin( originPoint );
  std::cout << "Euclidean distance between the origin and the query point A = "
            << distanceMetric->Evaluate( queryPointA )
            << std::endl;

  std::cout << "Euclidean distance between the two query points (A and B) = "
            << distanceMetric->Evaluate( queryPointA, queryPointB )
            << std::endl;

  std::cout << "Coordinate distance between "
            << "the first components of the two query points = "
            << distanceMetric->Evaluate( queryPointA[0], queryPointB[0] )
            << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
