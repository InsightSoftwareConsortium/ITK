/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    EuclideanDistance.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
// \index{itk::Statistics::EuclideanDistance|textbf}
//
// The Euclidean distance function
// (\subdoxygen{Statistics}{EuclideanDinstance} requires one
// template argument, the type of the measurement vector. We can use this
// function for any subclass objects of the \doxygen{FixedArray}. As a
// subclass of the \subdoxygen{Statistics}{DistanceMetric}, it has two
// basic methods, the \code{SetOrigin(measurement vector)} and the
// \code{Evaluate(mesaurement vector)}. The \code{Evaluate} method returns
// the distance between the argument (a measurement vector) of the
// \code{Evaluate} method and the measurement vector set by the
// \code{SetOrigin} method.
//
// In addition to the two methods, \code{EuclideanDistance} has two more
// methods that return the distance of two measurements -
// \code{Evaluate(measurement vector, measurement vector)} and the
// coordinate distance between two measurements (not vectors) -
// \code{Evaluate(measurement, measurement)}. The argument type of the
// latter method is the type of the component of the measurement vector.
// 
// We include the header files for the class and the \doxygen{Vector}.
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
#include "itkEuclideanDistance.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We define the type of the measurement vector that will be input of
// the Euclidean distance function. As a result, the measurement type
// is \code{float}.
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
typedef itk::Vector< float, 2 > MeasurementVectorType ;
// Software Guide : EndCodeSnippet

int main()
{

  // Software Guide : BeginLatex
  // The instantiation of the function is done through the usual
  // \code{New()} method and a smart pointer.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::EuclideanDistance< MeasurementVectorType > 
    DistanceMetricType ;
  DistanceMetricType::Pointer distanceMetric = DistanceMetricType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We create three measurement vectors, the \code{originPoint},
  // the \code{queryPointA}, and the \code{queryPointB}. The type of the
  // \code{originPoint} is fixed in the
  // \subdoxygen{Statistics}{DistanceMetric} base class as
  // \code{itk::Vector< double, length of the measurement vector of the
  // each distance metric instance>}.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  DistanceMetricType::OriginType originPoint ;
  MeasurementVectorType queryPointA ;
  MeasurementVectorType queryPointB ;

  originPoint[0] = 0 ;
  originPoint[1] = 0 ;

  queryPointA[0] = 2 ;
  queryPointA[1] = 2 ;

  queryPointB[0] = 3 ;
  queryPointB[1] = 3 ;
  // Software Guide : EndCodeSnippet 

  // Software Guide : BeginLatex 
  // In the following code snippet, we show the three different
  // \code{Evaluate} methods' use.  
  // Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet 
  distanceMetric->SetOrigin( originPoint ) ;
  std::cout << "Euclidean distance between the origin and the query point A = " 
            << distanceMetric->Evaluate( queryPointA ) 
            << std::endl ;
  
  std::cout << "Euclidean distance between the two query points (A and B) = " 
            << distanceMetric->Evaluate( queryPointA, queryPointB ) 
            << std::endl ;
  
  std::cout << "Coordinate disntace between " 
            << "the first components of the two query points = "
            << distanceMetric->Evaluate( queryPointA[0], queryPointB[0] ) 
            << std::endl ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
