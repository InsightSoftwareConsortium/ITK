/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    PointSetToListAdaptor.cxx
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
//
// We will decribe how to use \doxygen{PointSet} as a \code{Sample} 
// using an adaptor in this example.
//
// \index{itk::Statistics::PointSetToListAdaptor|textbf}
//
// The \subdoxygen{Statistics}{PointSetToListAdaptor} class requires
// the type of input \doxygen{PointSet} object. The \doxygen{PointSet}
// class is an associative data container. Each point in a
// \code{PointSet} object can have an associated data value
// optionally. For the statistics subsystem, current implementation of
// \code{PointSetToListAdaptor} takes only the point part into
// consideration. In other words, the measurement vectors from a
// \code{PointSetToListAdaptor} object are points from the
// \code{PointSet} object that is plugged into the adaptor object.
//
// To use an \subdoxygen{Statistics}{PointSetToListAdatpr} object, we
// include the header file for the class.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkPointSetToListAdaptor.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// Since we are using an adaptor, we also include the header file for
// the \doxygen{PointSet} class.
// Software Guide :EndLatex

// Software Guide : BeginCodeSnippet
#include "itkPointSet.h"
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // We assume you already know how to create an \doxygen{PointSet} object. The
  // following code snippet will create a \code{PointSet} object. that 
  // stores points (its coordinate value type is float) in 3D space.
  // 
  // Software Guide :EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::PointSet< short > PointSetType ;
  PointSetType::Pointer pointSet = PointSetType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // If we want to change the type of Point in terms of the coordinate value
  // and/or dimension, we have to modify the \code{TMeshTraits} (one of
  // the optional template arguments for the \code{PointSet} class. The easiest
  // way of having a custom mesh traits instance is specialization of the
  // existing \doxygen{DefaultStaticMeshTraits}. Among the template arguments 
  // of the \code{DefaultStaticMeshTraits}, by specifying the TCoordRep 
  // template argument, we can change the coordinate value type of a point.
  // By specifying the VPointDimension template argument, we can change the 
  // dimension of the point. As mentioned earlier, a 
  // \code{PointSetToListAdaptor} object cares only about the points, and 
  // the type of measurement vectors is the type of points. Therefore, we
  // can define the measurment vector type as in the following code snippet.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef PointSetType::PointType MeasurementVectorType ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // To make the example a little bit realistic, we add two point 
  // into the \code{pointSet}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointSetType::PointType point ;
  point[0] = 1.0 ;
  point[1] = 2.0 ;
  point[2] = 3.0 ;

  pointSet->SetPoint( 0UL, point) ;

  point[0] = 2.0 ;
  point[1] = 4.0 ;
  point[2] = 6.0 ;

  pointSet->SetPoint( 1UL, point ) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We have a \code{PointSet} object that has two points in it. And the 
  // \code{pointSet} is ready for plugging-in.
  // First, we create an instance of the \code{PointSetToListAdaptor} class
  // with the type of the input \code{PointSet} object.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::PointSetToListAdaptor< PointSetType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Second, just as we did with \code{ImageToListAdaptor} example in
  // section \ref{sec:ImageToListAdaptor}, all we have to is to
  // plug in the \code{PointSet} object to the adaptor.  After that,
  // we can use the common methods and iterator interfaces shown in
  // the section \ref{sec:SampleInterface}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sample->SetPointSet( pointSet ) ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
