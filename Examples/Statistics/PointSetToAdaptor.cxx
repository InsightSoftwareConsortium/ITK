/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    PointSetToAdaptor.cxx
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
// \index{itk::Sample!PointSetToListAdaptor|textbf}
//
// \subdoxygen{Statistics}{PointSetToListAdaptor} class requires the type 
// of input \doxygen{PointSet} object. The \doxygen{PointSet} class is an
// associative data container. Each point in a \code{PointSet} object can have
// its associated data value (optional). For the statistics subsystem, current
// implementation of \code{PointSetToListAdaptor} takes only the point part 
// into consideration. In other words, the measurement vectors from a
// \code{PointSetToListAdaptor} object are points from the \code{PointSet}
// object that is plugged-into the adaptor object.
//
// To use, an \doxygen{PointSetToListAdatpr} object, we include the 
// header file for the class.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkPointSetToListAdaptor.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// Since, we are using an adaptor, we also include the header file for
// the \doxygen{PointSet} class.
// Software Guide :EndLatex

// Software Guide : BeginCodeSnippet
#include "itkPointSet.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // We assume you already know how to create an \doxygen{PointSet} object. The
  // following code snippet will create a 2D image of float pixels filled
  // with random values.
  // 
  // Software Guide :EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::PointSet<float,2> FloatPointSet2DType ;

  itk::RandomPointSetSource<FloatPointSet2DType>::Pointer random ;
  random = itk::RandomPointSetSource<FloatPointSet2DType>::New() ;
  random->SetMin(0.0) ;
  random->SetMax(1000.0) ;
  
  unsigned long size[2] = {20, 20} ;
  random->SetSize(size) ;
  float spacing[2] = {0.7, 2.1} ;
  random->SetSpacing( spacing ) ;
  float origin[2] = {15, 400} ;
  random->SetOrigin( origin ) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We now have an \doxygen{PointSet} object and need to cast it to an
  // \doxygen{PointSet} object with array type (anything derived from
  // the \doxygen{FixedArray} class) pixels.
  //
  // Since, the \doxygen{PointSet} object's pixel type is \code{float},
  // We will use single element \code{float} \doxygen{FixedArray} 
  // as our measurement vector type. And that will also be our pixel 
  // type for the cast filter.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::FixedArray< float, 1 > MeasurementVectorType ;
  typedef itk::PointSet< MeasurementVectorType, 2 > ArrayPointSetType ;
  typedef itk::ScalarToArrayCastPointSetFilter< FloatPointSet2DType, 
    ArrayPointSetType > CasterType ;

  CasterType::Pointer caster = CasterType::New() ;
  caster->SetInput( random->GetOutput() ) ;
  caster->Update() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Up to now, we spend most of time to prepare an \doxygen{PointSet} object
  // suitable for the adaptor. Actually, the hard part of this example is
  // done. Now, we must define an adaptor with the image type and 
  // instantiate an object.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::PointSetToListAdaptor< ArrayPointSetType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The final thing we have to is to plug-in the image object to the adaptor.
  // After that, we can use the common methods and iterator interfaces 
  // shown in \ref{sec:StatInstantiationInterfaces}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sample->SetPointSet( caster->GetOutput() ) ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
