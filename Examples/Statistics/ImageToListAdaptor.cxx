/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageToListAdaptor.cxx
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
// This example shows how to instantiate an 
// \subdoxygen{Statistics}{ImageToListAdaptor} object and plug-in 
// \doxygen{Image} object as the data source for the adaptor.
//
// \index{itk::Statistics::ImageToListAdaptor|textbf}
//
// In this example, we use the
// \subdoxygen{Statistics}{ImageToListAdaptor} class that requires the
// type of input \doxygen{Image} object as the template argument. To
// the users of the \subdoxygen{Statistics}{ImageToListAdaptor}, the
// pixels of the input \doxygen{Image} object are treated as
// measurement vectors. The
// \subdoxygen{Statistics}{ImageToListAdaptor} is one of two adaptor
// classes among the subclasses of the
// \subdoxygen{Statistics}{Sample}. That means an
// \subdoxygen{Statistics}{ImageToListAdaptor} object doesn't store
// any real data. The data comes from other Insight data container
// classes. In this case, an \doxygen{Image} object is the source of
// data.
//
// To use an \subdoxygen{Statistics}{ImageToListAdaptor} object, you
// should include the header file for the class.  Since we are using
// an adaptor, we also should include the header file for the
// \doxygen{Image} class.  For illustration, we use the
// \doxygen{RandomImageSource} that generates an \doxygen{Image}
// object with random pixel values. So, we need to include the header
// file for that.  Another convenient filter is the
// \doxygen{ScalarToArrayCastImageFilter} which creates an
// \doxygen{Image} object with pixels of array type from one or more
// input \doxygen{Image} objects that has pixels of scalar type.
// Since an element of a \subdoxygen{Statistics}{Sample} object is a
// measurement \emph{vector}, you cannot plug-in an \doxygen{Image}
// object of scalar pixels.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageToListAdaptor.h"
#include "itkImage.h"
#include "itkRandomImageSource.h"
#include "itkScalarToArrayCastImageFilter.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // We assume you already know how to create an \doxygen{Image} object. The
  // following code snippet will create a 2D image of float pixels filled
  // with random values.
  // 
  // Software Guide :EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image<float,2> FloatImage2DType ;

  itk::RandomImageSource<FloatImage2DType>::Pointer random ;
  random = itk::RandomImageSource<FloatImage2DType>::New() ;
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
  // We now have an \doxygen{Image} object and need to cast it to an
  // \doxygen{Image} object with array type (anything derived from the
  // \doxygen{FixedArray} class such as \doxygen{Vector},
  // \doxygen{Point}, \doxygen{RGBPixel}, and
  // \doxygen{CovarinatVector}) pixels.
  //
  // Since, the \doxygen{Image} object's pixel type is \code{float},
  // We will use single element \code{float} \doxygen{FixedArray} as
  // our measurement vector type. And that will also be our pixel type
  // for the cast filter.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::FixedArray< float, 1 > MeasurementVectorType ;
  typedef itk::Image< MeasurementVectorType, 2 > ArrayImageType ;
  typedef itk::ScalarToArrayCastImageFilter< FloatImage2DType, 
    ArrayImageType > CasterType ;

  CasterType::Pointer caster = CasterType::New() ;
  caster->SetInput( random->GetOutput() ) ;
  caster->Update() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Up to now, we have spent most of time to prepare an
  // \doxygen{Image} object suitable for the adaptor. Actually, the
  // hard part of this example is done. Now, we must define an adaptor
  // with the image type and instantiate an object.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ImageToListAdaptor< ArrayImageType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The final thing we have to is to plug in the image object to the adaptor.
  // After that, we can use the common methods and iterator interfaces 
  // shown in section \ref{sec:SampleInterface}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sample->SetImage( caster->GetOutput() ) ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
