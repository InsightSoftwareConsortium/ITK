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
// \subdoxygen{Statistics}{ImageToListAdaptor} object and plug-in an 
// \doxygen{Image} object as the data source for the adaptor.
//
// \index{itk::Statistics::ImageToListAdaptor|textbf}
// \index{itk::Statistics::ScalarImageToListAdaptor|textbf}
// \index{itk::Statistics::JointDomainImageToListAdaptor|textbf}
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
// file for this class.  Another convenient filter is the
// \doxygen{ScalarToArrayCastImageFilter} which creates an
// \doxygen{Image} object with pixels of array type from one or more
// input \doxygen{Image} objects that has pixels of scalar type.
// Since an element of a \subdoxygen{Statistics}{Sample} object is a
// measurement \emph{vector}, you cannot plug-in an \doxygen{Image}
// object of scalar pixels. However, if we want to use an image with
// scalar pixels without the help from the
// \doxygen{ScalarToArrayCastImageFilter}, we can use the
// \subdoxygen{Statistics}{ScalarImageToListAdaptor} class that is
// derived from the \subdoxygen{Statistics}{ImageToListAdaptor}. The
// usage of the \code{ScalarImageToListAdaptor} is identical to that of
// the \code{ImageToListAdaptor}.
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
  // We assume you already know how to create an \doxygen{Image} object (see
  // section \ref{sec:CreatingAnImageSection} otherwise. The following code
  // snippet will create a 2D image of float pixels filled with random values.
  // 
  // Software Guide :EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image<float,2> FloatImage2DType;

  itk::RandomImageSource<FloatImage2DType>::Pointer random;
  random = itk::RandomImageSource<FloatImage2DType>::New();

  random->SetMin(    0.0 );
  random->SetMax( 1000.0 );
  
  unsigned long size[2] = {20, 20};
  random->SetSize( size );

  float spacing[2] = {0.7, 2.1};
  random->SetSpacing( spacing );

  float origin[2] = {15, 400};
  random->SetOrigin( origin );
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
  typedef itk::FixedArray< float, 1 > MeasurementVectorType;
  typedef itk::Image< MeasurementVectorType, 2 > ArrayImageType;
  typedef itk::ScalarToArrayCastImageFilter< 
                                      FloatImage2DType, 
                                      ArrayImageType >    CasterType;

  CasterType::Pointer caster = CasterType::New();
  caster->SetInput( random->GetOutput() );
  caster->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Up to now, we have spent most of our time preparing an \doxygen{Image}
  // object suitable for the adaptor. Actually, the hard part of this example
  // is done. Now, we just define an adaptor with the image type and
  // instantiate an object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ImageToListAdaptor< ArrayImageType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The final thing we have to do is to plug in the image object to
  // the adaptor.  After that, we can use the common methods and
  // iterator interfaces shown in section \ref{sec:SampleInterface}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sample->SetImage( caster->GetOutput() );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // If we are interested only in pixel values, the
  // \code{ScalarImageToListAdatpor} (scalar pixels) and the
  // \code{ImageToListAdaptor} (vector pixels) would be
  // sufficient. However, if we want to perform some statistical
  // analysis on spatial information (image index or pixel's physical
  // location) and pixel values altogether, we want to have a
  // measurement vector that consists of a pixel's value and physical
  // position. In that case, we can use the
  // \subdoxygen{Statistics}{JointDomainImageToListAdaptor}
  // class. With that class, when we call the
  // \code{GetMeasurementVector} method, the returned measurement
  // vector is composed of the physical coordinates and pixel
  // values. The usage is almost the same as with
  // \code{ImageToListAdaptor}. One important difference between the
  // \code{JointDomainImageToListAdaptor} and the other two image
  // adaptors is that the \code{JointDomainImageToListAdaptor} has the
  // \code{SetNormalizationFactors} method. Each component of a
  // measurement vector from the \code{JointDomainImageToListAdaptor}
  // is divided by the corresponding component value from the supplied
  // normalization factors.
  // Software Guide : EndLatex

  return 0;
}
