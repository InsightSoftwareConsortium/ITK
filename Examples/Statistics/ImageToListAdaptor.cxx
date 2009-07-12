/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageToListAdaptor.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// Software Guide : BeginLatex
//
// This example shows how to instantiate an 
// \subdoxygen{Statistics}{ImageToListAdaptor} object and plug-in an 
// \doxygen{Image} object as the data source for the adaptor.
//
// \index{itk::Statistics::Image\-To\-List\-Adaptor}
// \index{itk::Statistics::Scalar\-Image\-To\-List\-Adaptor|}
// \index{itk::Statistics::Joint\-Domain\-Image\-To\-List\-Adaptor}
//
// In this example, we use the ImageToListAdaptor class that requires the
// input type of Image as the template argument. To users of the
// ImageToListAdaptor, the pixels of the input image are treated as
// measurement vectors. The ImageToListAdaptor is one of two adaptor classes
// among the subclasses of the \subdoxygen{Statistics}{Sample}. That means an
// ImageToListAdaptor object does not store any real data. The data comes
// from other ITK data container classes. In this case, an instance of the
// Image class is the source of the data.
//
// To use an ImageToListAdaptor object, include the header file for the
// class.  Since we are using an adaptor, we also should include the header
// file for the Image class.  For illustration, we use the
// \doxygen{RandomImageSource} that generates an image with random pixel
// values. So, we need to include the header file for this class.  Another
// convenient filter is the \doxygen{ScalarToArrayCastImageFilter} which
// creates an image with pixels of array type from one or more input images
// have pixels of scalar type.  Since an element of a
// Sample object is a measurement \emph{vector}, you
// cannot plug-in an image of scalar pixels. However, if we
// want to use an image with scalar pixels without the help from the
// ScalarToArrayCastImageFilter, we can use the
// \subdoxygen{Statistics}{ScalarImageToListAdaptor} class that is derived
// from the \subdoxygen{Statistics}{ImageToListAdaptor}. The usage of the
// ScalarImageToListAdaptor is identical to that of the ImageToListAdaptor.
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
  // We assume you already know how to create an image (see
  // Section~\ref{sec:CreatingAnImageSection}). The following code snippet
  // will create a 2D image of float pixels filled with random values.
  // 
  // Software Guide :EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image<float,2> FloatImage2DType;

  itk::RandomImageSource<FloatImage2DType>::Pointer random;
  random = itk::RandomImageSource<FloatImage2DType>::New();

  random->SetMin(    0.0 );
  random->SetMax( 1000.0 );
  
  typedef FloatImage2DType::SpacingValueType  SpacingValueType;
  typedef FloatImage2DType::SizeValueType     SizeValueType;
  typedef FloatImage2DType::PointValueType    PointValueType;

  SizeValueType size[2] = {20, 20};
  random->SetSize( size );

  SpacingValueType spacing[2] = {0.7, 2.1};
  random->SetSpacing( spacing );

  PointValueType origin[2] = {15, 400};
  random->SetOrigin( origin );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We now have an instance of Image and need to cast it to an
  // Image object with an array pixel type (anything derived from the
  // \doxygen{FixedArray} class such as \doxygen{Vector},
  // \doxygen{Point}, \doxygen{RGBPixel}, and
  // \doxygen{CovariantVector}).
  //
  // Since in this example the image pixel type is \code{float},
  // we will use single element a \code{float} FixedArray as
  // our measurement vector type. And that will also be our pixel type
  // for the cast filter.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::FixedArray< float, 1 > MeasurementVectorType;
  typedef itk::Image< MeasurementVectorType, 2 > ArrayImageType;
  typedef itk::ScalarToArrayCastImageFilter< FloatImage2DType, ArrayImageType >
    CasterType;

  CasterType::Pointer caster = CasterType::New();
  caster->SetInput( random->GetOutput() );
  caster->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Up to now, we have spent most of our time creating an image
  // suitable for the adaptor. Actually, the hard part of this example
  // is done. Now, we just define an adaptor with the image type and
  // instantiate an object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ImageToListAdaptor< ArrayImageType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The final task is to plug in the image object to
  // the adaptor.  After that, we can use the common methods and
  // iterator interfaces shown in Section~\ref{sec:SampleInterface}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sample->SetImage( caster->GetOutput() );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // If we are interested only in pixel values, the
  // ScalarImageToListAdaptor (scalar pixels) and the
  // ImageToListAdaptor (vector pixels) would be
  // sufficient. However, if we want to perform some statistical
  // analysis on spatial information (image index or pixel's physical
  // location) and pixel values altogether, we want to have a
  // measurement vector that consists of a pixel's value and physical
  // position. In that case, we can use the
  // \subdoxygen{Statistics}{JointDomainImageToListAdaptor}
  // class. With that class, when we call the
  // \code{GetMeasurementVector()} method, the returned measurement
  // vector is composed of the physical coordinates and pixel
  // values. The usage is almost the same as with
  // ImageToListAdaptor. One important difference between 
  // JointDomainImageToListAdaptor and the other two image
  // adaptors is that the JointDomainImageToListAdaptor is the
  // \code{SetNormalizationFactors()} method. Each component of a
  // measurement vector from the JointDomainImageToListAdaptor
  // is divided by the corresponding component value from the supplied
  // normalization factors.
  //
  // Software Guide : EndLatex

  return 0;
}
