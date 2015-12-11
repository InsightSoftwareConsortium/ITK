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
// This example shows how to instantiate an
// \subdoxygen{Statistics}{ImageToListSampleAdaptor} object and plug-in an
// \doxygen{Image} object as the data source for the adaptor.
//
// \index{itk::Statistics::Image\-To\-List\-Adaptor}
// \index{itk::Statistics::Scalar\-Image\-To\-List\-Adaptor|}
// \index{itk::Statistics::Joint\-Domain\-Image\-To\-List\-Adaptor}
//
// In this example, we use the ImageToListSampleAdaptor class that requires the
// input type of Image as the template argument. To users of the
// ImageToListSampleAdaptor, the pixels of the input image are treated as
// measurement vectors. The ImageToListSampleAdaptor is one of two adaptor classes
// among the subclasses of the \subdoxygen{Statistics}{Sample}. That means an
// ImageToListSampleAdaptor object does not store any real data. The data comes
// from other ITK data container classes. In this case, an instance of the
// Image class is the source of the data.
//
// To use an ImageToListSampleAdaptor object, include the header file for the
// class.  Since we are using an adaptor, we also should include the header
// file for the Image class.  For illustration, we use the
// \doxygen{RandomImageSource} that generates an image with random pixel
// values. So, we need to include the header file for this class.  Another
// convenient filter is the \doxygen{ComposeImageFilter} which
// creates an image with pixels of array type from one or more input images
// composed of pixels of scalar type.  Since an element of a
// Sample object is a measurement \emph{vector}, you
// cannot plug in an image of scalar pixels. However, if we
// want to use an image of scalar pixels without the help from the
// ComposeImageFilter, we can use the
// \subdoxygen{Statistics}{ScalarImageToListSampleAdaptor} class that is derived
// from the \subdoxygen{Statistics}{ImageToListSampleAdaptor}. The usage of the
// ScalarImageToListSampleAdaptor is identical to that of the ImageToListSampleAdaptor.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageToListSampleAdaptor.h"
#include "itkImage.h"
#include "itkRandomImageSource.h"
#include "itkComposeImageFilter.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // We assume you already know how to create an image.
  // The following code snippet will create a 2D image of float pixels
  // filled with random values.
  //
  // Software Guide : EndLatex

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
  // \doxygen{Point}, \doxygen{RGBPixel}, or
  // \doxygen{CovariantVector}).
  //
  // Since the image pixel type is \code{float} in this example,
  // we will use a single element \code{float} FixedArray as
  // our measurement vector type. And that will also be our pixel type
  // for the cast filter.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::FixedArray< float, 1 >            MeasurementVectorType;
  typedef itk::Image< MeasurementVectorType, 2 > ArrayImageType;
  typedef itk::ComposeImageFilter< FloatImage2DType, ArrayImageType >
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
  typedef itk::Statistics::ImageToListSampleAdaptor<
                                                  ArrayImageType > SampleType;
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
  // ScalarImageToListSampleAdaptor (scalar pixels) or the
  // ImageToListSampleAdaptor (vector pixels) would be
  // sufficient. However, if we want to perform some statistical
  // analysis on spatial information (image index or pixel's physical
  // location) and pixel values altogether, we want to have a
  // measurement vector that consists of a pixel's value and physical
  // position. In that case, we can use the
  // \subdoxygen{Statistics}{JointDomainImageToListSampleAdaptor}
  // class. With this class, when we call the
  // \code{GetMeasurementVector()} method, the returned measurement
  // vector is composed of the physical coordinates and pixel
  // values. The usage is almost the same as with
  // ImageToListSampleAdaptor. One important difference between
  // JointDomainImageToListSampleAdaptor and the other two image
  // adaptors is that the JointDomainImageToListSampleAdaptor has the
  // \code{SetNormalizationFactors()} method. Each component of a
  // measurement vector from the JointDomainImageToListSampleAdaptor
  // is divided by the corresponding component value from the supplied
  // normalization factors.
  //
  // Software Guide : EndLatex

  return EXIT_SUCCESS;
}
