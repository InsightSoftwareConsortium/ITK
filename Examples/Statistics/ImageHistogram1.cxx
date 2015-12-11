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
// This example shows how to compute the histogram of a scalar image.  Since
// the statistics framework classes operate on Samples and
// ListOfSamples, we need to introduce a class that will make the image look
// like a list of samples. This class is the
// \subdoxygen{Statistics}{ImageToListSampleAdaptor}.  Once we have connected
// this adaptor to an image, we can proceed to use the
// \subdoxygen{Statistics}{SampleToHistogramFilter} in order to compute
// the histogram of the image.
//
// First, we need to include the headers for the
// \subdoxygen{Statistics}{ImageToListSampleAdaptor} and the \doxygen{Image} classes.
//
// \index{itk::Statistics::Scalar\-Image\-To\-List\-Adaptor!header}
// \index{Statistics!Images}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageToListSampleAdaptor.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Now we include the headers for the \code{Histogram}, the
// \code{SampleToHistogramFilter}, and the reader that we will use for
// reading the image from a file.
//
// \index{itk::Statistics::List\-Sample\-To\-Histogram\-Generator!header}
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkHistogram.h"
#include "itkSampleToHistogramFilter.h"
// Software Guide : EndCodeSnippet

int main( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageHistogram1  inputImageFileName " << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // The image type must be defined using the typical pair of pixel type and
  // dimension specification.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned char       PixelType;
  const unsigned int          Dimension = 2;

  typedef itk::Image<PixelType, Dimension > ImageType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Using the same image type we instantiate the type of the image reader that
  // will provide the image source for our example.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType > ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Now we introduce the central piece of this example, which is the use of the
  // adaptor that will present the \doxygen{Image} as if it was a list of
  // samples. We instantiate the type of the adaptor by using the actual image
  // type. Then construct the adaptor by invoking its \code{New()} method and
  // assigning the result to the corresponding smart pointer. Finally we connect
  // the output of the image reader to the input of the adaptor.
  //
  // \index{itk::Statistics::Scalar\-Image\-To\-List\-Adaptor!instantiation}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ImageToListSampleAdaptor< ImageType >   AdaptorType;

  AdaptorType::Pointer adaptor = AdaptorType::New();

  adaptor->SetImage(  reader->GetOutput() );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // You must keep in mind that adaptors are not pipeline objects. This means
  // that they do not propagate update calls. It is therefore your responsibility
  // to make sure that you invoke the \code{Update()} method of the reader before
  // you attempt to use the output of the adaptor. As usual, this must be done
  // inside a try/catch block because the read operation can potentially throw
  // exceptions.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // At this point, we are ready for instantiating the type of the histogram
  // filter. We must first declare the type of histogram we wish to use.
  // The adaptor type is also used as template parameter of the filter.
  // Having instantiated this type, we proceed to create one filter
  // by invoking its \code{New()} method.
  //
  // \index{itk::Statistics::Sample\-To\-Histogram\-Filter!instantiation}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef PixelType HistogramMeasurementType;
  typedef itk::Statistics::Histogram< HistogramMeasurementType >
    HistogramType;
  typedef itk::Statistics::SampleToHistogramFilter<
                                                AdaptorType,
                                                HistogramType>
                                                FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We define now the characteristics of the Histogram that we want to compute.
  // This typically includes the size of each one of the component, but given
  // that in this simple example we are dealing with a scalar image, then our
  // histogram will have a single component. For the sake of generality, however,
  // we use the \code{HistogramType} as defined inside of the Generator type. We
  // define also the marginal scale factor that will control the precision used
  // when assigning values to histogram bins. Finally we invoke the
  // \code{Update()} method in the filter.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfComponents = 1;
  HistogramType::SizeType size( numberOfComponents );
  size.Fill( 255 );

  filter->SetInput( adaptor );
  filter->SetHistogramSize( size );
  filter->SetMarginalScale( 10 );

  HistogramType::MeasurementVectorType min( numberOfComponents );
  HistogramType::MeasurementVectorType max( numberOfComponents );

  min.Fill( 0 );
  max.Fill( 255 );

  filter->SetHistogramBinMinimum( min );
  filter->SetHistogramBinMaximum( max );

  filter->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Now we are ready for using the image histogram for any further processing.
  // The histogram is obtained from the filter by invoking the
  // \code{GetOutput()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  HistogramType::ConstPointer histogram = filter->GetOutput();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // In this current example we simply print out the frequency values of all the
  // bins in the image histogram.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size " << histogramSize << std::endl;

  for (unsigned int bin=0; bin < histogramSize; ++bin)
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, 0 ) <<std::endl;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;

}
