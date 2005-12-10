/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageHistogram1.cxx
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
// This example shows how to compute the histogram of a scalar image.  Since
// the statistics framework classes operate on Samples and
// ListOfSamples, we need to introduce a class that will make the image look
// like a list of samples. This class is the
// \subdoxygen{Statistics}{ScalarImageToListAdaptor}.  Once we have connected
// this adaptor to an image, we can proceed to use the
// \subdoxygen{Statistics}{ListSampleToHistogramGenerator} in order to compute
// the histogram of the image.
//
// First, we need to include the headers for the
// \subdoxygen{Statistics}{ScalarImageToListAdaptor} and the \doxygen{Image} classes.
//
// \index{itk::Statistics::Scalar\-Image\-To\-List\-Adaptor!header}
// \index{Statistics!Images}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkScalarImageToListAdaptor.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// Now we include the headers for the \code{ListSampleToHistogramGenerator} and
// the reader that we will use for reading the image from a file.
//
// \index{itk::Statistics::List\-Sample\-To\-Histogram\-Generator!header}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkListSampleToHistogramGenerator.h"
// Software Guide : EndCodeSnippet

int main( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageHistogram1  inputImageFileName " << std::endl;
    return -1;
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
  typedef itk::Statistics::ScalarImageToListAdaptor< ImageType >   AdaptorType;

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
    return -1;
    }
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// At this point, we are ready for instantiating the type of the histogram
// generator. Note that the adaptor type is used as template parameter of the
// generator. Having instantiated this type, we proceed to create one generator
// by invoking its \code{New()} method.
//
// \index{itk::Statistics::List\-Sample\-To\-Histogram\-Generator!instantiation}
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef PixelType        HistogramMeasurementType;

  typedef itk::Statistics::ListSampleToHistogramGenerator< 
                                                AdaptorType, 
                                                HistogramMeasurementType 
                                                                > GeneratorType;

  GeneratorType::Pointer generator = GeneratorType::New();
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
// \code{Update()} method in the generator.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef GeneratorType::HistogramType  HistogramType;

  HistogramType::SizeType size;
  size.Fill( 255 );

  generator->SetListSample( adaptor );
  generator->SetNumberOfBins( size );
  generator->SetMarginalScale( 10.0 );

  generator->Update();
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// Now we are ready for using the image histogram for any further processing.
// The histogram is obtained from the generator by invoking the
// \code{GetOutput()} method.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  HistogramType::ConstPointer histogram = generator->GetOutput();
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

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, 0 ) <<std::endl;
    }
// Software Guide : EndCodeSnippet

  return 0;
  
}


