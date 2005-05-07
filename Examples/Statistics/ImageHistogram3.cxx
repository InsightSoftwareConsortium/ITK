/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageHistogram3.cxx
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
// By now, you are probably thinking that the statistics framework in ITK is
// too complex for simply computing histograms from images. Here we illustrate
// that the benefit for this complexity is the power that these methods provide
// for dealing with more complex and realistic uses of image statistics than the
// trivial 256-bin histogram of 8-bit images that most software packages
// provide. One of such cases is the computation of histograms from
// multi-component images such as Vector images and color images.
//
// This example shows how to compute the histogram of an RGB image by using the
// helper class \code{ImageToHistogramGenerator}.  In this first example we
// compute the histogram of each channel independently.
//
// We start by including the header of the
// \subdoxygen{Statistics}{ImageToHistogramGenerator}, as well as the headers
// for the image class and the RGBPixel class.
//
// \index{itk::Statistics::ImageToHistogramGenerator!header}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkImageToHistogramGenerator.h"
#include "itkImage.h"
#include "itkRGBPixel.h"
// Software Guide : EndCodeSnippet




#include "itkImageFileReader.h"

int main( int argc, char * argv [] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageHistogram1  inputRGBImageFileName " << std::endl;
    return -1;
    }



// Software Guide : BeginLatex
//
// The type of the RGB image is defined by first instantiating a RGBPixel and
// then using the image dimension specification.
//
// \index{itk::Statistics!Color Images}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef unsigned char                         PixelComponentType;

  typedef itk::RGBPixel< PixelComponentType >   RGBPixelType;

  const unsigned int                            Dimension = 2;

  typedef itk::Image< RGBPixelType, Dimension > RGBImageType;
// Software Guide : EndCodeSnippet




  typedef itk::ImageFileReader< RGBImageType >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return -1;
    }



// Software Guide : BeginLatex
//
// Using the RGB image type we can instantiate the type of the corresponding
// histogram generator and construct one generator by invoking its \code{New()}
// method.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ImageToHistogramGenerator< 
                            RGBImageType >   HistogramGeneratorType;

  HistogramGeneratorType::Pointer histogramGenerator = 
                                             HistogramGeneratorType::New();
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// The parameters of the histogram must be defined now. Probably the most
// important one is the arrangement of histogram bins. This is provided to the
// histogram through a size array. The type of the array can be taken from the
// traits of the \code{HistogramGeneratorType} type. We create one instance of
// the size object and fill in its content. In this particular case, the three
// components of the size array will correspond to the number of bins used for
// each one of the RGB components in the color image. The following lines show
// how to define a histogram on the red component of the image while
// disregarding the green and blue components.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef HistogramGeneratorType::SizeType   SizeType;

  SizeType size;

  size[0] = 255;        // number of bins for the Red   channel
  size[1] =   1;        // number of bins for the Green channel
  size[2] =   1;        // number of bins for the Blue  channel

  histogramGenerator->SetNumberOfBins( size );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// The marginal scale must be defined in the generator. This will determine the
// precision in the assignment of values to the histogram bins.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  histogramGenerator->SetMarginalScale( 10.0 );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// The input of the generator is taken from an image reader, and the
// computation of the histogram is triggered by invoking the \code{Compute()}
// method of the generator.
//
// \index{itk::Statistics::ImageToHistogramGenerator!Compute()}
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  histogramGenerator->SetInput(  reader->GetOutput()  );

  histogramGenerator->Compute();
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// We can now access the results of the histogram computation by declaring a
// pointer to histogram and getting its value from the generator using the
// \code{GetOutput()} method. Note that here we use a \code{const
// HistogramType} pointer instead of a const smart pointer because we are sure
// that the generator is not going to be destroyed while we access the values
// of the histogram. Depending on what you are doing, it may be safer to assign
// the histogram to a const smart pointer as shown in previous examples.
//
// \index{itk::Statistics::ImageToHistogramGenerator!GetOutput()}
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef HistogramGeneratorType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramGenerator->GetOutput();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// Just for the sake of exercising the experimental method~\cite{Popper2002}, we
// verify that the resulting histogram actually have the size that we requested
// when we configured the generator. This can be done by invoking the
// \code{Size()} method of the histogram and printing out the result.
//
// \index{itk::Statistics::Histogram!Size()}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size " << histogramSize << std::endl;
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// Strictly speaking, the histogram computed here is the joint histogram of the
// three RGB components. However, given that we set the resolution of the green
// and blue channels to be just one bin, the histogram is in practice
// representing just the red channel.  In the general case, we can alway access
// the frequency of a particular channel in a joint histogram, thanks to the
// fact that the histogram class offers a \code{GetFrequency()} method that
// accepts a channel as argument. This is illustrated in the following lines of
// code.
//
// \index{itk::Statistics::Histogram!GetFrequency()}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  unsigned int channel = 0;  // red channel

  std::cout << "Histogram of the red component" << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, channel ) << std::endl;
    }
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// In order to reinforce the concepts presented above, we modify now the setup
// of the histogram generator in order to compute the histogram of the green
// channel instead of the red one. This is done by simply changing the number
// of bins desired on each channel and invoking the computation of the
// generator again by calling the \code{Compute()} method.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  size[0] =   1;  // number of bins for the Red   channel
  size[1] = 255;  // number of bins for the Green channel
  size[2] =   1;  // number of bins for the Blue  channel

  histogramGenerator->SetNumberOfBins( size );
  
  histogramGenerator->Compute();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// The result can be verified now by setting the desired channel to green and
// invoking the \code{GetFrequency()} method.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  channel = 1;  // green channel

  std::cout << "Histogram of the green component" << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, channel ) << std::endl;
    }
// Software Guide : EndCodeSnippet




  
// Software Guide : BeginLatex
//
// To finalize the example, we do the same computation for the case of the blue
// channel.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  size[0] =   1;  // number of bins for the Red   channel
  size[1] =   1;  // number of bins for the Green channel
  size[2] = 255;  // number of bins for the Blue  channel

  histogramGenerator->SetNumberOfBins( size );
  
  histogramGenerator->Compute();
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// and verify the output.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  channel = 2;  // blue channel

  std::cout << "Histogram of the blue component" << std::endl;

  for( unsigned int bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, channel ) << std::endl;
    }
// Software Guide : EndCodeSnippet


  return 0;
  
}


