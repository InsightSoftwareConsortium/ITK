/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageHistogram4.cxx
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
// The statistics framework in ITK has been designed for managing multi-variate
// statistics in a natural way. The \subdoxygen{Statistics}{Histogram} class
// reflects this concept clearly since it is a N-variable joint histogram. This
// nature of the Histogram class is exploited in the following example in order
// to build the joint histogram of a color image encoded in RGB values.
//
// Note that the same treatment could be applied further to any vector image
// thanks to the generic programming approach used in the implementation of the
// statistical framework.
//
// The most relevant class in this example is the
// \subdoxygen{Statistics}{ImageToHistogramGenerator}. This class will take
// care of adapting the \doxygen{Image} to a list of samples and then to a
// histogram generator. The user is only bound to provide the desired
// resolution on the histogram bins for each one of the image components.
//
// In this example we compute the joint histogram of the three channels of an
// RGB image. Our output histogram will be equivalent to a 3D array of bins.
// This histogram could be used further for feeding a segmentation method based
// on statistical pattern recognition. Such method was actually used during the
// generation of the image in the cover of the Software Guide.
//
// The first step is to include the header files for the histogram generator,
// the RGB pixel type and the Image.
//
// \index{itk::Statistics::ImageToHistogramGenerator!header}
// \index{itk::RGBPixel!header}
// \index{itk::RGBPixel!Statistics}
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

  if( argc < 3 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageHistogram1  inputRGBImageFileName ";
    std::cerr << " histogramFilename.raw" << std::endl;
    return -1;
    }



// Software Guide : BeginLatex
//
// We declare now the type used for the components of the RGB pixel,
// instantiate the type of the RGBPixel and instantiate the image type.
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
    std::cerr << "Problem reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return -1;
    }


// Software Guide : BeginLatex
//
// Using the type of the color image, and in general of any vector image, we
// can now instantiate the type of the histogram generator class. We then use
// that type for constructing an instance of the generator by invoking its
// \code{New()} method and assigning the result to a smart pointer.
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
// The resolution at which the statistics of each one of the color component
// will be evaluated is defined by setting the number of bins along every
// component in the joint histogram. For this purpose we take the
// \code{SizeType} trait from the generator and use it to instantiate a
// \code{size} variable. We set in this variable the number of bins to use for
// each component of the color image.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef HistogramGeneratorType::SizeType   SizeType;

  SizeType size;

  size[0] = 255;  // number of bins for the Red   channel
  size[1] = 255;  // number of bins for the Green channel
  size[2] = 255;  // number of bins for the Blue  channel

  histogramGenerator->SetNumberOfBins( size );
// Software Guide : EndCodeSnippet



  

// Software Guide : BeginLatex
//
// The input to the histogram generator is taken from the output of an image
// reader. Of course, the output of any filter producing an RGB image could
// have been used instead of a reader.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  histogramGenerator->SetInput(  reader->GetOutput()  );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// The marginal scale is defined in the histogram generator. This value will
// define the precision in the assignment of values to the histogram bins.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  histogramGenerator->SetMarginalScale( 10.0 );
// Software Guide : EndCodeSnippet






// Software Guide : BeginLatex
//
// Finally, the computation of the histogram is triggered by invoking the
// \code{Compute()} method of the generator. Note that generators are not
// pipeline objects. It is therefore your responsibility to make sure that you
// update the filter that provides the input image to the generator.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  histogramGenerator->Compute();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// At this point, we can recover the histogram by calling the
// \code{GetOutput()} method of the generator. The result is assigned to a
// variable that is instantiated using the \code{HistogramType} trait of the
// generator type.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef HistogramGeneratorType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramGenerator->GetOutput();
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// We can verify that the computed histogram has the requested size by invoking
// its \code{Size()} method.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size " << histogramSize << std::endl;
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The values of the histogram can now be saved into a file by walking through
// all of the histogram bins and pushing them into a std::ofstream.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  std::ofstream histogramFile;
  histogramFile.open( argv[2] );
  
  HistogramType::ConstIterator itr = histogram->Begin();
  HistogramType::ConstIterator end = histogram->End();

  typedef HistogramType::FrequencyType FrequencyType;

  while( itr != end )
    {
    const FrequencyType frequency = itr.GetFrequency();
    histogramFile.write( (const char *)(&frequency), sizeof(frequency) );
    ++itr;
    }

  histogramFile.close();
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// Note that here the histogram is saved as a block of memory in a raw file. At
// this point you can use visualization software in order to explore the
// histogram in a display that would be equivalent to a scatter plot of the RGB
// components of the input color image.
//
// Software Guide : EndLatex 


 
  return 0;
  
  
}


