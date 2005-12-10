/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageHistogram2.cxx
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
// From the previous example you will have noticed that there is a significant
// number of operations to perform to compute the simple histogram of
// a scalar image. Given that this is a relatively common operation, it is
// convenient to encapsulate many of these operations in a single helper class. 
//
// The \subdoxygen{Statistics}{ScalarImageToHistogramGenerator} is the result
// of such encapsulation.  This example illustrates how to compute the
// histogram of a scalar image using this helper class.
//
// Software Guide : EndLatex 




// Software Guide : BeginLatex
//
// We should first include the header of the histogram generator and the image
// class.
//
// \index{itk::Statistics::Scalar\-Image\-To\-Histogram\-Generator!header}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkScalarImageToHistogramGenerator.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"

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



  typedef itk::ImageFileReader< ImageType > ReaderType;

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
// We use now the image type in order to instantiate the type of the
// corresponding histogram generator class, and invoke its \code{New()} method
// in order to construct one.
//
// \index{itk::Statistics::Scalar\-Image\-To\-Histogram\-Generator!header}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ScalarImageToHistogramGenerator< 
                                 ImageType >   HistogramGeneratorType;

  HistogramGeneratorType::Pointer histogramGenerator =
                                        HistogramGeneratorType::New();
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// The image to be passed as input to the histogram generator is taken in this
// case from the output of an image reader.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  histogramGenerator->SetInput(  reader->GetOutput() );
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// We define also the typical parameters that specify the characteristics of
// the histogram to be computed.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  histogramGenerator->SetNumberOfBins( 255 );
  histogramGenerator->SetMarginalScale( 10.0 );
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// Finally we trigger the computation of the histogram by invoking the
// \code{Compute()} method of the generator. Note again, that a generator is
// not a pipeline object and therefore it is up to you to make sure that the
// filters providing the input image have been updated.
//
// \index{itk::Statistics::Scalar\-Image\-To\-Histogram\-Generator!Compute()}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  histogramGenerator->Compute();
// Software Guide : EndCodeSnippet



// Software Guide : BeginLatex
//
// The resulting histogram can be obtained from the generator by invoking its
// \code{GetOutput()} method. It is also convenient to get the Histogram type
// from the traits of the generator type itself as shown in the code below.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef HistogramGeneratorType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramGenerator->GetOutput();
// Software Guide : EndCodeSnippet



  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size " << histogramSize << std::endl;

  unsigned int bin;
  for( bin=0; bin < histogramSize; bin++ )
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, 0 ) << std::endl;
    }




// Software Guide : BeginLatex
//
// In this case we simply print out the frequency values of the histogram.
// These values can be accessed by using iterators.
//
// \index{itk::Statistics::Histogram!Iterators}
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  HistogramType::ConstIterator itr = histogram->Begin();
  HistogramType::ConstIterator end = histogram->End();
 
  unsigned int binNumber = 0;
  while( itr != end )
    {
    std::cout << "bin = " << binNumber << " frequency = ";
    std::cout << itr.GetFrequency() << std::endl;     
    ++itr;
    ++binNumber;
    }
// Software Guide : EndCodeSnippet


  return 0;
  
}


