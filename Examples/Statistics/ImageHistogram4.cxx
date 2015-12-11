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
// \subdoxygen{Statistics}{ImageToHistogramFilter}. This class will take
// care of adapting the \doxygen{Image} to a list of samples and then to a
// histogram filter. The user is only bound to provide the desired
// resolution on the histogram bins for each one of the image components.
//
// In this example we compute the joint histogram of the three channels of an
// RGB image. Our output histogram will be equivalent to a 3D array of bins.
// This histogram could be used further for feeding a segmentation method based
// on statistical pattern recognition. Such method was actually used during the
// generation of the image in the cover of the Software Guide.
//
// The first step is to include the header files for the histogram filter,
// the RGB pixel type and the Image.
//
// \index{itk::Statistics::ImageToHistogramFilter!header}
// \index{itk::RGBPixel!header}
// \index{itk::RGBPixel!Statistics}
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkImageToHistogramFilter.h"
#include "itkImage.h"
#include "itkRGBPixel.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"

int main( int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageHistogram4  inputRGBImageFileName ";
    std::cerr << " histogramFilename.raw" << std::endl;
    return EXIT_FAILURE;
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
    return EXIT_FAILURE;
    }


  // Software Guide : BeginLatex
  //
  // Using the type of the color image, and in general of any vector image, we
  // can now instantiate the type of the histogram filter class. We then use
  // that type for constructing an instance of the filter by invoking its
  // \code{New()} method and assigning the result to a smart pointer.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ImageToHistogramFilter<
                                 RGBImageType >   HistogramFilterType;

  HistogramFilterType::Pointer histogramFilter =
                                           HistogramFilterType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The resolution at which the statistics of each one of the color component
  // will be evaluated is defined by setting the number of bins along every
  // component in the joint histogram. For this purpose we take the
  // \code{HistogramSizeType} trait from the filter and use it to instantiate a
  // \code{size} variable. We set in this variable the number of bins to use for
  // each component of the color image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef HistogramFilterType::HistogramSizeType   SizeType;

  SizeType size(3);

  size[0] = 256;  // number of bins for the Red   channel
  size[1] = 256;  // number of bins for the Green channel
  size[2] = 256;  // number of bins for the Blue  channel

  histogramFilter->SetHistogramSize( size );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Finally, we must specify the upper and lower bounds for the histogram
  // using the \code{SetHistogramBinMinimum()} and
  // \code{SetHistogramBinMaximum()} methods.
  //
  // Software Guide : EndLatexex

  // Software Guide : BeginCodeSnippet
  typedef HistogramFilterType::HistogramMeasurementVectorType
    HistogramMeasurementVectorType;

  HistogramMeasurementVectorType binMinimum( 3 );
  HistogramMeasurementVectorType binMaximum( 3 );

  binMinimum[0] = -0.5;
  binMinimum[1] = -0.5;
  binMinimum[2] = -0.5;

  binMaximum[0] = 255.5;
  binMaximum[1] = 255.5;
  binMaximum[2] = 255.5;

  histogramFilter->SetHistogramBinMinimum( binMinimum );
  histogramFilter->SetHistogramBinMaximum( binMaximum );
  //Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The input to the histogram filter is taken from the output of an image
  // reader. Of course, the output of any filter producing an RGB image could
  // have been used instead of a reader.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  histogramFilter->SetInput(  reader->GetOutput()  );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The marginal scale is defined in the histogram filter. This value will
  // define the precision in the assignment of values to the histogram bins.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  histogramFilter->SetMarginalScale( 10.0 );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Finally, the computation of the histogram is triggered by invoking the
  // \code{Update()} method of the filter.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  histogramFilter->Update();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // At this point, we can recover the histogram by calling the
  // \code{GetOutput()} method of the filter. The result is assigned to a
  // variable that is instantiated using the \code{HistogramType} trait of the
  // filter type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef HistogramFilterType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramFilter->GetOutput();
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

  typedef HistogramType::AbsoluteFrequencyType AbsoluteFrequencyType;

  while( itr != end )
    {
    const AbsoluteFrequencyType frequency = itr.GetFrequency();
    histogramFile.write( (const char *)(&frequency), sizeof(frequency) );

    if (frequency != 0)
      {
      HistogramType::IndexType index;
      index = histogram->GetIndex(itr.GetInstanceIdentifier());
      std::cout << "Index = " << index << ", Frequency = " << frequency
                << std::endl;
      }
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


  return EXIT_SUCCESS;


}
