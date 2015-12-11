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
//
// This example shows how to compute the entropy of an image.  More formally
// this should be said : The reduction in uncertainty gained when we measure
// the intensity of \emph{one} randomly selected pixel in this image, given
// that we already know the statistical distribution of the image intensity
// values.
//
// In practice it is almost never possible to know the real statistical
// distribution of intensities and we are forced to estimate it from the
// evaluation of the histogram from one or several images of similar nature.
// We can use the counts in histogram bins in order to compute frequencies and
// then consider those frequencies to be estimations of the probablility of a
// new value to belong to the intensity range of that bin.
//
// \index{Entropy!Images}
// \index{Image!Entropy}
// \index{Image!Amount of information}
// \index{Amount of information!Image}
//
// Software Guide : EndLatex

// Software Guide : BeginLatex
//
// Since the first stage in estimating the entropy of an image is to compute
// its histogram, we must start by including the headers of the classes that
// will perform such a computation. In this case, we are going to use a scalar
// image as input, therefore we need the
// \subdoxygen{Statistics}{ScalarImageToHistogramGenerator} class, as well as
// the image class.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkScalarImageToHistogramGenerator.h"
#include "itkImage.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"

int main( int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageEntropy1  inputImageFileName ";
    std::cerr << "numberOfHistogramBins" << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // The pixel type and dimension of the image are explicitly declared and then
  // used for instantiating the image type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned char       PixelType;
  const   unsigned int        Dimension = 3;

  typedef itk::Image< PixelType, Dimension > ImageType;
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
    std::cerr << "Problem encoutered while reading image file : " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // The image type is used as template parameter for instantiating the histogram
  // generator.
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
  // The parameters of the desired histogram are defined, including the
  // number of bins and the marginal scale. For convenience in this example, we
  // read the number of bins from the command line arguments. In this way we can
  // easily experiment with different values for the number of bins and see how
  // that choice affects the computation of the entropy.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int numberOfHistogramBins = atoi( argv[2] );

  histogramGenerator->SetNumberOfBins( numberOfHistogramBins );
  histogramGenerator->SetMarginalScale( 10.0 );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We can then connect as input the output image from a reader and trigger the
  // histogram computation by invoking the \code{Compute()} method in the
  // generator.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  histogramGenerator->SetInput(  reader->GetOutput() );

  histogramGenerator->Compute();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The resulting histogram can be recovered from the generator by using the
  // \code{GetOutput()} method. A histogram class can be declared using the
  // \code{HistogramType} trait from the generator.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef HistogramGeneratorType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramGenerator->GetOutput();
  // Software Guide : EndCodeSnippet


  const unsigned int histogramSize = histogram->Size();

  std::cout << "Histogram size " << histogramSize << std::endl;

  for (unsigned int bin=0; bin < histogramSize; ++bin)
    {
    std::cout << "bin = " << bin << " frequency = ";
    std::cout << histogram->GetFrequency( bin, 0 ) << std::endl;
    }


  // Software Guide : BeginLatex
  //
  // We proceed now to compute the \emph{estimation} of entropy given the
  // histogram. The first conceptual jump to be done here is to assume that
  // the histogram, which is the simple count of frequency of occurrence for the
  // gray scale values of the image pixels, can be normalized in order to estimate
  // the probability density function \textbf{PDF} of the actual statistical
  // distribution of pixel values.
  //
  //  First we declare an iterator that will visit all the bins in the histogram.
  //  Then we obtain the total number of counts using the
  //  \code{GetTotalFrequency()} method, and we initialize the entropy variable
  //  to zero.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginCodeSnippet
  HistogramType::ConstIterator itr = histogram->Begin();
  HistogramType::ConstIterator end = histogram->End();

  double Sum = histogram->GetTotalFrequency();

  double Entropy = 0.0;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We start now visiting every bin and estimating the probability of a pixel to
  // have a value in the range of that bin. The base 2 logarithm of that
  // probability is computed, and then weighted by the probability in order to
  // compute the expected amount of information for any given pixel. Note that a
  // minimum value is imposed for the probability in order to avoid computing
  // logarithms of zeros.
  //
  //  Note that the $\log{(2)}$ factor is used to convert the natural logarithm
  //  in to a logarithm of base 2, and makes it possible to report the entropy
  //  in its natural unit: the bit.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  while( itr != end )
    {
    const double probability = itr.GetFrequency() / Sum;

    if( probability > 0.99 / Sum )
      {
      Entropy += - probability * std::log( probability ) / std::log( 2.0 );
      }
    ++itr;
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The result of this sum is considered to be our estimation of the image
  // entropy. Note that the Entropy value will change depending on the number of
  // histogram bins that we use for computing the histogram. This is particularly
  // important when dealing with images whose pixel values have dynamic ranges so
  // large that our number of bins will always underestimate the variability of
  // the data.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Image entropy = " << Entropy << " bits " << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // As an illustration, the application of this program to the image
  //
  // \begin{itemize}
  // \item \code{Examples/Data/BrainProtonDensitySlice.png}
  // \end{itemize}
  //
  // results in the following values of entropy for different values of number of
  // histogram bins.
  //
  // \begin{center}
  // \begin{tabular}{|l|r|r|r|r|r|}
  // \hline
  // Number of Histogram Bins & 16    & 32    & 64    & 128   & 255 \cr
  // \hline
  // Estimated Entropy (bits) & 3.02  & 3.98  & 4.92  & 5.89  & 6.88 \cr
  // \hline
  // \end{tabular}
  // \end{center}
  //
  //
  // This table highlights the importance of carefully considering the
  // characteristics of the histograms used for estimating Information Theory
  // measures such as the entropy.
  //
  // Software Guide : EndLatex


  return EXIT_SUCCESS;

}
