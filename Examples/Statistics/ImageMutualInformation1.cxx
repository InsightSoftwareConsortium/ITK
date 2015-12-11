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
// This example illustrates how to compute the Mutual Information between two
// images using classes from the Statistics framework. Note that you could also
// use for this purpose the ImageMetrics designed for the image registration
// framework.
//
// For example, you could use:
//
// \begin{itemize}
// \item \doxygen{MutualInformationImageToImageMetric}
// \item \doxygen{MattesMutualInformationImageToImageMetric}
// \item \doxygen{MutualInformationHistogramImageToImageMetric}
// \item \doxygen{MutualInformationImageToImageMetric}
// \item \doxygen{NormalizedMutualInformationHistogramImageToImageMetric}
// \item \doxygen{KullbackLeiblerCompareHistogramImageToImageMetric}
// \end{itemize}
//
// Mutual Information as computed in this example, and as commonly used in the
// context of image registration provides a measure of how much uncertainty on
// the value of a pixel in one image is reduced by measuring the homologous
// pixel in the other image. Note that Mutual Information as used here does not
// measure the amount of information that one image provides on the other
// image; this would require us to take into account the spatial
// structures in the images as well as the semantics of the image context in
// terms of an observer.
//
// This implies that there is still an enormous unexploited potential on the
// use of the Mutual Information concept in the domain of medical images,
// among the most interesting of which is the semantic description of
// image in terms of anatomical structures.
//
// \index{Mutual Information!Statistics}
// \index{Statistics!Mutual Information}
// \index{Joint Entropy!Statistics}
// \index{Statistics!Joint Entropy}
// \index{Joint Histogram!Statistics}
// \index{Statistics!Joint Histogram}
//
// Software Guide : EndLatex


// Software Guide : BeginLatex
//
// In this particular example we make use of classes from the Statistics
// framework in order to compute the measure of Mutual Information between two
// images. We assume that both images have the same number of pixels along
// every dimension and that they have the same origin and spacing. Therefore
// the pixels from one image are perfectly aligned with those of the other
// image.
//
// We must start by including the header files of the image, histogram
// filter, reader and Join image filter. We will read both images and use
// the Join image filter in order to compose an image of two components using
// the information of each one of the input images in one component. This is
// the natural way of using the Statistics framework in ITK given that the
// fundamental statistical classes are expecting to receive multi-valued
// measures.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkJoinImageFilter.h"
#include "itkImageToHistogramFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage :  ImageMutualInformation1  inputImage1 inputImage2 " << std::endl;
    return EXIT_FAILURE;
    }


  // Software Guide : BeginLatex
  //
  // We define the pixel type and dimension of the images to be read.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef unsigned char                                 PixelComponentType;
  const unsigned int                                    Dimension = 2;

  typedef itk::Image< PixelComponentType, Dimension >   ImageType;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Using the image type we proceed to instantiate the readers for both input
  // images. Then, we take their filenames from the command line arguments.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >             ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  ReaderType::Pointer reader2 = ReaderType::New();

  reader1->SetFileName( argv[1] );
  reader2->SetFileName( argv[2] );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Using the \doxygen{JoinImageFilter} we use the two input images and put them
  // together in an image of two components.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::JoinImageFilter< ImageType, ImageType >  JoinFilterType;

  JoinFilterType::Pointer joinFilter = JoinFilterType::New();

  joinFilter->SetInput1( reader1->GetOutput() );
  joinFilter->SetInput2( reader2->GetOutput() );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // At this point we trigger the execution of the pipeline by invoking the
  // \code{Update()} method on the Join filter. We must put the call inside a
  // try/catch block because the Update() call may potentially result in
  // exceptions being thrown.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    joinFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We now prepare the types to be used for the computation of the joint
  // histogram. For this purpose, we take the type of the image resulting from
  // the JoinImageFilter and use it as template argument of the
  // \doxygen{ImageToHistogramFilter}. We then construct one by invoking the
  // \code{New()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef JoinFilterType::OutputImageType               VectorImageType;

  typedef itk::Statistics::ImageToHistogramFilter<
                                       VectorImageType >  HistogramFilterType;

  HistogramFilterType::Pointer histogramFilter = HistogramFilterType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We pass the multiple-component image as input to the histogram filter,
  // and setup the marginal scale value that will define the precision to be used
  // for classifying values into the histogram bins.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  histogramFilter->SetInput(  joinFilter->GetOutput()  );

  histogramFilter->SetMarginalScale( 10.0 );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We must now define the number of bins to use for each one of the components
  // in the joint image. For this purpose we take the \code{HistogramSizeType} from the
  // traits of the histogram filter type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef HistogramFilterType::HistogramSizeType   HistogramSizeType;

  HistogramSizeType size( 2 );

  size[0] = 255;  // number of bins for the first  channel
  size[1] = 255;  // number of bins for the second channel

  histogramFilter->SetHistogramSize( size );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Finally, we must specify the upper and lower bounds for the histogram
  // using the \code{SetHistogramBinMinimum()} and
  // \code{SetHistogramBinMaximum()} methods. The \code{Update()} method is then
  // called in order to trigger the computation of the histogram.
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

  histogramFilter->Update();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The histogram can be recovered from the filter by creating a variable
  // with the histogram type taken from the filter traits.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef HistogramFilterType::HistogramType  HistogramType;

  const HistogramType * histogram = histogramFilter->GetOutput();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We now walk over all the bins of the joint histogram and compute their
  // contribution to the value of the joint entropy. For this purpose we use
  // histogram iterators, and the \code{Begin()} and \code{End()} methods.  Since
  // the values returned from the histogram are measuring frequency we must
  // convert them to an estimation of probability by dividing them over the total
  // sum of frequencies returned by the \code{GetTotalFrequency()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  HistogramType::ConstIterator itr = histogram->Begin();
  HistogramType::ConstIterator end = histogram->End();

  const double Sum = histogram->GetTotalFrequency();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We initialize to zero the variable to use for accumulating the value of the
  // joint entropy, and then use the iterator for visiting all the bins of the
  // joint histogram. For every bin we compute their contribution to the reduction
  // of uncertainty. Note that in order to avoid logarithmic operations on zero
  // values, we skip over those bins that have less than one count. The entropy
  // contribution must be computed using logarithms in base two in order to
  // express entropy in \textbf{bits}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  double JointEntropy = 0.0;

  while( itr != end )
    {
    const double count = itr.GetFrequency();
    if( count > 0.0 )
      {
      const double probability = count / Sum;
      JointEntropy +=
        - probability * std::log( probability ) / std::log( 2.0 );
      }
    ++itr;
    }
  // Software Guide : EndCodeSnippet

  std::cout << "Joint Entropy      = " << JointEntropy << " bits " << std::endl;


  // Software Guide : BeginLatex
  //
  // Now that we have the value of the joint entropy we can proceed to estimate
  // the values of the entropies for each image independently. This can be done
  // by simply changing the number of bins and then recomputing the histogram.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  size[0] = 255;  // number of bins for the first  channel
  size[1] =   1;  // number of bins for the second channel

  histogramFilter->SetHistogramSize( size );
  histogramFilter->Update();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We initialize to zero another variable in order to start accumulating the
  // entropy contributions from every bin.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  itr = histogram->Begin();
  end = histogram->End();

  double Entropy1 = 0.0;

  while( itr != end )
    {
    const double count = itr.GetFrequency();
    if( count > 0.0 )
      {
      const double probability = count / Sum;
      Entropy1 += - probability * std::log( probability ) / std::log( 2.0 );
      }
    ++itr;
    }
  // Software Guide : EndCodeSnippet

  std::cout << "Image1 Entropy   = " << Entropy1 << " bits " << std::endl;


  // Software Guide : BeginLatex
  //
  // The same process is used for computing the entropy of the other component,
  // simply by swapping the number of bins in the histogram.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  size[0] =   1;  // number of bins for the first channel
  size[1] = 255;  // number of bins for the second channel

  histogramFilter->SetHistogramSize( size );
  histogramFilter->Update();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The entropy is computed in a similar manner, just by visiting all the bins on
  // the histogram and accumulating their entropy contributions.
  //
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  itr = histogram->Begin();
  end = histogram->End();

  double Entropy2 = 0.0;

  while( itr != end )
    {
    const double count = itr.GetFrequency();
    if( count > 0.0 )
      {
      const double probability = count / Sum;
      Entropy2 += - probability * std::log( probability ) / std::log( 2.0 );
      }
    ++itr;
    }
  // Software Guide : EndCodeSnippet

  std::cout << "Image2 Entropy   = " << Entropy2 << " bits " << std::endl;


  // Software Guide : BeginLatex
  //
  // At this point we can compute any of the popular measures of Mutual
  // Information. For example
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  double MutualInformation = Entropy1 + Entropy2 - JointEntropy;
  // Software Guide : EndCodeSnippet

  std::cout << "Mutual Information = " << MutualInformation << " bits " << std::endl;


  // Software Guide : BeginLatex
  //
  // or Normalized Mutual Information, where the value of Mutual Information is
  // divided by the mean entropy of the input images.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  double NormalizedMutualInformation1 =
                     2.0 * MutualInformation / ( Entropy1 + Entropy2 );
  // Software Guide : EndCodeSnippet

  std::cout << "Normalized Mutual Information 1 = " << NormalizedMutualInformation1 <<  std::endl;


  // Software Guide : BeginLatex
  //
  // A second form of Normalized Mutual Information has been defined as the mean
  // entropy of the two images divided by their joint entropy.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  double NormalizedMutualInformation2 = ( Entropy1 + Entropy2 ) / JointEntropy;
  // Software Guide : EndCodeSnippet


  std::cout << "Normalized Mutual Information 2 = " << NormalizedMutualInformation2 <<  std::endl;


  // Software Guide : BeginLatex
  //
  // You probably will find very interesting how the value of Mutual Information
  // is strongly dependent on the number of bins over which the histogram is
  // defined.
  //
  // Software Guide : EndLatex


  return EXIT_SUCCESS;

}
