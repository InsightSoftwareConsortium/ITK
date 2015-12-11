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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainT1Slice.png}
//    OUTPUTS: {NeighborhoodIterators4a.png}
//    ARGUMENTS:    0
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainT1Slice.png}
//    OUTPUTS: {NeighborhoodIterators4b.png}
//    ARGUMENTS:    1
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainT1Slice.png}
//    OUTPUTS: {NeighborhoodIterators4c.png}
//    ARGUMENTS:    2
//  Software Guide : EndCommandLineArgs
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainT1Slice.png}
//    OUTPUTS: {NeighborhoodIterators4d.png}
//    ARGUMENTS:    5
//  Software Guide : EndCommandLineArgs

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"

// Software Guide : BeginLatex
//
// We now introduce a variation on convolution filtering that is useful when a
// convolution kernel is separable.  In this example, we create a different
// neighborhood iterator for each axial direction of the image and then take
// separate inner products with a 1D discrete Gaussian kernel.
// The idea of using several neighborhood iterators at once has applications
// beyond convolution filtering and may improve efficiency when the size of
// the whole neighborhood relative to the portion of the neighborhood used
// in calculations becomes large.
//
// The only new class necessary for this example is the Gaussian operator.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGaussianOperator.h"
// Software Guide : EndCodeSnippet

int main( int argc, char ** argv )
{
  if ( argc < 4 )
    {
    std::cerr << "Missing parameters. " << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << " inputImageFile outputImageFile sigma"
              << std::endl;
    return EXIT_FAILURE;
    }

  typedef float                             PixelType;
  typedef itk::Image< PixelType, 2 >        ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;

  typedef itk::ConstNeighborhoodIterator< ImageType > NeighborhoodIteratorType;
  typedef itk::ImageRegionIterator< ImageType>        IteratorType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  try
    {
    reader->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  ImageType::Pointer output = ImageType::New();
  output->SetRegions(reader->GetOutput()->GetRequestedRegion());
  output->Allocate();

  itk::NeighborhoodInnerProduct<ImageType> innerProduct;

  typedef itk::NeighborhoodAlgorithm
    ::ImageBoundaryFacesCalculator< ImageType > FaceCalculatorType;

  FaceCalculatorType faceCalculator;
  FaceCalculatorType::FaceListType faceList;
  FaceCalculatorType::FaceListType::iterator fit;

  IteratorType out;
  NeighborhoodIteratorType it;


// Software Guide : BeginLatex
//
// The Gaussian operator, like the Sobel operator, is instantiated with a pixel
// type and a dimensionality.  Additionally, we set the variance of the
// Gaussian, which has been read from the command line as standard deviation.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  itk::GaussianOperator< PixelType, 2 > gaussianOperator;
  gaussianOperator.SetVariance( ::atof(argv[3]) * ::atof(argv[3]) );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The only further changes from the previous example are in the main loop.
// Once again we use the results from face calculator to construct a loop that
// processes boundary and non-boundary image regions separately.  Separable
// convolution, however, requires an additional, outer loop over all the image
// dimensions.  The direction of the Gaussian operator is reset at each
// iteration of the outer loop using the new dimension.  The iterators change
// direction to match because they are initialized with the radius of the
// Gaussian operator.
//
// Input and output buffers are swapped at each iteration so that the output of
// the previous iteration becomes the input for the current iteration. The swap
// is not performed on the last iteration.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  ImageType::Pointer input = reader->GetOutput();
  for (unsigned int i = 0; i < ImageType::ImageDimension; ++i)
    {
    gaussianOperator.SetDirection(i);
    gaussianOperator.CreateDirectional();

    faceList = faceCalculator(input, output->GetRequestedRegion(),
                              gaussianOperator.GetRadius());

    for ( fit=faceList.begin(); fit != faceList.end(); ++fit )
      {
      it = NeighborhoodIteratorType( gaussianOperator.GetRadius(),
                                     input, *fit );

      out = IteratorType( output, *fit );

      for (it.GoToBegin(), out.GoToBegin(); ! it.IsAtEnd(); ++it, ++out)
        {
        out.Set( innerProduct(it, gaussianOperator) );
        }
      }

    // Swap the input and output buffers
    if (i != ImageType::ImageDimension - 1)
      {
      ImageType::Pointer tmp = input;
      input = output;
      output = tmp;
      }
    }
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// The output is rescaled and written as in the previous examples.
// Figure~\ref{fig:NeighborhoodExample4} shows the results of Gaussian blurring
// the image \code{Examples/Data/BrainT1Slice.png} using increasing
// kernel widths.
//
// \begin{figure}
// \centering
// \includegraphics[width=0.23\textwidth]{NeighborhoodIterators4a}
// \includegraphics[width=0.23\textwidth]{NeighborhoodIterators4b}
// \includegraphics[width=0.23\textwidth]{NeighborhoodIterators4c}
// \includegraphics[width=0.23\textwidth]{NeighborhoodIterators4d}
// \itkcaption[Gaussian blurring by convolution filtering]{Results of
// convolution filtering with a Gaussian kernel of increasing standard
// deviation $\sigma$ (from left to right, $\sigma = 0$, $\sigma = 1$, $\sigma
// = 2$, $\sigma = 5$).  Increased blurring reduces contrast and changes the
// average intensity value of the image, which causes the image to appear
// brighter when rescaled.}
// \protect\label{fig:NeighborhoodExample4}
// \end{figure}
//
// Software Guide : EndLatex

  typedef unsigned char                          WritePixelType;
  typedef itk::Image< WritePixelType, 2 >        WriteImageType;
  typedef itk::ImageFileWriter< WriteImageType > WriterType;

  typedef itk::RescaleIntensityImageFilter<
    ImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput(output);

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );
  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject &err)
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
