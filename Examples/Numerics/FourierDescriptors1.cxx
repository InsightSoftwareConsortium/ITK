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

//  Software Guide : BeginLatex
//
//  Fourier Descriptors provide a mechanism for representing a closed curve in
//  space.  The represented curve has infinite continuiity because the
//  parametric coordinate of its points are computed from a Fourier Series.
//
//  In this example we illustrate how a curve that is initially defined by a
//  set of points in space can be represented in terms for Fourier Descriptors.
//  This representation is useful for several purposes. For example, it
//  provides a mechanmism for interpolating values among the points, it
//  provides a way of analyzing the smoothness of the curve.  In this particular
//  example we will focus on this second application of the Fourier Descriptors.
//
//  The first operation that we should use in this context is the computation
//  of the discrete fourier transform of the point coordinates. The coordinates
//  of the points are considered to be independent functions and each one is
//  decomposed in a Fourier Series. In this section we will use $t$ as the
//  parameter of the curve, and will assume that it goes from $0$ to $1$ and
//  cycles as we go around the closed curve. //
//  \begin{equation}
//  \textbf{V(t)} = \left( X(t), Y(t) \right)
//  \end{equation}
//
//  We take now the functions $X(t)$, $Y(t)$ and interpret them as the
//  components of a complex number for which we compute its discrete fourier
//  series in the form
//
//  \begin{equation}
//  V(t) = \sum_{k=0}^{N} \exp(-\frac{2 k \pi \textbf{i}}{N}) F_k
//  \end{equation}
//
//  Where the set of coefficients $F_k$ is the discrete spectrum of the complex
//  function $V(t)$. These coefficients are in general complex numbers and both
//  their magnitude and phase must be considered in any further analysis of the
//  spectrum.
//
//  Software Guide : EndLatex

//  Software Guide : BeginLatex
//
//  The class \code{vnl\_fft\_1d} is the VNL class that computes such transform.
//  In order to use it, we should include its header file first.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "vnl/algo/vnl_fft_1d.h"
// Software Guide : EndCodeSnippet

#include "itkPoint.h"
#include "itkVectorContainer.h"

#include <fstream>

int main(int argc, char * argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "inputFileWithPointCoordinates" << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  We should now instantiate the filter that will compute the Fourier
  //  transform of the set of coordinates.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef vnl_fft_1d< double > FFTCalculator;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The points representing the curve are stored in a
  //  \doxygen{VectorContainer} of \doxygen{Point}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Point< double, 2 >  PointType;

  typedef itk::VectorContainer< unsigned int, PointType >  PointsContainer;

  PointsContainer::Pointer points = PointsContainer::New();
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  In this example we read the set of points from a text file.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::ifstream inputFile;
  inputFile.open( argv[1] );

  if( inputFile.fail() )
    {
    std::cerr << "Problems opening file " << argv[1] << std::endl;
    }

  unsigned int numberOfPoints;
  inputFile >> numberOfPoints;

  points->Reserve( numberOfPoints );

  typedef PointsContainer::Iterator PointIterator;
  PointIterator pointItr = points->Begin();

  PointType point;
  for( unsigned int pt=0; pt<numberOfPoints; pt++)
    {
    inputFile >> point[0] >> point[1];
    pointItr.Value() = point;
    ++pointItr;
    }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  This class will compute the Fast Fourier transform of the input and it will
  //  return it in the same array. We must therefore copy the original data into
  //  an auxiliary array that will in its turn contain the results of the
  //  transform.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef std::complex<double>              FFTCoefficientType;
  typedef std::vector< FFTCoefficientType > FFTSpectrumType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // The choice of the spectrum size is very important. Here we select to use
  // the next power of two that is larger than the number of points.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int powerOfTwo   =
    (unsigned int)std::ceil( std::log( (double)(numberOfPoints)) /
                            std::log( (double)(2.0)) );

  const unsigned int spectrumSize = 1 << powerOfTwo;

  //  Software Guide : BeginLatex
  //
  //  The Fourier Transform type can now be used for constructing one of such
  //  filters. Note that this is a VNL class and does not follows ITK notation
  //  for construction and assignment to SmartPointers.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FFTCalculator  fftCalculator( spectrumSize );
  // Software Guide : EndCodeSnippet

  FFTSpectrumType signal( spectrumSize );

  pointItr = points->Begin();
  for(unsigned int p=0; p<numberOfPoints; p++)
    {
    signal[p] = FFTCoefficientType( pointItr.Value()[0], pointItr.Value()[1] );
    ++pointItr;
    }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // Fill in the rest of the input with zeros. This padding may have
  // undesirable effects on the spectrum if the signal is not attenuated to
  // zero close to their boundaries. Instead of zero-padding we could have used
  // repetition of the last value or mirroring of the signal.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  for(unsigned int pad=numberOfPoints; pad<spectrumSize; pad++)
    {
    signal[pad] = 0.0;
    }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Now we print out the signal as it is passed to the transform calculator
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Input to the FFT transform" << std::endl;
  for(unsigned int s=0; s<spectrumSize; s++)
    {
    std::cout << s << " : ";
    std::cout << signal[s] << std::endl;
    }
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The actual transform is computed by invoking the \code{fwd_transform}
  //  method in the FFT calculator class.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  fftCalculator.fwd_transform( signal );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Now we print out the results of the transform.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << std::endl;
  std::cout << "Result from the FFT transform" << std::endl;
  for(unsigned int k=0; k<spectrumSize; k++)
    {
    const double real = signal[k].real();
    const double imag = signal[k].imag();
    const double magnitude = std::sqrt( real * real + imag * imag );
    std::cout << k << "  " << magnitude << std::endl;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
