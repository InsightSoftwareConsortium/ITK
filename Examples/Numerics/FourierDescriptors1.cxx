/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    FourierDescriptors1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
//  cycles as we go around the closed curve. For the sake of simplicity we
//  describe here a $2D$ case, however the same analysis can be applied to a
//  curve in N-dimensional space.
//
//  \begin{equation}
//  \textbf{V(t)} = \left( X(t), Y(t) \rigth)
//  \end{equation}
//  
//  We take now each one of the functions $X(t)$, $Y(t)$ and compute their
//  discrete fourier series in the form
//
//  \begin{equation}
//  X(t) = \sum_{k=0}^{N} \exp(-\frac{2 k \pi \textbf{i}}{N}) X_k
//  \end{equation}
//  
//  Where the set of coefficients $X_k$ is the discrete spectrum of the
//  function $X(t)$. These coefficients are in general complex numbers and both
//  their magnitude and phase must be considered in any further analysis of the
//  spectrum.
//
//  Software Guide : EndLatex 


//  Software Guide : BeginLatex
//
//  The \code{FFTRealToComplexConjugateImageFilter} is the class in ITK that
//  computes such transform. In order to use it, we should include its header
//  file first. Three different implementations of this class are available.
//
//  \begin{itemize}
//  \item itkVnlFFTRealToComplexConjugateImageFilter
//  \item itkFFTWRealToComplexConjugateImageFilter
//  \item itkSCSLRealToComplexConjugateImageFilter
//  \end{itemize}
//
//  The first one uses functions from the VNL numerics library, the second uses
//  functions from the \texttt{fftw} library and the last one uses functions
//  from the SCSL library. The VNL implementation is the only one that is
//  self-contained in ITK. However it is not the most efficient when it comes
//  to computation time. If you have performance concerns you should consider
//  installing the additional libraries required for any of the two other
//  implementations.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkFFTRealToComplexConjugateImageFilter.h"
#include "itkVnlFFTRealToComplexConjugateImageFilter.h"
#include "vnl/algo/vnl_fft_1d.h"
// Software Guide : EndCodeSnippet


#include "itkPoint.h"
#include "itkVectorContainer.h"


int main()
{

    
  //  Software Guide : BeginLatex
  //
  //  We should now instantiate the filter that will compute the Fourier
  //  transform of the set of coordinates.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  typedef vnl_fft_1d< double > FFTCalculator;
  // Software Guide : EndCodeSnippet

  const unsigned int numberOfPoints = 16;
   
  typedef itk::Point< double, 2 >  PointType;

  typedef itk::VectorContainer< unsigned int, PointType >  PointsContainer;

  PointsContainer::Pointer points = PointsContainer::New();

  points->Reserve( numberOfPoints );

  typedef PointsContainer::Iterator PointIterator;
  
  PointIterator pointItr = points->Begin();

  unsigned int currentPoint = 0;

  PointType point;

  point[0] = 1.0;
  point[1] = 1.0;

  pointItr.Value() = point;
  ++pointItr;

  point[0] = 0.0;
  point[1] = 0.0;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  pointItr.Value() = point;
  ++pointItr;

  


  //  Software Guide : BeginLatex
  //
  //  This type can now be used for constructing one of such filters. Note that
  //  this is a VNL class and does not follows ITK notation for construction and
  //  assignment to SmartPointers.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  FFTCalculator  fftCalculator( numberOfPoints );
  // Software Guide : EndCodeSnippet


   
  //  Software Guide : BeginLatex
  //
  //  This class will compute the Fast Fourier transform of the input an it will
  //  return it in the same array. We must therefore copy the original data into
  //  an auxiliary array that will in its turn contain the results of the
  //  transform.
  //
  //  Software Guide : EndLatex 


  // Software Guide : BeginCodeSnippet
  typedef vcl_complex<double>   FFTCoefficientType;
  typedef vcl_vector< FFTCoefficientType > FFTSpectrumType;


  // The choice of the spectrum size is very important
  const unsigned int spectrumSize = 32;


  FFTSpectrumType signal( spectrumSize );  

  pointItr = points->Begin();
  for(unsigned int p=0; p<numberOfPoints; p++)
    {
    signal[p] = (pointItr.Value())[0];   // X coordinate
    ++pointItr;
    }

  // Fill in the rest of the input with zeros.
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
  std::cout << "Input to the FFT transform" << std::endl;
  for(unsigned int s=0; s<spectrumSize; s++)
    {
    std::cout << s << " : ";
    std::cout << signal[s] << std::endl;
    }




  // Software Guide : BeginCodeSnippet
  fftCalculator.fwd_transform( signal );
  // Software Guide : EndCodeSnippet
 



  //  Software Guide : BeginLatex
  //
  //  Now we print out the results of the transform.
  //
  //  Software Guide : EndLatex 
  std::cout << std::endl;
  std::cout << "Result from the FFT transform" << std::endl;
  for(unsigned int k=0; k<spectrumSize; k++)
    {
    std::cout << k << " : ";
    std::cout << signal[k] << std::endl;
    }



  return 0;
}



