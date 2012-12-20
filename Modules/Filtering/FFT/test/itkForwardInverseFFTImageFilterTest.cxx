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
#include "itkVnlForwardFFTImageFilter.h"
#include "itkVnlInverseFFTImageFilter.h"
#include "itkVnlRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkVnlHalfHermitianToRealInverseFFTImageFilter.h"

#if defined(ITK_USE_FFTWF) || defined(ITK_USE_FFTWD)
#include "itkFFTWForwardFFTImageFilter.h"
#include "itkFFTWInverseFFTImageFilter.h"
#include "itkFFTWRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkFFTWHalfHermitianToRealInverseFFTImageFilter.h"
#endif

#include "itkForwardInverseFFTTest.h"

int itkForwardInverseFFTImageFilterTest(int argc, char* argv[])
{
  if ( argc < 2 )
    {
    std::cout << "Usage: " << argv[0] << " <input file> " << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int                          Dimension = 2;
  typedef float                               FloatType;
  typedef double                              DoubleType;
  typedef itk::Image< FloatType, Dimension >  FloatImageType;
  typedef itk::Image< DoubleType, Dimension > DoubleImageType;

  bool success = true;

  typedef itk::VnlForwardFFTImageFilter< FloatImageType >
    FloatVnlFullFFTType;
  typedef itk::VnlInverseFFTImageFilter< FloatVnlFullFFTType::OutputImageType >
    FloatVnlFullIFFTType;
  if ( !ForwardInverseFullFFTTest< FloatVnlFullFFTType, FloatVnlFullIFFTType >( argv[1] ) )
    {
    success = false;
    std::cerr << "Test failed for FloatVnlFullFFTType" << std::endl;
    }
  else
    {
    std::cout << "Test passed for FloatVnlFullFFTType" << std::endl;
    }

  typedef itk::VnlForwardFFTImageFilter< DoubleImageType >
    DoubleVnlFullFFTType;
  typedef itk::VnlInverseFFTImageFilter< DoubleVnlFullFFTType::OutputImageType >
    DoubleVnlFullIFFTType;
  if ( !ForwardInverseFullFFTTest< DoubleVnlFullFFTType, DoubleVnlFullIFFTType >( argv[1] ) )
    {
    success = false;
    std::cerr << "Test failed for DoubleVnlFullFFTType" << std::endl;
    }
  else
    {
    std::cout << "Test passed for DoubleVnlFullFFTType" << std::endl;
    }

#if defined(ITK_USE_FFTWF)
  typedef itk::FFTWForwardFFTImageFilter< FloatImageType >
    FloatFFTWFullFFTType;
  typedef itk::FFTWInverseFFTImageFilter< FloatFFTWFullFFTType::OutputImageType >
    FloatFFTWFullIFFTType;
  if ( !ForwardInverseFullFFTTest< FloatFFTWFullFFTType, FloatFFTWFullIFFTType >( argv[1] ) )
    {
    success = false;
    std::cerr << "Test failed for FloatFFTWFullFFTType" << std::endl;
    }
  else
    {
    std::cout << "Test passed for FloatFFTWFullFFTType" << std::endl;
    }
#endif

#if defined(ITK_USE_FFTWD)
  typedef itk::FFTWForwardFFTImageFilter< DoubleImageType >
    DoubleFFTWFullFFTType;
  typedef itk::FFTWInverseFFTImageFilter< DoubleFFTWFullFFTType::OutputImageType >
    DoubleFFTWFullIFFTType;
  if ( !ForwardInverseFullFFTTest< DoubleFFTWFullFFTType, DoubleFFTWFullIFFTType >( argv[1] ) )
    {
    success = false;
    std::cerr << "Test failed for DoubleFFTWFullFFTType" << std::endl;
    }
  else
    {
    std::cout << "Test passed for DoubleFFTWFullFFTType" << std::endl;
    }

#endif


  typedef itk::VnlRealToHalfHermitianForwardFFTImageFilter< FloatImageType >
    FloatVnlHalfFFTType;
  typedef itk::VnlHalfHermitianToRealInverseFFTImageFilter< FloatVnlHalfFFTType::OutputImageType >
    FloatVnlHalfIFFTType;
  if ( !ForwardInverseHalfFFTTest< FloatVnlHalfFFTType, FloatVnlHalfIFFTType >( argv[1] ) )
    {
    success = false;
    std::cerr << "Test failed for FloatVnlHalfFFTType" << std::endl;
    }
  else
    {
    std::cout << "Test passed for FloatVnlHalfFFTType" << std::endl;
    }

  typedef itk::VnlRealToHalfHermitianForwardFFTImageFilter< DoubleImageType >
    DoubleVnlHalfFFTType;
  typedef itk::VnlHalfHermitianToRealInverseFFTImageFilter< DoubleVnlHalfFFTType::OutputImageType >
    DoubleVnlHalfIFFTType;
  if ( !ForwardInverseHalfFFTTest< DoubleVnlHalfFFTType, DoubleVnlHalfIFFTType >( argv[1] ) )
    {
    success = false;
    std::cerr << "Test failed for DoubleVnlHalfFFTType" << std::endl;
    }
  else
    {
    std::cout << "Test passed for DoubleVnlHalfFFTType" << std::endl;
    }

#if defined(ITK_USE_FFTWF)
  typedef itk::FFTWRealToHalfHermitianForwardFFTImageFilter< FloatImageType >
    FloatFFTWHalfFFTType;
  typedef itk::FFTWHalfHermitianToRealInverseFFTImageFilter< FloatFFTWHalfFFTType::OutputImageType >
    FloatFFTWHalfIFFTType;
  if ( !ForwardInverseHalfFFTTest< FloatFFTWHalfFFTType, FloatFFTWHalfIFFTType >( argv[1] ) )
    {
    success = false;
    std::cerr << "Test failed for FloatFFTWHalfFFTType" << std::endl;
    }
  else
    {
    std::cout << "Test passed for FloatFFTWHalfFFTType" << std::endl;
    }
#endif

#if defined(ITK_USE_FFTWD)
  typedef itk::FFTWRealToHalfHermitianForwardFFTImageFilter< DoubleImageType >
    DoubleFFTWHalfFFTType;
  typedef itk::FFTWHalfHermitianToRealInverseFFTImageFilter< DoubleFFTWHalfFFTType::OutputImageType >
    DoubleFFTWHalfIFFTType;
  if ( !ForwardInverseHalfFFTTest< DoubleFFTWHalfFFTType, DoubleFFTWHalfIFFTType >( argv[1] ) )
    {
    success = false;
    std::cerr << "Test failed for DoubleFFTWHalfFFTType" << std::endl;
    }
  else
    {
    std::cout << "Test passed for DoubleFFTWHalfFFTType" << std::endl;
    }
#endif

  return success ? EXIT_SUCCESS : EXIT_FAILURE;
}
