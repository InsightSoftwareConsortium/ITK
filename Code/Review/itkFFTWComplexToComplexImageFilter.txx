/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFFTWComplexToComplexImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

/**
 *
 * Attribution Notice. This research work was made possible by
 * Grant Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from
 * the National Center for Research Resources (NCRR), a component of the
 * National Institutes of Health (NIH).  Its contents are solely the
 * responsibility of the authors and do not necessarily represent the
 * official view of NCRR or NIH.
 *
 * This class was taken from the Insight Journal paper:
 * http://insight-journal.org/midas/handle.php?handle=1926/326
 *
 */

#ifndef __itkFFTWComplexToComplexImageFilter_txx
#define __itkFFTWComplexToComplexImageFilter_txx

#if defined( USE_FFTWF ) || defined( USE_FFTWD )

#include "itkFFTWComplexToComplexImageFilter.h"
#include <iostream>
#include "itkIndent.h"
#include "itkMetaDataObject.h"
#include "itkImageRegionIterator.h"

namespace itk
{
/** TODO:  There should be compile time type checks so that
           if only USE_FFTWF is defined, then only floats are valid.
           and if USE_FFTWD is defined, then only doubles are valid.
*/

#if defined( USE_FFTWF )
template< unsigned int NDimension >
void
FFTWComplexToComplexImageFilter< float, NDimension >::GenerateData()
{
  // get pointers to the input and output
  typename InputImageType::ConstPointer inputPtr  = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  const typename InputImageType::SizeType &   outputSize =
    outputPtr->GetLargestPossibleRegion().GetSize();
  const unsigned int num_dims = outputPtr->GetImageDimension();

  if ( num_dims != outputPtr->GetImageDimension() )
    {
    return;
    }

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  std::complex< TPixel > *in =
    const_cast< std::complex< TPixel > * >( inputPtr->GetBufferPointer() );

  unsigned int total_size = 1;

    {
    // This reinterpret_cast only makes sense if TPixel is float...
    fftwf_complex *dptr = reinterpret_cast< fftwf_complex * >( in );
    fftwf_complex *out = reinterpret_cast< fftwf_complex * >( outputPtr->GetBufferPointer() );

    int transformDirection = 1;
    if ( this->GetTransformDirection() == Superclass::INVERSE )
      {
      transformDirection = -1;
      }

    switch ( num_dims )
      {
      case 1:
        this->m_Plan = fftwf_plan_dft_1d(outputSize[0],
                                         dptr, out,
                                         transformDirection, FFTW_ESTIMATE);
        total_size = outputSize[0];
        break;
      case 2:
        this->m_Plan = fftwf_plan_dft_2d(outputSize[1], outputSize[0],
                                         dptr, out,
                                         transformDirection, FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1];
        break;
      case 3:
        this->m_Plan = fftwf_plan_dft_3d(outputSize[2], outputSize[1], outputSize[0],
                                         dptr, out,
                                         transformDirection, FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1] * outputSize[2];
        break;
      default:
        int *sizes = new int[num_dims];
        for ( unsigned int i = 0; i < num_dims; i++ )
          {
          sizes[( num_dims - 1 ) - i] = outputSize[i];
          total_size *= outputSize[i];
          }
        this->m_Plan = fftwf_plan_dft(num_dims, sizes,
                                      dptr, out, transformDirection, FFTW_ESTIMATE);
        delete[] sizes;
      }
    this->m_PlanComputed = true;
    fftwf_execute(this->m_Plan);
    }

  typedef ImageRegionIterator< OutputImageType > IteratorType;

  IteratorType it( outputPtr, outputPtr->GetLargestPossibleRegion() );

  //
  // Normalize the output if backward transform
  //
  if ( this->GetTransformDirection() == Superclass::INVERSE )
    {
    std::complex< TPixel > val;
    while ( !it.IsAtEnd() )
      {
      val = it.Value();
      val /= total_size;
      it.Set(val);
      ++it;
      }
    }
}

template< unsigned int NDimension >
bool
FFTWComplexToComplexImageFilter< float, NDimension >::FullMatrix()
{
  return false;
}

#endif // defined(USE_FFTWF)

#if defined( USE_FFTWD )
template< unsigned int NDimension >
void
FFTWComplexToComplexImageFilter< double, NDimension >::GenerateData()
{
  // get pointers to the input and output
  typename InputImageType::ConstPointer inputPtr  = this->GetInput();
  typename OutputImageType::Pointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  const typename InputImageType::SizeType &   outputSize =
    outputPtr->GetLargestPossibleRegion().GetSize();

  const unsigned int num_dims = outputPtr->GetImageDimension();

  if ( num_dims != outputPtr->GetImageDimension() )
    {
    return;
    }

  // allocate output buffer memory
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  std::complex< TPixel > *in = const_cast< std::complex< TPixel > * >
                               ( inputPtr->GetBufferPointer() );

  unsigned int total_size = 1;

    {
    // This reinterpret_cast only makes sense if TPixel is double...
    fftw_complex *dptr = reinterpret_cast< fftw_complex * >( in );
    fftw_complex *out  = reinterpret_cast< fftw_complex * >( outputPtr->GetBufferPointer() );

    int transformDirection = 1;
    if ( this->GetTransformDirection() == Superclass::INVERSE )
      {
      transformDirection = -1;
      }

    switch ( num_dims )
      {
      case 1:
        this->m_Plan = fftw_plan_dft_1d(outputSize[0],
                                        dptr, out,
                                        transformDirection, FFTW_ESTIMATE);
        total_size = outputSize[0];
        break;
      case 2:
        this->m_Plan = fftw_plan_dft_2d(outputSize[1], outputSize[0],
                                        dptr, out,
                                        transformDirection, FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1];
        break;
      case 3:
        this->m_Plan = fftw_plan_dft_3d(outputSize[2], outputSize[1], outputSize[0],
                                        dptr, out,
                                        transformDirection, FFTW_ESTIMATE);
        total_size = outputSize[0] * outputSize[1] * outputSize[2];
        break;
      default:
        int *sizes = new int[num_dims];
        for ( unsigned int i = 0; i < num_dims; i++ )
          {
          sizes[( num_dims - 1 ) - i] = outputSize[i];
          total_size *= outputSize[i];
          }
        this->m_Plan = fftw_plan_dft(num_dims, sizes,
                                     dptr, out, transformDirection, FFTW_ESTIMATE);
        delete[] sizes;
      }
    this->m_PlanComputed = true;
    fftw_execute(this->m_Plan);
    }

  ImageRegionIterator< OutputImageType > it( outputPtr, outputPtr->GetLargestPossibleRegion() );

  //
  // Normalize the output if backward transform
  //
  if ( this->GetTransformDirection() == Superclass::INVERSE )
    {
    std::complex< TPixel > val;
    while ( !it.IsAtEnd() )
      {
      val = it.Value();
      val /= total_size;
      it.Set(val);
      ++it;
      }
    }
}

template< unsigned int NDimension >
bool
FFTWComplexToComplexImageFilter< double, NDimension >::FullMatrix()
{
  return false;
}

#endif // defined(USE_FFTWD)
} // namespace itk

#endif // defined(USE_FFTWF) || defined(USE_FFTWD)

#endif // _itkFFTWComplexToComplexImageFilter_txx
