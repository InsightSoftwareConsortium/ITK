/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageKernelOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageKernelOperator_txx
#define __itkImageKernelOperator_txx

#include "itkImageKernelOperator.h"

#include "itkImageRegionIterator.h"

#include "vnl/vnl_math.h"

namespace itk
{
template< class TPixel, unsigned int VDimension, class TAllocator >
typename ImageKernelOperator< TPixel, VDimension, TAllocator >
::CoefficientVector
ImageKernelOperator< TPixel, VDimension, TAllocator >
::GenerateCoefficients()
{
  CoefficientVector coeff;

  ImageRegionIterator< ImageType > It( this->m_ImageKernel,
                                       this->m_ImageKernel->GetLargestPossibleRegion() );

  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    coeff.push_back( It.Get() );
    }

  return coeff;
}

template< class TPixel, unsigned int VDimension, class TAllocator >
void
ImageKernelOperator< TPixel, VDimension, TAllocator >
::Fill(const CoefficientVector & coeff)
{
  // Initialize all coefficients to zero
  this->InitializeToZero();

  std::slice *temp_slice;
  typename CoefficientVector::const_iterator it;

  temp_slice = new std::slice(0, coeff.size(), 1);
  it = coeff.begin();

  typename Superclass::SliceIteratorType data(this, *temp_slice);
  delete temp_slice;

  // Copy the coefficients into the neighborhood, truncating them if there
  // are too many.
  for ( data = data.Begin(); data < data.End(); ++data, ++it )
    {
    *data = static_cast< TPixel >( *it );
    }
}
} // end namespace itk

#endif
