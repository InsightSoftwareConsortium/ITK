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
#ifndef __itkImageKernelOperator_txx
#define __itkImageKernelOperator_txx

#include "itkImageKernelOperator.h"

#include "itkImageRegionIterator.h"

#include "vnl/vnl_math.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Image Kernel Convolution"
 * by Tustison N., Gee J.
 * http://hdl.handle.net/1926/1323
 * http://www.insight-journal.org/browse/publication/208
 *
 */

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
