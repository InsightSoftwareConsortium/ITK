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
#ifndef itkImageKernelOperator_hxx
#define itkImageKernelOperator_hxx

#include "itkImageKernelOperator.h"

#include "itkImageRegionConstIterator.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Image Kernel Convolution"
 * by Tustison N., Gee J.
 * https://hdl.handle.net/1926/1323
 * http://www.insight-journal.org/browse/publication/208
 *
 */

namespace itk
{

template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
ImageKernelOperator< TPixel, VDimension, TAllocator >
::SetImageKernel(const ImageType *kernel)
{
  m_ImageKernel = kernel;
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
const typename ImageKernelOperator< TPixel, VDimension, TAllocator >::ImageType *
ImageKernelOperator< TPixel, VDimension, TAllocator >
::GetImageKernel() const
{
  return m_ImageKernel;
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
typename ImageKernelOperator< TPixel, VDimension, TAllocator >::CoefficientVector
ImageKernelOperator< TPixel, VDimension, TAllocator >
::GenerateCoefficients()
{
  // Check that the input image is fully buffered.
  if ( m_ImageKernel->GetBufferedRegion() != m_ImageKernel->GetLargestPossibleRegion() )
    {
    itkExceptionMacro( << "ImageKernel is not fully buffered. " << std::endl
                       << "Buffered region: " << m_ImageKernel->GetBufferedRegion()
                       << std::endl
                       << "Largest possible region: " << m_ImageKernel->GetLargestPossibleRegion()
                       << std::endl
                       << "You should call UpdateLargestPossibleRegion() on "
                       << "the filter whose output is passed to "
                       << "SetImageKernel()." );
    }

  // Check that the size of the kernel is odd in all dimensions.
  for ( unsigned int i = 0; i < VDimension; i++)
    {
    if ( m_ImageKernel->GetLargestPossibleRegion().GetSize()[i] % 2 == 0 )
      {
      itkExceptionMacro( << "ImageKernelOperator requires an input image "
                         << "whose size is odd in all dimensions. The provided "
                         << "image has size "
                         << m_ImageKernel->GetLargestPossibleRegion().GetSize() );
      }
    }

  CoefficientVector coeff;

  ImageRegionConstIterator< ImageType > It( m_ImageKernel,
                                            m_ImageKernel->GetLargestPossibleRegion() );

  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    coeff.push_back( It.Get() );
    }

  return coeff;
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
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
