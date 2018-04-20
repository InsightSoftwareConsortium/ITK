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
#ifndef itkGaussianDerivativeImageFunction_hxx
#define itkGaussianDerivativeImageFunction_hxx

#include "itkGaussianDerivativeImageFunction.h"

#include "itkNeighborhoodInnerProduct.h"
#include "itkMath.h"

#include <algorithm>  // For fill_n.
#include <cassert>

namespace itk
{

template< typename TInputImage, typename TOutput >
GaussianDerivativeImageFunction< TInputImage, TOutput >
::GaussianDerivativeImageFunction()
  :
  m_UseImageSpacing{true},
  m_GaussianDerivativeFunction{ GaussianDerivativeFunctionType::New() }
{
  std::fill_n(m_Sigma, Self::ImageDimension2, 1.0);
  std::fill_n(m_Extent, Self::ImageDimension2, 1.0);
  m_GaussianDerivativeFunction->SetNormalized(false); // faster
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::SetInputImage(const InputImageType *ptr)
{
  Superclass::SetInputImage(ptr);
  this->RecomputeGaussianKernel();
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::SetSigma(const double *sigma)
{
  unsigned int i;

  for ( i = 0; i < Self::ImageDimension2; i++ )
    {
    if ( sigma[i] != m_Sigma[i] )
      {
      break;
      }
    }
  if ( i < Self::ImageDimension2 )
    {
    for ( i = 0; i < Self::ImageDimension2; i++ )
      {
      m_Sigma[i] = sigma[i];
      }
    this->RecomputeGaussianKernel();
    }
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::SetSigma(const double sigma)
{
  unsigned int i;

  for ( i = 0; i < Self::ImageDimension2; i++ )
    {
    if ( Math::NotExactlyEquals(sigma, m_Sigma[i]) )
      {
      break;
      }
    }
  if ( i < Self::ImageDimension2 )
    {
    for ( i = 0; i < Self::ImageDimension2; i++ )
      {
      m_Sigma[i] = sigma;
      }
    this->RecomputeGaussianKernel();
    }
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::SetExtent(const double *extent)
{
  unsigned int i;

  for ( i = 0; i < Self::ImageDimension2; i++ )
    {
    if ( extent[i] != m_Extent[i] )
      {
      break;
      }
    }
  if ( i < Self::ImageDimension2 )
    {
    for ( i = 0; i < Self::ImageDimension2; i++ )
      {
      m_Extent[i] = extent[i];
      }
    this->RecomputeGaussianKernel();
    }
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::SetExtent(const double extent)
{
  unsigned int i;

  for ( i = 0; i < Self::ImageDimension2; i++ )
    {
    if ( Math::NotExactlyEquals(extent, m_Extent[i]) )
      {
      break;
      }
    }
  if ( i < Self::ImageDimension2 )
    {
    for ( i = 0; i < Self::ImageDimension2; i++ )
      {
      m_Extent[i] = extent;
      }
    this->RecomputeGaussianKernel();
    }
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::RecomputeGaussianKernel()
{
  const TInputImage* const inputImage = this->GetInputImage();

  if (inputImage == nullptr)
    {
    // Do clean-up, to ensure that the neighborhood iterators and operators will
    // not refer to a previous image, and to reduce memory usage.
    for(auto& neighborhoodIterator: m_NeighborhoodIterators)
      {
      neighborhoodIterator.reset();
      }
    m_OperatorArray = OperatorArrayType();
    }
  else
    {
    using SpacingType = typename TInputImage::SpacingType;
    const SpacingType spacing = m_UseImageSpacing ? inputImage->GetSpacing() : SpacingType(1);

    for ( unsigned int direction = 0; direction < Self::ImageDimension2; ++direction )
      {
      // Set the derivative of the Gaussian first
      OperatorNeighborhoodType dogNeighborhood;
      typename GaussianDerivativeFunctionType::InputType pt;
      typename NeighborhoodType::SizeType size;
      size.Fill(0);
      size[direction] = static_cast<SizeValueType>( m_Sigma[direction] * m_Extent[direction] );
      dogNeighborhood.SetRadius(size);

      typename GaussianDerivativeFunctionType::ArrayType s;
      s[0] = m_Sigma[direction];
      m_GaussianDerivativeFunction->SetSigma(s);

      typename OperatorNeighborhoodType::Iterator it = dogNeighborhood.Begin();

      unsigned int i = 0;

      const typename TInputImage::SpacingValueType directionSpacing = spacing[direction];
      assert(directionSpacing != 0);

      while ( it != dogNeighborhood.End() )
        {
        pt[0] = dogNeighborhood.GetOffset(i)[direction] * directionSpacing;
        ( *it ) = m_GaussianDerivativeFunction->Evaluate(pt);
        ++i;
        ++it;
        }

      m_OperatorArray[direction] = dogNeighborhood;

      // Note: A future version of ITK could possibly also set a Gaussian blurring operator
      // here, which should then be applied at EvaluateAtIndex(index).

      m_NeighborhoodIterators[direction].reset(new ConstNeighborhoodIterator<TInputImage>
        { size, inputImage, inputImage->GetRequestedRegion() });
      }
    }
}

template< typename TInputImage, typename TOutput >
typename GaussianDerivativeImageFunction< TInputImage, TOutput >::OutputType
GaussianDerivativeImageFunction< TInputImage, TOutput >
::EvaluateAtIndex(const IndexType & index) const
{
  OutputType gradient;

  for ( unsigned int direction = 0; direction < Self::ImageDimension2; ++direction )
    {
    // Note: A future version of ITK should do Gaussian blurring here.

    // Apply each Gaussian kernel to a subset of the image
    ConstNeighborhoodIterator< InputImageType >* const neighborhoodIterator =
      m_NeighborhoodIterators[direction].get();
    assert(neighborhoodIterator != nullptr);
    neighborhoodIterator->SetLocation(index);
    using InnerProduct = NeighborhoodInnerProduct< InputImageType, TOutput >;

    gradient[direction] = InnerProduct::Compute(*neighborhoodIterator, m_OperatorArray[direction]);
    }

  return gradient;
}

template< typename TInputImage, typename TOutput >
typename GaussianDerivativeImageFunction< TInputImage, TOutput >::OutputType
GaussianDerivativeImageFunction< TInputImage, TOutput >
::Evaluate(const PointType & point) const
{
  IndexType index;

  this->ConvertPointToNearestIndex(point, index);
  return this->EvaluateAtIndex (index);
}

template< typename TInputImage, typename TOutput >
typename GaussianDerivativeImageFunction< TInputImage, TOutput >::OutputType
GaussianDerivativeImageFunction< TInputImage, TOutput >
::EvaluateAtContinuousIndex(const ContinuousIndexType & cindex) const
{
  IndexType index;

  this->ConvertContinuousIndexToNearestIndex(cindex, index);
  return this->EvaluateAtIndex(index);
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;

  os << indent << "Sigma: " << m_Sigma << std::endl;
  os << indent << "Extent: " << m_Extent << std::endl;

  os << indent << "OperatorArray: " << m_OperatorArray << std::endl;
  os << indent << "GaussianDerivativeFunction: "
     << m_GaussianDerivativeFunction << std::endl;
}

} // end namespace itk

#endif
