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
#ifndef itkDiscreteGaussianDerivativeImageFunction_hxx
#define itkDiscreteGaussianDerivativeImageFunction_hxx

#include "itkDiscreteGaussianDerivativeImageFunction.h"
#include "itkNeighborhoodOperatorImageFilter.h"

namespace itk
{
/** Set the Input Image */
template< typename TInputImage, typename TOutput >
DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >
::DiscreteGaussianDerivativeImageFunction():
  m_MaximumError(0.005),
  m_MaximumKernelWidth(30),
  m_NormalizeAcrossScale(true),
  m_UseImageSpacing(true),
  m_InterpolationMode(NearestNeighbourInterpolation)
{
  m_Variance.Fill(1.0);
  m_Order.Fill(0);
  m_Order[0] = 1; // by default calculate derivative in x
  m_OperatorImageFunction = OperatorImageFunctionType::New();
}

/** Print self method */
template< typename TInputImage, typename TOutput >
void
DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;
  os << indent << "NormalizeAcrossScale: " << m_NormalizeAcrossScale << std::endl;
  os << indent << "Variance: " << m_Variance << std::endl;
  os << indent << "Order: " << m_Order << std::endl;
  os << indent << "MaximumError: " << m_MaximumError << std::endl;
  os << indent << "MaximumKernelWidth: " << m_MaximumKernelWidth << std::endl;
  os << indent << "InterpolationMode: " << m_InterpolationMode << std::endl;
  os << indent << "OperatorArray: " << m_OperatorArray << std::endl;
  os << indent << "DerivativeKernel: " << m_DerivativeKernel << std::endl;
  os << indent << "OperatorImageFunction: " << m_OperatorImageFunction << std::endl;
}

/** Set the input image */
template< typename TInputImage, typename TOutput >
void
DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >
::SetInputImage(const InputImageType *ptr)
{
  Superclass::SetInputImage(ptr);
  m_OperatorImageFunction->SetInputImage(ptr);
}

/** Recompute the gaussian kernel used to evaluate indexes
 *  This should use a fastest Derivative Gaussian operator */
template< typename TInputImage, typename TOutput >
void
DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >
::RecomputeGaussianKernel()
{
  // Create N operators (N=ImageDimension) with the order specified in m_Order
  unsigned int idx;

  for ( unsigned int direction = 0;
        direction < itkGetStaticConstMacro(ImageDimension2);
        direction++ )
    {
    m_OperatorArray[direction].SetDirection(direction);
    m_OperatorArray[direction].SetMaximumKernelWidth(m_MaximumKernelWidth);
    m_OperatorArray[direction].SetMaximumError(m_MaximumError);

    if ( ( m_UseImageSpacing == true ) && ( this->GetInputImage() ) )
      {
      if ( this->GetInputImage()->GetSpacing()[direction] == 0.0 )
        {
        itkExceptionMacro(<< "Pixel spacing cannot be zero");
        }
      else
        {
        m_OperatorArray[direction].SetSpacing(this->GetInputImage()->GetSpacing()[direction]);
        }
      }

    // GaussianDerivativeOperator modifies the variance when setting
    // image spacing
    m_OperatorArray[direction].SetVariance(m_Variance[direction]);
    m_OperatorArray[direction].SetOrder(m_Order[direction]);
    m_OperatorArray[direction].SetNormalizeAcrossScale(m_NormalizeAcrossScale);
    m_OperatorArray[direction].CreateDirectional();
    }

  // Now precompute the N-dimensional kernel. This fastest as we don't
  // have to perform N convolutions for each point we calculate but
  // only one.

  typedef itk::Image< TOutput, itkGetStaticConstMacro(ImageDimension2) > KernelImageType;
  typename KernelImageType::Pointer kernelImage = KernelImageType::New();

  typedef typename KernelImageType::RegionType RegionType;
  RegionType region;

  typename RegionType::SizeType size;
  size.Fill(4 * m_OperatorArray[0].GetRadius()[0] + 1);
  region.SetSize(size);

  kernelImage->SetRegions(region);
  kernelImage->Allocate();
  kernelImage->FillBuffer(itk::NumericTraits< TOutput >::ZeroValue());

  // Initially the kernel image will be an impulse at the center
  typename KernelImageType::IndexType centerIndex;
  centerIndex.Fill(2 * m_OperatorArray[0].GetRadius()[0]);   // include also
                                                             // boundaries
  kernelImage->SetPixel(centerIndex, itk::NumericTraits< TOutput >::OneValue());

  // Create an image region to be used later that does not include boundaries
  RegionType kernelRegion;
  size.Fill(2 * m_OperatorArray[0].GetRadius()[0] + 1);
  typename RegionType::IndexType origin;
  origin.Fill(m_OperatorArray[0].GetRadius()[0]);
  kernelRegion.SetSize(size);
  kernelRegion.SetIndex(origin);

  // Now create an image filter to perform successive convolutions
  typedef itk::NeighborhoodOperatorImageFilter< KernelImageType, KernelImageType >
  NeighborhoodFilterType;
  typename NeighborhoodFilterType::Pointer convolutionFilter = NeighborhoodFilterType::New();

  for ( unsigned int direction = 0; direction < itkGetStaticConstMacro(ImageDimension2); ++direction )
    {
    convolutionFilter->SetInput(kernelImage);
    convolutionFilter->SetOperator(m_OperatorArray[direction]);
    convolutionFilter->Update();
    kernelImage = convolutionFilter->GetOutput();
    kernelImage->DisconnectPipeline();
    }

  // Set the size of the kernel
  m_DerivativeKernel.SetRadius(m_OperatorArray[0].GetRadius()[0]);

  // Copy kernel image to neighborhood. Do not copy boundaries.
  ImageRegionConstIterator< KernelImageType > it(kernelImage, kernelRegion);
  it.GoToBegin();
  idx = 0;

  while ( !it.IsAtEnd() )
    {
    m_DerivativeKernel[idx] = it.Get();
    ++idx;
    ++it;
    }
}

/** Evaluate the function at the specifed index */
template< typename TInputImage, typename TOutput >
typename DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >::OutputType
DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >
::EvaluateAtIndex(const IndexType & index) const
{
  OutputType derivative;

  m_OperatorImageFunction->SetOperator(m_DerivativeKernel);
  derivative = m_OperatorImageFunction->EvaluateAtIndex(index);


  return derivative;
}

/** Evaluate the function at the specifed point */
template< typename TInputImage, typename TOutput >
typename DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >::OutputType
DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >
::Evaluate(const PointType & point) const
{
  if ( m_InterpolationMode == NearestNeighbourInterpolation )
    {
    IndexType index;
    this->ConvertPointToNearestIndex(point, index);
    return this->EvaluateAtIndex (index);
    }
  else
    {
    ContinuousIndexType cindex;
    this->ConvertPointToContinuousIndex(point, cindex);
    return this->EvaluateAtContinuousIndex(cindex);
    }
}

/** Evaluate the function at specified ContinuousIndex position.*/
template< typename TInputImage, typename TOutput >
typename DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >::OutputType
DiscreteGaussianDerivativeImageFunction< TInputImage, TOutput >
::EvaluateAtContinuousIndex(const ContinuousIndexType & cindex) const
{
  if ( m_InterpolationMode == NearestNeighbourInterpolation )
    {
    IndexType index;
    this->ConvertContinuousIndexToNearestIndex(cindex, index);
    return this->EvaluateAtIndex(index);
    }
  else
    {
    typedef unsigned int NumberOfNeighborsType;

    unsigned int  dim; // index over dimension
    NumberOfNeighborsType numberOfNeighbors = 1 << ImageDimension2;

    // Compute base index = closet index below point
    // Compute distance from point to base index
    IndexType baseIndex;
    double    distance[ImageDimension2];

    for ( dim = 0; dim < ImageDimension2; dim++ )
      {
      baseIndex[dim] = Math::Floor< IndexValueType >(cindex[dim]);
      distance[dim] = cindex[dim] - static_cast< double >( baseIndex[dim] );
      }

    // Interpolated value is the weighted sum of each of the surrounding
    // neighbors. The weight for each neighbor is the fraction overlap
    // of the neighbor pixel with respect to a pixel centered on point.
    TOutput value = NumericTraits< TOutput >::ZeroValue();
    TOutput totalOverlap = NumericTraits< TOutput >::ZeroValue();

    for ( NumberOfNeighborsType counter = 0; counter < numberOfNeighbors; counter++ )
      {
      double       overlap = 1.0;    // fraction overlap
      NumberOfNeighborsType upper = counter;  // each bit indicates upper/lower neighbour
      IndexType    neighIndex;

      // get neighbor index and overlap fraction
      for ( dim = 0; dim < ImageDimension2; dim++ )
        {
        if ( upper & 1 )
          {
          neighIndex[dim] = baseIndex[dim] + 1;
          overlap *= distance[dim];
          }
        else
          {
          neighIndex[dim] = baseIndex[dim];
          overlap *= 1.0 - distance[dim];
          }
        upper >>= 1;
        }

      // get neighbor value only if overlap is not zero
      if ( overlap )
        {
        value += overlap * static_cast< TOutput >( this->EvaluateAtIndex(neighIndex) );
        totalOverlap += overlap;
        }

      if ( totalOverlap == 1.0 )
        {
        // finished
        break;
        }
      }
    return ( static_cast< OutputType >( value ) );
    }
}
} // end namespace itk

#endif
