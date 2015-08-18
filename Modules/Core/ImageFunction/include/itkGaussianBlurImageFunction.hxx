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
#ifndef itkGaussianBlurImageFunction_hxx
#define itkGaussianBlurImageFunction_hxx

#include "itkGaussianBlurImageFunction.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkMath.h"

namespace itk
{
/** Set the Input Image */
template< typename TInputImage, typename TOutput >
GaussianBlurImageFunction< TInputImage, TOutput >
::GaussianBlurImageFunction()
{
  typename GaussianFunctionType::ArrayType mean;
  mean[0] = 0.0f;
  for ( unsigned int i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    m_Sigma[i] = 1.0f;
    m_MaximumError[i] = 0.001f;
    m_MaximumKernelWidth = 32;
    m_Extent[i] = 1.0f;
    }
  m_UseImageSpacing = true;

  m_GaussianFunction = GaussianFunctionType::New();
  m_GaussianFunction->SetMean(mean);
  m_GaussianFunction->SetNormalized(false); // faster
  m_OperatorImageFunction = OperatorImageFunctionType::New();
  m_OperatorInternalImageFunction = OperatorInternalImageFunctionType::New();
  m_InternalImage = InternalImageType::New();
  this->RecomputeGaussianKernel();
}

/** Set the input image */
template< typename TInputImage, typename TOutput >
void
GaussianBlurImageFunction< TInputImage, TOutput >
::SetInputImage(const InputImageType *ptr)
{
  Superclass::SetInputImage(ptr);
}

/** Print self method */
template< typename TInputImage, typename TOutput >
void
GaussianBlurImageFunction< TInputImage, TOutput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);

  for ( unsigned int i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    os << indent << "Sigma[" << i << "] : " <<  m_Sigma[i] << std::endl;
    os << indent << "MaximumError[" << i << "] : " << m_MaximumError[i] << std::endl;
    os << indent << "Extent[" << i << "] : " << m_Extent[i] << std::endl;
    }
  os << indent << "MaximumKernelWidth: " << m_MaximumKernelWidth << std::endl;
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;

  os << indent << "Internal Image : " << m_InternalImage << std::endl;
}

/** Set the variance of the gaussian in each direction */
template< typename TInputImage, typename TOutput >
void
GaussianBlurImageFunction< TInputImage, TOutput >
::SetSigma(const double *sigma)
{
  unsigned int i;

  for ( i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    if ( sigma[i] != m_Sigma[i] )
      {
      break;
      }
    }
  if ( i < itkGetStaticConstMacro(ImageDimension) )
    {
    for ( i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
      {
      m_Sigma[i] = sigma[i];
      }
    this->RecomputeGaussianKernel();
    }
}

/** Set the variance of the gaussian in each direction */
template< typename TInputImage, typename TOutput >
void
GaussianBlurImageFunction< TInputImage, TOutput >
::SetSigma(const double sigma)
{
  unsigned int i;

  for ( i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    if ( Math::NotExactlyEquals(sigma, m_Sigma[i]) )
      {
      break;
      }
    }
  if ( i < itkGetStaticConstMacro(ImageDimension) )
    {
    for ( i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
      {
      m_Sigma[i] = sigma;
      }
    this->RecomputeGaussianKernel();
    }
}

/** Set the extent of the gaussian in each direction */
template< typename TInputImage, typename TOutput >
void
GaussianBlurImageFunction< TInputImage, TOutput >
::SetExtent(const double *extent)
{
  unsigned int i;

  for ( i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    if ( extent[i] != m_Extent[i] )
      {
      break;
      }
    }
  if ( i < itkGetStaticConstMacro(ImageDimension) )
    {
    for ( i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
      {
      m_Extent[i] = extent[i];
      }
    this->RecomputeGaussianKernel();
    }
}

/** Set the extent of the gaussian in each direction */
template< typename TInputImage, typename TOutput >
void
GaussianBlurImageFunction< TInputImage, TOutput >
::SetExtent(const double extent)
{
  unsigned int i;

  for ( i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    if ( Math::NotExactlyEquals(extent, m_Extent[i]) )
      {
      break;
      }
    }
  if ( i < itkGetStaticConstMacro(ImageDimension) )
    {
    for ( i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
      {
      m_Extent[i] = extent;
      }
    this->RecomputeGaussianKernel();
    }
}

/** Recompute the gaussian kernel used to evaluate indexes
 *  And allocate the internal image for processing depending on
 *  the size of the operator */
template< typename TInputImage, typename TOutput >
void
GaussianBlurImageFunction< TInputImage, TOutput >
::RecomputeGaussianKernel()
{
  typename InternalImageType::SizeType size;
  // Compute the convolution of each kernel in each direction
  for ( unsigned int direction = 0; direction < itkGetStaticConstMacro(ImageDimension); direction++ )
    {
    GaussianOperatorType gaussianOperator;

    gaussianOperator.SetDirection(direction);
    gaussianOperator.SetMaximumError(m_MaximumError[direction]);
    gaussianOperator.SetMaximumKernelWidth(m_MaximumKernelWidth);

    if ( ( m_UseImageSpacing == true ) && ( this->GetInputImage() ) )
      {
      if ( this->GetInputImage()->GetSpacing()[direction] == 0.0 )
        {
        itkExceptionMacro(<< "Pixel spacing cannot be zero");
        }
      else
        {
        gaussianOperator.SetVariance(m_Sigma[direction] * m_Sigma[direction]
                                     / this->GetInputImage()->GetSpacing()[direction]);
        }
      }
    else
      {
      gaussianOperator.SetVariance(m_Sigma[direction] * m_Sigma[direction]);
      }

    gaussianOperator.CreateDirectional();
    m_OperatorArray[direction] = gaussianOperator;
    size[direction] = gaussianOperator.GetSize()[direction];
    }

  // Allocate the internal image
  m_InternalImage = InternalImageType::New();
  typename InternalImageType::RegionType region;
  region.SetSize(size);
  m_InternalImage->SetRegions(region);
  m_InternalImage->Allocate(true); // initialize buffer to zero
}

/** Evaluate the function at the specifed point */
template< typename TInputImage, typename TOutput >
TOutput
GaussianBlurImageFunction< TInputImage, TOutput >
::EvaluateAtIndex(const IndexType & index) const
{
  return this->EvaluateAtIndex(index, m_OperatorArray);
}

/** Evaluate the function at the specifed point */
template< typename TInputImage, typename TOutput >
TOutput
GaussianBlurImageFunction< TInputImage, TOutput >
::EvaluateAtIndex(const IndexType & index, const OperatorArrayType & operatorArray) const
{
  const InputImageType * inputImage = this->GetInputImage();

  // First time we use the complete image and fill the internal image
  m_OperatorImageFunction->SetInputImage( inputImage );
  m_OperatorImageFunction->SetOperator(operatorArray[0]);

  // if 1D Image we return the result
  if ( itkGetStaticConstMacro(ImageDimension) == 1 )
    {
    return m_OperatorImageFunction->EvaluateAtIndex(index);
    }

  // Compute the centered index of the neighborhood
  IndexType centerIndex;
  for ( unsigned int i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    centerIndex[i] = (IndexValueType)( (float)m_InternalImage->GetBufferedRegion().GetSize()[i] / 2.0 );
    }

  // first direction
  typename InternalImageType::IndexType ind;
  ind = index;

  //Define the region of the iterator
  typename InternalImageType::RegionType region;
  typename InternalImageType::SizeType size = m_InternalImage->GetBufferedRegion().GetSize();
  size[0] = 1;
  region.SetSize(size);

  for ( unsigned int i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    if ( i != 0 )
      {
      ind[i] -= centerIndex[i];
      }
    }
  region.SetIndex(ind);

  typename InternalImageType::RegionType regionN;
  regionN.SetSize(size);
  ind = centerIndex;
  for ( unsigned int i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    if ( i != 0 )
      {
      ind[i] = 0;
      }
    }
  regionN.SetIndex(ind);

  typename InternalImageType::RegionType regionS = region;
  regionS.Crop( inputImage->GetBufferedRegion() );

  itk::ImageLinearConstIteratorWithIndex< InputImageType >     it(inputImage, regionS);
  itk::ImageLinearIteratorWithIndex< InternalImageType >   itN(m_InternalImage, regionN);
  it.SetDirection(1);
  itN.SetDirection(1);
  it.GoToBeginOfLine();
  itN.GoToBeginOfLine();
  while ( !it.IsAtEnd() )
    {
    while ( !it.IsAtEndOfLine() )
      {
      itN.Set( m_OperatorImageFunction->EvaluateAtIndex( it.GetIndex() ) );
      ++it;
      ++itN;
      }
    it.NextLine();
    itN.NextLine();
    }

  // Do the convolution in other directions
  for ( unsigned int direction = 1; direction < itkGetStaticConstMacro(ImageDimension); direction++ )
    {
    size[direction] = 1;
    ind = centerIndex;
    for ( unsigned int i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
      {
      if ( i > direction )
        {
        ind[i] = 0;
        }
      }
    region.SetSize(size);
    region.SetIndex(ind);

    m_OperatorInternalImageFunction->SetInputImage(m_InternalImage);
    m_OperatorInternalImageFunction->SetOperator(operatorArray[direction]);

    itk::ImageLinearIteratorWithIndex< InternalImageType > itr(m_InternalImage, region);

    unsigned int dir = direction + 1;
    if ( dir == itkGetStaticConstMacro(ImageDimension) )
      {
      dir = itkGetStaticConstMacro(ImageDimension) - 1;
      }

    itr.SetDirection(dir);
    itr.GoToBeginOfLine();
    while ( !itr.IsAtEnd() )
      {
      while ( !itr.IsAtEndOfLine() )
        {
        itr.Set( m_OperatorInternalImageFunction->EvaluateAtIndex( itr.GetIndex() ) );
        ++itr;
        }
      itr.NextLine();
      }
    }

  return m_InternalImage->GetPixel(centerIndex);
}

/** Recompute the gaussian kernel used to evaluate indexes
 *  The variance should be uniform */
template< typename TInputImage, typename TOutput >
void
GaussianBlurImageFunction< TInputImage, TOutput >
::RecomputeContinuousGaussianKernel(const double *offset) const
{
  for ( unsigned int direction = 0; direction < itkGetStaticConstMacro(ImageDimension); direction++ )
    {
    typename NeighborhoodType::SizeType size;
    size.Fill(0);
    size[direction] = static_cast<SizeValueType>( m_Sigma[direction] * m_Extent[direction] );

    NeighborhoodType gaussianNeighborhood;
    gaussianNeighborhood.SetRadius(size);


    itk::FixedArray< double, 1 > s;
    s[0] = m_Sigma[direction];
    m_GaussianFunction->SetSigma(s);

    unsigned int i = 0;
    float        sum = 0;
    typename NeighborhoodType::Iterator it = gaussianNeighborhood.Begin();
    while ( it != gaussianNeighborhood.End() )
      {
      typename GaussianFunctionType::InputType pt;
      pt[0] = gaussianNeighborhood.GetOffset(i)[direction] - offset[direction];
      if ( ( m_UseImageSpacing == true ) && ( this->GetInputImage() ) )
        {
        if ( this->GetInputImage()->GetSpacing()[direction] == 0.0 )
          {
          itkExceptionMacro(<< "Pixel spacing cannot be zero");
          }
        else
          {
          pt[0] *= this->GetInputImage()->GetSpacing()[direction];
          }
        }

      ( *it ) = m_GaussianFunction->Evaluate(pt);
      sum += ( *it );
      i++;
      it++;
      }

    // Make the filter DC-Constant
    it = gaussianNeighborhood.Begin();
    while ( it != gaussianNeighborhood.End() )
      {
      ( *it ) /= sum;
      it++;
      }
    m_ContinuousOperatorArray[direction] = gaussianNeighborhood;
    }
}

/** Evaluate the function at the specifed point */
template< typename TInputImage, typename TOutput >
TOutput
GaussianBlurImageFunction< TInputImage, TOutput >
::Evaluate(const PointType & point) const
{
  ContinuousIndexType cindex;

  this->m_InternalImage->TransformPhysicalPointToContinuousIndex(point, cindex);

  return this->EvaluateAtContinuousIndex(cindex);
}

/** Evaluate the function at specified ContinuousIndex position.*/
template< typename TInputImage, typename TOutput >
TOutput
GaussianBlurImageFunction< TInputImage, TOutput >
::EvaluateAtContinuousIndex(const ContinuousIndexType & cindex) const
{
  IndexType index;

  index.CopyWithRound(cindex);

  double offset[itkGetStaticConstMacro(ImageDimension)];
  for ( unsigned int i = 0; i < itkGetStaticConstMacro(ImageDimension); i++ )
    {
    offset[i] = cindex[i] - index[i];
    }

  this->RecomputeContinuousGaussianKernel(offset);

  return this->EvaluateAtIndex(index, m_ContinuousOperatorArray);
}
} // end namespace itk

#endif
