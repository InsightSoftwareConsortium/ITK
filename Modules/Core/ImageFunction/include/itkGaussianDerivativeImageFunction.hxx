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

#include "itkCompensatedSummation.h"
#include "itkMath.h"

namespace itk
{

template< typename TInputImage, typename TOutput >
GaussianDerivativeImageFunction< TInputImage, TOutput >
::GaussianDerivativeImageFunction()
{
  typename GaussianFunctionType::ArrayType mean;
  mean[0] = 0.0;
  for ( unsigned int i = 0; i < itkGetStaticConstMacro(ImageDimension2); i++ )
    {
    m_Sigma[i] = 1.0;
    m_Extent[i] = 1.0;
    }
  m_UseImageSpacing = true;
  m_GaussianDerivativeFunction = GaussianDerivativeFunctionType::New();
  m_GaussianFunction = GaussianFunctionType::New();
  m_OperatorImageFunction = OperatorImageFunctionType::New();
  m_GaussianFunction->SetMean(mean);
  m_GaussianFunction->SetNormalized(false);           // faster
  m_GaussianDerivativeFunction->SetNormalized(false); // faster
  this->RecomputeGaussianKernel();
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::SetInputImage(const InputImageType *ptr)
{
  Superclass::SetInputImage(ptr);
  m_OperatorImageFunction->SetInputImage(ptr);
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::SetSigma(const double *sigma)
{
  unsigned int i;

  for ( i = 0; i < itkGetStaticConstMacro(ImageDimension2); i++ )
    {
    if ( sigma[i] != m_Sigma[i] )
      {
      break;
      }
    }
  if ( i < itkGetStaticConstMacro(ImageDimension2) )
    {
    for ( i = 0; i < itkGetStaticConstMacro(ImageDimension2); i++ )
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

  for ( i = 0; i < itkGetStaticConstMacro(ImageDimension2); i++ )
    {
    if ( Math::NotExactlyEquals(sigma, m_Sigma[i]) )
      {
      break;
      }
    }
  if ( i < itkGetStaticConstMacro(ImageDimension2) )
    {
    for ( i = 0; i < itkGetStaticConstMacro(ImageDimension2); i++ )
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

  for ( i = 0; i < itkGetStaticConstMacro(ImageDimension2); i++ )
    {
    if ( extent[i] != m_Extent[i] )
      {
      break;
      }
    }
  if ( i < itkGetStaticConstMacro(ImageDimension2) )
    {
    for ( i = 0; i < itkGetStaticConstMacro(ImageDimension2); i++ )
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

  for ( i = 0; i < itkGetStaticConstMacro(ImageDimension2); i++ )
    {
    if ( Math::NotExactlyEquals(extent, m_Extent[i]) )
      {
      break;
      }
    }
  if ( i < itkGetStaticConstMacro(ImageDimension2) )
    {
    for ( i = 0; i < itkGetStaticConstMacro(ImageDimension2); i++ )
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
  unsigned int direction = 0;

  for ( unsigned int op = 0; op < itkGetStaticConstMacro(ImageDimension2); ++op )
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
    while ( it != dogNeighborhood.End() )
      {
      pt[0] = dogNeighborhood.GetOffset(i)[direction];

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
      ( *it ) = m_GaussianDerivativeFunction->Evaluate(pt);
      ++i;
      ++it;
      }

    m_OperatorArray[op * 2] = dogNeighborhood;

    // Set the Gaussian operator
    m_GaussianFunction->SetSigma(s);
    OperatorNeighborhoodType gaussianNeighborhood;
    gaussianNeighborhood.SetRadius(size);

    it = gaussianNeighborhood.Begin();

    i = 0;
    CompensatedSummation< TOutput > sum;
    while ( it != gaussianNeighborhood.End() )
      {
      pt[0] = gaussianNeighborhood.GetOffset(i)[direction];

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
      ++i;
      ++it;
      }

    // Make the filter DC-Constant
    it = gaussianNeighborhood.Begin();
    const TOutput sumInverse = 1. / sum.GetSum();
    while ( it != gaussianNeighborhood.End() )
      {
      ( *it ) *= sumInverse;
      ++it;
      }

    m_OperatorArray[op * 2 + 1] = gaussianNeighborhood;
    ++direction;
    }
}

template< typename TInputImage, typename TOutput >
typename GaussianDerivativeImageFunction< TInputImage, TOutput >::OutputType
GaussianDerivativeImageFunction< TInputImage, TOutput >
::EvaluateAtIndex(const IndexType & index) const
{
  OutputType gradient;

  for ( unsigned int ii = 0; ii < itkGetStaticConstMacro(ImageDimension2); ++ii )
    {
    // Apply each Gaussian kernel to a subset of the image
    typedef typename OutputType::RealValueType OutputRealValueType;
    OutputRealValueType value = static_cast< OutputRealValueType >( this->GetInputImage()->GetPixel(index) );

    // Gaussian blurring first
    for ( unsigned int direction = 0; direction < itkGetStaticConstMacro(ImageDimension2); ++direction )
      {
      if ( ii != direction )
        {
        const unsigned int idx = 2 * direction + 1; // select only Gaussian kernel
        const unsigned int center = (unsigned int)( ( m_OperatorArray[idx].GetSize()[direction] - 1 ) / 2 );
        TOutput      centerval = m_OperatorArray[idx].GetCenterValue();
        m_OperatorArray[idx][center] = 0;
        m_OperatorImageFunction->SetOperator(m_OperatorArray[idx]);
        value = m_OperatorImageFunction->EvaluateAtIndex(index) + centerval * value;
        }
      }

    // then derivative in the direction
    const unsigned int idx = 2 * ii;
    const signed int center = (unsigned int)( ( m_OperatorArray[idx].GetSize()[ii] - 1 ) / 2 );
    TOutput    centerval = m_OperatorArray[idx].GetCenterValue();
    m_OperatorArray[idx][center] = 0;
    m_OperatorImageFunction->SetOperator(m_OperatorArray[idx]);
    value = m_OperatorImageFunction->EvaluateAtIndex(index) + centerval * value;

    gradient[ii] = static_cast< typename OutputType::ComponentType >( value );
    }

  return gradient;
}

template< typename TInputImage, typename TOutput >
void
GaussianDerivativeImageFunction< TInputImage, TOutput >
::RecomputeContinuousGaussianKernel(
  const double *offset) const
{
  unsigned int direction = 0;

  for ( unsigned int op = 0; op < itkGetStaticConstMacro(ImageDimension2); ++op )
    {
    // Set the derivative of the Gaussian first
    OperatorNeighborhoodType dogNeighborhood;
    typename GaussianDerivativeFunctionType::InputType pt;
    typename OperatorNeighborhoodType::SizeType size;
    size.Fill(0);
    size[direction] = static_cast<SizeValueType>( m_Sigma[direction] * m_Extent[direction] );
    dogNeighborhood.SetRadius(size);

    typename GaussianDerivativeFunctionType::ArrayType s;
    s[0] = m_Sigma[direction];
    m_GaussianDerivativeFunction->SetSigma(s);

    typename OperatorNeighborhoodType::Iterator it = dogNeighborhood.Begin();

    unsigned int ii = 0;
    while ( it != dogNeighborhood.End() )
      {
      pt[0] = dogNeighborhood.GetOffset(ii)[direction] - offset[direction];

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
      ( *it ) = m_GaussianDerivativeFunction->Evaluate(pt);
      ++ii;
      ++it;
      }

    m_ContinuousOperatorArray[op * 2] = dogNeighborhood;

    // Set the Gaussian operator
    m_GaussianFunction->SetSigma(s);
    OperatorNeighborhoodType gaussianNeighborhood;
    gaussianNeighborhood.SetRadius(size);

    it = gaussianNeighborhood.Begin();

    ii = 0;
    CompensatedSummation< TOutput > sum;
    while ( it != gaussianNeighborhood.End() )
      {
      pt[0] = gaussianNeighborhood.GetOffset(ii)[direction] - offset[direction];

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
      ++ii;
      ++it;
      }

    // Make the filter DC-Constant
    it = gaussianNeighborhood.Begin();
    const TOutput sumInverse = 1. / sum.GetSum();
    while ( it != gaussianNeighborhood.End() )
      {
      ( *it ) *= sumInverse;
      ++it;
      }

    m_ContinuousOperatorArray[op * 2 + 1] = gaussianNeighborhood;
    ++direction;
    }
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
  os << indent << "ContinuousOperatorArray: "
     << m_ContinuousOperatorArray << std::endl;
  os << indent << "OperatorImageFunction: "
     << m_OperatorImageFunction << std::endl;
  os << indent << "GaussianDerivativeFunction: "
     << m_GaussianDerivativeFunction << std::endl;
  os << indent << "GaussianFunction: "
     << m_GaussianFunction << std::endl;
}

} // end namespace itk

#endif
