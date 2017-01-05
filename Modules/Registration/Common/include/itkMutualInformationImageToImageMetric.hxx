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
#ifndef itkMutualInformationImageToImageMetric_hxx
#define itkMutualInformationImageToImageMetric_hxx

#include "itkMutualInformationImageToImageMetric.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkMath.h"
#include "itkGaussianKernelFunction.h"
#include "itkCompensatedSummation.h"

namespace itk
{
/**
 * Constructor
 */
template <typename TFixedImage, typename TMovingImage>
MutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::MutualInformationImageToImageMetric()
{
  m_NumberOfSpatialSamples = 0;
  this->SetNumberOfSpatialSamples(50);

  m_KernelFunction  = dynamic_cast<KernelFunctionType *>(
      GaussianKernelFunction<double>::New().GetPointer() );

  m_FixedImageStandardDeviation = 0.4;
  m_MovingImageStandardDeviation = 0.4;

  m_MinProbability = 0.0001;

  //
  // Following initialization is related to
  // calculating image derivatives
  this->SetComputeGradient(false); // don't use the default gradient for now
  m_DerivativeCalculator = DerivativeFunctionType::New();
  m_DerivativeCalculator->UseImageDirectionOn();
}

template <typename TFixedImage, typename TMovingImage>
void
MutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NumberOfSpatialSamples: ";
  os << m_NumberOfSpatialSamples << std::endl;
  os << indent << "FixedImageStandardDeviation: ";
  os << m_FixedImageStandardDeviation << std::endl;
  os << indent << "MovingImageStandardDeviation: ";
  os << m_MovingImageStandardDeviation << std::endl;
  os << indent << "KernelFunction: ";
  os << m_KernelFunction.GetPointer() << std::endl;
}

/*
 * Set the number of spatial samples
 */
template <typename TFixedImage, typename TMovingImage>
void
MutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::SetNumberOfSpatialSamples(
  unsigned int num)
{
  if( num == m_NumberOfSpatialSamples )
    {
    return;
    }

  this->Modified();

  // clamp to minimum of 1
  m_NumberOfSpatialSamples = ( ( num > 1 ) ? num : 1 );

  // resize the storage vectors
  m_SampleA.resize(m_NumberOfSpatialSamples);
  m_SampleB.resize(m_NumberOfSpatialSamples);
}

/*
 * Uniformly sample the fixed image domain. Each sample consists of:
 *  - the fixed image value
 *  - the corresponding moving image value
 *
 * \warning Note that this method has a different signature than the one in
 * the base OptImageToImageMetric and therefore they are not intended to
 * provide polymorphism. That is, this function is not overriding the one in
 * the base class.
 *
 */
template <typename TFixedImage, typename TMovingImage>
void
MutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::SampleFixedImageDomain(
  SpatialSampleContainer & samples) const
{
  typedef ImageRandomConstIteratorWithIndex<FixedImageType> RandomIterator;
  RandomIterator randIter( this->m_FixedImage, this->GetFixedImageRegion() );
  if (this->m_ReseedIterator)
    {
    randIter.ReinitializeSeed();
    }
  else
    {
    randIter.ReinitializeSeed(this->m_RandomSeed++);
    }
  randIter.SetNumberOfSamples(m_NumberOfSpatialSamples);
  randIter.GoToBegin();

  typename SpatialSampleContainer::iterator iter;
  typename SpatialSampleContainer::const_iterator end = samples.end();

  bool allOutside = true;

  this->m_NumberOfPixelsCounted = 0;    // Number of pixels that map into the
                                        // fixed and moving image mask, if
                                        // specified
                                        // and the resampled fixed grid after
                                        // transformation.

  // Number of random picks made from the portion of fixed image within the
  // fixed mask
  SizeValueType numberOfFixedImagePixelsVisited = 0;
  SizeValueType dryRunTolerance = this->GetFixedImageRegion().GetNumberOfPixels();
  for( iter = samples.begin(); iter != end; ++iter )
    {
    // Get sampled index
    FixedImageIndexType index = randIter.GetIndex();
    // Get sampled fixed image value
    ( *iter ).FixedImageValue = randIter.Get();
    // Translate index to point
    this->m_FixedImage->TransformIndexToPhysicalPoint(index,
                                                      ( *iter ).FixedImagePointValue);

    // If not inside the fixed mask, ignore the point
    if( this->m_FixedImageMask
        && !this->m_FixedImageMask->IsInside( ( *iter ).FixedImagePointValue ) )
      {
      ++randIter; // jump to another random position
      continue;
      }

    if( allOutside )
      {
      ++numberOfFixedImagePixelsVisited;
      if( numberOfFixedImagePixelsVisited > dryRunTolerance )
        {
        // We randomly visited as many points as is the size of the fixed image
        // region.. Too may samples mapped ouside.. go change your transform
        itkExceptionMacro(<< "Too many samples mapped outside the moving buffer");
        }
      }

    MovingImagePointType mappedPoint =
      this->m_Transform->TransformPoint( ( *iter ).FixedImagePointValue );

    // If the transformed point after transformation does not lie within the
    // MovingImageMask, skip it.
    if( this->m_MovingImageMask
        && !this->m_MovingImageMask->IsInside(mappedPoint) )
      {
      ++randIter;
      continue;
      }

    // The interpolator does not need to do bounds checking if we have masks,
    // since we know that the point is within the fixed and moving masks. But
    // a crazy user can specify masks that are bigger than the image. Then we
    // will need bounds checking.. So keep this anyway.
    if( this->m_Interpolator->IsInsideBuffer(mappedPoint) )
      {
      ( *iter ).MovingImageValue = this->m_Interpolator->Evaluate(mappedPoint);
      this->m_NumberOfPixelsCounted++;
      allOutside = false;
      }
    else
      {
      ( *iter ).MovingImageValue = 0;
      }

    // Jump to random position
    ++randIter;
    }

  if( allOutside )
    {
    // if all the samples mapped to the outside throw an exception
    itkExceptionMacro(<< "All the sampled point mapped to outside of the moving image");
    }
}

/*
 * Get the match Measure
 */
template <typename TFixedImage, typename TMovingImage>
typename MutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::MeasureType
MutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValue(const ParametersType & parameters) const
{
  // make sure the transform has the current parameters
  this->m_Transform->SetParameters(parameters);

  // collect sample set A
  this->SampleFixedImageDomain(m_SampleA);

  // collect sample set B
  this->SampleFixedImageDomain(m_SampleB);

  // calculate the mutual information

  typedef CompensatedSummation< double > SumType;
  SumType dLogSumFixed;
  SumType dLogSumMoving;
  SumType dLogSumJoint;
  SumType dSumFixed;
  SumType dSumMoving;
  SumType dSumJoint;

  typename SpatialSampleContainer::const_iterator aiter;
  typename SpatialSampleContainer::const_iterator aend = m_SampleA.end();
  typename SpatialSampleContainer::const_iterator biter;
  typename SpatialSampleContainer::const_iterator bend = m_SampleB.end();
  for( biter = m_SampleB.begin(); biter != bend; ++biter )
    {
    dSumFixed.ResetToZero();
    dSumMoving.ResetToZero();
    dSumJoint.ResetToZero();
    dSumFixed += m_MinProbability;
    dSumMoving += m_MinProbability;
    dSumJoint += m_MinProbability;
    for( aiter = m_SampleA.begin(); aiter != aend; ++aiter )
      {
      double valueFixed;
      double valueMoving;

      valueFixed = ( ( *biter ).FixedImageValue - ( *aiter ).FixedImageValue )
        / m_FixedImageStandardDeviation;
      valueFixed = m_KernelFunction->Evaluate(valueFixed);

      valueMoving = ( ( *biter ).MovingImageValue - ( *aiter ).MovingImageValue )
        / m_MovingImageStandardDeviation;
      valueMoving = m_KernelFunction->Evaluate(valueMoving);

      dSumFixed += valueFixed;
      dSumMoving += valueMoving;
      dSumJoint += valueFixed * valueMoving;
      } // end of sample A loop

    if( dSumFixed.GetSum() > 0.0 )
      {
      dLogSumFixed -= std::log( dSumFixed.GetSum() );
      }
    if( dSumMoving.GetSum() > 0.0 )
      {
      dLogSumMoving -= std::log( dSumMoving.GetSum() );
      }
    if( dSumJoint.GetSum() > 0.0 )
      {
      dLogSumJoint -= std::log( dSumJoint.GetSum() );
      }
    } // end of sample B loop

  double nsamp   = double(m_NumberOfSpatialSamples);

  double threshold = -0.5 *nsamp *std::log(m_MinProbability);
  if( dLogSumMoving.GetSum() > threshold || dLogSumFixed.GetSum() > threshold
      || dLogSumJoint.GetSum() > threshold  )
    {
    // at least half the samples in B did not occur within
    // the Parzen window width of samples in A
    itkExceptionMacro(<< "Standard deviation is too small");
    }

  MeasureType measure = dLogSumFixed.GetSum() + dLogSumMoving.GetSum() - dLogSumJoint.GetSum();
  measure /= nsamp;
  measure += std::log(nsamp);

  return measure;
}

/*
 * Get the both Value and Derivative Measure
 */
template <typename TFixedImage, typename TMovingImage>
void
MutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValueAndDerivative(
  const ParametersType & parameters,
  MeasureType & value,
  DerivativeType & derivative) const
{
  value = NumericTraits<MeasureType>::ZeroValue();
  unsigned int   numberOfParameters = this->m_Transform->GetNumberOfParameters();
  DerivativeType temp(numberOfParameters);
  temp.Fill(0);
  derivative = temp;

  // make sure the transform has the current parameters
  this->m_Transform->SetParameters(parameters);

  // set the DerivativeCalculator
  m_DerivativeCalculator->SetInputImage(this->m_MovingImage);

  // collect sample set A
  this->SampleFixedImageDomain(m_SampleA);

  // collect sample set B
  this->SampleFixedImageDomain(m_SampleB);

  // calculate the mutual information
  typedef CompensatedSummation< double > SumType;
  SumType dLogSumFixed;
  SumType dLogSumMoving;
  SumType dLogSumJoint;
  SumType dSumFixed;
  SumType dDenominatorMoving;
  SumType dDenominatorJoint;

  typename SpatialSampleContainer::iterator aiter;
  typename SpatialSampleContainer::const_iterator aend = m_SampleA.end();
  typename SpatialSampleContainer::iterator biter;
  typename SpatialSampleContainer::const_iterator bend = m_SampleB.end();

  // precalculate all the image derivatives for sample A
  typedef std::vector<DerivativeType> DerivativeContainer;
  DerivativeContainer sampleADerivatives;
  sampleADerivatives.resize(m_NumberOfSpatialSamples);

  typename DerivativeContainer::iterator aditer;
  DerivativeType        tempDeriv(numberOfParameters);
  TransformJacobianType jacobian(numberOfParameters, numberOfParameters);
  for( aiter = m_SampleA.begin(), aditer = sampleADerivatives.begin();
       aiter != aend; ++aiter, ++aditer )
    {
    /** FIXME: is there a way to avoid the extra copying step? */
    this->CalculateDerivatives( ( *aiter ).FixedImagePointValue, tempDeriv, jacobian );
    ( *aditer ) = tempDeriv;
    }

  DerivativeType derivB(numberOfParameters);
  for( biter = m_SampleB.begin(); biter != bend; ++biter )
    {
    dDenominatorMoving.ResetToZero();
    dDenominatorMoving += m_MinProbability;
    dDenominatorJoint.ResetToZero();
    dDenominatorJoint += m_MinProbability;
    dSumFixed.ResetToZero();
    dSumFixed += m_MinProbability;
    for( aiter = m_SampleA.begin(); aiter != aend; ++aiter )
      {
      double valueFixed;
      double valueMoving;

      valueFixed = ( ( *biter ).FixedImageValue - ( *aiter ).FixedImageValue )
        / m_FixedImageStandardDeviation;
      valueFixed = m_KernelFunction->Evaluate(valueFixed);

      valueMoving = ( ( *biter ).MovingImageValue - ( *aiter ).MovingImageValue )
        / m_MovingImageStandardDeviation;
      valueMoving = m_KernelFunction->Evaluate(valueMoving);

      dDenominatorMoving += valueMoving;
      dDenominatorJoint += valueMoving * valueFixed;

      dSumFixed += valueFixed;
      } // end of sample A loop

    if( dSumFixed.GetSum() > 0.0 )
      {
      dLogSumFixed -= std::log( dSumFixed.GetSum() );
      }
    if( dDenominatorMoving.GetSum() > 0.0 )
      {
      dLogSumMoving -= std::log( dDenominatorMoving.GetSum() );
      }
    if( dDenominatorJoint.GetSum() > 0.0 )
      {
      dLogSumJoint -= std::log( dDenominatorJoint.GetSum() );
      }

    /** get the image derivative for this B sample */
    this->CalculateDerivatives( ( *biter ).FixedImagePointValue, derivB, jacobian );

    SumType totalWeight;
    for( aiter = m_SampleA.begin(), aditer = sampleADerivatives.begin();
         aiter != aend; ++aiter, ++aditer )
      {
      double valueFixed;
      double valueMoving;
      double weightMoving;
      double weightJoint;
      double weight;

      valueFixed = ( ( *biter ).FixedImageValue - ( *aiter ).FixedImageValue )
        / m_FixedImageStandardDeviation;
      valueFixed = m_KernelFunction->Evaluate(valueFixed);

      valueMoving = ( ( *biter ).MovingImageValue - ( *aiter ).MovingImageValue )
        / m_MovingImageStandardDeviation;
      valueMoving = m_KernelFunction->Evaluate(valueMoving);

      weightMoving = valueMoving / dDenominatorMoving.GetSum();
      weightJoint = valueMoving * valueFixed / dDenominatorJoint.GetSum();

      weight = ( weightMoving - weightJoint );
      weight *= ( *biter ).MovingImageValue - ( *aiter ).MovingImageValue;

      totalWeight += weight;
      derivative -= ( *aditer ) * weight;
      } // end of sample A loop

    derivative += derivB * totalWeight.GetSum();
    } // end of sample B loop

  double nsamp    = double(m_NumberOfSpatialSamples);

  double threshold = -0.5 *nsamp *std::log(m_MinProbability);
  if( dLogSumMoving.GetSum() > threshold || dLogSumFixed.GetSum() > threshold
      || dLogSumJoint.GetSum() > threshold  )
    {
    // at least half the samples in B did not occur within
    // the Parzen window width of samples in A
    itkExceptionMacro(<< "Standard deviation is too small");
    }

  value  = dLogSumFixed.GetSum() + dLogSumMoving.GetSum() - dLogSumJoint.GetSum();
  value /= nsamp;
  value += std::log(nsamp);

  derivative /= nsamp;
  derivative /= itk::Math::sqr(m_MovingImageStandardDeviation);
}

/*
 * Get the match measure derivative
 */
template <typename TFixedImage, typename TMovingImage>
void
MutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const
{
  MeasureType value;

  // call the combined version
  this->GetValueAndDerivative(parameters, value, derivative);
}

/*
 * Calculate derivatives of the image intensity with respect
 * to the transform parmeters.
 *
 * This should really be done by the mapper.
 *
 * This is a temporary solution until this feature is implemented
 * in the mapper. This solution only works for any transform
 * that support ComputeJacobianWithRespectToParameters()
 */
template <typename TFixedImage, typename TMovingImage>
void
MutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::CalculateDerivatives(
  const FixedImagePointType & point,
  DerivativeType & derivatives,
  TransformJacobianType &jacobian) const
{
  MovingImagePointType mappedPoint = this->m_Transform->TransformPoint(point);

  CovariantVector<double, MovingImageDimension> imageDerivatives;

  if( m_DerivativeCalculator->IsInsideBuffer(mappedPoint) )
    {
    imageDerivatives = m_DerivativeCalculator->Evaluate(mappedPoint);
    }
  else
    {
    derivatives.Fill(0.0);
    return;
    }

  this->m_Transform->ComputeJacobianWithRespectToParameters(point, jacobian);

  unsigned int numberOfParameters = this->m_Transform->GetNumberOfParameters();
  for( unsigned int k = 0; k < numberOfParameters; k++ )
    {
    derivatives[k] = 0.0;
    for( unsigned int j = 0; j < MovingImageDimension; j++ )
      {
      derivatives[k] += jacobian[j][k] * imageDerivatives[j];
      }
    }
}

} // end namespace itk

#endif
