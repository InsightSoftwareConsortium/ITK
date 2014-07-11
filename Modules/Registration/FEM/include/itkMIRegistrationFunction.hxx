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
#ifndef __itkMIRegistrationFunction_hxx
#define __itkMIRegistrationFunction_hxx

#include "itkMIRegistrationFunction.h"
#include "itkImageRandomIteratorWithIndex.h"
#include "itkMacro.h"
#include "vnl/vnl_math.h"
#include "itkNeighborhoodIterator.h"

#include "vnl/vnl_matrix.h"

namespace itk
{
/**
 * Default constructor
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
MIRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::MIRegistrationFunction()
{
  RadiusType   r;
  unsigned int j;

  m_NumberOfSamples = 1;
  m_NumberOfBins = 4;

  for ( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 2;
    m_NumberOfSamples *= ( r[j] * 2 + 1 );
    }
  this->SetRadius(r);

  m_MetricTotal = 0.0;

  m_TimeStep = 1.0;
  m_Minnorm = 1.0;
  m_DenominatorThreshold = 1e-9;
  m_IntensityDifferenceThreshold = 0.001;
  this->SetMovingImage(ITK_NULLPTR);
  this->SetFixedImage(ITK_NULLPTR);
  m_FixedImageGradientCalculator = GradientCalculatorType::New();

  m_DoInverse = true;
  m_DoInverse = false;

  if ( m_DoInverse )
    {
    m_MovingImageGradientCalculator = GradientCalculatorType::New();
    }
  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_MovingImageInterpolator = static_cast< InterpolatorType * >(
    interp.GetPointer() );
}

/*
 * Standard "PrintSelf" method.
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
MIRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
/*
  os << indent << "MovingImageIterpolator: ";
  os << m_MovingImageInterpolator.GetPointer() << std::endl;
  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "DenominatorThreshold: ";
  os << m_DenominatorThreshold << std::endl;
  os << indent << "IntensityDifferenceThreshold: ";
  os << m_IntensityDifferenceThreshold << std::endl;
*/
}

/*
 * Set the function state values before each iteration
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
void
MIRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::InitializeIteration()
{
  if ( !this->m_MovingImage || !this->m_FixedImage || !m_MovingImageInterpolator )
    {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or Interpolator not set");
    }

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage(this->m_FixedImage);

  if ( m_DoInverse )
    {
    // setup gradient calculator
    m_MovingImageGradientCalculator->SetInputImage(this->m_MovingImage);
    }
  // setup moving image interpolator
  m_MovingImageInterpolator->SetInputImage(this->m_MovingImage);

  m_MetricTotal = 0.0;
}

/**
 * Compute update at a non boundary neighbourhood
 */
template< typename TFixedImage, typename TMovingImage, typename TDisplacementField >
typename MIRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::PixelType
MIRegistrationFunction< TFixedImage, TMovingImage, TDisplacementField >
::ComputeUpdate( const NeighborhoodType & it, void *itkNotUsed(globalData),
                 const FloatOffsetType & itkNotUsed(offset) )
{
  // we compute the derivative of MI w.r.t. the infinitesimal
  // displacement, following viola and wells.

  // 1)  collect samples from  M (Moving) and F (Fixed)
  // 2)  compute minimum and maximum values of M and F
  // 3)  discretized M and F into N bins
  // 4)  estimate joint probability P(M,F) and P(F)
  // 5)  derivatives is given as :
  //
  //  $$ \nabla MI = \frac{1}{N} \sum_i \sum_j (F_i-F_j)
  //    ( W(F_i,F_j) \frac{1}{\sigma_v} -
  //                W((F_i,M_i),(F_j,M_j)) \frac{1}{\sigma_vv} ) \nabla F
  //
  // NOTE : must estimate sigma for each pdf

  typedef std::vector< double >              sampleContainerType;
  typedef std::vector< CovariantVectorType > gradContainerType;
  typedef std::vector< double >              gradMagContainerType;
  typedef std::vector< unsigned int >        inImageIndexContainerType;

  PixelType    update;
  PixelType    derivative;
  unsigned int j;

  const IndexType oindex = it.GetIndex();

  unsigned int indct;

  for ( indct = 0; indct < ImageDimension; indct++ )
    {
    update[indct] = 0.0;
    derivative[indct] = 0.0;
    }

  float thresh2 = 1.0 / 255.; //  FIX ME : FOR PET LUNG ONLY !!
  float thresh1 = 1.0 / 255.;
  if ( this->m_MovingImage->GetPixel(oindex) <= thresh1
       && this->m_FixedImage->GetPixel(oindex) <= thresh2 ) { return update; }

  typename FixedImageType::SizeType hradius = this->GetRadius();

  FixedImageType *img = const_cast< FixedImageType * >( this->m_FixedImage.GetPointer() );
  typename FixedImageType::SizeType imagesize = img->GetLargestPossibleRegion().GetSize();

  bool inimage;

// now collect the samples
  sampleContainerType       fixedSamplesA;
  sampleContainerType       movingSamplesA;
  sampleContainerType       fixedSamplesB;
  sampleContainerType       movingSamplesB;
  inImageIndexContainerType inImageIndicesA;
  gradContainerType         fixedGradientsA;
  gradMagContainerType      fixedGradMagsA;
  inImageIndexContainerType inImageIndicesB;
  gradContainerType         fixedGradientsB;
  gradMagContainerType      fixedGradMagsB;

  unsigned int samplestep = 2; //m_Radius[0];

  double minf = 1.e9, minm = 1.e9, maxf = 0.0, maxm = 0.0;
  double movingMean = 0.0;
  double fixedMean = 0.0;
  double fixedValue = 0, movingValue = 0;

  unsigned int sampct = 0;

  ConstNeighborhoodIterator< DisplacementFieldType >
  asamIt( hradius,
          this->GetDisplacementField(),
          this->GetDisplacementField()->GetRequestedRegion() );
  asamIt.SetLocation(oindex);
  unsigned int hoodlen = asamIt.Size();

// first get the density-related sample
  for ( indct = 0; indct < hoodlen; indct = indct + samplestep )
    {
    IndexType index = asamIt.GetIndex(indct);
    inimage = true;
    for ( unsigned int dd = 0; dd < ImageDimension; dd++ )
      {
      if ( index[dd] < 0 || index[dd] >
           static_cast< typename IndexType::IndexValueType >( imagesize[dd] - 1 ) ) { inimage = false; }
      }
    if ( inimage )
      {
      fixedValue = 0.;
      movingValue = 0.0;
      CovariantVectorType fixedGradient;

      // Get fixed image related information
      fixedValue = (double)this->m_FixedImage->GetPixel(index);

      fixedGradient = m_FixedImageGradientCalculator->EvaluateAtIndex(index);

      // Get moving image related information
      typedef typename DisplacementFieldType::PixelType DeformationPixelType;
      const DeformationPixelType itvec = this->GetDisplacementField()->GetPixel(index);

      PointType mappedPoint;
      this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedPoint);
      for ( j = 0; j < ImageDimension; j++ )
        {
        mappedPoint[j] += itvec[j];
        }
      if ( m_MovingImageInterpolator->IsInsideBuffer(mappedPoint) )
        {
        movingValue = m_MovingImageInterpolator->Evaluate(mappedPoint);
        }
      else
        {
        movingValue = 0.0;
        }

      if ( fixedValue > maxf ) { maxf = fixedValue; }
      else if ( fixedValue < minf )
        {
        minf = fixedValue;
        }
      if ( movingValue > maxm ) { maxm = movingValue; }
      else if ( movingValue < minm )
        {
        minm = movingValue;
        }

      fixedMean += fixedValue;
      movingMean += movingValue;

      fixedSamplesA.insert(fixedSamplesA.begin(), (double)fixedValue);
      fixedGradientsA.insert(fixedGradientsA.begin(), fixedGradient);
      movingSamplesA.insert(movingSamplesA.begin(), (double)movingValue);

//        fixedSamplesB.insert(fixedSamplesB.begin(),(double)fixedValue);
//        fixedGradientsB.insert(fixedGradientsB.begin(),fixedGradient);
//        movingSamplesB.insert(movingSamplesB.begin(),(double)movingValue);

      sampct++;
      }
    }

// BEGIN RANDOM A SAMPLES
  bool getrandasamples = true;
  if ( getrandasamples )
    {
    typename FixedImageType::RegionType region = img->GetLargestPossibleRegion();

    ImageRandomIteratorWithIndex< FixedImageType > randasamit(img, region);
    unsigned int                                   numberOfSamples = 20;
    randasamit.SetNumberOfSamples(numberOfSamples);
//  numberOfSamples=100;

    indct = 0;

    randasamit.GoToBegin();
    while ( !randasamit.IsAtEnd() &&  indct < numberOfSamples )
      {
      IndexType index = randasamit.GetIndex();
      inimage = true;

      float d = 0.0;
      for ( unsigned int dd = 0; dd < ImageDimension; dd++ )
        {
        if ( index[dd] < 0 || index[dd] >
             static_cast< typename IndexType::IndexValueType >( imagesize[dd] - 1 ) ) { inimage = false; }
        d += ( index[dd] - oindex[dd] ) * ( index[dd] - oindex[dd] );
        }

      if ( inimage )
        {
        fixedValue = 0.;
        movingValue = 0.0;
        CovariantVectorType fixedGradient;
        double              fgm = 0;
        // Get fixed image related information
        fixedValue = (double)this->m_FixedImage->GetPixel(index);
        fixedGradient = m_FixedImageGradientCalculator->EvaluateAtIndex(index);

        for ( j = 0; j < ImageDimension; j++ )
          {
          fgm += fixedGradient[j] * fixedGradient[j];
          }
        // Get moving image related information
        typedef typename DisplacementFieldType::PixelType DeformationPixelType;
        const DeformationPixelType itvec = this->GetDisplacementField()->GetPixel(index);
        PointType                  mappedPoint;
        this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedPoint);
        for ( j = 0; j < ImageDimension; j++ )
          {
          mappedPoint[j] += itvec[j];
          }
        if ( m_MovingImageInterpolator->IsInsideBuffer(mappedPoint) )
          {
          movingValue = m_MovingImageInterpolator->Evaluate(mappedPoint);
          }
        else
          {
          movingValue = 0.0;
          }

        //      if ( (fixedValue > 0 || movingValue > 0 || fgm > 0) ||
        // !filtersamples)

        if ( fixedValue > 0 || movingValue > 0 || fgm > 0 )
          {
          fixedMean += fixedValue;
          movingMean += movingValue;

          fixedSamplesA.insert(fixedSamplesA.begin(), (double)fixedValue);
          fixedGradientsA.insert(fixedGradientsA.begin(), fixedGradient);
          movingSamplesA.insert(movingSamplesA.begin(), (double)movingValue);
          sampct++;
          indct++;
          }
        }
      ++randasamit;
      }
    }
  // END RANDOM A SAMPLES

  const DisplacementFieldType *const field = this->GetDisplacementField();

  for ( j = 0; j < ImageDimension; j++ )
    {
    hradius[j] = 0;
    }
  ConstNeighborhoodIterator< DisplacementFieldType >
  hoodIt( hradius, field, field->GetRequestedRegion() );
  hoodIt.SetLocation(oindex);

  // then get the entropy ( and MI derivative ) related sample
  for ( indct = 0; indct < hoodIt.Size(); indct = indct + 1 )
    {
    const IndexType index = hoodIt.GetIndex(indct);
    inimage = true;
    float d = 0.0;
    for ( unsigned int dd = 0; dd < ImageDimension; dd++ )
      {
      if ( index[dd] < 0 || index[dd] >
           static_cast< typename IndexType::IndexValueType >( imagesize[dd] - 1 ) ) { inimage = false; }
      d += ( index[dd] - oindex[dd] ) * ( index[dd] - oindex[dd] );
      }
    if ( inimage  && std::sqrt(d) <= 1.0 )
      {
      fixedValue = 0.;
      movingValue = 0.0;
      CovariantVectorType fixedGradient;

      // Get fixed image related information
      fixedValue = (double)this->m_FixedImage->GetPixel(index);
      fixedGradient = m_FixedImageGradientCalculator->EvaluateAtIndex(index);

      // Get moving image related information
      // Get moving image related information
      const typename DisplacementFieldType::PixelType hooditvec = this->m_DisplacementField->GetPixel(index);
      PointType mappedPoint;
      this->GetFixedImage()->TransformIndexToPhysicalPoint(index, mappedPoint);
      for ( j = 0; j < ImageDimension; j++ )
        {
        mappedPoint[j] += hooditvec[j];
        }
      if ( m_MovingImageInterpolator->IsInsideBuffer(mappedPoint) )
        {
        movingValue = m_MovingImageInterpolator->Evaluate(mappedPoint);
        }
      else
        {
        movingValue = 0.0;
        }

      fixedSamplesB.insert(fixedSamplesB.begin(), (double)fixedValue);
      fixedGradientsB.insert(fixedGradientsB.begin(), fixedGradient);
      movingSamplesB.insert(movingSamplesB.begin(), (double)movingValue);
      }
    }

  double fsigma = 0.0;
  double msigma = 0.0;
  double jointsigma = 0.0;

  const double numsamplesB = (double)fixedSamplesB.size();
  const double numsamplesA = (double)fixedSamplesA.size();
  double       nsamp = numsamplesB;
//  if (maxf == minf && maxm == minm) return update;
//    else std::cout << " b samps " << fixedSamplesB.size()
//    << " a samps " <<  fixedSamplesA.size() <<
//    oindex  << hoodIt.Size() << it.Size() << std::endl;

  fixedMean /= (double)sampct;
  movingMean /= (double)sampct;

  bool mattes = false;

  for ( indct = 0; indct < (unsigned int)numsamplesA; indct++ )
    {
// Get fixed image related information
    fixedValue = fixedSamplesA[indct];
    movingValue = movingSamplesA[indct];

    fsigma += ( fixedValue - fixedMean ) * ( fixedValue - fixedMean );
    msigma += ( movingValue - movingMean ) * ( movingValue - movingMean );
    jointsigma += fsigma + msigma;

    if ( mattes )
      {
      fixedSamplesA[indct] = fixedSamplesA[indct] - minf;
      movingSamplesA[indct] = movingSamplesA[indct] - minm;
      if ( indct < numsamplesB )
        {
        fixedSamplesB[indct] = fixedSamplesB[indct] - minf;
        movingSamplesB[indct] = movingSamplesB[indct] - minm;
        }
      }
    }

  fsigma = std::sqrt(fsigma / numsamplesA);
  float  sigmaw = 0.8;
  double m_FixedImageStandardDeviation = fsigma * sigmaw;
  msigma = std::sqrt(msigma / numsamplesA);
  double m_MovingImageStandardDeviation = msigma * sigmaw;
  jointsigma = std::sqrt(jointsigma / numsamplesA);

  if ( fsigma < 1.e-7 || msigma < 1.e-7 ) { return update; }

  double       m_MinProbability = 0.0001;
  double       dLogSumFixed = 0., dLogSumMoving = 0., dLogSumJoint = 0.0;
  unsigned int bsamples;
  unsigned int asamples;

  // the B samples estimate the entropy
  for ( bsamples = 0; bsamples < (unsigned int)numsamplesB; bsamples++ )
    {
    double dDenominatorMoving = m_MinProbability;
    double dDenominatorJoint = m_MinProbability;
    double dDenominatorFixed = m_MinProbability;
    double dSumFixed = m_MinProbability;

    // this loop estimates the density
    for ( asamples = 0; asamples < (unsigned int)numsamplesA; asamples++ )
      {
      double valueFixed = ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] )
                          / m_FixedImageStandardDeviation;
      valueFixed = std::exp(-0.5 * valueFixed * valueFixed);

      double valueMoving = ( movingSamplesB[bsamples] - movingSamplesA[asamples] )
                           / m_MovingImageStandardDeviation;
      valueMoving = std::exp(-0.5 * valueMoving * valueMoving);

      dDenominatorMoving += valueMoving;
      dDenominatorFixed += valueFixed;
      dSumFixed += valueFixed;

// everything above here can be pre-computed only once and stored,
//  assuming const v.f. in small n-hood
      dDenominatorJoint += valueMoving * valueFixed;
      } // end of sample A loop

    dLogSumFixed -= std::log(dSumFixed);
    dLogSumMoving -= std::log(dDenominatorMoving);
    dLogSumJoint -= std::log(dDenominatorJoint);

    // this loop estimates the density
    for ( asamples = 0; asamples < (unsigned int)numsamplesA; asamples++ )
      {
      double valueFixed = ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] )
                          / m_FixedImageStandardDeviation;
      valueFixed = std::exp(-0.5 * valueFixed * valueFixed);

      double valueMoving = ( movingSamplesB[bsamples] - movingSamplesA[asamples] )
                           / m_MovingImageStandardDeviation;
      valueMoving = std::exp(-0.5 * valueMoving * valueMoving);
      const double weightFixed = valueFixed / dDenominatorFixed;
// dDenominatorJoint and weightJoint are what need to be computed each time
      const double weightJoint = valueMoving * valueFixed / dDenominatorJoint;

// begin where we may switch fixed and moving
      double weight = ( weightFixed - weightJoint );
      weight *= ( fixedSamplesB[bsamples] - fixedSamplesA[asamples] );
// end where we may switch fixed and moving

// this can also be stored away
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        derivative[i] += ( fixedGradientsB[bsamples][i] - fixedGradientsA[asamples][i] ) * weight;
        }
      } // end of sample A loop
    }   // end of sample B loop

  const double threshold = -0.1 *nsamp *std::log(m_MinProbability);
  if ( dLogSumMoving > threshold || dLogSumFixed > threshold
       || dLogSumJoint > threshold  )
    {
    // at least half the samples in B did not occur within
    // the Parzen window width of samples in A
    return update;
    }

  double value = 0.0;
  value  = dLogSumFixed + dLogSumMoving - dLogSumJoint;
  value /= nsamp;
  value += std::log(nsamp);

  m_MetricTotal += value;
  this->m_Energy += value;

  derivative /= nsamp;
  derivative /= vnl_math_sqr(m_FixedImageStandardDeviation);

  double updatenorm = 0.0;
  for ( unsigned int tt = 0; tt < ImageDimension; tt++ )
    {
    updatenorm += derivative[tt] * derivative[tt];
    }
  updatenorm = std::sqrt(updatenorm);

  if ( updatenorm > 1.e-20 && this->GetNormalizeGradient() )
    {
    derivative = derivative / updatenorm;
    }

  return derivative * this->GetGradientStep();
}
} // end namespace itk

#endif
