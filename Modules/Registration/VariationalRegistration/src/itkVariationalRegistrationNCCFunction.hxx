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
#ifndef __itkVariationalRegistrationNCCFunction_txx
#define __itkVariationalRegistrationNCCFunction_txx

#include "itkVariationalRegistrationNCCFunction.h"
#include "itkExceptionObject.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 * Default constructor
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::VariationalRegistrationNCCFunction()
{
  std::cout << "\n NCCRegistrationFunction::NCCRegistrationFunction()" << std::flush;

  RadiusType r;
  // set default radius to : 2
  for (unsigned int dim = 0; dim < ImageDimension; dim++)
  {
    r[dim] = 2;
  }
  this->SetRadius(r);

  m_Normalizer = 1.0;

  m_FixedImageGradientCalculator = GradientCalculatorType::New();
  m_WarpedImageGradientCalculator = GradientCalculatorType::New();

  // TODO: set appropriate default values
  m_GradientType = GRADIENT_TYPE_FIXED;

  // default: normalize time step with image spacing
  m_ScaleGlobalTimeStep = true;
}

/*
 * Standard "PrintSelf" method.
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
void
VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::PrintSelf(std::ostream & os,
                                                                                             Indent indent) const
{

  Superclass::PrintSelf(os, indent);

  os << indent << "Normalizer: ";
  os << m_Normalizer << std::endl;
  os << indent << "ScaleGlobalTimeStep: ";
  os << m_ScaleGlobalTimeStep << std::endl;
  os << indent << "GradientType: ";
  os << m_GradientType << std::endl;
  os << indent << "FixedImageGradientCalculator: ";
  os << m_FixedImageGradientCalculator.GetPointer() << std::endl;
  os << indent << "WarpedImageGradientCalculator: ";
  os << m_WarpedImageGradientCalculator.GetPointer() << std::endl;
}

/** This class uses the constant time step m_TimeStep. */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
typename VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::Superclass::TimeStepType
VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::ComputeGlobalTimeStep(
  void * gd) const
{
  std::cout << "\n NCCRegistrationFunction::ComputeGlobalTimeStep() = " << this->GetTimeStep()
            << " Norm: " << this->GetTimeStep() * m_Normalizer << std::flush;
  if (m_ScaleGlobalTimeStep)
  {
    return this->GetTimeStep() * m_Normalizer;
  }

  return this->GetTimeStep();
}

/*
 * Set the function state values before each iteration
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
void
VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::InitializeIteration()
{
  std::cout << "\n NCCRegistrationFunction::InitializeIteration()" << std::flush;

  Superclass::InitializeIteration();
  if (!this->GetMovingImage() || !this->GetFixedImage()) //|| !m_MovingImageInterpolator )
  {
    itkExceptionMacro(<< "MovingImage, FixedImage and/or Interpolator not set");
  }

  // cache fixed image information
  SpacingType fixedImageSpacing = this->GetFixedImage()->GetSpacing();

  // compute the normalizer
  m_Normalizer = 0.0;
  for (unsigned int k = 0; k < ImageDimension; k++)
  {
    m_Normalizer += fixedImageSpacing[k] * fixedImageSpacing[k];
  }
  m_Normalizer /= static_cast<double>(ImageDimension);

  // setup gradient calculator
  m_FixedImageGradientCalculator->SetInputImage(this->GetFixedImage());
  m_WarpedImageGradientCalculator->SetInputImage(this->GetWarpedImage());
}

/*
 * Compute update at a non boundary neighbourhood
 */
template <class TFixedImage, class TMovingImage, class TDisplacementField>
typename VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::PixelType
VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::ComputeUpdate(
  const NeighborhoodType & it,
  void *                   gd,
  const FloatOffsetType &  itkNotUsed(offset))
{
  // initialize update value to compute with zero
  PixelType update;
  update.Fill(0.0);

  // Get the index at current location
  const IndexType index = it.GetIndex();

  // Check if index lies inside mask
  const MaskImageType * mask = this->GetMaskImage();
  if (mask)
    if (mask->GetPixel(index) <= this->GetMaskBackgroundThreshold())
    {
      return update;
    }

  // We compute the CC on the fixed and warped moving image
  // VariationalRegistrationFunction::WarpMovingImage() has to be called before
  FixedImagePointer fixedImage = this->GetFixedImage();
  FixedImagePointer warpedImage = this->GetWarpedImage();

  // check if both are valid
  if (!fixedImage || !warpedImage)
  {
    itkExceptionMacro(
      << "Fixed image or warped moving image not present for VariationalRegistrationNCCFunction::ComputeUpdate()");
  }

  //
  // Get fixed image information and neighborhood radius
  //
  const RadiusType                        radius = it.GetRadius(); // should be same as this->GetRadius()
  const typename FixedImageType::SizeType fixedImageSize = fixedImage->GetLargestPossibleRegion().GetSize();

  //
  // Setup an NeighborhoodIterator to compute sums and mean in local neighborhood
  // of current position index
  ConstNeighborhoodIterator<FixedImageType> hoodIt(radius, fixedImage, fixedImage->GetRequestedRegion());
  hoodIt.SetLocation(index);

  // Iterate of current neighborhood to compute the following values:
  // mean of fixed (f) and warped moving (m) image
  // Sum f*f, Sum m*m, Sum m*f
  double             sf = 0.0;
  double             sm = 0.0;
  double             sff = 0.0;
  double             smm = 0.0;
  double             sfm = 0.0;
  unsigned int       pixelCounter = 0;
  const unsigned int hoodSize = hoodIt.Size();
  for (unsigned int indct = 0; indct < hoodSize; indct++)
  {
    const IndexType neighIndex = hoodIt.GetIndex(indct);
    if (fixedImage->GetLargestPossibleRegion().IsInside(neighIndex))
    {
      const double fixedNeighValue = static_cast<double>(fixedImage->GetPixel(neighIndex));
      const double movingNeighValue = static_cast<double>(warpedImage->GetPixel(neighIndex));

      sf += fixedNeighValue;
      sm += movingNeighValue;
      sff += fixedNeighValue * fixedNeighValue;
      smm += movingNeighValue * movingNeighValue;
      sfm += fixedNeighValue * movingNeighValue;
      pixelCounter++;
    }
  }
  const double fixedMean = sf / static_cast<double>(pixelCounter);
  const double movingMean = sm / static_cast<double>(pixelCounter);

  // Compute Sum_i (f-meanF)^2 , Sum_i (m-meanM)^2, Sum_i (f-meanF)(m-meanM)
  // Sum_i (f-meanF)^2 = Sum_i f*f - 2* meanF*Sum_i f + Sum_i meanF*meanF
  // Sum_i (f-meanF)(m-meanM) = Sum_i f*m - meanF*Sum_i m - meanM*Sum_i f + Sum_i meanF*meanM
  const double SumFF = sff - 2 * fixedMean * sf + pixelCounter * fixedMean * fixedMean;
  const double SumMM = smm - 2 * movingMean * sm + pixelCounter * movingMean * movingMean;
  const double SumFM = sfm - fixedMean * sm - movingMean * sf + pixelCounter * movingMean * fixedMean;

  // Compute cross correlation and derivative only for non-homogeneous regions
  // cross correlation, if one region is homogeneous is here defined as 1
  double localCrossCorrelation = 1.0;

  // check for homogeneous region
  const double SumFFMultSumMM = SumFF * SumMM;
  // if(SumFF != 0.0 && SumMM != 0.0)
  if (SumFFMultSumMM > 1.e-5)
  {
    // compute local cross correlation
    localCrossCorrelation = SumFM * SumFM / SumFFMultSumMM;

    // Get grayvalues for fixed and warped images
    const double warpedValue = static_cast<double>(warpedImage->GetPixel(index));
    const double fixedValue = static_cast<double>(fixedImage->GetPixel(index));

    const double centerWarpedValue = warpedValue - movingMean;
    const double centerFixedValue = fixedValue - fixedMean;

    typename GradientCalculatorType::OutputType gradient;

    // Compute the gradient of either fixed or warped moving image or as the mean of both (symmetric)
    if (m_GradientType == GRADIENT_TYPE_WARPED)
    {
      gradient = m_WarpedImageGradientCalculator->EvaluateAtIndex(index);
    }
    else if (m_GradientType == GRADIENT_TYPE_FIXED)
    {
      gradient = m_FixedImageGradientCalculator->EvaluateAtIndex(index);
    }
    else if (m_GradientType == GRADIENT_TYPE_SYMMETRIC)
    {
      gradient = 0.5 * (m_WarpedImageGradientCalculator->EvaluateAtIndex(index) +
                        m_FixedImageGradientCalculator->EvaluateAtIndex(index));
    }
    else
    {
      itkExceptionMacro(<< "Unknown gradient type!");
    }

    // compute the gradient magnitude
    // const double gradientMagnitude = gradient.GetSquaredNorm();

    // Compute the derivative of LCC as given in Hermosillo et al., IJCV 50(3), 2002
    // and Avants et al., Med Image Anal 12(1), 2008 (except Jacobian term):
    //
    //       2* Sum_i (f-meanF)(m-meanM)        (             Sum_i (f-meanF)(m-meanM)          )
    //    -------------------------------------*( (m-meanM) - ------------------------*(f-meanF)) * gradient f
    //    Sum_i (f-meanF)^2 * Sum_i (m-meanM)^2 (             Sum_i (f-meanF)^2                 )
    //
    const double preComputeFactor =
      (2.0 * SumFM / SumFFMultSumMM) * (centerWarpedValue - SumFM / SumFF * centerFixedValue);

    for (unsigned int dim = 0; dim < ImageDimension; dim++)
    {
      update[dim] = (-1) * preComputeFactor * gradient[dim];
    }
  }

  // Update the global data (metric etc.)
  GlobalDataStruct * globalData = (GlobalDataStruct *)gd;
  if (globalData)
  {
    globalData->m_NumberOfPixelsProcessed += 1;
    // use 1 - CC to get a decreasing metric value
    globalData->m_SumOfMetricValues += 1.0 - localCrossCorrelation;
    globalData->m_SumOfSquaredChange += update.GetSquaredNorm();
  }

  return update;
}

} // end namespace itk

#endif
