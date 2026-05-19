/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkVariationalRegistrationFastNCCFunction_hxx
#define itkVariationalRegistrationFastNCCFunction_hxx

#include "itkVariationalRegistrationFastNCCFunction.h"
#include "itkMacro.h"
#include "itkMath.h"

namespace itk
{

/**
 * Default constructor
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
VariationalRegistrationFastNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::
  VariationalRegistrationFastNCCFunction() = default;

/*
 * Standard "PrintSelf" method.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFastNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::PrintSelf(std::ostream & os,
                                                                                                 Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/*
 * Compute update at a non boundary neighbourhood
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
typename VariationalRegistrationFastNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::PixelType
VariationalRegistrationFastNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::ComputeUpdate(
  const NeighborhoodType & it,
  void *                   gd,
  const FloatOffsetType &  itkNotUsed(offset))
{
  auto * globalData = (NCCGlobalDataStruct *)gd;
  assert(globalData != NULL);

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
      globalData->bValuesAreValid = false;
      return update;
    }

  // We assume that NeighborhoodAlgorithm::ImageBoundaryFacesCalculator was used
  // to separate boundary regions from inside region
  // We accelerate the algorithm in inside regions by assuming that each thread
  // process the image line-by-line (dimension 0).
  // Inside a line we can reuse the value from the previous pixel.

  // flag whether boundary conditions have to be processed here
  const bool bNeedToUseBoundaryCondition = it.GetNeedToUseBoundaryCondition();

  // flag to show if we can speed up computation
  bool bProcessFollowingPixel = false;

  // never speed up in boundary regions
  if (!bNeedToUseBoundaryCondition)
  {
    //
    // Check if we have processed the previous pixel in the last call
    //
    if (index[0] == (globalData->m_LastIndex[0] + 1))
    {
      bProcessFollowingPixel = true;
      for (unsigned int d = 1; d < FixedImageType::GetImageDimension(); d++)
        if (index[d] != (globalData->m_LastIndex[d]))
          bProcessFollowingPixel = false;
    }
  }
  // save the currently processed pixel index
  globalData->m_LastIndex = index;

  // We compute the CC on the fixed and warped moving image
  // VariationalRegistrationFunction::WarpMovingImage() has to be called before
  FixedImagePointer fixedImage = this->GetFixedImage();
  FixedImagePointer warpedImage = this->GetWarpedImage();

  //
  // Get fixed image information and neighborhood radius
  //

  // Compute sums and mean in local neighborhood
  // of current position index
  // Iterate in current neighborhood to compute the following values:
  // mean of fixed (f) and warped moving (m) image
  // Sum f*f, Sum m*m, Sum m*f
  double             sf = 0.0;
  double             sm = 0.0;
  double             sff = 0.0;
  double             smm = 0.0;
  double             sfm = 0.0;
  unsigned int       pixelCounter = 0;
  const unsigned int hoodSize = it.Size();
  if (!bNeedToUseBoundaryCondition)
  {
    if (!bProcessFollowingPixel || !globalData->bValuesAreValid)
    {
      //
      // Start a new line
      //

      const unsigned int numNeighborhoodSlices = it.GetSize(0);
      const unsigned int neighborhoodSliceSize = it.Size() / numNeighborhoodSlices;
      const unsigned int neighborhoodSliceStride = it.GetSize(0);

      // go over all slices
      for (unsigned int slice = 0; slice < numNeighborhoodSlices; slice++)
      {
        // initialize values for this slice
        double sfTemp = 0.0;
        double smTemp = 0.0;
        double sffTemp = 0.0;
        double smmTemp = 0.0;
        double sfmTemp = 0.0;

        // go over all pixels in this slice
        unsigned int indct = slice;
        for (unsigned int i = 0; i < neighborhoodSliceSize; i++)
        {
          const IndexType neighIndex = it.GetIndex(indct);
          const auto      fixedNeighValue = static_cast<double>(fixedImage->GetPixel(neighIndex));
          const auto      movingNeighValue = static_cast<double>(warpedImage->GetPixel(neighIndex));

          sfTemp += fixedNeighValue;
          smTemp += movingNeighValue;
          sffTemp += fixedNeighValue * fixedNeighValue;
          smmTemp += movingNeighValue * movingNeighValue;
          sfmTemp += fixedNeighValue * movingNeighValue;

          // increment neighbour index by stride in direction 0
          indct += neighborhoodSliceStride;
          pixelCounter++;
        }
        // store the values for this slice in the lists
        globalData->sfSliceValueList[slice] = sfTemp;
        globalData->smSliceValueList[slice] = smTemp;
        globalData->sffSliceValueList[slice] = sffTemp;
        globalData->smmSliceValueList[slice] = smmTemp;
        globalData->sfmSliceValueList[slice] = sfmTemp;
        // add slice-values to the sum
        sf += sfTemp;
        sm += smTemp;
        sff += sffTemp;
        smm += smmTemp;
        sfm += sfmTemp;
      } // for slice=0 ..
      globalData->sfLastValue = sf;
      globalData->smLastValue = sm;
      globalData->sffLastValue = sff;
      globalData->smmLastValue = smm;
      globalData->sfmLastValue = sfm;
      globalData->lastSliceIndex = 0;
      globalData->bValuesAreValid = true;
    }
    else
    {
      //
      // Continue the current line
      //
      // Inside a line we only have to modify the sum values of the last pixel.
      // We have to subtract the values of slice leaving the neighborhood and we have
      // to add the values of the slice entering the neighborhood

      // subtract neighborhood slice going out of the neighborhood region
      const unsigned int lastSliceIndex = globalData->lastSliceIndex;
      sf = globalData->sfLastValue - globalData->sfSliceValueList[lastSliceIndex];
      sm = globalData->smLastValue - globalData->smSliceValueList[lastSliceIndex];
      sff = globalData->sffLastValue - globalData->sffSliceValueList[lastSliceIndex];
      smm = globalData->smmLastValue - globalData->smmSliceValueList[lastSliceIndex];
      sfm = globalData->sfmLastValue - globalData->sfmSliceValueList[lastSliceIndex];

      const unsigned int numNeighborhoodSlices = it.GetSize(0);
      const unsigned int neighborhoodSliceSize = it.Size() / numNeighborhoodSlices;
      const unsigned int neighborhoodSliceStride = it.GetSize(0);

      // initialize values for this slice
      double sfTemp = 0.0;
      double smTemp = 0.0;
      double sffTemp = 0.0;
      double smmTemp = 0.0;
      double sfmTemp = 0.0;

      // go over all pixels in the last slice of the neighborhood
      unsigned int indct = numNeighborhoodSlices - 1;
      for (unsigned int i = 0; i < neighborhoodSliceSize; i++)
      {
        const IndexType neighIndex = it.GetIndex(indct);
        const auto      fixedNeighValue = static_cast<double>(fixedImage->GetPixel(neighIndex));
        const auto      movingNeighValue = static_cast<double>(warpedImage->GetPixel(neighIndex));

        sfTemp += fixedNeighValue;
        smTemp += movingNeighValue;
        sffTemp += fixedNeighValue * fixedNeighValue;
        smmTemp += movingNeighValue * movingNeighValue;
        sfmTemp += fixedNeighValue * movingNeighValue;

        // increment neighbour index by stride in direction 0
        indct += neighborhoodSliceStride;
      }
      // add slice-values of the last slice to the sum
      sf += sfTemp;
      sm += smTemp;
      sff += sffTemp;
      smm += smmTemp;
      sfm += sfmTemp;
      // store the values for the last slice in the lists
      globalData->sfSliceValueList[lastSliceIndex] = sfTemp;
      globalData->smSliceValueList[lastSliceIndex] = smTemp;
      globalData->sffSliceValueList[lastSliceIndex] = sffTemp;
      globalData->smmSliceValueList[lastSliceIndex] = smmTemp;
      globalData->sfmSliceValueList[lastSliceIndex] = sfmTemp;
      globalData->lastSliceIndex = (lastSliceIndex + 1) % numNeighborhoodSlices;
      globalData->sfLastValue = sf;
      globalData->smLastValue = sm;
      globalData->sffLastValue = sff;
      globalData->smmLastValue = smm;
      globalData->sfmLastValue = sfm;
      globalData->bValuesAreValid = true;

      pixelCounter = hoodSize;
    }
  }
  else
  {
    //
    // We have to account for boundary conditions
    //

    for (unsigned int indct = 0; indct < hoodSize; indct++)
    {
      const IndexType neighIndex = it.GetIndex(indct);
      if (fixedImage->GetBufferedRegion().IsInside(neighIndex))
      {
        const auto fixedNeighValue = static_cast<double>(fixedImage->GetPixel(neighIndex));
        const auto movingNeighValue = static_cast<double>(warpedImage->GetPixel(neighIndex));

        sf += fixedNeighValue;
        sm += movingNeighValue;
        sff += fixedNeighValue * fixedNeighValue;
        smm += movingNeighValue * movingNeighValue;
        sfm += fixedNeighValue * movingNeighValue;
        pixelCounter++;
      }
    }
    // values in global data struct are not valid if boundary computation occurs
    globalData->bValuesAreValid = false;
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
    const auto warpedValue = static_cast<double>(warpedImage->GetPixel(index));
    const auto fixedValue = static_cast<double>(fixedImage->GetPixel(index));

    const double centerWarpedValue = warpedValue - movingMean;
    const double centerFixedValue = fixedValue - fixedMean;

    typename GradientCalculatorType::OutputType gradient;

    // Compute the gradient of either fixed or warped moving image or as the mean of both (symmetric)
    if (this->m_GradientType ==
        VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::GRADIENT_TYPE_WARPED)
    {
      gradient = this->m_WarpedImageGradientCalculator->EvaluateAtIndex(index);
    }
    else if (this->m_GradientType ==
             VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::GRADIENT_TYPE_FIXED)
    {
      gradient = this->m_FixedImageGradientCalculator->EvaluateAtIndex(index);
    }
    else if (this->m_GradientType ==
             VariationalRegistrationNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::GRADIENT_TYPE_SYMMETRIC)
    {
      gradient = 0.5 * (this->m_WarpedImageGradientCalculator->EvaluateAtIndex(index) +
                        this->m_FixedImageGradientCalculator->EvaluateAtIndex(index));
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
  if (globalData)
  {
    globalData->m_NumberOfPixelsProcessed += 1;
    // use 1 - CC to get a decreasing metric value
    globalData->m_SumOfMetricValues += 1.0 - localCrossCorrelation;
    globalData->m_SumOfSquaredChange += update.GetSquaredNorm();
  }

  return update;
}

/**
 * Returns an empty struct that is used by the threads to include the
 * required update information for each thread.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void *
VariationalRegistrationFastNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::GetGlobalDataPointer() const
{
  auto * globalData = new NCCGlobalDataStruct();

  globalData->m_SumOfMetricValues = 0.0;
  globalData->m_NumberOfPixelsProcessed = 0L;
  globalData->m_SumOfSquaredChange = 0;

  unsigned int numSlices = this->GetRadius()[0] * 2 + 1;
  globalData->sfSliceValueList.resize(numSlices);
  globalData->smSliceValueList.resize(numSlices);
  globalData->sffSliceValueList.resize(numSlices);
  globalData->smmSliceValueList.resize(numSlices);
  globalData->sfmSliceValueList.resize(numSlices);
  globalData->sfLastValue = 0;
  globalData->smLastValue = 0;
  globalData->sffLastValue = 0;
  globalData->smmLastValue = 0;
  globalData->sfmLastValue = 0;
  globalData->bValuesAreValid = false;

  // set the last index to a value outside the image
  SizeType size = this->GetFixedImage()->GetLargestPossibleRegion().GetSize();
  globalData->m_LastIndex = this->GetFixedImage()->GetLargestPossibleRegion().GetIndex();
  for (unsigned int d = 0; d < FixedImageType::GetImageDimension(); d++)
    globalData->m_LastIndex[d] += size[d];

  return globalData;
}

/**
 * Update the metric and release the per-thread-global data.
 */
template <typename TFixedImage, typename TMovingImage, typename TDisplacementField>
void
VariationalRegistrationFastNCCFunction<TFixedImage, TMovingImage, TDisplacementField>::ReleaseGlobalDataPointer(
  void * gd) const
{
  auto * globalData = (NCCGlobalDataStruct *)gd;

  auto * baseRegFunctionGlobalData = new GlobalDataStruct();
  baseRegFunctionGlobalData->m_SumOfMetricValues = globalData->m_SumOfMetricValues;
  baseRegFunctionGlobalData->m_NumberOfPixelsProcessed = globalData->m_NumberOfPixelsProcessed;
  baseRegFunctionGlobalData->m_SumOfSquaredChange = globalData->m_SumOfSquaredChange;

  VariationalRegistrationFunction<TFixedImage, TMovingImage, TDisplacementField>::ReleaseGlobalDataPointer(
    baseRegFunctionGlobalData);

  globalData->sfSliceValueList.clear();
  globalData->smSliceValueList.clear();
  globalData->sffSliceValueList.clear();
  globalData->smmSliceValueList.clear();
  globalData->sfmSliceValueList.clear();

  delete globalData;
}

} // end namespace itk

#endif
