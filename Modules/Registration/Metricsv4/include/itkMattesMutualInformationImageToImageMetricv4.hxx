/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkMattesMutualInformationImageToImageMetricv4_hxx
#define itkMattesMutualInformationImageToImageMetricv4_hxx

#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkCompensatedSummation.h"
#include <mutex>

namespace itk
{

template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::MattesMutualInformationImageToImageMetricv4()
  : m_MovingImageNormalizedMin(0.0)
  , m_FixedImageNormalizedMin(0.0)
  , m_FixedImageTrueMin(0.0)
  , m_FixedImageTrueMax(0.0)
  , m_MovingImageTrueMin(0.0)
  , m_MovingImageTrueMax(0.0)
  , m_FixedImageBinSize(0.0)
  , m_MovingImageBinSize(0.0)
  , m_CubicBSplineKernel(nullptr)
  , m_CubicBSplineDerivativeKernel(nullptr)
  , m_PRatioArray(0)
  ,
  // Initialize memory
  m_MovingImageMarginalPDF(0)
  , m_ThreaderFixedImageMarginalPDF(0)
  ,
  // For multi-threading the metric
  m_ThreaderJointPDF(0)
  , m_JointPDFDerivatives(nullptr)
  , m_JointPDFSum(0.0)
{
  // We have our own GetValueAndDerivativeThreader's that we want
  // ImageToImageMetricv4 to use.
  this->m_DenseGetValueAndDerivativeThreader = MattesMutualInformationDenseGetValueAndDerivativeThreaderType::New();
  this->m_SparseGetValueAndDerivativeThreader = MattesMutualInformationSparseGetValueAndDerivativeThreaderType::New();
  this->m_CubicBSplineKernel = CubicBSplineFunctionType::New();
  this->m_CubicBSplineDerivativeKernel = CubicBSplineDerivativeFunctionType::New();
}

/**
 * Initialize
 */
template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::Initialize()
{
  /* Superclass initialization */
  this->Superclass::Initialize();

  /* Expects moving image gradient source */
  if (this->GetGradientSourceIncludesFixed() || !this->GetGradientSourceIncludesMoving())
  {
    itkExceptionMacro("Expected gradient source to be only Moving. Instead gradient source is: "
                      " Fixed: "
                      << this->GetGradientSourceIncludesFixed()
                      << " Moving: " << this->GetGradientSourceIncludesMoving());
  }

  {
    /**
     * Compute the minimum and maximum within the specified
     * analysis region (specified by a mask or sampled point set)
     * for creating the size of the 2D joint histogram.
     * Areas outside the defined analysis region should be ignored
     * in computing the range of intensity values.
     */

    // Determine the min/max of the fixed image axis of the joint histogram.
    // We need to make robust measures only over the intensity values from requested analysis region
    this->m_FixedImageTrueMin = NumericTraits<typename TFixedImage::PixelType>::max();
    this->m_FixedImageTrueMax = NumericTraits<typename TFixedImage::PixelType>::NonpositiveMin();

    // NOTE: If m_UseSampledPointSet is true, then *ONLY* the sparse sampled points
    //       are used for analysis of the metric, and the fixed space intensity range values
    //       will be fixed to those values identified in the sparse sampled points.
    //       The masked value items are not relevant when the sparse sampling is set.
    if (this->m_UseSampledPointSet) // Analysis region defined by SampledPointSet
    {
      if (this->m_UseVirtualSampledPointSet) // Sparse points defined in VirtualSpace
      {
        typename Superclass::FixedSampledPointSetType::PointsContainerConstIterator pit =
          this->m_VirtualSampledPointSet->GetPoints()->Begin();
        typename Superclass::FixedSampledPointSetType::PointsContainerConstIterator end =
          this->m_VirtualSampledPointSet->GetPoints()->End();

        if (this->m_FixedTransform.IsNull())
        {
          itkExceptionMacro(
            "Unable to get transform for mapping sampled point set from virtual space to fixed image space.");
        }

        while (pit != end)
        {
          const typename TFixedImage::PointType fixPnt = this->m_FixedTransform->TransformPoint(pit.Value());

          typename TFixedImage::IndexType idx;
          const bool                      isInside = this->m_FixedImage->TransformPhysicalPointToIndex(fixPnt, idx);
          if (isInside)
          {
            const typename TFixedImage::PixelType currValue = this->m_FixedImage->GetPixel(idx);
            this->m_FixedImageTrueMin = (m_FixedImageTrueMin < currValue) ? this->m_FixedImageTrueMin : currValue;
            this->m_FixedImageTrueMax = (m_FixedImageTrueMax > currValue) ? this->m_FixedImageTrueMax : currValue;
          }
          ++pit;
        }
      }
      else // Sparse points defined in Fixed image space
      {
        typename Superclass::FixedSampledPointSetType::PointsContainerConstIterator pit =
          this->m_FixedSampledPointSet->GetPoints()->Begin();
        typename Superclass::FixedSampledPointSetType::PointsContainerConstIterator end =
          this->m_FixedSampledPointSet->GetPoints()->End();
        while (pit != end)
        {
          typename TFixedImage::IndexType idx;
          const bool isInside = this->m_FixedImage->TransformPhysicalPointToIndex(pit.Value(), idx);
          if (isInside)
          {
            const typename TFixedImage::PixelType currValue = this->m_FixedImage->GetPixel(idx);
            this->m_FixedImageTrueMin = (m_FixedImageTrueMin < currValue) ? this->m_FixedImageTrueMin : currValue;
            this->m_FixedImageTrueMax = (m_FixedImageTrueMax > currValue) ? this->m_FixedImageTrueMax : currValue;
          }
          ++pit;
        }
      }
    }
    else // Use dense sampling potentially with dense mask to identify intensity range
    {
      // A null mask implies entire space is to be used.
      if (!this->m_FixedImageMask.IsNull()) // use only masked samples.
      {
        itk::ImageRegionConstIteratorWithIndex<TFixedImage> fi(this->m_FixedImage,
                                                               this->m_FixedImage->GetBufferedRegion());
        while (!fi.IsAtEnd())
        {
          typename TFixedImage::PointType fixedSpacePhysicalPoint;
          this->m_FixedImage->TransformIndexToPhysicalPoint(fi.GetIndex(), fixedSpacePhysicalPoint);
          const bool usePoint = this->m_FixedImageMask->IsInsideInWorldSpace(fixedSpacePhysicalPoint);
          if (usePoint)
          {
            const typename TFixedImage::PixelType & currValue = fi.Value();
            this->m_FixedImageTrueMin = (m_FixedImageTrueMin < currValue) ? this->m_FixedImageTrueMin : currValue;
            this->m_FixedImageTrueMax = (m_FixedImageTrueMax > currValue) ? this->m_FixedImageTrueMax : currValue;
          }
          ++fi;
        }
      }
      else // use entire image for fixed image intensity range of joint histogram
      {
        itk::ImageRegionConstIteratorWithIndex<TFixedImage> fi(this->m_FixedImage,
                                                               this->m_FixedImage->GetBufferedRegion());
        while (!fi.IsAtEnd())
        {
          const typename TFixedImage::PixelType & currValue = fi.Value();
          this->m_FixedImageTrueMin = (m_FixedImageTrueMin < currValue) ? this->m_FixedImageTrueMin : currValue;
          this->m_FixedImageTrueMax = (m_FixedImageTrueMax > currValue) ? this->m_FixedImageTrueMax : currValue;
          ++fi;
        }
      }
    }

    // NOTE: The moving image joint histogram range is not affected by sparse
    //      sampling of the fixed image space because the moving image samples
    //      interrogated will change based on updates to the moving image
    //      transform updates.
    // Determine the min/max of the moving image axis of the joint histogram.
    this->m_MovingImageTrueMin = NumericTraits<typename TMovingImage::PixelType>::max();
    this->m_MovingImageTrueMax = NumericTraits<typename TMovingImage::PixelType>::NonpositiveMin();
    {
      itk::ImageRegionConstIteratorWithIndex<TMovingImage> mi(this->m_MovingImage,
                                                              this->m_MovingImage->GetBufferedRegion());

      if (!this->m_MovingImageMask.IsNull())
      { // A null mask implies entire space is to be used.
        while (!mi.IsAtEnd())
        {
          typename TMovingImage::PointType movingSpacePhysicalPoint;
          this->m_MovingImage->TransformIndexToPhysicalPoint(mi.GetIndex(), movingSpacePhysicalPoint);
          const bool usePoint = this->m_MovingImageMask->IsInsideInWorldSpace(movingSpacePhysicalPoint);
          if (usePoint)
          {
            const typename TMovingImage::PixelType & currValue = mi.Value();
            this->m_MovingImageTrueMin = (m_MovingImageTrueMin < currValue) ? this->m_MovingImageTrueMin : currValue;
            this->m_MovingImageTrueMax = (m_MovingImageTrueMax > currValue) ? this->m_MovingImageTrueMax : currValue;
          }
          ++mi;
        }
      }
      else
      {
        while (!mi.IsAtEnd())
        {
          const typename TMovingImage::PixelType & currValue = mi.Value();
          this->m_MovingImageTrueMin = (m_MovingImageTrueMin < currValue) ? this->m_MovingImageTrueMin : currValue;
          this->m_MovingImageTrueMax = (m_MovingImageTrueMax > currValue) ? this->m_MovingImageTrueMax : currValue;
          ++mi;
        }
      }
    }
    itkDebugMacro(" FixedImageMin: " << this->m_FixedImageTrueMin << " FixedImageMax: " << this->m_FixedImageTrueMax
                                     << std::endl);
    itkDebugMacro(" MovingImageMin: " << this->m_MovingImageTrueMin << " MovingImageMax: " << this->m_MovingImageTrueMax
                                      << std::endl);
  }

  /**
   * Compute binsize for the histograms.
   *
   * The binsize for the image intensities needs to be adjusted so that
   * we can avoid dealing with boundary conditions using the cubic
   * spline as the Parzen window.  We do this by increasing the size
   * of the bins so that the joint histogram becomes "padded" at the
   * borders. Because we are changing the binsize,
   * we also need to shift the minimum by the padded amount in order to
   * avoid minimum values filling in our padded region.
   *
   * Note that there can still be non-zero bin values in the padded region,
   * it's just that these bins will never be a central bin for the Parzen
   * window.
   *
   */
  constexpr int padding = 2; // this will pad by 2 bins

  this->m_FixedImageBinSize = (this->m_FixedImageTrueMax - this->m_FixedImageTrueMin) /
                              static_cast<PDFValueType>(this->m_NumberOfHistogramBins - 2 * padding);
  this->m_FixedImageNormalizedMin =
    this->m_FixedImageTrueMin / this->m_FixedImageBinSize - static_cast<PDFValueType>(padding);

  this->m_MovingImageBinSize = (this->m_MovingImageTrueMax - this->m_MovingImageTrueMin) /
                               static_cast<PDFValueType>(this->m_NumberOfHistogramBins - 2 * padding);
  this->m_MovingImageNormalizedMin =
    this->m_MovingImageTrueMin / this->m_MovingImageBinSize - static_cast<PDFValueType>(padding);

  itkDebugMacro("FixedImageNormalizedMin: " << this->m_FixedImageNormalizedMin);
  itkDebugMacro("MovingImageNormalizedMin: " << this->m_MovingImageNormalizedMin);
  itkDebugMacro("FixedImageBinSize: " << this->m_FixedImageBinSize);
  itkDebugMacro("MovingImageBinSize; " << this->m_MovingImageBinSize);

  /* Porting note: the rest of the initialization that was performed
   * in MattesMutualImageToImageMetric::Initialize
   * is now performed in the threader BeforeThreadedExecution method */
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::FinalizeThread(const ThreadIdType threadId)
{
  if (this->GetComputeDerivative() && (!this->HasLocalSupport()))
  {
    this->m_ThreaderDerivativeManager[threadId].BlockAndReduce();
  }
}


template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::ComputeResults() const
{
  if (this->GetNumberOfValidPoints() == 0)
  {
    itkExceptionMacro("All samples map outside moving image buffer. "
                      "The images do not sufficiently "
                      "overlap. They need to be initialized to have more overlap before this "
                      "metric will work. For instance, you can align the image centers by translation."
                      << std::endl);
  }
  if (this->m_JointPDFSum < itk::NumericTraits<PDFValueType>::epsilon())
  {
    itkExceptionMacro("Joint PDF summed to zero");
  }
  const PDFValueType normalizationFactor = 1.0 / this->m_JointPDFSum;

  // Create aliases to make variable name intent more clear
  // At this point the multiple thread partial values have been merged into
  // the zero'th element of the m_ThreaderJointPDF and m_ThreaderFixedImageMarginalPDF.
  const auto &                l_JointPDF = this->m_ThreaderJointPDF[0];
  std::vector<PDFValueType> & l_FixedImageMarginalPDF = this->m_ThreaderFixedImageMarginalPDF[0];

  /* FixedMarginalPDF         JointPDF
   *      (j)            -------------------
   *   [ 3 ]             |  1  |  2  |  0  |
   *                       ---   ---   ---
   *   [ 5 ]             |  4  |  1  |  0  |
   *                       ---   ---   ---
   *   [ 0 ]             |  0  |  0  |  0  |
   *                     -------------------
   *
   *                       [ 5 ] [ 3 ] [ 0 ]  (i)  <-- MovingMarginalPDF
   */

  auto nomalize_labmda = [&normalizationFactor](const JointPDFValueType & c) -> JointPDFValueType {
    return c * normalizationFactor;
  };
  const size_t        number_of_JointPDF_bins = this->m_NumberOfHistogramBins * this->m_NumberOfHistogramBins;
  JointPDFValueType * pdfPtr = l_JointPDF->GetBufferPointer();
  std::transform(pdfPtr, pdfPtr + number_of_JointPDF_bins, pdfPtr, nomalize_labmda);
  std::transform(
    l_FixedImageMarginalPDF.cbegin(), l_FixedImageMarginalPDF.cend(), l_FixedImageMarginalPDF.begin(), nomalize_labmda);
  {
    size_t curr_col = 0;
    for (auto & currMarginalBin : this->m_MovingImageMarginalPDF)
    {
      currMarginalBin = 0;
      const auto start = pdfPtr + curr_col++;
      const auto stop = pdfPtr + number_of_JointPDF_bins;
      for (auto colptr = start; colptr < stop; colptr += this->m_NumberOfHistogramBins)
      {
        currMarginalBin += *colptr;
      }
    }
  }

  static constexpr PDFValueType closeToZero = std::numeric_limits<PDFValueType>::epsilon();
  const PDFValueType            nFactor = 1.0 / (this->m_MovingImageBinSize * this->GetNumberOfValidPoints());

  auto const temp_num_histogram_bins = this->m_NumberOfHistogramBins;
  /**
   * Compute the metric by double summation over histogram.
   */
  PDFValueType sum = 0.0;
  for (unsigned int fixedIndex = 0; fixedIndex < temp_num_histogram_bins; ++fixedIndex)
  {
    const PDFValueType fixedImageMarginalPDFValue = l_FixedImageMarginalPDF[fixedIndex];

    // If fixedImageMarginalPDFValue is <= closeToZero, then the entire row of values is <= closeToZero, so
    // by definition, each of the individual jointPDFVlaue's must also be close to zero in the line below!
    if (fixedImageMarginalPDFValue > closeToZero)
    {
      // NOTE: If fixedImageMarginalPDFValue <=> closeToZero, logfixedImageMarginalPDFValue is never used
      //       The common case is that it is used, so perform std::log(fixedImageMarginalPDFValue) one time
      const PDFValueType logfixedImageMarginalPDFValue = std::log(fixedImageMarginalPDFValue);
      // Setup pointer to point to the first bin of this row in the jointPDF
      const JointPDFValueType * jointPDFRowPtr = l_JointPDF->GetBufferPointer() + fixedIndex * temp_num_histogram_bins;
      for (unsigned int movingIndex = 0; movingIndex < temp_num_histogram_bins; ++movingIndex)
      {
        const PDFValueType & movingImageMarginalPDF = this->m_MovingImageMarginalPDF[movingIndex];
        const PDFValueType   jointPDFValue = *(jointPDFRowPtr++); /* Get Value and goto next bin in row */

        // check for non-zero bin contribution, if movingImageMarginalPDF <= closeToZero, then so is joinPDFValue
        if (movingImageMarginalPDF > closeToZero &&
            jointPDFValue > closeToZero) //<-- This check is always false if not isNotNearZerofixedImageMarginalPDFValue
        {
          const PDFValueType pRatio = std::log(jointPDFValue / movingImageMarginalPDF);
          sum += jointPDFValue * (pRatio - logfixedImageMarginalPDFValue);

          if (this->GetComputeDerivative())
          {
            if (!this->HasLocalSupport())
            {
              // Collect global derivative contributions
              JointPDFValueType const * derivPtr = this->m_JointPDFDerivatives->GetBufferPointer() +
                                                   (fixedIndex * this->m_JointPDFDerivatives->GetOffsetTable()[2]) +
                                                   (movingIndex * this->m_JointPDFDerivatives->GetOffsetTable()[1]);
              // move joint pdf derivative pointer to the right position
              for (unsigned int parameter = 0, lastParameter = this->GetNumberOfLocalParameters();
                   parameter < lastParameter;
                   ++parameter, derivPtr++)
              {
                // Ref: eqn 23 of Thevenaz & Unser paper [3]
                (*(this->m_DerivativeResult))[parameter] += (*derivPtr) * pRatio;
              } // end for-loop over parameters
            }
            else
            {
              // Collect the pRatio per pdf indices.
              // Will be applied subsequently to local-support derivative
              const OffsetValueType index = movingIndex + (fixedIndex * this->m_NumberOfHistogramBins);
              this->m_PRatioArray[index] = pRatio * nFactor;
            }
          }
        } // end if( jointPDFValue > closeToZero && movingImageMarginalPDF > closeToZero )
      }   // end for-loop over moving index
    }     // end conditional for fixedmarginalPDF > close to zero
  }       // end for-loop over fixed index

  // Apply the pRatio and sum the per-window derivative
  // contributions, in the local-support case.
  if (this->GetComputeDerivative())
  {
    if (this->HasLocalSupport())
    {
      for (SizeValueType i = 0, derivativeSize = this->m_DerivativeResult->Size(); i < derivativeSize; ++i)
      {
        for (SizeValueType bin = 0; bin < 4; ++bin)
        {
          // Increment the m_JointPdfIndex1DArray index by bin in order to recover
          // the pRatio at the moving indices used for each portion of the derivative.
          // Note: in old v3 metric ComputeDerivatives, derivativeContribution is subtracted in global case,
          // but added in "local" (implicit) case. These operations have been switched to minimize the metric.
          const SizeValueType pRatioIndex = this->m_JointPdfIndex1DArray[i] + bin;
          (*(this->m_DerivativeResult))[i] -= m_LocalDerivativeByParzenBin[bin][i] * this->m_PRatioArray[pRatioIndex];
        }
      }
    }
  }

  // in ITKv4, metrics always minimize
  this->m_Value = static_cast<MeasureType>(-1.0 * sum);
}


template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::GetValueCommonAfterThreadedExecution()
{
  const ThreadIdType localNumberOfWorkUnitsUsed = this->GetNumberOfWorkUnitsUsed();

  const SizeValueType       numberOfVoxels = this->m_NumberOfHistogramBins * this->m_NumberOfHistogramBins;
  JointPDFValueType * const pdfPtrStart = this->m_ThreaderJointPDF[0]->GetBufferPointer();

  for (unsigned int t = 1; t < localNumberOfWorkUnitsUsed; ++t)
  {
    JointPDFValueType *             pdfPtr = pdfPtrStart;
    JointPDFValueType const *       tPdfPtr = this->m_ThreaderJointPDF[t]->GetBufferPointer();
    JointPDFValueType const * const tPdfPtrEnd = tPdfPtr + numberOfVoxels;
    while (tPdfPtr < tPdfPtrEnd)
    {
      *(pdfPtr++) += *(tPdfPtr++);
    }
    for (SizeValueType i = 0; i < this->m_NumberOfHistogramBins; ++i)
    {
      this->m_ThreaderFixedImageMarginalPDF[0][i] += this->m_ThreaderFixedImageMarginalPDF[t][i];
    }
  }

  // Sum of this threads domain into the this->m_JointPDFSum that covers that part of the domain.
  JointPDFValueType const *          pdfPtr = pdfPtrStart;
  CompensatedSummation<PDFValueType> jointPDFSum;
  for (SizeValueType i = 0; i < numberOfVoxels; ++i)
  {
    jointPDFSum += *(pdfPtr++);
  }
  this->m_JointPDFSum = jointPDFSum.GetSum();
}


template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
OffsetValueType
MattesMutualInformationImageToImageMetricv4<
  TFixedImage,
  TMovingImage,
  TVirtualImage,
  TInternalComputationValueType,
  TMetricTraits>::ComputeSingleFixedImageParzenWindowIndex(const FixedImagePixelType & value) const
{
  // Note. The previous version of this metric pre-computed these values
  // during metric Initializaiton. But with the Metricv4 design, it's
  // more difficult to do so and retrieve as needed in an efficient way.

  // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
  const PDFValueType windowTerm =
    static_cast<PDFValueType>(value) / this->m_FixedImageBinSize - this->m_FixedImageNormalizedMin;
  auto pindex = static_cast<OffsetValueType>(windowTerm);

  // Make sure the extreme values are in valid bins
  if (pindex < 2)
  {
    pindex = 2;
  }
  else
  {
    const OffsetValueType nindex = static_cast<OffsetValueType>(this->m_NumberOfHistogramBins) - 3;
    if (pindex > nindex)
    {
      pindex = nindex;
    }
  }

  return pindex;
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::DerivativeBufferManager ::
  Initialize(size_t                                    maxBufferLength,
             const size_t                              cachedNumberOfLocalParameters,
             std::mutex *                              parentDerivativeLockPtr,
             typename JointPDFDerivativesType::Pointer parentJointPDFDerivatives)
{
  m_CurrentFillSize = 0;
  m_MemoryBlockSize = cachedNumberOfLocalParameters * maxBufferLength;
  m_BufferPDFValuesContainer.resize(maxBufferLength, nullptr);
  m_BufferOffsetContainer.resize(maxBufferLength, 0);
  m_CachedNumberOfLocalParameters = cachedNumberOfLocalParameters;
  m_MaxBufferSize = maxBufferLength;
  m_ParentJointPDFDerivativesLockPtr = parentDerivativeLockPtr;
  m_ParentJointPDFDerivatives = parentJointPDFDerivatives;
  // Allocate and initialize to zero (note the () at the end of the new
  // operator)
  // the memory as a single block
  m_MemoryBlock.resize(m_MemoryBlockSize, 0.0);
  for (size_t index = 0; index < maxBufferLength; ++index)
  {
    this->m_BufferPDFValuesContainer[index] = &(this->m_MemoryBlock[0]) + index * m_CachedNumberOfLocalParameters;
  }
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::DerivativeBufferManager ::DoubleBufferSize()
{
  m_MaxBufferSize = m_MaxBufferSize * 2;
  m_MemoryBlockSize = m_MemoryBlockSize * 2;
  m_BufferPDFValuesContainer.resize(m_MaxBufferSize, nullptr);
  m_BufferOffsetContainer.resize(m_MaxBufferSize, 0);
  m_MemoryBlock.resize(m_MemoryBlockSize, 0.0);
  for (size_t index = 0; index < m_MaxBufferSize; ++index)
  {
    this->m_BufferPDFValuesContainer[index] = &(this->m_MemoryBlock[0]) + index * m_CachedNumberOfLocalParameters;
  }
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::DerivativeBufferManager ::CheckAndReduceIfNecessary()
{
  if (m_CurrentFillSize == m_MaxBufferSize)
  {
    // Attempt to acquire the lock once
    std::unique_lock<std::mutex> FirstTryLockHolder(*this->m_ParentJointPDFDerivativesLockPtr, std::try_to_lock);
    if (FirstTryLockHolder.owns_lock())
    {
      ReduceBuffer();
    }
    else if (m_MaxBufferSize < 5000)
    {
      DoubleBufferSize();
      // Attempt to acquire the lock a second time
      std::unique_lock<std::mutex> SecondTryLockHolder(*this->m_ParentJointPDFDerivativesLockPtr, std::try_to_lock);
      if (SecondTryLockHolder.owns_lock())
      {
        ReduceBuffer();
      }
    }
    else
    {
      // when CPU speed is higher than memory bandwith
      // the buffer could grow endlessly, so we limit it
      BlockAndReduce();
    }
  }
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::DerivativeBufferManager ::BlockAndReduce()
{
  if (m_CurrentFillSize > 0)
  {
    std::lock_guard<std::mutex> LockHolder(*this->m_ParentJointPDFDerivativesLockPtr);
    ReduceBuffer();
  }
}

template <typename TFixedImage,
          typename TMovingImage,
          typename TVirtualImage,
          typename TInternalComputationValueType,
          typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,
                                            TMovingImage,
                                            TVirtualImage,
                                            TInternalComputationValueType,
                                            TMetricTraits>::DerivativeBufferManager ::ReduceBuffer()
{
  auto BufferOffsetContainerIter(this->m_BufferOffsetContainer.begin());
  auto BufferPDFValuesContainerIter(this->m_BufferPDFValuesContainer.begin());

  // NOTE: Only need to write out portion of buffer filled.
  size_t bufferIndex = 0;

  while (bufferIndex < m_CurrentFillSize)
  {
    const OffsetValueType          ThisIndexOffset = *BufferOffsetContainerIter;
    JointPDFDerivativesValueType * derivPtr = this->m_ParentJointPDFDerivatives->GetBufferPointer() + ThisIndexOffset;

    PDFValueType *             derivativeContribution = *BufferPDFValuesContainerIter;
    const PDFValueType * const endContribution = derivativeContribution + m_CachedNumberOfLocalParameters;
    while (derivativeContribution < endContribution)
    {
      *(derivPtr) += *(derivativeContribution);
      // NOTE: Preliminary inconclusive tests indicates that setting to zero
      // while it's local in cache is faster than bulk memset after the loop
      // for small data sets
      *(derivativeContribution) = 0.0; // Reset to zero after getting
                                       // value
      ++derivativeContribution;
      ++derivPtr;
    }

    ++BufferOffsetContainerIter;
    ++BufferPDFValuesContainerIter;
    ++bufferIndex;
  }
  m_CurrentFillSize = 0; // Reset fill size back to zero.
}

} // end namespace itk

#endif
