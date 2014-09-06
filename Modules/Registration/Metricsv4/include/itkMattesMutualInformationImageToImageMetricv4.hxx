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
#ifndef __itkMattesMutualInformationImageToImageMetricv4_hxx
#define __itkMattesMutualInformationImageToImageMetricv4_hxx

#include "itkMattesMutualInformationImageToImageMetricv4.h"
#include "itkCompensatedSummation.h"
#include "itkThreadedIndexedContainerPartitioner.h"

namespace itk
{

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::MattesMutualInformationImageToImageMetricv4() :
  m_NumberOfHistogramBins(50),
  m_MovingImageNormalizedMin(0.0),
  m_FixedImageNormalizedMin(0.0),
  m_FixedImageTrueMin(0.0),
  m_FixedImageTrueMax(0.0),
  m_MovingImageTrueMin(0.0),
  m_MovingImageTrueMax(0.0),
  m_FixedImageBinSize(0.0),
  m_MovingImageBinSize(0.0),

  m_CubicBSplineKernel(ITK_NULLPTR),
  m_CubicBSplineDerivativeKernel(ITK_NULLPTR),

  m_PRatioArray(0),

  // Initialize memory
  m_MovingImageMarginalPDF(0),
  m_ThreaderFixedImageMarginalPDF(0),

  // For multi-threading the metric
  m_ThreaderJointPDF(0),
  m_ThreaderJointPDFDerivatives(0),
  m_JointPDFSum(0.0)
{
  // We have our own GetValueAndDerivativeThreader's that we want
  // ImageToImageMetricv4 to use.
  this->m_DenseGetValueAndDerivativeThreader  = MattesMutualInformationDenseGetValueAndDerivativeThreaderType::New();
  this->m_SparseGetValueAndDerivativeThreader = MattesMutualInformationSparseGetValueAndDerivativeThreaderType::New();
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::~MattesMutualInformationImageToImageMetricv4()
{
}


/**
 * Initialize
 */
template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::Initialize(void) throw ( itk::ExceptionObject )
{
  /* Superclass initialization */
  this->Superclass::Initialize();

  /* Expects moving image gradient source */
  if( this->GetGradientSourceIncludesFixed() || !this->GetGradientSourceIncludesMoving() )
    {
    itkExceptionMacro("Expected gradient source to be only Moving. Instead gradient source is: "
                      " Fixed: " << this->GetGradientSourceIncludesFixed()
                      << " Moving: " << this->GetGradientSourceIncludesMoving() );
    }

  {
  /**
   * Compute the minimum and maximum within the specified mask
   * region for creating the size of the 2D joint histogram.
   * Areas outside the masked region should be ignored
   * in computing the range of intensity values.
   */

  this->m_FixedImageTrueMin = std::numeric_limits<typename TFixedImage::PixelType>::max();
  this->m_FixedImageTrueMax = std::numeric_limits<typename TFixedImage::PixelType>::min();
  this->m_MovingImageTrueMin = std::numeric_limits<typename TMovingImage::PixelType>::max();
  this->m_MovingImageTrueMax = std::numeric_limits<typename TMovingImage::PixelType>::min();

  // We need to make robust measures only over the requested mask region
  itk::ImageRegionConstIteratorWithIndex<TFixedImage> fi(this->m_FixedImage, this->m_FixedImage->GetBufferedRegion() );
  while( !fi.IsAtEnd() )
    {
    bool usePoint = true;
    if( ! this->m_FixedImageMask.IsNull() )
      {
      // A null mask implies entire space is to be used.
      typename TFixedImage::PointType fixedSpacePhysicalPoint;
      this->m_FixedImage->TransformIndexToPhysicalPoint(fi.GetIndex(), fixedSpacePhysicalPoint);
      usePoint = this->m_FixedImageMask->IsInside(fixedSpacePhysicalPoint);
      }
    if( usePoint )
      {
      const typename TFixedImage::PixelType currValue = fi.Get();
      this->m_FixedImageTrueMin = (m_FixedImageTrueMin < currValue) ? this->m_FixedImageTrueMin : currValue;
      this->m_FixedImageTrueMax = (m_FixedImageTrueMax > currValue) ? this->m_FixedImageTrueMax : currValue;
      }
    ++fi;
    }

  {
  itk::ImageRegionConstIteratorWithIndex<TMovingImage> mi(this->m_MovingImage,
                                                          this->m_MovingImage->GetBufferedRegion() );
  while( !mi.IsAtEnd() )
    {
    bool usePoint = true;
    if( ! this->m_MovingImageMask.IsNull() )
      { // A null mask implies entire space is to be used.
      typename TMovingImage::PointType movingSpacePhysicalPoint;
      this->m_MovingImage->TransformIndexToPhysicalPoint(mi.GetIndex(), movingSpacePhysicalPoint);
      usePoint = this->m_MovingImageMask->IsInside(movingSpacePhysicalPoint);
      }
    if( usePoint )
      {
      const typename TMovingImage::PixelType currValue = mi.Get();
      this->m_MovingImageTrueMin = (m_MovingImageTrueMin < currValue) ? this->m_MovingImageTrueMin : currValue;
      this->m_MovingImageTrueMax = (m_MovingImageTrueMax > currValue) ? this->m_MovingImageTrueMax : currValue;
      }
    ++mi;
    }
  }

  itkDebugMacro(" FixedImageMin: " << this->m_FixedImageTrueMin << " FixedImageMax: " << this->m_FixedImageTrueMax << std::endl);
  itkDebugMacro(" MovingImageMin: " << this->m_MovingImageTrueMin << " MovingImageMax: " << this->m_MovingImageTrueMax << std::endl);
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
  const int padding = 2;  // this will pad by 2 bins

  this->m_FixedImageBinSize = ( this->m_FixedImageTrueMax - this->m_FixedImageTrueMin )
    / static_cast<PDFValueType>( this->m_NumberOfHistogramBins - 2 * padding );
  this->m_FixedImageNormalizedMin = this->m_FixedImageTrueMin / this->m_FixedImageBinSize - static_cast<PDFValueType>( padding );

  this->m_MovingImageBinSize = ( this->m_MovingImageTrueMax - this->m_MovingImageTrueMin )
    / static_cast<PDFValueType>( this->m_NumberOfHistogramBins - 2 * padding );
  this->m_MovingImageNormalizedMin = this->m_MovingImageTrueMin / this->m_MovingImageBinSize - static_cast<PDFValueType>( padding );

  itkDebugMacro("FixedImageNormalizedMin: " << this->m_FixedImageNormalizedMin);
  itkDebugMacro("MovingImageNormalizedMin: " << this->m_MovingImageNormalizedMin);
  itkDebugMacro("FixedImageBinSize: " << this->m_FixedImageBinSize);
  itkDebugMacro("MovingImageBinSize; " << this->m_MovingImageBinSize);

  /* Porting note: the rest of the initialization that was performed
   * in MattesMutualImageToImageMetric::Initialize
   * is now performed in the threader BeforeThreadedExecution method */
  }

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::InitializeThread( const ThreadIdType threadId )
{
  /* This block of code is from
     MattesMutualImageToImageMetric::GetValueAndDerivativeThreadPreProcess */
  std::fill(
    this->m_ThreaderFixedImageMarginalPDF[threadId].begin(),
    this->m_ThreaderFixedImageMarginalPDF[threadId].end(), 0.0F);

  /*
   * Allocate memory for the joint PDF and joint PDF derivatives accumulator caches
   * The joint PDF and joint PDF derivatives are store as itk::Image.
   *
   * Avoid allocations if already the correct size.
   * Only recreate if size differ from last time.  If size is the same,
   * there is no need to recreate the memory
   */
  JointPDFRegionType jointPDFRegion;
    {
    // For the joint PDF define a region starting from {0,0}
    // with size {m_NumberOfHistogramBins, this->m_NumberOfHistogramBins}.
    // The dimension represents fixed image bin size
    // and moving image bin size , respectively.
    JointPDFIndexType jointPDFIndex;
    jointPDFIndex.Fill(0);
    JointPDFSizeType jointPDFSize;
    jointPDFSize.Fill(this->m_NumberOfHistogramBins);

    jointPDFRegion.SetIndex(jointPDFIndex);
    jointPDFRegion.SetSize(jointPDFSize);
    }

  const bool reinitializeThreaderJointPDF =( this->m_ThreaderJointPDF[threadId].IsNull()
    ||  ( jointPDFRegion != this->m_ThreaderJointPDF[threadId]->GetBufferedRegion() ) );
  if (  reinitializeThreaderJointPDF )
    {
    // By setting these values, the joint histogram physical locations will
    // correspond to intensity values.
    typename JointPDFType::PointType origin;
    origin[0] = this->m_FixedImageTrueMin;
    origin[1] = this->m_MovingImageTrueMin;
    typename JointPDFType::SpacingType spacing;
    spacing[0] = this->m_FixedImageBinSize;
    spacing[1] = this->m_MovingImageBinSize;

      {
      this->m_ThreaderJointPDF[threadId] = JointPDFType::New();
      this->m_ThreaderJointPDF[threadId]->SetRegions(jointPDFRegion);
      this->m_ThreaderJointPDF[threadId]->SetOrigin(origin);
      this->m_ThreaderJointPDF[threadId]->SetSpacing(spacing);
      // NOTE: true = initizize to zero
      this->m_ThreaderJointPDF[threadId]->Allocate(true);
      }
    }
  else
    {
    // Still need to reset to zero for subsequent runs
    this->m_ThreaderJointPDF[threadId]->FillBuffer(0.0);
    }

  if( this->GetComputeDerivative()  &&  ! this->HasLocalSupport() )
    {
    JointPDFDerivativesRegionType jointPDFDerivativesRegion;
      {
      // For the derivatives of the joint PDF define a region starting from
      // {0,0,0}
      // with size {m_NumberOfParameters,m_NumberOfHistogramBins,
      // this->m_NumberOfHistogramBins}. The dimension represents transform
      // parameters,
      // fixed image parzen window index and moving image parzen window index,
      // respectively.
      JointPDFDerivativesIndexType jointPDFDerivativesIndex;
      jointPDFDerivativesIndex.Fill(0);
      JointPDFDerivativesSizeType jointPDFDerivativesSize;
      jointPDFDerivativesSize[0] = this->GetNumberOfLocalParameters();
      jointPDFDerivativesSize[1] = this->m_NumberOfHistogramBins;
      jointPDFDerivativesSize[2] = this->m_NumberOfHistogramBins;

      jointPDFDerivativesRegion.SetIndex(jointPDFDerivativesIndex);
      jointPDFDerivativesRegion.SetSize(jointPDFDerivativesSize);
      }
    const bool reinitializeThreaderJointPDFDerivatives = ( this->m_ThreaderJointPDFDerivatives[threadId].IsNull() ||
      (jointPDFDerivativesRegion != this->m_ThreaderJointPDFDerivatives[threadId]->GetBufferedRegion() ));
    if( reinitializeThreaderJointPDFDerivatives )
      {
      // Set the regions and allocate
      this->m_ThreaderJointPDFDerivatives[threadId] = JointPDFDerivativesType::New();
      this->m_ThreaderJointPDFDerivatives[threadId]->SetRegions( jointPDFDerivativesRegion);
      this->m_ThreaderJointPDFDerivatives[threadId]->Allocate(true);
      }
    else
      {
      // Still need to reset to zero for subsequent runs
      this->m_ThreaderJointPDFDerivatives[threadId]->FillBuffer(0.0);
      }
    }
}



template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::ComputeResults() const
{
  if( this->m_JointPDFSum < itk::NumericTraits< PDFValueType >::epsilon() )
    {
    itkExceptionMacro("Joint PDF summed to zero");
    }

  std::fill(this->m_MovingImageMarginalPDF.begin(), this->m_MovingImageMarginalPDF.end(), 0.0F);

  // Collect some results
  PDFValueType       totalMassOfPDF = 0.0;
  for( unsigned int i = 0; i < this->m_NumberOfHistogramBins; ++i )
    {
    totalMassOfPDF += this->m_ThreaderFixedImageMarginalPDF[0][i];
    }

  const PDFValueType normalizationFactor = 1.0 / this->m_JointPDFSum;
  JointPDFValueType *pdfPtr = this->m_ThreaderJointPDF[0]->GetBufferPointer();
  for( unsigned int i = 0; i < this->m_NumberOfHistogramBins; ++i )
    {
    PDFValueType * movingMarginalPtr = &(m_MovingImageMarginalPDF[0]);
    for( unsigned int j = 0; j < this->m_NumberOfHistogramBins; j++ )
      {
      *( pdfPtr ) *= normalizationFactor;
      *( movingMarginalPtr++ ) += *( pdfPtr++ );
      }
    }

  const SizeValueType numberOfPoints = this->GetNumberOfDomainPoints();

  if( this->GetNumberOfValidPoints() < numberOfPoints / 16 )
    {
    itkExceptionMacro("Too many samples map outside moving image buffer. There are only "
                      << this->m_NumberOfValidPoints << " valid points out of "
                      << numberOfPoints << " total points. The images do not sufficiently "
                      "overlap. They need to be initialized to have more overlap before this "
                      "metric will work. For instance, you can align the image centers by translation."
                      << std::endl);
    }

  // Normalize the fixed image marginal PDF
  if( totalMassOfPDF == 0.0 )
    {
    itkExceptionMacro("Fixed image marginal PDF summed to zero");
    }
  for( unsigned int bin = 0; bin < this->m_NumberOfHistogramBins; ++bin )
    {
    this->m_ThreaderFixedImageMarginalPDF[0][bin] /= totalMassOfPDF;
    }

  /**
   * Compute the metric by double summation over histogram.
   */

  // Setup pointer to point to the first bin
  JointPDFValueType *jointPDFPtr = this->m_ThreaderJointPDF[0]->GetBufferPointer();

  // Initialize sum to zero
  PDFValueType sum = 0.0;

  const PDFValueType nFactor = 1.0 / ( this->m_MovingImageBinSize * this->GetNumberOfValidPoints() );

  static const PDFValueType closeToZero = std::numeric_limits<PDFValueType>::epsilon();
  for( unsigned int fixedIndex = 0; fixedIndex < this->m_NumberOfHistogramBins; ++fixedIndex )
    {
    const PDFValueType fixedImagePDFValue = this->m_ThreaderFixedImageMarginalPDF[0][fixedIndex];
    for( unsigned int movingIndex = 0; movingIndex < this->m_NumberOfHistogramBins; ++movingIndex, jointPDFPtr++ )
      {
      const PDFValueType movingImagePDFValue = this->m_MovingImageMarginalPDF[movingIndex];
      const PDFValueType jointPDFValue = *( jointPDFPtr );

      // check for non-zero bin contribution
      if( ! (jointPDFValue > closeToZero &&  movingImagePDFValue > closeToZero) )
        {
        continue;
        }
      const PDFValueType pRatio = std::log(jointPDFValue / movingImagePDFValue);

      if( fixedImagePDFValue > closeToZero )
        {
        sum += jointPDFValue * ( pRatio - std::log(fixedImagePDFValue) );
        }

      if( this->GetComputeDerivative() )
        {
        if( ! this->HasLocalSupport() )
          {
          // Collect global derivative contributions

          // move joint pdf derivative pointer to the right position
          JointPDFValueType const * derivPtr = this->m_ThreaderJointPDFDerivatives[0]->GetBufferPointer()
            + ( fixedIndex  * this->m_ThreaderJointPDFDerivatives[0]->GetOffsetTable()[2] )
            + ( movingIndex * this->m_ThreaderJointPDFDerivatives[0]->GetOffsetTable()[1] );
          for( unsigned int parameter = 0, lastParameter = this->GetNumberOfLocalParameters();
               parameter < lastParameter;
               ++parameter, derivPtr++ )
            {
            // Ref: eqn 23 of Thevenaz & Unser paper [3]
            (*(this->m_DerivativeResult))[parameter] += ( *derivPtr ) * pRatio;
            }  // end for-loop over parameters
          }
        else
          {
          // Collect the pRatio per pdf indecies.
          // Will be applied subsequently to local-support derivative
          const OffsetValueType index = movingIndex + (fixedIndex * this->m_NumberOfHistogramBins);
          this->m_PRatioArray[index] = pRatio * nFactor;
          }
        }
      }   // end for-loop over moving index
    }     // end for-loop over fixed index

  // Apply the pRatio and sum the per-window derivative
  // contributions, in the local-support case.
  if( this->GetComputeDerivative() )
    {
    if( this->HasLocalSupport() )
      {
      for( SizeValueType i = 0, derivativeSize = this->m_DerivativeResult->Size(); i < derivativeSize; ++i )
        {
        for( SizeValueType bin = 0; bin < 4; ++bin )
          {
          // Increment the m_JointPdfIndex1DArray index by bin in order to recover
          // the pRatio at the moving indecies used for each portion of the derivative.
          // Note: in old v3 metric ComputeDerivatives, derivativeContribution is subtracted in global case,
          // but added in "local" (implicit) case. These operations have been switched to minimize the metric.
          const SizeValueType pRatioIndex = this->m_JointPdfIndex1DArray[i] + bin;
          (*(this->m_DerivativeResult))[i] -= m_LocalDerivativeByParzenBin[bin][i] * this->m_PRatioArray[pRatioIndex];
          }
        }
      }
    }

  // in ITKv4, metrics always minimize
  this->m_Value = static_cast<MeasureType>( -1.0 * sum );
}


template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::GetValueCommonAfterThreadedExecution()
{
  const ThreadIdType localNumberOfThreadsUsed = this->GetNumberOfThreadsUsed();
  // This method is from MattesMutualImageToImageMetric::GetValueThreadPostProcess. Common
  // code used by GetValue and GetValueAndDerivative.
  // Should be threaded. But if modified to do so, should probably not be threaded
  // separately, but rather as a part of all post-processing.
  const size_t numberOfVoxels = this->m_NumberOfHistogramBins* this->m_NumberOfHistogramBins;
  JointPDFValueType * const pdfPtrStart = this->m_ThreaderJointPDF[0]->GetBufferPointer();
  for( unsigned int t = 1; t < localNumberOfThreadsUsed; ++t )
    {
    JointPDFValueType *                 pdfPtr = pdfPtrStart;
    JointPDFValueType const *          tPdfPtr = this->m_ThreaderJointPDF[t]->GetBufferPointer();
    JointPDFValueType const * const tPdfPtrEnd = tPdfPtr + numberOfVoxels;
    while( tPdfPtr < tPdfPtrEnd )
      {
      *( pdfPtr++ ) += *( tPdfPtr++ );
      }
    for( size_t i = 0; i < this->m_NumberOfHistogramBins; ++i )
      {
      this->m_ThreaderFixedImageMarginalPDF[0][i] += this->m_ThreaderFixedImageMarginalPDF[t][i];
      }
    }
  // Sum of this threads domain into the this->m_JointPDFSum that covers that part of the domain.
  JointPDFValueType const * pdfPtr = pdfPtrStart;
  CompensatedSummation< PDFValueType > jointPDFSum;
  for( SizeValueType i = 0; i < numberOfVoxels; ++i )
    {
    jointPDFSum += *( pdfPtr++ );
    }
  this->m_JointPDFSum = jointPDFSum.GetSum();
}


template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
OffsetValueType
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage, TInternalComputationValueType, TMetricTraits>
::ComputeSingleFixedImageParzenWindowIndex( const FixedImagePixelType & value ) const
{
  // Note. The previous version of this metric pre-computed these values
  // during metric Initializaiton. But with the Metricv4 design, it's
  // more difficult to do so and retrieve as needed in an efficient way.

  // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
  const PDFValueType windowTerm = static_cast<PDFValueType>( value ) / this->m_FixedImageBinSize - this->m_FixedImageNormalizedMin;
  OffsetValueType pindex = static_cast<OffsetValueType>( windowTerm );

  // Make sure the extreme values are in valid bins
  if( pindex < 2 )
    {
    pindex = 2;
    }
  else
    {
    const OffsetValueType nindex = static_cast<OffsetValueType>( this->m_NumberOfHistogramBins ) - 3;
    if( pindex > nindex )
      {
      pindex = nindex;
      }
    }

  return pindex;
}

} // end namespace itk

#endif
