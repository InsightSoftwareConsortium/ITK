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

namespace itk
{

template < class TFixedImage, class TMovingImage, class TVirtualImage >
MattesMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage>
::MattesMutualInformationImageToImageMetricv4() :
  m_NumberOfHistogramBins(50),
  m_MovingImageNormalizedMin(0.0),
  m_FixedImageNormalizedMin(0.0),
  m_MovingImageTrueMin(0.0),
  m_MovingImageTrueMax(0.0),
  m_FixedImageBinSize(0.0),
  m_MovingImageBinSize(0.0),

  m_CubicBSplineKernel(NULL),
  m_CubicBSplineDerivativeKernel(NULL),

  m_PRatioArray(0,0),

  m_ThreaderMetricDerivative(0),

  // Initialize memory
  m_MovingImageMarginalPDF(0),
  m_ThreaderFixedImageMarginalPDF(0),

  // For multi-threading the metric
  m_ThreaderJointPDF(0),
  m_ThreaderJointPDFDerivatives(0),
  m_ThreaderJointPDFStartBin(0),
  m_ThreaderJointPDFEndBin(0),
  m_ThreaderJointPDFSum(0),

  m_UseExplicitPDFDerivatives(true),
  m_ImplicitDerivativesSecondPass(false)
{
  // We have our own GetValueAndDerivativeThreader's that we want
  // ImageToImageMetricv4 to use.
  this->m_DenseGetValueAndDerivativeThreader  = MattesMutualInformationDenseGetValueAndDerivativeThreaderType::New();
  this->m_SparseGetValueAndDerivativeThreader = MattesMutualInformationSparseGetValueAndDerivativeThreaderType::New();

}

template < class TFixedImage, class TMovingImage, class TVirtualImage >
MattesMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage>
::~MattesMutualInformationImageToImageMetricv4()
{
}


/**
 * Initialize
 */
template <class TFixedImage, class TMovingImage, class TVirtualImage>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::Initialize(void) throw ( itk::ExceptionObject )
{
  /* Superclass initialization */
  this->Superclass::Initialize();

  /* Check for use of local-support transforms. Not yet suported */
  if( this->GetMovingTransform()->HasLocalSupport() ||
      this->GetFixedTransform()->HasLocalSupport() )
      {
      itkExceptionMacro("Local-support transforms are not yet supported.");
      }

  /* Check for implict derivatives method. Not yet supported. */
  if( ! this->m_UseExplicitPDFDerivatives )
    {
    itkExceptionMacro("Implict derivatives method not yet supported.");
    }

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

  this->m_FixedImageTrueMin = vcl_numeric_limits<typename TFixedImage::PixelType>::max();
  this->m_FixedImageTrueMax = vcl_numeric_limits<typename TFixedImage::PixelType>::min();
  this->m_MovingImageTrueMin = vcl_numeric_limits<typename TMovingImage::PixelType>::max();
  this->m_MovingImageTrueMax = vcl_numeric_limits<typename TMovingImage::PixelType>::min();

  // We need to make robust measures only over the requested mask region
  itk::ImageRegionConstIteratorWithIndex<TFixedImage> fi(this->m_FixedImage, this->m_FixedImage->GetBufferedRegion() );
  while( !fi.IsAtEnd() )
    {
    typename TFixedImage::PointType fixedSpacePhysicalPoint;
    this->m_FixedImage->TransformIndexToPhysicalPoint(fi.GetIndex(), fixedSpacePhysicalPoint);
    if( this->m_FixedImageMask.IsNull()  // A null mask implies entire space is to be used.
        || this->m_FixedImageMask->IsInside(fixedSpacePhysicalPoint) )
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
    typename TMovingImage::PointType movingSpacePhysicalPoint;
    this->m_MovingImage->TransformIndexToPhysicalPoint(mi.GetIndex(), movingSpacePhysicalPoint);
    if( this->m_MovingImageMask.IsNull()  // A null mask implies entire space is to be used.
        || this->m_MovingImageMask->IsInside(movingSpacePhysicalPoint) )
      {
      const typename TMovingImage::PixelType currValue = mi.Get();
      this->m_MovingImageTrueMin = (m_MovingImageTrueMin < currValue) ? this->m_MovingImageTrueMin : currValue;
      this->m_MovingImageTrueMax = (m_MovingImageTrueMax > currValue) ? this->m_MovingImageTrueMax : currValue;
      }
    ++mi;
    }
  }

  itkDebugMacro(" FixedImageMin: " << this->m_FixedImageTrueMin
                                   << " FixedImageMax: " << this->m_FixedImageTrueMax << std::endl);
  itkDebugMacro(" MovingImageMin: " << this->m_MovingImageTrueMin
                                    << " MovingImageMax: " << this->m_MovingImageTrueMax << std::endl);
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

template <class TFixedImage, class TMovingImage, class TVirtualImage>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::GetValueAndDerivative( MeasureType & value,
                                      DerivativeType & derivative ) const
{
  /* We reimplement this method to more easily drop-in the code from
   * old MattesMutual metric, and allow for running through the threaded
   * processing twice. */

  // Set output values to zero
  value = NumericTraits<MeasureType>::Zero;

  this->m_DerivativeResult = &derivative;
  // This will size and initialize m_DerivativeResult.
  // We'll be skipping the usual update of m_DerivativeResult in this metric,
  // and doing it below directly.
  this->InitializeForIteration();

  // With implicit method, 'derivative' gets assigned at end of this method, so
  // don't actually need to alloc and set it now, although it's done already
  // above in call to InitializeForIteration.
  if( ! this->m_UseExplicitPDFDerivatives )
    {
    this->m_PRatioArray.Fill(0.0);
    for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
      {
      this->m_ThreaderMetricDerivative[threadID].Fill(NumericTraits<MeasureType>::Zero);
      }
    this->m_ImplicitDerivativesSecondPass = false;
    }

  // Threaded processing. In the case of implicit PDF derivative calculation, this
  // is the first pass.
  this->GetValueAndDerivativeExecute();

  // Collect some results
  for( ThreadIdType threadID = 1; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
    {
    this->m_ThreaderJointPDFSum[0] += this->m_ThreaderJointPDFSum[threadID];
    }
  if( this->m_ThreaderJointPDFSum[0] < itk::NumericTraits< PDFValueType >::epsilon() )
    {
    itkExceptionMacro("Joint PDF summed to zero");
    }

  std::fill(this->m_MovingImageMarginalPDF.begin(), this->m_MovingImageMarginalPDF.end(), 0.0F);

  PDFValueType       totalMassOfPDF = 0.0;
  for( unsigned int i = 0; i < this->m_NumberOfHistogramBins; i++ )
    {
    totalMassOfPDF += this->m_ThreaderFixedImageMarginalPDF[0][i];
    }

  const PDFValueType normalizationFactor = 1.0 / this->m_ThreaderJointPDFSum[0];
  JointPDFValueType *pdfPtr = this->m_ThreaderJointPDF[0]->GetBufferPointer();
  for( unsigned int i = 0; i < this->m_NumberOfHistogramBins; i++ )
    {
    PDFValueType * movingMarginalPtr = &(m_MovingImageMarginalPDF[0]);
    for( unsigned int j = 0; j < this->m_NumberOfHistogramBins; j++ )
      {
      *( pdfPtr ) *= normalizationFactor;
      *( movingMarginalPtr++ ) += *( pdfPtr++ );
      }
    }

  SizeValueType numberOfPoints;
  if( this->m_UseFixedSampledPointSet )
    {
    numberOfPoints = this->m_FixedSampledPointSet->GetNumberOfPoints();
    }
  else
    {
    typename FixedImageType::RegionType region = this->m_FixedImage->GetBufferedRegion();
    numberOfPoints = region.GetNumberOfPixels();
    }

  if( this->m_NumberOfValidPoints < numberOfPoints / 16 )
    {
    itkExceptionMacro("Too many samples map outside moving image buffer: "
                      << this->m_NumberOfValidPoints << " / "
                      << numberOfPoints
                      << std::endl);
    }

  // Normalize the fixed image marginal PDF
  if( totalMassOfPDF == 0.0 )
    {
    itkExceptionMacro("Fixed image marginal PDF summed to zero");
    }
  for( unsigned int bin = 0; bin < this->m_NumberOfHistogramBins; bin++ )
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

  const PDFValueType nFactor = 1.0 / ( this->m_MovingImageBinSize
                                 * this->m_NumberOfValidPoints );
  for( unsigned int fixedIndex = 0;
       fixedIndex < this->m_NumberOfHistogramBins;
       ++fixedIndex )
    {
    const PDFValueType fixedImagePDFValue = this->m_ThreaderFixedImageMarginalPDF[0][fixedIndex];
    for( unsigned int movingIndex = 0;
         movingIndex < this->m_NumberOfHistogramBins;
         ++movingIndex, jointPDFPtr++ )
      {
      const PDFValueType movingImagePDFValue = this->m_MovingImageMarginalPDF[movingIndex];
      const PDFValueType jointPDFValue = *( jointPDFPtr );

      // check for non-zero bin contribution
      static const PDFValueType closeToZero = vcl_numeric_limits<PDFValueType>::epsilon();
      if( jointPDFValue > closeToZero &&  movingImagePDFValue > closeToZero )
        {
        const PDFValueType pRatio = vcl_log(jointPDFValue / movingImagePDFValue);

        if( fixedImagePDFValue > closeToZero )
          {
          sum += jointPDFValue * ( pRatio - vcl_log(fixedImagePDFValue) );
          }

        if( this->m_UseExplicitPDFDerivatives )
          {
          // move joint pdf derivative pointer to the right position
          JointPDFValueType const * derivPtr = this->m_ThreaderJointPDFDerivatives[0]->GetBufferPointer()
            + ( fixedIndex  * this->m_ThreaderJointPDFDerivatives[0]->GetOffsetTable()[2] )
            + ( movingIndex * this->m_ThreaderJointPDFDerivatives[0]->GetOffsetTable()[1] );
          for( unsigned int parameter = 0; parameter < this->GetNumberOfParameters(); ++parameter, derivPtr++ )
            {
            // Ref: eqn 23 of Thevenaz & Unser paper [3]
            // 'derivative' is pointed to by m_DerivativeReturn
            derivative[parameter] -= ( *derivPtr ) * pRatio;
            }  // end for-loop over parameters
          }
        else
          {
          this->m_PRatioArray[fixedIndex][movingIndex] = pRatio * nFactor;
          }
        } // end if-block to check non-zero bin contribution
      }   // end for-loop over moving index
    }     // end for-loop over fixed index

  if( !( this->m_UseExplicitPDFDerivatives ) )
    {
    // Second pass: This one is done for accumulating the contributions
    //              to the derivative array.
    //
    this->m_ImplicitDerivativesSecondPass = true;

    // Threaded processing.
    this->GetValueAndDerivativeExecute();

    // Consolidate the contributions from each one of the threads to the total
    // derivative.
    for( unsigned int t = 1; t < this->GetNumberOfThreadsUsed(); t++ )
      {
      DerivativeType const * const source = &( this->m_ThreaderMetricDerivative[t] );
      for( unsigned int pp = 0; pp < this->GetNumberOfParameters(); pp++ )
        {
        this->m_ThreaderMetricDerivative[0][pp] += ( *source )[pp];
        }
      }

    // derivative is pointed to by m_DerivativeResult.
    // We could have m_ThreaderMetricDerivative[0] point to 'derivative'
    // during initialization to save some memory.
    derivative = this->m_ThreaderMetricDerivative[0];
    }

  // in ITKv4, metrics always minimize
  derivative = derivative * -1;
  value = static_cast<MeasureType>( -1.0 * sum );
}

/**
 * Common post-threading code.
 */
template <class TFixedImage, class TMovingImage, class TVirtualImage>
void
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::GetValueCommonAfterThreadedExecution()
{
  //This method is from MattesMutualImageToImageMetric::GetValueThreadPostProcess. Common
  // code used by GetValue and GetValueAndDerivative. Should be threaded.
  for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
    {
    const int maxI = this->m_NumberOfHistogramBins
      * ( this->m_ThreaderJointPDFEndBin[threadID] - this->m_ThreaderJointPDFStartBin[threadID] + 1 );

    const unsigned int tPdfPtrOffset = ( this->m_ThreaderJointPDFStartBin[threadID] * this->m_ThreaderJointPDF[0]->GetOffsetTable()[1] );
    JointPDFValueType * const pdfPtrStart = this->m_ThreaderJointPDF[0]->GetBufferPointer() + tPdfPtrOffset;

    // The PDF domain is chunked based on thread.  Each thread consolodates independant parts of the PDF.
    for( unsigned int t = 1; t < this->GetNumberOfThreadsUsed(); t++ )
      {
      JointPDFValueType *                 pdfPtr = pdfPtrStart;
      JointPDFValueType const *          tPdfPtr = this->m_ThreaderJointPDF[t]->GetBufferPointer() + tPdfPtrOffset;
      JointPDFValueType const * const tPdfPtrEnd = tPdfPtr + maxI;
      // for(i=0; i < maxI; i++)
      while( tPdfPtr < tPdfPtrEnd )
        {
        *( pdfPtr++ ) += *( tPdfPtr++ );
        }
      for( int i = this->m_ThreaderJointPDFStartBin[threadID]; i <= this->m_ThreaderJointPDFEndBin[threadID]; i++ )
        {
        this->m_ThreaderFixedImageMarginalPDF[0][i] += this->m_ThreaderFixedImageMarginalPDF[t][i];
        }
      }
    // Sum of this threads domain into the this->m_ThreaderJointPDFSum that covers that part of the domain.
    PDFValueType                    jointPDFSum = 0.0;
    JointPDFValueType const * pdfPtr = pdfPtrStart;
    for( int i = 0; i < maxI; i++ )
      {
      jointPDFSum += *( pdfPtr++ );
      }
    this->m_ThreaderJointPDFSum[threadID] = jointPDFSum;
  }
}

/**
 * PrintSelf
 */
template < class TFixedImage, class TMovingImage, class TVirtualImage  >
void
MattesMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/**
 * ComputeSingleFixedImageParzenWindowIndex.
 */
template <class TFixedImage, class TMovingImage, class TVirtualImage>
OffsetValueType
MattesMutualInformationImageToImageMetricv4<TFixedImage, TMovingImage, TVirtualImage>
::ComputeSingleFixedImageParzenWindowIndex( const FixedImagePixelType & value ) const
{
  // Note. The previous verison of this metric pre-computed these values
  // during metric Initializaiton. But with the Metricv4 design, it's
  // more difficult to do so and retrieve as needed in an efficient way.

  // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
  const PDFValueType windowTerm = static_cast<PDFValueType>( value )
    / this->m_FixedImageBinSize
    - this->m_FixedImageNormalizedMin;
  OffsetValueType pindex = static_cast<OffsetValueType>( windowTerm );

  // Make sure the extreme values are in valid bins
  if( pindex < 2 )
    {
    pindex = 2;
    }
  else
    {
    const OffsetValueType nindex =
      static_cast<OffsetValueType>( this->m_NumberOfHistogramBins ) - 3;
    if( pindex > nindex )
      {
      pindex = nindex;
      }
    }

  return pindex;
}

} // end namespace itk


#endif
