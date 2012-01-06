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
#ifndef __itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader_hxx
#define __itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader_hxx

#include "itkMattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader.h"

namespace itk
{

template< class TDomainPartitioner, class TImageToImageMetric, class TMattesMutualInformationMetric >
void
MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TMattesMutualInformationMetric >
::BeforeThreadedExecution()
{
  /* Most of this code needs to be here because we need to know the number
   * of threads the threader will use, which isn't known for sure until this
   * method is called. */

  /* Allocates and inits per-thread members. */
  Superclass::BeforeThreadedExecution();

  /* Convenience assignment */
  TMattesMutualInformationMetric * associate = dynamic_cast<TMattesMutualInformationMetric*>(this->m_Associate);

  /* Porting: these next blocks of code are from MattesMutualImageToImageMetric::Initialize */

  /**
   * Allocate memory for the marginal PDF and initialize values
   * to zero. The marginal PDFs are stored as std::vector.
   */
  associate->m_MovingImageMarginalPDF.resize(associate->m_NumberOfHistogramBins, 0.0F);
  associate->m_ThreaderFixedImageMarginalPDF.resize(associate->GetNumberOfThreadsUsed(),
                                         std::vector<PDFValueType>(associate->m_NumberOfHistogramBins, 0.0F) );

    {
    associate->m_ThreaderJointPDFStartBin.resize( this->GetNumberOfThreadsUsed() );
    associate->m_ThreaderJointPDFEndBin.resize(this->GetNumberOfThreadsUsed() );
    const int binRange = associate->m_NumberOfHistogramBins / this->GetNumberOfThreadsUsed();
    for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
      {
      associate->m_ThreaderJointPDFStartBin[threadID] = threadID * binRange;
      associate->m_ThreaderJointPDFEndBin[threadID] = ( threadID + 1 ) * binRange - 1;
      }
    // Ensure that the last EndBin range contains the last histogram bin
    associate->m_ThreaderJointPDFStartBin[this->GetNumberOfThreadsUsed() - 1] = ( this->GetNumberOfThreadsUsed() - 1 ) * binRange;
    associate->m_ThreaderJointPDFEndBin[this->GetNumberOfThreadsUsed() - 1] = associate->m_NumberOfHistogramBins - 1;
    }

  associate->m_ThreaderJointPDFSum.resize(this->GetNumberOfThreadsUsed());

    {
    JointPDFRegionType jointPDFRegion;
      {
      // For the joint PDF define a region starting from {0,0}
      // with size {m_NumberOfHistogramBins, this->m_NumberOfHistogramBins}.
      // The dimension represents fixed image bin size
      // and moving image bin size , respectively.
      JointPDFIndexType jointPDFIndex;
      jointPDFIndex.Fill(0);
      JointPDFSizeType jointPDFSize;
      jointPDFSize.Fill(associate->m_NumberOfHistogramBins);

      jointPDFRegion.SetIndex(jointPDFIndex);
      jointPDFRegion.SetSize(jointPDFSize);
      }

    // By setting these values, the joint histogram physical locations will correspond to intensity values.
    typename JointPDFType::PointType origin;
    origin[0] = associate->m_FixedImageTrueMin;
    origin[1] = associate->m_MovingImageTrueMin;
    typename JointPDFType::SpacingType spacing;
    spacing[0] = associate->m_FixedImageBinSize;
    spacing[1] = associate->m_MovingImageBinSize;
    /**
     * Allocate memory for the joint PDF and joint PDF derivatives.
     * The joint PDF and joint PDF derivatives are store as itk::Image.
     */
    associate->m_ThreaderJointPDF.resize(this->GetNumberOfThreadsUsed());
    for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); ++threadID )
      {
      associate->m_ThreaderJointPDF[threadID] = JointPDFType::New();
      associate->m_ThreaderJointPDF[threadID]->SetRegions(jointPDFRegion);
      associate->m_ThreaderJointPDF[threadID]->SetOrigin(origin);
      associate->m_ThreaderJointPDF[threadID]->SetSpacing(spacing);
      associate->m_ThreaderJointPDF[threadID]->Allocate();
      }
    }

  //
  // Now allocate memory according to the user-selected method.
  //
  if( associate->m_UseExplicitPDFDerivatives )
    {
    // Deallocate the memory that may have been allocated for
    // previous runs of the metric.
    // and by allocating very small the static ones
    associate->m_PRatioArray.SetSize(0, 0);          // Not needed if this->m_UseExplicitPDFDerivatives
    associate->m_ThreaderMetricDerivative.resize(0); // Not needed if this->m_UseExplicitPDFDerivatives

      {
      JointPDFDerivativesRegionType jointPDFDerivativesRegion;

        {
        // For the derivatives of the joint PDF define a region starting from
        // {0,0,0}
        // with size {m_NumberOfParameters,m_NumberOfHistogramBins,
        // this->m_NumberOfHistogramBins}. The dimension represents transform parameters,
        // fixed image parzen window index and moving image parzen window index,
        // respectively.
        JointPDFDerivativesIndexType jointPDFDerivativesIndex;
        jointPDFDerivativesIndex.Fill(0);
        JointPDFDerivativesSizeType jointPDFDerivativesSize;
        jointPDFDerivativesSize[0] = associate->GetNumberOfParameters();
        jointPDFDerivativesSize[1] = associate->m_NumberOfHistogramBins;
        jointPDFDerivativesSize[2] = associate->m_NumberOfHistogramBins;

        jointPDFDerivativesRegion.SetIndex(jointPDFDerivativesIndex);
        jointPDFDerivativesRegion.SetSize(jointPDFDerivativesSize);
        }

      associate->m_ThreaderJointPDFDerivatives.resize(this->GetNumberOfThreadsUsed());
      // Set the regions and allocate
      for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
        {
        associate->m_ThreaderJointPDFDerivatives[threadID] = JointPDFDerivativesType::New();
        associate->m_ThreaderJointPDFDerivatives[threadID]->SetRegions( jointPDFDerivativesRegion);
        associate->m_ThreaderJointPDFDerivatives[threadID]->Allocate();
        }
      }
    }
  else
    {
    // Deallocate the memory that may have been allocated for
    // previous runs of the metric.
    associate->m_ThreaderJointPDFDerivatives.resize(0); // Not needed if this->m_UseExplicitPDFDerivatives=false

    /** Allocate memory for helper array that will contain the pRatios
     *  for each bin of the joint histogram. This is part of the effort
     *  for flattening the computation of the PDF Jacobians.
     */
    associate->m_PRatioArray.SetSize(associate->m_NumberOfHistogramBins, associate->m_NumberOfHistogramBins);
    associate->m_PRatioArray.Fill(0.0);
    associate->m_ThreaderMetricDerivative.resize(this->GetNumberOfThreadsUsed(), DerivativeType( associate->GetNumberOfParameters() ) );
    for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
      {
      associate->m_ThreaderMetricDerivative[threadID].Fill(NumericTraits<MeasureType>::Zero);
      }
    }
  /**
   * Setup the kernels used for the Parzen windows.
   */
  associate->m_CubicBSplineKernel = CubicBSplineFunctionType::New();
  associate->m_CubicBSplineDerivativeKernel = CubicBSplineDerivativeFunctionType::New();

  /* This block of code is from MattesMutualImageToImageMetric::GetValueAndDerivativeThreadPreProcess */

  for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
    {
    std::fill(
      associate->m_ThreaderFixedImageMarginalPDF[threadID].begin(),
      associate->m_ThreaderFixedImageMarginalPDF[threadID].end(), 0.0F);
    associate->m_ThreaderJointPDF[threadID]->FillBuffer(0.0F);
    if( associate->m_UseExplicitPDFDerivatives )
      {
      associate->m_ThreaderJointPDFDerivatives[threadID]->FillBuffer(0.0F);
      }
    }
}


template< class TDomainPartitioner, class TImageToImageMetric, class TMattesMutualInformationMetric >
bool
MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TMattesMutualInformationMetric >
::ProcessPoint( const VirtualPointType &,
                const FixedImagePointType &        fixedImagePoint,
                const FixedImagePixelType &        fixedImageValue,
                const FixedImageGradientType &,
                const MovingImagePointType &,
                const MovingImagePixelType &       movingImageValue,
                const MovingImageGradientType &    movingImageGradient,
                MeasureType &,
                DerivativeType &,
                const ThreadIdType                 threadID) const
{
  /* Convenience assignment */
  TMattesMutualInformationMetric * associate = dynamic_cast<TMattesMutualInformationMetric*>(this->m_Associate);

  /**
   * Compute this sample's contribution to the marginal
   *   and joint distributions.
   *
   */
  if( movingImageValue < associate->m_MovingImageTrueMin )
    {
    return false;
    }
  else if( movingImageValue > associate->m_MovingImageTrueMax )
    {
    return false;
    }

  // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
  PDFValueType movingImageParzenWindowTerm = movingImageValue / associate->m_MovingImageBinSize - associate->m_MovingImageNormalizedMin;
  OffsetValueType movingImageParzenWindowIndex = static_cast<OffsetValueType>( movingImageParzenWindowTerm );

  // Make sure the extreme values are in valid bins
  if( movingImageParzenWindowIndex < 2 )
    {
    movingImageParzenWindowIndex = 2;
    }
  else
    {
    const OffsetValueType nindex =
      static_cast<OffsetValueType>( associate->m_NumberOfHistogramBins ) - 3;
    if( movingImageParzenWindowIndex > nindex )
      {
      movingImageParzenWindowIndex = nindex;
      }
    }
  // Move the pointer to the fist affected bin
  int pdfMovingIndex = static_cast<int>( movingImageParzenWindowIndex ) - 1;
  const int pdfMovingIndexMax = static_cast<int>( movingImageParzenWindowIndex ) + 2;

  const OffsetValueType fixedImageParzenWindowIndex = associate->ComputeSingleFixedImageParzenWindowIndex( fixedImageValue );

  // Since a zero-order BSpline (box car) kernel is used for
  // the fixed image marginal pdf, we need only increment the
  // fixedImageParzenWindowIndex by value of 1.0.
  associate->m_ThreaderFixedImageMarginalPDF[threadID][fixedImageParzenWindowIndex] += 1;

  /**
    * The region of support of the parzen window determines which bins
    * of the joint PDF are effected by the pair of image values.
    * Since we are using a cubic spline for the moving image parzen
    * window, four bins are effected.  The fixed image parzen window is
    * a zero-order spline (box car) and thus effects only one bin.
    *
    *  The PDF is arranged so that moving image bins corresponds to the
    * zero-th (column) dimension and the fixed image bins corresponds
    * to the first (row) dimension.
    *
    */
  PDFValueType movingImageParzenWindowArg = static_cast<PDFValueType>( pdfMovingIndex ) - static_cast<PDFValueType>( movingImageParzenWindowTerm );

  // Pointer to affected bin to be updated
  JointPDFValueType *pdfPtr = associate->m_ThreaderJointPDF[threadID]->GetBufferPointer()
    + ( fixedImageParzenWindowIndex * associate->m_NumberOfHistogramBins )
    + pdfMovingIndex;

  while( pdfMovingIndex <= pdfMovingIndexMax )
    {
    PDFValueType val = static_cast<PDFValueType>( associate->m_CubicBSplineKernel ->Evaluate( movingImageParzenWindowArg) );
    *( pdfPtr++ ) += val; 

    if( associate->m_UseExplicitPDFDerivatives || associate->m_ImplicitDerivativesSecondPass )
      {
      // Compute the cubicBSplineDerivative for later repeated use.
      const PDFValueType cubicBSplineDerivativeValue = associate->m_CubicBSplineDerivativeKernel->Evaluate(movingImageParzenWindowArg);

      // Compute PDF derivative contribution.
      this->ComputePDFDerivatives(threadID,
                                  fixedImageParzenWindowIndex,
                                  fixedImagePoint,
                                  pdfMovingIndex,
                                  movingImageGradient,
                                  cubicBSplineDerivativeValue);
      }

    movingImageParzenWindowArg += 1.0;
    ++pdfMovingIndex;
    }

  /* Count the point and return false to prevent the calling method from trying to save
   * other results. This class uses its own members to save results and
   * assigns them to final return variables in GetValueAndDerivative. */
  this->m_NumberOfValidPointsPerThread[threadID]++;
  
  return false;
}

/**
 * ComputePDFDerivative
 */
template< class TDomainPartitioner, class TImageToImageMetric, class TMattesMutualInformationMetric >
void
MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TMattesMutualInformationMetric >
::ComputePDFDerivatives(const ThreadIdType              threadID,
                        const OffsetValueType           fixedImageParzenWindowIndex,
                        const FixedImagePointType       fixedImagePoint,
                        const int                       pdfMovingIndex,
                        const MovingImageGradientType & movingImageGradient,
                        const PDFValueType              cubicBSplineDerivativeValue) const
{
  /* Convenience assignment */
  TMattesMutualInformationMetric * associate = dynamic_cast<TMattesMutualInformationMetric*>(this->m_Associate);

  // Update bins in the PDF derivatives for the current intensity pair
  // Could pre-compute

  PDFValueType precomputedWeight = 0.0;

  const int pdfFixedIndex = fixedImageParzenWindowIndex; //this->m_FixedImageSamples[sampleNumber].valueIndex;

  JointPDFDerivativesValueType *derivPtr=0;
  if( associate->m_UseExplicitPDFDerivatives )
    {
    derivPtr = associate->m_ThreaderJointPDFDerivatives[threadID]->GetBufferPointer()
      + ( pdfFixedIndex  * associate->m_ThreaderJointPDFDerivatives[threadID]->GetOffsetTable()[2] )
      + ( pdfMovingIndex * associate->m_ThreaderJointPDFDerivatives[threadID]->GetOffsetTable()[1] );
    }
  else
    {
    // Recover the precomputed weight for this specific PDF bin
    precomputedWeight = associate->m_PRatioArray[pdfFixedIndex][pdfMovingIndex];
    }

  // Compute the transform Jacobian.
  // Should pre-compute
  /* Use a pre-allocated jacobian object for efficiency */
  typedef JacobianType & JacobianReferenceType;
  JacobianReferenceType jacobian = this->m_MovingTransformJacobianPerThread[threadID];
  /** For dense transforms, this returns identity */
  associate->GetMovingTransform()->ComputeJacobianWithRespectToParameters( fixedImagePoint, jacobian);
  for( unsigned int mu = 0; mu < associate->GetNumberOfParameters(); mu++ )
    {
    PDFValueType innerProduct = 0.0;
    for( unsigned int dim = 0; dim < associate->FixedImageDimension; dim++ )
      {
      innerProduct += jacobian[dim][mu] * movingImageGradient[dim];
      }

    const PDFValueType derivativeContribution = innerProduct * cubicBSplineDerivativeValue;

    if( associate->m_UseExplicitPDFDerivatives )
      {
      *( derivPtr ) -= derivativeContribution;
      ++derivPtr;
      }
    else
      {
      associate->m_ThreaderMetricDerivative[threadID][mu] += precomputedWeight * derivativeContribution;
      }
    }
}

template< class TDomainPartitioner, class TImageToImageMetric, class TMattesMutualInformationMetric >
void
MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TMattesMutualInformationMetric >
::AfterThreadedExecution()
{
   /* Convenience assignment */
  TMattesMutualInformationMetric * associate = dynamic_cast<TMattesMutualInformationMetric*>(this->m_Associate);

  /* Porting: This code is from
   * MattesMutualInformationImageToImageMetric::GetValueAndDerivativeThreadPostProcess */

  /* Note: skipping this because it sums things we're not using. */
  /* Superclass::AfterThreadedExecution */

  /* Store the number of valid points in the enclosing class
   * m_NumberOfValidPoints by collecting the valid points per thread.
   * We do this here because we're skipping Superclass::AfterThreadedExecution*/
  associate->m_NumberOfValidPoints = NumericTraits< SizeValueType >::Zero;
  for (ThreadIdType i=0; i<this->GetNumberOfThreadsUsed(); i++)
    {
    associate->m_NumberOfValidPoints += this->m_NumberOfValidPointsPerThread[i];
    }

  /* Post-processing that is common the GetValue and GetValueAndDerivative */
  associate->GetValueCommonAfterThreadedExecution();

  /* This should be done via its own threader class. Previously it was threaded using
   * old ImageToImageMetric threaded post-processing. */
  for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
    {
    if( associate->m_UseExplicitPDFDerivatives )
      {
      const unsigned int rowSize = associate->GetNumberOfParameters() * associate->m_NumberOfHistogramBins;

      const unsigned int maxI =
        rowSize * ( associate->m_ThreaderJointPDFEndBin[threadID]
                    - associate->m_ThreaderJointPDFStartBin[threadID] + 1 );

      JointPDFDerivativesValueType *const pdfDPtrStart = associate->m_ThreaderJointPDFDerivatives[0]->GetBufferPointer()
        + ( associate->m_ThreaderJointPDFStartBin[threadID] * rowSize );
      const unsigned int tPdfDPtrOffset = associate->m_ThreaderJointPDFStartBin[threadID] *  rowSize;
      for( unsigned int t = 1; t < this->GetNumberOfThreadsUsed(); t++ )
        {
        JointPDFDerivativesValueType *      pdfDPtr = pdfDPtrStart;
        JointPDFDerivativesValueType const *tPdfDPtr = associate->m_ThreaderJointPDFDerivatives[t]->GetBufferPointer()
          + tPdfDPtrOffset;
        JointPDFDerivativesValueType const * const tPdfDPtrEnd = tPdfDPtr + maxI;
        // for(i = 0; i < maxI; i++)
        while( tPdfDPtr < tPdfDPtrEnd )
          {
          *( pdfDPtr++ ) += *( tPdfDPtr++ );
          }
        }

      const PDFValueType nFactor = 1.0 / ( associate->m_MovingImageBinSize
                                     * associate->m_NumberOfValidPoints );

      JointPDFDerivativesValueType *             pdfDPtr = pdfDPtrStart;
      JointPDFDerivativesValueType const * const tPdfDPtrEnd = pdfDPtrStart + maxI;
      // for(int i = 0; i < maxI; i++)
      while( pdfDPtr < tPdfDPtrEnd )
        {
        *( pdfDPtr++ ) *= nFactor;
        }
      }
    }
}

} // end namespace itk

#endif
