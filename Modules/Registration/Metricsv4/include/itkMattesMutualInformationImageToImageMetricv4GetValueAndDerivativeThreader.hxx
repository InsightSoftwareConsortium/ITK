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

  /* Allocates and inits per-thread members.
   * We need a couple of these and the rest will be ignored. */
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

  associate->m_ThreaderJointPDFStartBin.resize( this->GetNumberOfThreadsUsed() );
  associate->m_ThreaderJointPDFEndBin.resize(this->GetNumberOfThreadsUsed() );
  const OffsetValueType binRange = associate->m_NumberOfHistogramBins / this->GetNumberOfThreadsUsed();
  for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
    {
    associate->m_ThreaderJointPDFStartBin[threadID] = threadID * binRange;
    associate->m_ThreaderJointPDFEndBin[threadID] = ( threadID + 1 ) * binRange - 1;
    }
  // Ensure that the last EndBin range contains the last histogram bin
  associate->m_ThreaderJointPDFStartBin[this->GetNumberOfThreadsUsed() - 1] = ( this->GetNumberOfThreadsUsed() - 1 ) * binRange;
  associate->m_ThreaderJointPDFEndBin[this->GetNumberOfThreadsUsed() - 1] = associate->m_NumberOfHistogramBins - 1;

  associate->m_ThreaderJointPDFSum.resize(this->GetNumberOfThreadsUsed());

  JointPDFRegionType jointPDFRegion;
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

  //
  // Now allocate memory according to transform type
  //
  if( associate->HasLocalSupport() )
    {
    associate->m_PRatioArray.assign(associate->m_NumberOfHistogramBins * associate->m_NumberOfHistogramBins, 0.0);
    associate->m_JointPdfIndex1DArray.assign( associate->GetNumberOfParameters(), 0 );
    // Don't need this with local-support
    associate->m_ThreaderJointPDFDerivatives.resize(0);
    // This always has four entries because the parzen window size is fixed.
    associate->m_LocalDerivativeByParzenBin.resize(4);
    // The first container cannot point to the existing derivative result object
    // for efficiency, because of multi-variate metric.
    for( SizeValueType n = 0; n<4; n++ )
      {
      associate->m_LocalDerivativeByParzenBin[n].SetSize( associate->GetNumberOfParameters() );
      // Initialize to zero because we accumulate, and so skipped points will behave properly
      associate->m_LocalDerivativeByParzenBin[n].Fill( NumericTraits< DerivativeValueType >::Zero );
      }
    }
  else
    {
    // Don't need this with global transforms
    associate->m_PRatioArray.resize(0);
    associate->m_JointPdfIndex1DArray.resize(0);
    associate->m_LocalDerivativeByParzenBin.resize(0);

    JointPDFDerivativesRegionType jointPDFDerivativesRegion;

    // For the derivatives of the joint PDF define a region starting from
    // {0,0,0}
    // with size {m_NumberOfParameters,m_NumberOfHistogramBins,
    // this->m_NumberOfHistogramBins}. The dimension represents transform parameters,
    // fixed image parzen window index and moving image parzen window index,
    // respectively.
    JointPDFDerivativesIndexType jointPDFDerivativesIndex;
    jointPDFDerivativesIndex.Fill(0);
    JointPDFDerivativesSizeType jointPDFDerivativesSize;
    jointPDFDerivativesSize[0] = associate->GetNumberOfLocalParameters();
    jointPDFDerivativesSize[1] = associate->m_NumberOfHistogramBins;
    jointPDFDerivativesSize[2] = associate->m_NumberOfHistogramBins;

    jointPDFDerivativesRegion.SetIndex(jointPDFDerivativesIndex);
    jointPDFDerivativesRegion.SetSize(jointPDFDerivativesSize);

    associate->m_ThreaderJointPDFDerivatives.resize(this->GetNumberOfThreadsUsed());
    // Set the regions and allocate
    for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
      {
      associate->m_ThreaderJointPDFDerivatives[threadID] = JointPDFDerivativesType::New();
      associate->m_ThreaderJointPDFDerivatives[threadID]->SetRegions( jointPDFDerivativesRegion);
      associate->m_ThreaderJointPDFDerivatives[threadID]->Allocate();
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
    if( ! associate->HasLocalSupport() )
      {
      associate->m_ThreaderJointPDFDerivatives[threadID]->FillBuffer(0.0F);
      }
    }
}


template< class TDomainPartitioner, class TImageToImageMetric, class TMattesMutualInformationMetric >
bool
MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TMattesMutualInformationMetric >
::ProcessPoint( const VirtualIndexType &           virtualIndex,
                const VirtualPointType &           virtualPoint,
                const FixedImagePointType &,
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
  OffsetValueType pdfMovingIndex = static_cast<OffsetValueType>( movingImageParzenWindowIndex ) - 1;
  const OffsetValueType pdfMovingIndexMax = static_cast<OffsetValueType>( movingImageParzenWindowIndex ) + 2;

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
    */
  PDFValueType movingImageParzenWindowArg = static_cast<PDFValueType>( pdfMovingIndex ) - static_cast<PDFValueType>( movingImageParzenWindowTerm );

  // Pointer to affected bin to be updated
  JointPDFValueType *pdfPtr = associate->m_ThreaderJointPDF[threadID]->GetBufferPointer()
    + ( fixedImageParzenWindowIndex * associate->m_NumberOfHistogramBins )
    + pdfMovingIndex;

  OffsetValueType localDerivativeOffset = 0;
  // Store the pdf indecies for this point.
  // Just store the starting pdfMovingIndex and we'll iterate later
  // over the next four to collect results.
  if( associate->HasLocalSupport() )
    {
    OffsetValueType jointPdfIndex1D = pdfMovingIndex + (fixedImageParzenWindowIndex * associate->m_NumberOfHistogramBins);
    localDerivativeOffset = associate->ComputeParameterOffsetFromVirtualDomainIndex( virtualIndex, associate->GetNumberOfLocalParameters() );
    for (NumberOfParametersType i=0; i < associate->GetNumberOfLocalParameters(); i++)
      {
      associate->m_JointPdfIndex1DArray[localDerivativeOffset + i] = jointPdfIndex1D;
      }
    }

  // Compute the transform Jacobian.
  typedef JacobianType & JacobianReferenceType;
  JacobianReferenceType jacobian = this->m_MovingTransformJacobianPerThread[threadID];
  associate->GetMovingTransform()->ComputeJacobianWithRespectToParameters( virtualPoint, jacobian);

  SizeValueType movingParzenBin = 0;

  while( pdfMovingIndex <= pdfMovingIndexMax )
    {
    PDFValueType val = static_cast<PDFValueType>( associate->m_CubicBSplineKernel ->Evaluate( movingImageParzenWindowArg) );
    *( pdfPtr++ ) += val;

    // Compute the cubicBSplineDerivative for later repeated use.
    const PDFValueType cubicBSplineDerivativeValue = associate->m_CubicBSplineDerivativeKernel->Evaluate(movingImageParzenWindowArg);

    // Pointer to local derivative partial result container.
    // Not used with global support transforms.
    DerivativeValueType * localSupportDerivativeResultPtr = NULL;

    if( associate->HasLocalSupport() )
      {
      // ptr to where the derivative result should go, for efficiency
      localSupportDerivativeResultPtr = &( associate->m_LocalDerivativeByParzenBin[movingParzenBin][localDerivativeOffset] );
      }

    // Compute PDF derivative contribution.
    this->ComputePDFDerivatives(threadID,
                                fixedImageParzenWindowIndex,
                                jacobian,
                                pdfMovingIndex,
                                movingImageGradient,
                                cubicBSplineDerivativeValue,
                                localSupportDerivativeResultPtr);

    movingImageParzenWindowArg += 1.0;
    ++pdfMovingIndex;
    ++movingParzenBin;
    }

  // have to do this here since we're returning false
  this->m_NumberOfValidPointsPerThread[threadID]++;

  // Return false to avoid the storage of results in parent class.
  return false;
}

/**
 * ComputePDFDerivative
 */
template< class TDomainPartitioner, class TImageToImageMetric, class TMattesMutualInformationMetric >
void
MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TMattesMutualInformationMetric >
::ComputePDFDerivatives(const ThreadIdType &            threadID,
                        const OffsetValueType &         fixedImageParzenWindowIndex,
                        const JacobianType &            jacobian,
                        const OffsetValueType &         pdfMovingIndex,
                        const MovingImageGradientType & movingImageGradient,
                        const PDFValueType &            cubicBSplineDerivativeValue,
                        DerivativeValueType *           localSupportDerivativeResultPtr) const
{
  /* Convenience assignment */
  TMattesMutualInformationMetric * associate = dynamic_cast<TMattesMutualInformationMetric*>(this->m_Associate);

  // Update bins in the PDF derivatives for the current intensity pair

  const OffsetValueType pdfFixedIndex = fixedImageParzenWindowIndex;

  JointPDFDerivativesValueType *derivPtr=0;
  if( ! associate->HasLocalSupport() )
    {
    derivPtr = associate->m_ThreaderJointPDFDerivatives[threadID]->GetBufferPointer()
      + ( pdfFixedIndex  * associate->m_ThreaderJointPDFDerivatives[threadID]->GetOffsetTable()[2] )
      + ( pdfMovingIndex * associate->m_ThreaderJointPDFDerivatives[threadID]->GetOffsetTable()[1] );
    }

  for( NumberOfParametersType mu = 0; mu < associate->GetNumberOfLocalParameters(); mu++ )
    {
    PDFValueType innerProduct = 0.0;
    for( SizeValueType dim = 0; dim < associate->MovingImageDimension; dim++ )
      {
      innerProduct += jacobian[dim][mu] * movingImageGradient[dim];
      }

    const PDFValueType derivativeContribution = innerProduct * cubicBSplineDerivativeValue;
    if( associate->HasLocalSupport() )
      {
      *( localSupportDerivativeResultPtr ) += derivativeContribution;
      localSupportDerivativeResultPtr++;
      }
    else
      {
      *( derivPtr ) -= derivativeContribution;
      ++derivPtr;
      }
    }
}

template< class TDomainPartitioner, class TImageToImageMetric, class TMattesMutualInformationMetric >
void
MattesMutualInformationImageToImageMetricv4GetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TMattesMutualInformationMetric >
::AfterThreadedExecution()
{
  TMattesMutualInformationMetric * associate = dynamic_cast<TMattesMutualInformationMetric*>(this->m_Associate);

  /* Store the number of valid points.
   * This is the only code we need from Superclass::AfterThreadedExecution */
  associate->m_NumberOfValidPoints = NumericTraits< SizeValueType >::Zero;
  for (ThreadIdType i=0; i<this->GetNumberOfThreadsUsed(); i++)
    {
    associate->m_NumberOfValidPoints += this->m_NumberOfValidPointsPerThread[i];
    }

  /* Porting: This code is from
   * MattesMutualInformationImageToImageMetric::GetValueAndDerivativeThreadPostProcess */

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
  if( ! associate->HasLocalSupport() )
    {
    for( ThreadIdType threadID = 0; threadID < this->GetNumberOfThreadsUsed(); threadID++ )
      {
      const NumberOfParametersType rowSize = associate->GetNumberOfLocalParameters() * associate->m_NumberOfHistogramBins;

      const SizeValueType maxI =
        rowSize * ( associate->m_ThreaderJointPDFEndBin[threadID]
                    - associate->m_ThreaderJointPDFStartBin[threadID] + 1 );

      JointPDFDerivativesValueType *const pdfDPtrStart = associate->m_ThreaderJointPDFDerivatives[0]->GetBufferPointer()
        + ( associate->m_ThreaderJointPDFStartBin[threadID] * rowSize );
      const SizeValueType tPdfDPtrOffset = associate->m_ThreaderJointPDFStartBin[threadID] *  rowSize;
      for( SizeValueType t = 1; t < this->GetNumberOfThreadsUsed(); t++ )
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
                                     * associate->GetNumberOfValidPoints() );

      JointPDFDerivativesValueType *             pdfDPtr = pdfDPtrStart;
      JointPDFDerivativesValueType const * const tPdfDPtrEnd = pdfDPtrStart + maxI;
      while( pdfDPtr < tPdfDPtrEnd )
        {
        *( pdfDPtr++ ) *= nFactor;
        }
      }
    }
}

} // end namespace itk

#endif
