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
#ifndef itkMattesMutualInformationImageToImageMetric_hxx
#define itkMattesMutualInformationImageToImageMetric_hxx

#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkImageRegionIterator.h"
#include "itkImageIterator.h"
#include "itkMath.h"
#include "itkStatisticsImageFilter.h"

#include "vnl/vnl_vector.h"
#include "vnl/vnl_c_vector.h"

namespace itk
{
/**
 * Constructor
 */
template <typename TFixedImage, typename TMovingImage>
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::MattesMutualInformationImageToImageMetric() :
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

  m_PRatioArray(0,0),

  // Initialize memory
  m_MovingImageMarginalPDF(0),

  m_MMIMetricPerThreadVariables(ITK_NULLPTR),

  m_UseExplicitPDFDerivatives(true),
  m_ImplicitDerivativesSecondPass(false)
{
  this->SetComputeGradient(false); // don't use the default gradient for now
  this->m_WithinThreadPreProcess = true;
  this->m_WithinThreadPostProcess = false;
  this->m_ComputeGradient = false;
}

template <typename TFixedImage, typename TMovingImage>
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::~MattesMutualInformationImageToImageMetric()
{
  delete[] this->m_MMIMetricPerThreadVariables;
}

template <typename TFixedImage, typename TMovingImage>
void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfHistogramBins: ";
  os << this->m_NumberOfHistogramBins << std::endl;

  // Debugging information
  os << indent << "FixedImageNormalizedMin: ";
  os << this->m_FixedImageNormalizedMin << std::endl;
  os << indent << "MovingImageNormalizedMin: ";
  os << this->m_MovingImageNormalizedMin << std::endl;
  os << indent << "MovingImageTrueMin: ";
  os << this->m_MovingImageTrueMin << std::endl;
  os << indent << "MovingImageTrueMax: ";
  os << this->m_MovingImageTrueMax << std::endl;
  os << indent << "FixedImageBinSize: ";
  os << this->m_FixedImageBinSize << std::endl;
  os << indent << "MovingImageBinSize: ";
  os << this->m_MovingImageBinSize << std::endl;
  os << indent << "UseExplicitPDFDerivatives: ";
  os << this->m_UseExplicitPDFDerivatives << std::endl;
  os << indent << "ImplicitDerivativesSecondPass: ";
  os << this->m_ImplicitDerivativesSecondPass << std::endl;
  if( this->m_MMIMetricPerThreadVariables && this->m_MMIMetricPerThreadVariables[0].JointPDF.IsNotNull() )
    {
    os << indent << "JointPDF: ";
    os << this->m_MMIMetricPerThreadVariables[0].JointPDF << std::endl;
    }
  if( this->m_MMIMetricPerThreadVariables && this->m_MMIMetricPerThreadVariables[0].JointPDFDerivatives.IsNotNull() )
    {
    os << indent << "JointPDFDerivatives: ";
    os << this->m_MMIMetricPerThreadVariables[0].JointPDFDerivatives;
    }
}

template <typename TFixedImage, typename TMovingImage>
void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::Initialize(void)
{
  this->Superclass::Initialize();
  this->Superclass::MultiThreadingInitialize();
    {
    /**
     * Compute the minimum and maximum within the specified mask
     * region for creating the size of the 2D joint histogram.
     * Areas outside the masked region should be ignored
     * in computing the range of intensity values.
     */

    this->m_FixedImageTrueMin = NumericTraits<typename TFixedImage::PixelType>::max();
    this->m_FixedImageTrueMax = NumericTraits<typename TFixedImage::PixelType>::NonpositiveMin();
    this->m_MovingImageTrueMin = NumericTraits<typename TMovingImage::PixelType>::max();
    this->m_MovingImageTrueMax = NumericTraits<typename TMovingImage::PixelType>::NonpositiveMin();

    // We need to make robust measures only over the requested mask region
    itk::ImageRegionConstIteratorWithIndex<TFixedImage> fi(this->m_FixedImage, this->m_FixedImage->GetBufferedRegion() );
    const bool fixedMaskIsPresent = !(this->m_FixedImageMask.IsNull());
      if( fixedMaskIsPresent )
        {
        typename TFixedImage::PointType fixedSpacePhysicalPoint;
        while( !fi.IsAtEnd() )
          {
          this->m_FixedImage->TransformIndexToPhysicalPoint(fi.GetIndex(), fixedSpacePhysicalPoint);
          const bool shouldCheckPixelIntensity = this->m_FixedImageMask->IsInside( fixedSpacePhysicalPoint  );
          if( shouldCheckPixelIntensity )
            {
            const PDFValueType & currValue = fi.Get();
            this->m_FixedImageTrueMin = std::min(m_FixedImageTrueMin, currValue);
            this->m_FixedImageTrueMax = std::max(m_FixedImageTrueMax, currValue);
            }
          ++fi;
          }
        }
      else
        {
        while( !fi.IsAtEnd() )
          {
          const PDFValueType & currValue = fi.Get();
          this->m_FixedImageTrueMin = std::min(m_FixedImageTrueMin, currValue);
          this->m_FixedImageTrueMax = std::max(m_FixedImageTrueMax, currValue);
          ++fi;
          }
        }

      {
      itk::ImageRegionConstIteratorWithIndex<TMovingImage> mi(this->m_MovingImage,
                                                              this->m_MovingImage->GetBufferedRegion() );
      const bool movingMaskIsPresent = !(this->m_MovingImageMask.IsNull());
      if( movingMaskIsPresent )
        {
        typename TMovingImage::PointType movingSpacePhysicalPoint;
        while( !mi.IsAtEnd() )
          {
          this->m_MovingImage->TransformIndexToPhysicalPoint(mi.GetIndex(), movingSpacePhysicalPoint);
          const bool shouldCheckPixelIntensity = this->m_MovingImageMask->IsInside(movingSpacePhysicalPoint);
          if( shouldCheckPixelIntensity )
            {
            const PDFValueType & currValue = mi.Get();
            this->m_MovingImageTrueMin = std::min(m_MovingImageTrueMin, currValue);
            this->m_MovingImageTrueMax = std::max(m_MovingImageTrueMax, currValue);
            }
          ++mi;
          }
        }
      else
        {
        while( !mi.IsAtEnd() )
          {
          const PDFValueType & currValue = mi.Get();
          this->m_MovingImageTrueMin = std::min(m_MovingImageTrueMin, currValue);
          this->m_MovingImageTrueMax = std::max(m_MovingImageTrueMax, currValue);
          ++mi;
          }
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
  this->m_FixedImageNormalizedMin = this->m_FixedImageTrueMin / this->m_FixedImageBinSize -
    static_cast<PDFValueType>( padding );

  this->m_MovingImageBinSize = ( this->m_MovingImageTrueMax - this->m_MovingImageTrueMin )
    / static_cast<PDFValueType>( this->m_NumberOfHistogramBins - 2 * padding );
  this->m_MovingImageNormalizedMin = this->m_MovingImageTrueMin / this->m_MovingImageBinSize -
    static_cast<PDFValueType>( padding );

  itkDebugMacro("FixedImageNormalizedMin: " << this->m_FixedImageNormalizedMin);
  itkDebugMacro("MovingImageNormalizedMin: " << this->m_MovingImageNormalizedMin);
  itkDebugMacro("FixedImageBinSize: " << this->m_FixedImageBinSize);
  itkDebugMacro("MovingImageBinSize; " << this->m_MovingImageBinSize);

  /**
   * Allocate memory for the marginal PDF and initialize values
   * to zero. The marginal PDFs are stored as std::vector.
   */
  this->m_MovingImageMarginalPDF.resize(m_NumberOfHistogramBins, 0.0F);

  delete[] this->m_MMIMetricPerThreadVariables;
  this->m_MMIMetricPerThreadVariables = new AlignedMMIMetricPerThreadStruct[ this->m_NumberOfThreads ];

    {
    const int binRange = this->m_NumberOfHistogramBins / this->m_NumberOfThreads;
    for( ThreadIdType threadId = 0; threadId < this->m_NumberOfThreads; threadId++ )
      {
      this->m_MMIMetricPerThreadVariables[threadId].JointPDFStartBin = threadId * binRange;
      this->m_MMIMetricPerThreadVariables[threadId].JointPDFEndBin = ( threadId + 1 ) * binRange - 1;

      }
    // Ensure that the last EndBin range contains the last histogram bin
    this->m_MMIMetricPerThreadVariables[this->m_NumberOfThreads -
                                        1].JointPDFStartBin = ( this->m_NumberOfThreads - 1 ) * binRange;
    this->m_MMIMetricPerThreadVariables[this->m_NumberOfThreads - 1].JointPDFEndBin = this->m_NumberOfHistogramBins - 1;

    }

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
      jointPDFSize.Fill(m_NumberOfHistogramBins);

      jointPDFRegion.SetIndex(jointPDFIndex);
      jointPDFRegion.SetSize(jointPDFSize);
      }

    // By setting these values, the joint histogram physical locations will
    // correspond to intensity values.
    typename JointPDFType::PointType origin;
    origin[0] = this->m_FixedImageTrueMin;
    origin[1] = this->m_MovingImageTrueMin;
    typename JointPDFType::SpacingType spacing;
    spacing[0] = this->m_FixedImageBinSize;
    spacing[1] = this->m_MovingImageBinSize;
    /**
     * Allocate memory for the joint PDF and joint PDF derivatives.
     * The joint PDF and joint PDF derivatives are store as itk::Image.
     */
    for( ThreadIdType threadId = 0; threadId < this->m_NumberOfThreads; ++threadId )
      {
      this->m_MMIMetricPerThreadVariables[threadId].JointPDF = JointPDFType::New();
      this->m_MMIMetricPerThreadVariables[threadId].JointPDF->SetRegions(jointPDFRegion);
      this->m_MMIMetricPerThreadVariables[threadId].JointPDF->SetOrigin(origin);
      this->m_MMIMetricPerThreadVariables[threadId].JointPDF->SetSpacing(spacing);
      this->m_MMIMetricPerThreadVariables[threadId].JointPDF->Allocate();
      }
    }

  //
  // Now allocate memory according to the user-selected method.
  //
  if( this->m_UseExplicitPDFDerivatives )
    {
    // Deallocate the memory that may have been allocated for
    // previous runs of the metric.
    // and by allocating very small the static ones

    // Not needed if this->m_UseExplicitPDFDerivatives
    this->m_PRatioArray.SetSize(0, 0);
      {
      JointPDFDerivativesRegionType jointPDFDerivativesRegion;
        {
        // For the derivatives of the joint PDF define a region
        // starting from {0,0,0} with size
        // {m_NumberOfParameters,m_NumberOfHistogramBins,
        // this->m_NumberOfHistogramBins}. The dimension represents
        // transform parameters, fixed image parzen window index and
        // moving image parzen window index, respectively.
        JointPDFDerivativesIndexType jointPDFDerivativesIndex;
        jointPDFDerivativesIndex.Fill(0);
        JointPDFDerivativesSizeType jointPDFDerivativesSize;
        jointPDFDerivativesSize[0] = this->m_NumberOfParameters;
        jointPDFDerivativesSize[1] = this->m_NumberOfHistogramBins;
        jointPDFDerivativesSize[2] = this->m_NumberOfHistogramBins;

        jointPDFDerivativesRegion.SetIndex(jointPDFDerivativesIndex);
        jointPDFDerivativesRegion.SetSize(jointPDFDerivativesSize);
        }

      // Set the regions and allocate
      for( ThreadIdType threadId = 0; threadId < this->m_NumberOfThreads; threadId++ )
        {
        this->m_MMIMetricPerThreadVariables[threadId].JointPDFDerivatives = JointPDFDerivativesType::New();
        this->m_MMIMetricPerThreadVariables[threadId].JointPDFDerivatives->SetRegions( jointPDFDerivativesRegion);
        this->m_MMIMetricPerThreadVariables[threadId].JointPDFDerivatives->Allocate();
        }
      }
    }
  else
    {
    // Deallocate the memory that may have been allocated for
    // previous runs of the metric.
    for( ThreadIdType threadId = 0; threadId < this->m_NumberOfThreads; ++threadId )
      {
      // Not needed if this->m_UseExplicitPDFDerivatives=false
      this->m_MMIMetricPerThreadVariables[threadId].JointPDFDerivatives = ITK_NULLPTR;
      }

    /** Allocate memory for helper array that will contain the pRatios
     *  for each bin of the joint histogram. This is part of the effort
     *  for flattening the computation of the PDF Jacobians.
     */
    this->m_PRatioArray.SetSize(this->m_NumberOfHistogramBins, this->m_NumberOfHistogramBins);
    this->m_PRatioArray.Fill(0.0);

    for( ThreadIdType threadId = 0; threadId < this->m_NumberOfThreads; threadId++ )
      {
      this->m_MMIMetricPerThreadVariables[threadId].MetricDerivative.SetSize( this->GetNumberOfParameters() );
      this->m_MMIMetricPerThreadVariables[threadId].MetricDerivative.Fill(NumericTraits<MeasureType>::ZeroValue());
      }
    }
  /**
   * Setup the kernels used for the Parzen windows.
   */
  this->m_CubicBSplineKernel = CubicBSplineFunctionType::New();
  this->m_CubicBSplineDerivativeKernel = CubicBSplineDerivativeFunctionType::New();

  /**
   * Pre-compute the fixed image parzen window index for
   * each point of the fixed image sample points list.
   */
  // NOTE:  Need to have computed this->m_FisedImageBinSize here.
  this->ComputeFixedImageParzenWindowIndices(this->m_FixedImageSamples);

}

/**
 * From the pre-computed samples, now
 * fill in the parzen window index locations
 */
template <typename TFixedImage, typename TMovingImage>
void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::ComputeFixedImageParzenWindowIndices( FixedImageSampleContainer & samples)
{
  const typename FixedImageSampleContainer::const_iterator end = samples.end();
  for( typename FixedImageSampleContainer::iterator iter = samples.begin();
       iter != end; ++iter )
    {
    // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
    const PDFValueType windowTerm = static_cast<PDFValueType>( ( *iter ).value )
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

    ( *iter ).valueIndex = pindex;
    }
}

template <typename TFixedImage, typename TMovingImage>
inline void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValueThreadPreProcess(ThreadIdType threadId,
                           bool withinSampleThread) const
{
  this->Superclass::GetValueThreadPreProcess(threadId, withinSampleThread);

  this->m_MMIMetricPerThreadVariables[threadId].JointPDF->FillBuffer(0.0F);

  this->m_MMIMetricPerThreadVariables[threadId].FixedImageMarginalPDF = std::vector<PDFValueType>(
      m_NumberOfHistogramBins, 0.0F);
}

template <typename TFixedImage, typename TMovingImage>
inline bool
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValueThreadProcessSample(ThreadIdType threadId,
                              SizeValueType fixedImageSample,
                              const MovingImagePointType & itkNotUsed(mappedPoint),
                              double movingImageValue) const
{
  /**
   * Compute this sample's contribution to the marginal and
   * joint distributions.
   *
   */

  if( movingImageValue < this->m_MovingImageTrueMin )
    {
    return false;
    }
  else if( movingImageValue > this->m_MovingImageTrueMax )
    {
    return false;
    }

  // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
  const PDFValueType movingImageParzenWindowTerm = movingImageValue
    / this->m_MovingImageBinSize
    - this->m_MovingImageNormalizedMin;
  // Same as floor
  OffsetValueType movingImageParzenWindowIndex =
    static_cast<OffsetValueType>( movingImageParzenWindowTerm );
  if( movingImageParzenWindowIndex < 2 )
    {
    movingImageParzenWindowIndex = 2;
    }
  else
    {
    const OffsetValueType nindex =
      static_cast<OffsetValueType>( this->m_NumberOfHistogramBins ) - 3;
    if( movingImageParzenWindowIndex > nindex )
      {
      movingImageParzenWindowIndex = nindex;
      }
    }

  const unsigned int fixedImageParzenWindowIndex = this->m_FixedImageSamples[fixedImageSample].valueIndex;

  this->m_MMIMetricPerThreadVariables[threadId].FixedImageMarginalPDF[fixedImageParzenWindowIndex] += 1;

  // Pointer to affected bin to be updated
  JointPDFValueType *pdfPtr = this->m_MMIMetricPerThreadVariables[threadId].JointPDF->GetBufferPointer()
    + ( fixedImageParzenWindowIndex * this->m_MMIMetricPerThreadVariables[threadId].JointPDF->GetOffsetTable()[1] );

  // Move the pointer to the first affected bin
  int pdfMovingIndex = static_cast<int>( movingImageParzenWindowIndex ) - 1;
  pdfPtr += pdfMovingIndex;
  const int pdfMovingIndexMax = static_cast<int>( movingImageParzenWindowIndex ) + 2;

  PDFValueType movingImageParzenWindowArg = static_cast<PDFValueType>( pdfMovingIndex ) - movingImageParzenWindowTerm;

  while( pdfMovingIndex <= pdfMovingIndexMax )
    {
    *( pdfPtr++ ) += static_cast<PDFValueType>( this->m_CubicBSplineKernel->Evaluate( movingImageParzenWindowArg) );
    movingImageParzenWindowArg += 1;
    ++pdfMovingIndex;
    }

  return true;
}

template <typename TFixedImage, typename TMovingImage>
inline void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValueThreadPostProcess( ThreadIdType threadId,
                             bool itkNotUsed(withinSampleThread) ) const
{
  const int maxI = this->m_NumberOfHistogramBins
    * ( this->m_MMIMetricPerThreadVariables[threadId].JointPDFEndBin-
        this->m_MMIMetricPerThreadVariables[threadId].JointPDFStartBin + 1 );

  const unsigned int tPdfPtrOffset =
    ( this->m_MMIMetricPerThreadVariables[threadId].JointPDFStartBin *
     this->m_MMIMetricPerThreadVariables[0].JointPDF->GetOffsetTable()[1] );
  JointPDFValueType * const pdfPtrStart = this->m_MMIMetricPerThreadVariables[0].JointPDF->GetBufferPointer() +
    tPdfPtrOffset;

  // The PDF domain is chunked based on thread.  Each thread consolodates
  // independent parts of the PDF.
  for( unsigned int t = 1; t < this->m_NumberOfThreads; t++ )
    {
    JointPDFValueType *       pdfPtr = pdfPtrStart;
    JointPDFValueType const * tPdfPtr = this->m_MMIMetricPerThreadVariables[t].JointPDF->GetBufferPointer() +
      tPdfPtrOffset;
    JointPDFValueType const * const tPdfPtrEnd = tPdfPtr + maxI;
    // for(i=0; i < maxI; i++)
    while( tPdfPtr < tPdfPtrEnd )
      {
      *( pdfPtr++ ) += *( tPdfPtr++ );
      }
    }

  for( int i = this->m_MMIMetricPerThreadVariables[threadId].JointPDFStartBin;
       i <= this->m_MMIMetricPerThreadVariables[threadId].JointPDFEndBin; i++ )
    {
    PDFValueType PDFacc = this->m_MMIMetricPerThreadVariables[0].FixedImageMarginalPDF[i];
    for( unsigned int t = 1; t < this->m_NumberOfThreads; t++ )
      {
      PDFacc += this->m_MMIMetricPerThreadVariables[t].FixedImageMarginalPDF[i];
      }
    this->m_MMIMetricPerThreadVariables[0].FixedImageMarginalPDF[i]  = PDFacc;
    }

  // Sum of this threads domain into the
  // this->m_MMIMetricPerThreadVariables[].JointPDFSum
  // that covers that part of the domain.
  this->m_MMIMetricPerThreadVariables[threadId].JointPDFSum = 0.0;
  JointPDFValueType const * pdfPtr = pdfPtrStart;
  for( int i = 0; i < maxI; i++ )
    {
    this->m_MMIMetricPerThreadVariables[threadId].JointPDFSum += *( pdfPtr++ );
    }

}

template <typename TFixedImage, typename TMovingImage>
typename MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::MeasureType
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValue(const ParametersType & parameters) const
{
  // Set up the parameters in the transform
  this->m_Transform->SetParameters(parameters);

  // MUST BE CALLED TO INITIATE PROCESSING
  this->GetValueMultiThreadedInitiate();

  // MUST BE CALLED TO INITIATE PROCESSING
  this->GetValueMultiThreadedPostProcessInitiate();
  // Consolidate to the first element in the vector
  for( ThreadIdType threadId = 1; threadId < this->m_NumberOfThreads; threadId++ )
    {
    this->m_MMIMetricPerThreadVariables[0].JointPDFSum += this->m_MMIMetricPerThreadVariables[threadId].JointPDFSum;
    }
  if( this->m_MMIMetricPerThreadVariables[0].JointPDFSum < itk::NumericTraits< PDFValueType >::epsilon() )
    {
    itkExceptionMacro("Joint PDF summed to zero\n" << this->m_MMIMetricPerThreadVariables[0].JointPDF );
    }

  std::fill(this->m_MovingImageMarginalPDF.begin(), this->m_MovingImageMarginalPDF.end(), 0.0F);

  //NOTE: Since the m_ThreaderFixedImageMarginalPDF is the sum of mass
  // in the fixed image dimension, accumulating these values gives the
  // same answer as computing the the sum of individual values over
  // the the entire histogram.  IMPORTANT NOTICE: THIS MAKES AN
  // ASSUMPTION OF CONSERVATION OF MASS OF THE BSPLINE SMOOTHING.  The
  // sum of all the values should equal the number of samples being
  // used, since each sample contributes only one sample somewhere in
  // the histogram
  PDFValueType totalMassOfPDF = 0.0;
  for( unsigned int i = 0; i < this->m_NumberOfHistogramBins; i++ )
    {
    totalMassOfPDF += this->m_MMIMetricPerThreadVariables[0].FixedImageMarginalPDF[i];
    }
  const PDFValueType normalizationFactor = 1.0 / this->m_MMIMetricPerThreadVariables[0].JointPDFSum;
  JointPDFValueType *pdfPtr = this->m_MMIMetricPerThreadVariables[0].JointPDF->GetBufferPointer();
  for( unsigned int i = 0; i < this->m_NumberOfHistogramBins; i++ )
    {
    PDFValueType * movingMarginalPtr = &(m_MovingImageMarginalPDF[0]);
    for( unsigned int j = 0; j < this->m_NumberOfHistogramBins; j++ )
      {
      *( pdfPtr ) *= normalizationFactor;
      *( movingMarginalPtr++ ) += *( pdfPtr++ );
      }
    }

  if( this->m_NumberOfPixelsCounted < this->m_NumberOfFixedImageSamples / 16 )
    {
    itkExceptionMacro("Too many samples map outside moving image buffer: "
                      << this->m_NumberOfPixelsCounted << " / "
                      << this->m_NumberOfFixedImageSamples
                      << std::endl);
    }

  // Normalize the fixed image marginal PDF
  if( totalMassOfPDF == 0.0 )
    {
    itkExceptionMacro("Fixed image marginal PDF summed to zero");
    }
  for( unsigned int bin = 0; bin < this->m_NumberOfHistogramBins; bin++ )
    {
    this->m_MMIMetricPerThreadVariables[0].FixedImageMarginalPDF[bin] /= totalMassOfPDF;
    }

  /**
   * Compute the metric by double sumdation over histogram.
   */

  // Setup pointer to point to the first bin
  JointPDFValueType *jointPDFPtr = this->m_MMIMetricPerThreadVariables[0].JointPDF->GetBufferPointer();

  PDFValueType sum = 0.0;
  for( unsigned int fixedIndex = 0;
       fixedIndex < this->m_NumberOfHistogramBins;
       ++fixedIndex )
    {
    const PDFValueType fixedImagePDFValue = this->m_MMIMetricPerThreadVariables[0].FixedImageMarginalPDF[fixedIndex];
    for( unsigned int movingIndex = 0;
         movingIndex < this->m_NumberOfHistogramBins;
         ++movingIndex, jointPDFPtr++ )
      {
      const PDFValueType movingImagePDFValue = this->m_MovingImageMarginalPDF[movingIndex];
      const PDFValueType jointPDFValue = *( jointPDFPtr );

      // check for non-zero bin contribution
      const PDFValueType closeToZero = std::numeric_limits<PDFValueType>::epsilon();
      if( jointPDFValue > closeToZero &&  movingImagePDFValue > closeToZero )
        {
        const PDFValueType pRatio = std::log(jointPDFValue / movingImagePDFValue);
        if( fixedImagePDFValue > closeToZero )
          {
          sum += jointPDFValue * ( pRatio - std::log(fixedImagePDFValue) );
          }
        } // end if-block to check non-zero bin contribution
      }   // end for-loop over moving index
    }     // end for-loop over fixed index

  return static_cast<MeasureType>( -1.0 * sum );
}

template <typename TFixedImage, typename TMovingImage>
inline void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValueAndDerivativeThreadPreProcess( ThreadIdType threadId,
                                         bool itkNotUsed(withinSampleThread) ) const
{
  this->m_MMIMetricPerThreadVariables[threadId].FixedImageMarginalPDF = std::vector<PDFValueType>(
      m_NumberOfHistogramBins, 0.0F);
  this->m_MMIMetricPerThreadVariables[threadId].JointPDF->FillBuffer(0.0F);
  if( this->m_UseExplicitPDFDerivatives )
    {
    this->m_MMIMetricPerThreadVariables[threadId].JointPDFDerivatives->FillBuffer(0.0F);
    }
}

template <typename TFixedImage, typename TMovingImage>
inline bool
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValueAndDerivativeThreadProcessSample(ThreadIdType threadId,
                                           SizeValueType fixedImageSample,
                                           const MovingImagePointType & itkNotUsed(mappedPoint),
                                           double movingImageValue,
                                           const ImageDerivativesType &
                                           movingImageGradientValue) const
{
  /**
   * Compute this sample's contribution to the marginal
   *   and joint distributions.
   *
   */
  if( movingImageValue < this->m_MovingImageTrueMin )
    {
    return false;
    }
  else if( movingImageValue > this->m_MovingImageTrueMax )
    {
    return false;
    }

  // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
  PDFValueType movingImageParzenWindowTerm = movingImageValue / this->m_MovingImageBinSize -
    this->m_MovingImageNormalizedMin;
  OffsetValueType movingImageParzenWindowIndex = static_cast<OffsetValueType>( movingImageParzenWindowTerm );

  // Make sure the extreme values are in valid bins
  if( movingImageParzenWindowIndex < 2 )
    {
    movingImageParzenWindowIndex = 2;
    }
  else
    {
    const OffsetValueType nindex =
      static_cast<OffsetValueType>( this->m_NumberOfHistogramBins ) - 3;
    if( movingImageParzenWindowIndex > nindex )
      {
      movingImageParzenWindowIndex = nindex;
      }
    }
  // Move the pointer to the fist affected bin
  int       pdfMovingIndex = static_cast<int>( movingImageParzenWindowIndex ) - 1;
  const int pdfMovingIndexMax = static_cast<int>( movingImageParzenWindowIndex ) + 2;

  const unsigned int fixedImageParzenWindowIndex = this->m_FixedImageSamples[fixedImageSample].valueIndex;

  // Since a zero-order BSpline (box car) kernel is used for
  // the fixed image marginal pdf, we need only increment the
  // fixedImageParzenWindowIndex by value of 1.0.
  this->m_MMIMetricPerThreadVariables[threadId].FixedImageMarginalPDF[fixedImageParzenWindowIndex] += 1;

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
  PDFValueType movingImageParzenWindowArg = static_cast<PDFValueType>( pdfMovingIndex ) -
    static_cast<PDFValueType>( movingImageParzenWindowTerm );

  // Pointer to affected bin to be updated
  JointPDFValueType *pdfPtr = this->m_MMIMetricPerThreadVariables[threadId].JointPDF->GetBufferPointer()
    + ( fixedImageParzenWindowIndex * this->m_NumberOfHistogramBins )
    + pdfMovingIndex;

  while( pdfMovingIndex <= pdfMovingIndexMax )
    {
    *( pdfPtr++ ) += static_cast<PDFValueType>( this->m_CubicBSplineKernel->Evaluate( movingImageParzenWindowArg) );

    if( this->m_UseExplicitPDFDerivatives || this->m_ImplicitDerivativesSecondPass )
      {
      // Compute the cubicBSplineDerivative for later repeated use.
      const PDFValueType cubicBSplineDerivativeValue = this->m_CubicBSplineDerivativeKernel->Evaluate(
          movingImageParzenWindowArg);

      // Compute PDF derivative contribution.
      this->ComputePDFDerivatives(threadId,
                                  fixedImageSample,
                                  pdfMovingIndex,
                                  movingImageGradientValue,
                                  cubicBSplineDerivativeValue);
      }

    movingImageParzenWindowArg += 1.0;
    ++pdfMovingIndex;
    }

  return true;
}

template <typename TFixedImage, typename TMovingImage>
inline void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValueAndDerivativeThreadPostProcess(ThreadIdType threadId,
                                         bool withinSampleThread) const
{
  this->GetValueThreadPostProcess(threadId, withinSampleThread);

  if( this->m_UseExplicitPDFDerivatives )
    {
    const unsigned int rowSize = this->m_NumberOfParameters * this->m_NumberOfHistogramBins;

    const unsigned int maxI =
      rowSize * ( this->m_MMIMetricPerThreadVariables[threadId].JointPDFEndBin
                  - this->m_MMIMetricPerThreadVariables[threadId].JointPDFStartBin + 1 );

    JointPDFDerivativesValueType *const pdfDPtrStart =
      this->m_MMIMetricPerThreadVariables[0].JointPDFDerivatives->GetBufferPointer()
      + ( this->m_MMIMetricPerThreadVariables[threadId].JointPDFStartBin * rowSize );
    const unsigned int tPdfDPtrOffset = this->m_MMIMetricPerThreadVariables[threadId].JointPDFStartBin *  rowSize;
    for( unsigned int t = 1; t < this->m_NumberOfThreads; t++ )
      {
      JointPDFDerivativesValueType *      pdfDPtr = pdfDPtrStart;
      JointPDFDerivativesValueType const *tPdfDPtr =
        this->m_MMIMetricPerThreadVariables[t].JointPDFDerivatives->GetBufferPointer()
        + tPdfDPtrOffset;
      JointPDFDerivativesValueType const * const tPdfDPtrEnd = tPdfDPtr + maxI;
      // for(i = 0; i < maxI; i++)
      while( tPdfDPtr < tPdfDPtrEnd )
        {
        *( pdfDPtr++ ) += *( tPdfDPtr++ );
        }
      }

    const PDFValueType nFactor = 1.0 / ( this->m_MovingImageBinSize
                                         * this->m_NumberOfPixelsCounted );

    JointPDFDerivativesValueType *             pdfDPtr = pdfDPtrStart;
    JointPDFDerivativesValueType const * const tPdfDPtrEnd = pdfDPtrStart + maxI;
    // for(int i = 0; i < maxI; i++)
    while( pdfDPtr < tPdfDPtrEnd )
      {
      *( pdfDPtr++ ) *= nFactor;
      }
    }
}

/**
 * Get the both Value and Derivative Measure
 */
template <typename TFixedImage, typename TMovingImage>
void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetValueAndDerivative(const ParametersType & parameters,
                        MeasureType & value,
                        DerivativeType & derivative) const
{
  // Set output values to zero
  value = NumericTraits<MeasureType>::ZeroValue();

  if( this->m_UseExplicitPDFDerivatives )
    {
    // Set output values to zero
    if( derivative.GetSize() != this->m_NumberOfParameters )
      {
      derivative = DerivativeType(this->m_NumberOfParameters);
      }
    memset( derivative.data_block(), 0, this->m_NumberOfParameters * sizeof( PDFValueType ) );
    }
  else
    {
    this->m_PRatioArray.Fill(0.0);
    for( ThreadIdType threadId = 0; threadId < this->m_NumberOfThreads; threadId++ )
      {
      this->m_MMIMetricPerThreadVariables[threadId].MetricDerivative.Fill(NumericTraits<MeasureType>::ZeroValue());
      }
    this->m_ImplicitDerivativesSecondPass = false;
    }

  // Set up the parameters in the transform
  this->m_Transform->SetParameters(parameters);

  // MUST BE CALLED TO INITIATE PROCESSING ON SAMPLES
  this->GetValueAndDerivativeMultiThreadedInitiate();

  // CALL IF DOING THREADED POST PROCESSING
  this->GetValueAndDerivativeMultiThreadedPostProcessInitiate();
  for( ThreadIdType threadId = 1; threadId < this->m_NumberOfThreads; threadId++ )
    {
    this->m_MMIMetricPerThreadVariables[0].JointPDFSum += this->m_MMIMetricPerThreadVariables[threadId].JointPDFSum;
    }
  if( this->m_MMIMetricPerThreadVariables[0].JointPDFSum < itk::NumericTraits< PDFValueType >::epsilon() )
    {
    itkExceptionMacro("Joint PDF summed to zero");
    }

  std::fill(this->m_MovingImageMarginalPDF.begin(), this->m_MovingImageMarginalPDF.end(), 0.0F);

  PDFValueType totalMassOfPDF = 0.0;
  for( unsigned int i = 0; i < this->m_NumberOfHistogramBins; i++ )
    {
    totalMassOfPDF += this->m_MMIMetricPerThreadVariables[0].FixedImageMarginalPDF[i];
    }

  const PDFValueType normalizationFactor = 1.0 / this->m_MMIMetricPerThreadVariables[0].JointPDFSum;
  JointPDFValueType *pdfPtr = this->m_MMIMetricPerThreadVariables[0].JointPDF->GetBufferPointer();
  for( unsigned int i = 0; i < this->m_NumberOfHistogramBins; i++ )
    {
    PDFValueType * movingMarginalPtr = &(m_MovingImageMarginalPDF[0]);
    for( unsigned int j = 0; j < this->m_NumberOfHistogramBins; j++ )
      {
      *( pdfPtr ) *= normalizationFactor;
      *( movingMarginalPtr++ ) += *( pdfPtr++ );
      }
    }

  if( this->m_NumberOfPixelsCounted < this->m_NumberOfFixedImageSamples / 16 )
    {
    itkExceptionMacro("Too many samples map outside moving image buffer: "
                      << this->m_NumberOfPixelsCounted << " / "
                      << this->m_NumberOfFixedImageSamples
                      << std::endl);
    }

  // Normalize the fixed image marginal PDF
  if( totalMassOfPDF == 0.0 )
    {
    itkExceptionMacro("Fixed image marginal PDF summed to zero");
    }
  for( unsigned int bin = 0; bin < this->m_NumberOfHistogramBins; bin++ )
    {
    this->m_MMIMetricPerThreadVariables[0].FixedImageMarginalPDF[bin] /= totalMassOfPDF;
    }

  /**
   * Compute the metric by double summation over histogram.
   */

  // Setup pointer to point to the first bin
  JointPDFValueType *jointPDFPtr = this->m_MMIMetricPerThreadVariables[0].JointPDF->GetBufferPointer();

  // Initialize sum to zero
  PDFValueType sum = 0.0;

  const PDFValueType nFactor = 1.0 / ( this->m_MovingImageBinSize
                                       * this->m_NumberOfPixelsCounted );
  for( unsigned int fixedIndex = 0;
       fixedIndex < this->m_NumberOfHistogramBins;
       ++fixedIndex )
    {
    const PDFValueType fixedImagePDFValue = this->m_MMIMetricPerThreadVariables[0].FixedImageMarginalPDF[fixedIndex];
    for( unsigned int movingIndex = 0;
         movingIndex < this->m_NumberOfHistogramBins;
         ++movingIndex, jointPDFPtr++ )
      {
      const PDFValueType movingImagePDFValue = this->m_MovingImageMarginalPDF[movingIndex];
      const PDFValueType jointPDFValue = *( jointPDFPtr );

      // check for non-zero bin contribution
      const PDFValueType closeToZero = std::numeric_limits<PDFValueType>::epsilon();
      if( jointPDFValue > closeToZero &&  movingImagePDFValue > closeToZero )
        {
        const PDFValueType pRatio = std::log(jointPDFValue / movingImagePDFValue);

        if( fixedImagePDFValue > closeToZero )
          {
          sum += jointPDFValue * ( pRatio - std::log(fixedImagePDFValue) );
          }

        if( this->m_UseExplicitPDFDerivatives )
          {
          // move joint pdf derivative pointer to the right position
          JointPDFValueType const * derivPtr =
            this->m_MMIMetricPerThreadVariables[0].JointPDFDerivatives->GetBufferPointer()
            + ( fixedIndex  * this->m_MMIMetricPerThreadVariables[0].JointPDFDerivatives->GetOffsetTable()[2] )
            + ( movingIndex * this->m_MMIMetricPerThreadVariables[0].JointPDFDerivatives->GetOffsetTable()[1] );
          for( unsigned int parameter = 0; parameter < this->m_NumberOfParameters; ++parameter, derivPtr++ )
            {
            // Ref: eqn 23 of Thevenaz & Unser paper [3]
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
    //
    // MUST BE CALLED TO INITIATE PROCESSING ON SAMPLES
    this->GetValueAndDerivativeMultiThreadedInitiate();

    // CALL IF DOING THREADED POST PROCESSING
    this->GetValueAndDerivativeMultiThreadedPostProcessInitiate();
    // Consolidate the contributions from each one of the threads to the total
    // derivative.
    for( ThreadIdType threadId = 1; threadId < this->m_NumberOfThreads; ++threadId )
      {
      DerivativeType const * const source = &( this->m_MMIMetricPerThreadVariables[threadId].MetricDerivative );
      for( unsigned int pp = 0; pp < this->m_NumberOfParameters; pp++ )
        {
        this->m_MMIMetricPerThreadVariables[0].MetricDerivative[pp] += ( *source )[pp];
        }
      }

    derivative = this->m_MMIMetricPerThreadVariables[0].MetricDerivative;
    }

  value = static_cast<MeasureType>( -1.0 * sum );
}

/**
 * Get the match measure derivative
 */
template <typename TFixedImage, typename TMovingImage>
void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::GetDerivative(const ParametersType & parameters,
                DerivativeType & derivative) const
{
  MeasureType value;

  // call the combined version
  this->GetValueAndDerivative(parameters, value, derivative);
}

/**
 * Compute PDF derivatives contribution for each parameter
 */
template <typename TFixedImage, typename TMovingImage>
void
MattesMutualInformationImageToImageMetric<TFixedImage, TMovingImage>
::ComputePDFDerivatives(ThreadIdType threadId,
                        unsigned int sampleNumber,
                        int pdfMovingIndex,
                        const ImageDerivativesType & movingImageGradientValue,
                        PDFValueType cubicBSplineDerivativeValue) const
{
  // Update bins in the PDF derivatives for the current intensity pair
  // Could pre-compute

  PDFValueType precomputedWeight = 0.0;

  const int pdfFixedIndex = this->m_FixedImageSamples[sampleNumber].valueIndex;

  JointPDFDerivativesValueType *derivPtr=ITK_NULLPTR;

  if( this->m_UseExplicitPDFDerivatives )
    {
    derivPtr = this->m_MMIMetricPerThreadVariables[threadId].JointPDFDerivatives->GetBufferPointer()
      + ( pdfFixedIndex  * this->m_MMIMetricPerThreadVariables[threadId].JointPDFDerivatives->GetOffsetTable()[2] )
      + ( pdfMovingIndex * this->m_MMIMetricPerThreadVariables[threadId].JointPDFDerivatives->GetOffsetTable()[1] );
    }
  else
    {
    // Recover the precomputed weight for this specific PDF bin
    precomputedWeight = this->m_PRatioArray[pdfFixedIndex][pdfMovingIndex];
    }

  if( !this->m_TransformIsBSpline )
    {
    /**
     * Generic version which works for all transforms.
     */

    // Need to use one of the threader transforms if we're
    // not in thread 0.
    //
    // Use a raw pointer here to avoid the overhead of smart pointers.
    // For instance, Register and UnRegister have mutex locks around
    // the reference counts.
    TransformType *transform;
    if( threadId > 0 )
      {
      transform = this->m_ThreaderTransform[threadId - 1];
      }
    else
      {
      transform = this->m_Transform;
      }

    // Compute the transform Jacobian.
    // Should pre-compute
    typename TransformType::JacobianType &jacobian = this->m_MMIMetricPerThreadVariables[threadId].Jacobian;
    transform->ComputeJacobianWithRespectToParameters( this->m_FixedImageSamples[sampleNumber].point, jacobian);
    for( unsigned int mu = 0; mu < this->m_NumberOfParameters; mu++ )
      {
      PDFValueType innerProduct = 0.0;
      for( unsigned int dim = 0; dim < Superclass::FixedImageDimension; dim++ )
        {
        innerProduct += jacobian[dim][mu] * movingImageGradientValue[dim];
        }

      const PDFValueType derivativeContribution = innerProduct * cubicBSplineDerivativeValue;

      if( this->m_UseExplicitPDFDerivatives )
        {
        *( derivPtr ) -= derivativeContribution;
        ++derivPtr;
        }
      else
        {
        this->m_MMIMetricPerThreadVariables[threadId].MetricDerivative[mu] += precomputedWeight *
          derivativeContribution;
        }
      }
    }
  else
    {
    const WeightsValueType *weights = ITK_NULLPTR;
    const IndexValueType *  indices = ITK_NULLPTR;

    BSplineTransformWeightsType *   weightsHelper = ITK_NULLPTR;
    BSplineTransformIndexArrayType *indicesHelper = ITK_NULLPTR;

    if( this->m_UseCachingOfBSplineWeights )
      {
      //
      // If the transform is of type BSplineTransform, we can obtain
      // a speed up by only processing the affected parameters.  Note that
      // these pointers are just pointing to pre-allocated rows of the caching
      // arrays. There is therefore, no need to free this memory.
      //
      weights = this->m_BSplineTransformWeightsArray[sampleNumber];
      indices = this->m_BSplineTransformIndicesArray[sampleNumber];
      }
    else
      {
      if( threadId > 0 )
        {
        weightsHelper = &( this->m_ThreaderBSplineTransformWeights[threadId - 1] );
        indicesHelper = &( this->m_ThreaderBSplineTransformIndices[threadId - 1] );
        }
      else
        {
        weightsHelper = &( this->m_BSplineTransformWeights );
        indicesHelper = &( this->m_BSplineTransformIndices );
        }

      // Get Jacobian at a point. A very specialized function just for BSplines
      this->m_BSplineTransform->ComputeJacobianFromBSplineWeightsWithRespectToPosition(
        this->m_FixedImageSamples[sampleNumber].point, *weightsHelper, *indicesHelper);
      }
    for( unsigned int dim = 0; dim < Superclass::FixedImageDimension; dim++ )
      {
      for( unsigned int mu = 0; mu < this->m_NumBSplineWeights; mu++ )
        {
        /* The array weights contains the Jacobian values in a 1-D array
         * (because for each parameter the Jacobian is non-zero in only 1 of the
         * possible dimensions) which is multiplied by the moving image
         * gradient. */
        PDFValueType innerProduct;
        int          parameterIndex;
        if( this->m_UseCachingOfBSplineWeights )
          {
          innerProduct = movingImageGradientValue[dim] * weights[mu];
          parameterIndex = indices[mu] + this->m_BSplineParametersOffset[dim];
          }
        else
          {
          innerProduct = movingImageGradientValue[dim] * ( *weightsHelper )[mu];
          parameterIndex = ( *indicesHelper )[mu] + this->m_BSplineParametersOffset[dim];
          }

        const PDFValueType derivativeContribution = innerProduct * cubicBSplineDerivativeValue;

        if( this->m_UseExplicitPDFDerivatives )
          {
          JointPDFValueType * const ptr = derivPtr + parameterIndex;
          *( ptr ) -= derivativeContribution;
          }
        else
          {
          this->m_MMIMetricPerThreadVariables[threadId].MetricDerivative[parameterIndex] += precomputedWeight *
            derivativeContribution;
          }
        } // end mu for loop
      }   // end dim for loop
    }     // end if-block transform is BSpline
}

} // end namespace itk

#endif
