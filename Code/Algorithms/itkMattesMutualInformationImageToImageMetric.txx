/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMattesMutualInformationImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMattesMutualInformationImageToImageMetric_txx
#define __itkMattesMutualInformationImageToImageMetric_txx

#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkCovariantVector.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageIterator.h"
#include "vnl/vnl_math.h"
#include "itkStatisticsImageFilter.h"

#include "vnl/vnl_vector.txx"
#include "vnl/vnl_c_vector.txx"

namespace itk
{
/**
 * Constructor
 */
template< class TFixedImage, class TMovingImage >
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::MattesMutualInformationImageToImageMetric()
{
  m_NumberOfHistogramBins = 50;

  this->SetComputeGradient(false); // don't use the default gradient for now

  // Initialize PDFs to NULL
  m_JointPDF = NULL;
  m_JointPDFDerivatives = NULL;

  // Initialize memory
  m_FixedImageMarginalPDF = NULL;
  m_MovingImageMarginalPDF = NULL;

  m_MovingImageNormalizedMin = 0.0;
  m_FixedImageNormalizedMin = 0.0;
  m_MovingImageTrueMin = 0.0;
  m_MovingImageTrueMax = 0.0;
  m_FixedImageBinSize = 0.0;
  m_MovingImageBinSize = 0.0;

  m_CubicBSplineDerivativeKernel = NULL;

  // For multi-threading the metric
  m_ThreaderFixedImageMarginalPDF = NULL;
  m_ThreaderJointPDF = NULL;
  m_ThreaderJointPDFDerivatives = NULL;
  m_ThreaderJointPDFStartBin = NULL;
  m_ThreaderJointPDFEndBin = NULL;
  m_ThreaderJointPDFSum = NULL;
  this->m_WithinThreadPreProcess = true;
  this->m_WithinThreadPostProcess = false;

  this->m_ThreaderMetricDerivative = NULL;
  this->m_UseExplicitPDFDerivatives = true;
  this->m_ImplicitDerivativesSecondPass = false;
}

template< class TFixedImage, class TMovingImage >
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::~MattesMutualInformationImageToImageMetric()
{
  if ( m_FixedImageMarginalPDF != NULL )
    {
    delete[] m_FixedImageMarginalPDF;
    }
  m_FixedImageMarginalPDF = NULL;
  if ( m_MovingImageMarginalPDF != NULL )
    {
    delete[] m_MovingImageMarginalPDF;
    }
  m_MovingImageMarginalPDF = NULL;
  if ( m_ThreaderJointPDF != NULL )
    {
    delete[] m_ThreaderJointPDF;
    }
  m_ThreaderJointPDF = NULL;

  if ( m_ThreaderJointPDFDerivatives != NULL )
    {
    delete[] m_ThreaderJointPDFDerivatives;
    }
  m_ThreaderJointPDFDerivatives = NULL;

  if ( m_ThreaderFixedImageMarginalPDF != NULL )
    {
    delete[] m_ThreaderFixedImageMarginalPDF;
    }
  m_ThreaderFixedImageMarginalPDF = NULL;

  if ( m_ThreaderJointPDFStartBin != NULL )
    {
    delete[] m_ThreaderJointPDFStartBin;
    }
  m_ThreaderJointPDFStartBin = NULL;

  if ( m_ThreaderJointPDFEndBin != NULL )
    {
    delete[] m_ThreaderJointPDFEndBin;
    }
  m_ThreaderJointPDFEndBin = NULL;

  if ( m_ThreaderJointPDFSum != NULL )
    {
    delete[] m_ThreaderJointPDFSum;
    }
  m_ThreaderJointPDFSum = NULL;

  if ( this->m_ThreaderMetricDerivative != NULL )
    {
    delete[] this->m_ThreaderMetricDerivative;
    }
  this->m_ThreaderMetricDerivative = NULL;
}

/**
 * Print out internal information about this class
 */
template< class TFixedImage, class TMovingImage  >
void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
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
}

/**
 * Initialize
 */
template< class TFixedImage, class TMovingImage >
void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::Initialize(void)
throw ( ExceptionObject )
{
  this->Superclass::Initialize();
  this->Superclass::MultiThreadingInitialize();

  typedef StatisticsImageFilter< FixedImageType > FixedImageStatisticsFilterType;
  typename FixedImageStatisticsFilterType::Pointer fixedImageStats =
    FixedImageStatisticsFilterType::New();
  fixedImageStats->SetInput(this->m_FixedImage);
  fixedImageStats->SetNumberOfThreads(this->m_NumberOfThreads);
  fixedImageStats->Update();

  m_FixedImageTrueMin = fixedImageStats->GetMinimum();
  m_FixedImageTrueMax = fixedImageStats->GetMaximum();
  double fixedImageMin = m_FixedImageTrueMin;
  double fixedImageMax = m_FixedImageTrueMax;

  typedef StatisticsImageFilter< MovingImageType >
  MovingImageStatisticsFilterType;
  typename MovingImageStatisticsFilterType::Pointer movingImageStats =
    MovingImageStatisticsFilterType::New();
  movingImageStats->SetInput(this->m_MovingImage);
  movingImageStats->SetNumberOfThreads(this->m_NumberOfThreads);
  movingImageStats->Update();

  m_MovingImageTrueMin = movingImageStats->GetMinimum();
  m_MovingImageTrueMax = movingImageStats->GetMaximum();
  double movingImageMin = m_MovingImageTrueMin;
  double movingImageMax = m_MovingImageTrueMax;

  itkDebugMacro(" FixedImageMin: " << fixedImageMin
                                   << " FixedImageMax: " << fixedImageMax << std::endl);
  itkDebugMacro(" MovingImageMin: " << movingImageMin
                                    << " MovingImageMax: " << movingImageMax << std::endl);

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

  m_FixedImageBinSize = ( fixedImageMax - fixedImageMin )
                        / static_cast< double >( m_NumberOfHistogramBins
                                                 - 2 * padding );
  m_FixedImageNormalizedMin = fixedImageMin / m_FixedImageBinSize
                              - static_cast< double >( padding );

  m_MovingImageBinSize = ( movingImageMax - movingImageMin )
                         / static_cast< double >( m_NumberOfHistogramBins
                                                  - 2 * padding );
  m_MovingImageNormalizedMin = movingImageMin / m_MovingImageBinSize
                               - static_cast< double >( padding );

  itkDebugMacro("FixedImageNormalizedMin: " << m_FixedImageNormalizedMin);
  itkDebugMacro("MovingImageNormalizedMin: " << m_MovingImageNormalizedMin);
  itkDebugMacro("FixedImageBinSize: " << m_FixedImageBinSize);
  itkDebugMacro("MovingImageBinSize; " << m_MovingImageBinSize);

  /**
   * Allocate memory for the marginal PDF and initialize values
   * to zero. The marginal PDFs are stored as std::vector.
   */
  if ( m_FixedImageMarginalPDF != NULL )
    {
    delete[] m_FixedImageMarginalPDF;
    }
  m_FixedImageMarginalPDF = new PDFValueType[m_NumberOfHistogramBins];
  if ( m_MovingImageMarginalPDF != NULL )
    {
    delete[] m_MovingImageMarginalPDF;
    }
  m_MovingImageMarginalPDF = new PDFValueType[m_NumberOfHistogramBins];

  /**
   * Allocate memory for the joint PDF and joint PDF derivatives.
   * The joint PDF and joint PDF derivatives are store as itk::Image.
   */
  m_JointPDF = JointPDFType::New();
  m_JointPDFDerivatives = JointPDFDerivativesType::New();

  // Instantiate a region, index, size
  JointPDFRegionType jointPDFRegion;
  JointPDFIndexType  jointPDFIndex;
  JointPDFSizeType   jointPDFSize;

  // Deallocate the memory that may have been allocated for
  // previous runs of the metric.
  this->m_JointPDFDerivatives = NULL;  // by destroying the dynamic array
  this->m_PRatioArray.SetSize(1, 1);   // and by allocating very small the
                                       // static ones
  this->m_MetricDerivative = DerivativeType(1);

  JointPDFDerivativesRegionType jointPDFDerivativesRegion;

  //
  // Now allocate memory according to the user-selected method.
  //
  if ( this->m_UseExplicitPDFDerivatives )
    {
    this->m_JointPDFDerivatives = JointPDFDerivativesType::New();

    JointPDFDerivativesIndexType jointPDFDerivativesIndex;
    JointPDFDerivativesSizeType  jointPDFDerivativesSize;

    // For the derivatives of the joint PDF define a region starting from
    // {0,0,0}
    // with size {m_NumberOfParameters,m_NumberOfHistogramBins,
    // m_NumberOfHistogramBins}. The dimension represents transform parameters,
    // fixed image parzen window index and moving image parzen window index,
    // respectively.
    jointPDFDerivativesIndex.Fill(0);
    jointPDFDerivativesSize[0] = this->m_NumberOfParameters;
    jointPDFDerivativesSize[1] = this->m_NumberOfHistogramBins;
    jointPDFDerivativesSize[2] = this->m_NumberOfHistogramBins;

    jointPDFDerivativesRegion.SetIndex(jointPDFDerivativesIndex);
    jointPDFDerivativesRegion.SetSize(jointPDFDerivativesSize);

    // Set the regions and allocate
    m_JointPDFDerivatives->SetRegions(jointPDFDerivativesRegion);
    m_JointPDFDerivatives->Allocate();
    m_JointPDFDerivativesBufferSize = jointPDFDerivativesSize[0]
                                      * jointPDFDerivativesSize[1]
                                      * jointPDFDerivativesSize[2]
                                      * sizeof( JointPDFDerivativesValueType );
    }
  else
    {
    /** Allocate memory for helper array that will contain the pRatios
     *  for each bin of the joint histogram. This is part of the effort
     *  for flattening the computation of the PDF Jacobians.
     */
    this->m_PRatioArray.SetSize(this->m_NumberOfHistogramBins, this->m_NumberOfHistogramBins);
    this->m_MetricDerivative = DerivativeType( this->GetNumberOfParameters() );
    }

  // For the joint PDF define a region starting from {0,0}
  // with size {m_NumberOfHistogramBins, m_NumberOfHistogramBins}.
  // The dimension represents fixed image parzen window index
  // and moving image parzen window index, respectively.
  jointPDFIndex.Fill(0);
  jointPDFSize.Fill(m_NumberOfHistogramBins);

  jointPDFRegion.SetIndex(jointPDFIndex);
  jointPDFRegion.SetSize(jointPDFSize);

  // Set the regions and allocate
  m_JointPDF->SetRegions(jointPDFRegion);
  m_JointPDF->Allocate();

  m_JointPDFBufferSize = jointPDFSize[0] * jointPDFSize[1] * sizeof( PDFValueType );

  /**
   * Setup the kernels used for the Parzen windows.
   */
  m_CubicBSplineKernel = CubicBSplineFunctionType::New();
  m_CubicBSplineDerivativeKernel = CubicBSplineDerivativeFunctionType::New();

  /**
   * Pre-compute the fixed image parzen window index for
   * each point of the fixed image sample points list.
   */
  this->ComputeFixedImageParzenWindowIndices(this->m_FixedImageSamples);

  if ( m_ThreaderFixedImageMarginalPDF != NULL )
    {
    delete[] m_ThreaderFixedImageMarginalPDF;
    }
  // Assumes number of threads doesn't change between calls to Initialize
  m_ThreaderFixedImageMarginalPDF = new
                                    PDFValueType[( this->m_NumberOfThreads - 1 )
                                                 * m_NumberOfHistogramBins];

  if ( m_ThreaderJointPDF != NULL )
    {
    delete[] m_ThreaderJointPDF;
    }
  m_ThreaderJointPDF = new typename
                       JointPDFType::Pointer[this->m_NumberOfThreads - 1];

  if ( m_ThreaderJointPDFStartBin != NULL )
    {
    delete[] m_ThreaderJointPDFStartBin;
    }
  m_ThreaderJointPDFStartBin = new int[this->m_NumberOfThreads];

  if ( m_ThreaderJointPDFEndBin != NULL )
    {
    delete[] m_ThreaderJointPDFEndBin;
    }
  m_ThreaderJointPDFEndBin = new int[this->m_NumberOfThreads];

  if ( m_ThreaderJointPDFSum != NULL )
    {
    delete[] m_ThreaderJointPDFSum;
    }
  m_ThreaderJointPDFSum = new double[this->m_NumberOfThreads];

  unsigned int threadID;

  int binRange = m_NumberOfHistogramBins / this->m_NumberOfThreads;

  for ( threadID = 0; threadID < this->m_NumberOfThreads - 1; threadID++ )
    {
    m_ThreaderJointPDF[threadID] = JointPDFType::New();
    m_ThreaderJointPDF[threadID]->SetRegions(jointPDFRegion);
    m_ThreaderJointPDF[threadID]->Allocate();

    m_ThreaderJointPDFStartBin[threadID] = threadID * binRange;
    m_ThreaderJointPDFEndBin[threadID] = ( threadID + 1 ) * binRange - 1;
    }

  m_ThreaderJointPDFStartBin[this->m_NumberOfThreads - 1] =
    ( this->m_NumberOfThreads - 1 ) * binRange;

  m_ThreaderJointPDFEndBin[this->m_NumberOfThreads - 1] = m_NumberOfHistogramBins - 1;

  // Release memory of arrays that may have been used for
  // previous executions of this metric with different settings
  // of the memory caching flags.
  if ( m_ThreaderJointPDFDerivatives != NULL )
    {
    delete[] m_ThreaderJointPDFDerivatives;
    }
  m_ThreaderJointPDFDerivatives = NULL;

  if ( m_ThreaderMetricDerivative != NULL )
    {
    delete[] m_ThreaderMetricDerivative;
    }
  m_ThreaderMetricDerivative = NULL;

  if ( this->m_UseExplicitPDFDerivatives )
    {
    m_ThreaderJointPDFDerivatives = new typename
                                    JointPDFDerivativesType::Pointer[this->m_NumberOfThreads - 1];

    for ( threadID = 0; threadID < this->m_NumberOfThreads - 1; threadID++ )
      {
      m_ThreaderJointPDFDerivatives[threadID] = JointPDFDerivativesType::New();
      m_ThreaderJointPDFDerivatives[threadID]->SetRegions(
        jointPDFDerivativesRegion);
      m_ThreaderJointPDFDerivatives[threadID]->Allocate();
      }
    }
  else
    {
    m_ThreaderMetricDerivative = new DerivativeType[this->m_NumberOfThreads - 1];

    for ( threadID = 0; threadID < this->m_NumberOfThreads - 1; threadID++ )
      {
      this->m_ThreaderMetricDerivative[threadID] = DerivativeType( this->GetNumberOfParameters() );
      }
    }
}

/**
 * Uniformly sample the fixed image domain using a random walk
 */
template< class TFixedImage, class TMovingImage >
void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::ComputeFixedImageParzenWindowIndices(
  FixedImageSampleContainer & samples)
{
  typename FixedImageSampleContainer::iterator iter;
  typename FixedImageSampleContainer::const_iterator end = samples.end();

  for ( iter = samples.begin(); iter != end; ++iter )
    {
    // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
    double windowTerm = static_cast< double >( ( *iter ).value )
                        / m_FixedImageBinSize
                        - m_FixedImageNormalizedMin;
    OffsetValueType pindex = static_cast< OffsetValueType >( windowTerm );

    // Make sure the extreme values are in valid bins
    if ( pindex < 2 )
      {
      pindex = 2;
      }
    else
      {
      const OffsetValueType nindex =
        static_cast< OffsetValueType >( this->m_NumberOfHistogramBins ) - 3;
      if ( pindex > nindex )
        {
        pindex = nindex;
        }
      }

    ( *iter ).valueIndex = pindex;
    }
}

template< class TFixedImage, class TMovingImage  >
inline void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::GetValueThreadPreProcess(unsigned int threadID,
                           bool withinSampleThread) const
{
  this->Superclass::GetValueThreadPreProcess(threadID, withinSampleThread);

  if ( threadID > 0 )
    {
    memset(m_ThreaderJointPDF[threadID - 1]->GetBufferPointer(),
           0,
           m_JointPDFBufferSize);
    memset( &( m_ThreaderFixedImageMarginalPDF[( threadID - 1 )
                                               * m_NumberOfHistogramBins] ),
            0,
            m_NumberOfHistogramBins * sizeof( PDFValueType ) );
    }
  else
    {
    // zero-th thread uses the variables directly
    memset(m_JointPDF->GetBufferPointer(),
           0,
           m_JointPDFBufferSize);
    memset( m_FixedImageMarginalPDF,
            0,
            m_NumberOfHistogramBins * sizeof( PDFValueType ) );
    }
}

template< class TFixedImage, class TMovingImage  >
inline bool
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::GetValueThreadProcessSample(unsigned int threadID,
                              unsigned long fixedImageSample,
                              const MovingImagePointType & itkNotUsed(mappedPoint),
                              double movingImageValue) const
{
  /**
   * Compute this sample's contribution to the marginal and
   * joint distributions.
   *
   */

  if ( movingImageValue < m_MovingImageTrueMin )
    {
    return false;
    }
  else if ( movingImageValue > m_MovingImageTrueMax )
    {
    return false;
    }

  // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
  double movingImageParzenWindowTerm = movingImageValue
                                       / m_MovingImageBinSize
                                       - m_MovingImageNormalizedMin;
  // Same as floor
  OffsetValueType movingImageParzenWindowIndex =
    static_cast< OffsetValueType >( movingImageParzenWindowTerm );
  if ( movingImageParzenWindowIndex < 2 )
    {
    movingImageParzenWindowIndex = 2;
    }
  else
    {
    const OffsetValueType nindex =
      static_cast< OffsetValueType >( this->m_NumberOfHistogramBins ) - 3;
    if ( movingImageParzenWindowIndex > nindex )
      {
      movingImageParzenWindowIndex = nindex;
      }
    }

  unsigned int fixedImageParzenWindowIndex =
    this->m_FixedImageSamples[fixedImageSample].valueIndex;
  if ( threadID > 0 )
    {
    m_ThreaderFixedImageMarginalPDF[( threadID - 1 ) * m_NumberOfHistogramBins
                                    + fixedImageParzenWindowIndex] += 1;
    }
  else
    {
    m_FixedImageMarginalPDF[fixedImageParzenWindowIndex] += 1;
    }

  // Pointer to affected bin to be updated
  JointPDFValueType *pdfPtr;
  if ( threadID > 0 )
    {
    pdfPtr = m_ThreaderJointPDF[threadID - 1]->GetBufferPointer()
             + ( fixedImageParzenWindowIndex
                 * m_ThreaderJointPDF[threadID - 1]
                 ->GetOffsetTable()[1] );
    }
  else
    {
    pdfPtr = m_JointPDF->GetBufferPointer()
             + ( fixedImageParzenWindowIndex
                 * m_JointPDF->GetOffsetTable()[1] );
    }

  // Move the pointer to the first affected bin
  int pdfMovingIndex = static_cast< int >( movingImageParzenWindowIndex ) - 1;
  pdfPtr += pdfMovingIndex;
  int pdfMovingIndexMax = static_cast< int >( movingImageParzenWindowIndex ) + 2;

  double movingImageParzenWindowArg =
    static_cast< double >( pdfMovingIndex )
    - movingImageParzenWindowTerm;

  while ( pdfMovingIndex <= pdfMovingIndexMax )
    {
    *( pdfPtr++ ) += static_cast< PDFValueType >( m_CubicBSplineKernel
                                                  ->Evaluate(
                                                    movingImageParzenWindowArg) );
    movingImageParzenWindowArg += 1;
    ++pdfMovingIndex;
    }

  return true;
}

template< class TFixedImage, class TMovingImage  >
inline void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::GetValueThreadPostProcess( unsigned int threadID,
                             bool itkNotUsed(withinSampleThread) ) const
{
  unsigned int t;
  int          i;
  int          maxI;

  maxI = m_NumberOfHistogramBins
         * ( m_ThreaderJointPDFEndBin[threadID]
             - m_ThreaderJointPDFStartBin[threadID] + 1 );
  JointPDFValueType *pdfPtr;
  JointPDFValueType *pdfPtrStart;
  pdfPtrStart = m_JointPDF->GetBufferPointer()
                + ( m_ThreaderJointPDFStartBin[threadID]
                    * m_JointPDF->GetOffsetTable()[1] );
  JointPDFValueType *tPdfPtr;
  JointPDFValueType *tPdfPtrEnd;
  unsigned int       tPdfPtrOffset;
  tPdfPtrOffset = ( m_ThreaderJointPDFStartBin[threadID]
                    * m_JointPDF->GetOffsetTable()[1] );
  for ( t = 0; t < this->m_NumberOfThreads - 1; t++ )
    {
    pdfPtr = pdfPtrStart;
    tPdfPtr = m_ThreaderJointPDF[t]->GetBufferPointer() + tPdfPtrOffset;
    tPdfPtrEnd = tPdfPtr + maxI;
    //for(i=0; i < maxI; i++)
    while ( tPdfPtr < tPdfPtrEnd )
      {
      *( pdfPtr++ ) += *( tPdfPtr++ );
      }
    for ( i = m_ThreaderJointPDFStartBin[threadID];
          i <= m_ThreaderJointPDFEndBin[threadID];
          i++ )
      {
      m_FixedImageMarginalPDF[i] += m_ThreaderFixedImageMarginalPDF[
        ( t * m_NumberOfHistogramBins ) + i];
      }
    }
  double jointPDFSum = 0.0;
  pdfPtr = pdfPtrStart;
  for ( i = 0; i < maxI; i++ )
    {
    jointPDFSum += *( pdfPtr++ );
    }
  if ( threadID > 0 )
    {
    m_ThreaderJointPDFSum[threadID - 1] = jointPDFSum;
    }
  else
    {
    m_JointPDFSum = jointPDFSum;
    }
}

template< class TFixedImage, class TMovingImage  >
typename MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::MeasureType
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::GetValue(const ParametersType & parameters) const
{
  // Set up the parameters in the transform
  this->m_Transform->SetParameters(parameters);
  this->m_Parameters = parameters;

  // MUST BE CALLED TO INITIATE PROCESSING
  this->GetValueMultiThreadedInitiate();

  // MUST BE CALLED TO INITIATE PROCESSING
  this->GetValueMultiThreadedPostProcessInitiate();

  for ( unsigned int threadID = 0; threadID < this->m_NumberOfThreads - 1; threadID++ )
    {
    m_JointPDFSum += m_ThreaderJointPDFSum[threadID];
    }
  if ( m_JointPDFSum == 0.0 )
    {
    itkExceptionMacro("Joint PDF summed to zero");
    }

  memset( m_MovingImageMarginalPDF,
          0,
          m_NumberOfHistogramBins * sizeof( PDFValueType ) );

  JointPDFValueType *pdfPtr;
  PDFValueType *     movingMarginalPtr;
  unsigned int       i, j;
  double             fixedPDFSum = 0.0;
  double             nFactor = 1.0 / m_JointPDFSum;
  pdfPtr = m_JointPDF->GetBufferPointer();
  for ( i = 0; i < m_NumberOfHistogramBins; i++ )
    {
    fixedPDFSum += m_FixedImageMarginalPDF[i];
    movingMarginalPtr = m_MovingImageMarginalPDF;
    for ( j = 0; j < m_NumberOfHistogramBins; j++ )
      {
      *( pdfPtr ) *= nFactor;
      *( movingMarginalPtr++ ) += *( pdfPtr++ );
      }
    }

  if ( this->m_NumberOfPixelsCounted <
       this->m_NumberOfFixedImageSamples / 16 )
    {
    itkExceptionMacro("Too many samples map outside moving image buffer: "
                      << this->m_NumberOfPixelsCounted << " / "
                      << this->m_NumberOfFixedImageSamples
                      << std::endl);
    }

  // Normalize the fixed image marginal PDF
  if ( fixedPDFSum == 0.0 )
    {
    itkExceptionMacro("Fixed image marginal PDF summed to zero");
    }
  for ( unsigned int bin = 0; bin < m_NumberOfHistogramBins; bin++ )
    {
    m_FixedImageMarginalPDF[bin] /= fixedPDFSum;
    }

  /**
   * Compute the metric by double summation over histogram.
   */

  // Setup pointer to point to the first bin
  JointPDFValueType *jointPDFPtr = m_JointPDF->GetBufferPointer();

  double sum = 0.0;

  for ( unsigned int fixedIndex = 0;
        fixedIndex < m_NumberOfHistogramBins;
        ++fixedIndex )
    {
    double fixedImagePDFValue = m_FixedImageMarginalPDF[fixedIndex];

    for ( unsigned int movingIndex = 0;
          movingIndex < m_NumberOfHistogramBins;
          ++movingIndex, jointPDFPtr++ )
      {
      double movingImagePDFValue = m_MovingImageMarginalPDF[movingIndex];
      double jointPDFValue = *( jointPDFPtr );

      // check for non-zero bin contribution
      if ( jointPDFValue > 1e-16 &&  movingImagePDFValue > 1e-16 )
        {
        double pRatio = vcl_log(jointPDFValue / movingImagePDFValue);
        if ( fixedImagePDFValue > 1e-16 )
          {
          sum += jointPDFValue * ( pRatio - vcl_log(fixedImagePDFValue) );
          }
        } // end if-block to check non-zero bin contribution
      }   // end for-loop over moving index
    }     // end for-loop over fixed index

  return static_cast< MeasureType >( -1.0 * sum );
}

template< class TFixedImage, class TMovingImage  >
inline void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeThreadPreProcess( unsigned int threadID,
                                         bool itkNotUsed(withinSampleThread) ) const
{
  if ( threadID > 0 )
    {
    memset(m_ThreaderJointPDF[threadID - 1]->GetBufferPointer(),
           0,
           m_JointPDFBufferSize);

    memset( &( m_ThreaderFixedImageMarginalPDF[( threadID - 1 )
                                               * m_NumberOfHistogramBins] ),
            0,
            m_NumberOfHistogramBins * sizeof( PDFValueType ) );

    if ( this->m_UseExplicitPDFDerivatives )
      {
      memset(m_ThreaderJointPDFDerivatives[threadID - 1]->GetBufferPointer(),
             0,
             m_JointPDFDerivativesBufferSize);
      }
    }
  else
    {
    memset(m_JointPDF->GetBufferPointer(),
           0,
           m_JointPDFBufferSize);
    memset( m_FixedImageMarginalPDF,
            0,
            m_NumberOfHistogramBins * sizeof( PDFValueType ) );

    if ( this->m_UseExplicitPDFDerivatives )
      {
      memset(m_JointPDFDerivatives->GetBufferPointer(),
             0,
             m_JointPDFDerivativesBufferSize);
      }
    }
}

template< class TFixedImage, class TMovingImage  >
inline bool
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeThreadProcessSample(unsigned int threadID,
                                           unsigned long fixedImageSample,
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
  if ( movingImageValue < m_MovingImageTrueMin )
    {
    return false;
    }
  else if ( movingImageValue > m_MovingImageTrueMax )
    {
    return false;
    }

  unsigned int fixedImageParzenWindowIndex =
    this->m_FixedImageSamples[fixedImageSample].valueIndex;

  // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).
  double movingImageParzenWindowTerm = movingImageValue
                                       / m_MovingImageBinSize
                                       - m_MovingImageNormalizedMin;
  OffsetValueType movingImageParzenWindowIndex =
    static_cast< OffsetValueType >( movingImageParzenWindowTerm );

  // Make sure the extreme values are in valid bins
  if ( movingImageParzenWindowIndex < 2 )
    {
    movingImageParzenWindowIndex = 2;
    }
  else
    {
    const OffsetValueType nindex =
      static_cast< OffsetValueType >( this->m_NumberOfHistogramBins ) - 3;
    if ( movingImageParzenWindowIndex > nindex )
      {
      movingImageParzenWindowIndex = nindex;
      }
    }

  // Since a zero-order BSpline (box car) kernel is used for
  // the fixed image marginal pdf, we need only increment the
  // fixedImageParzenWindowIndex by value of 1.0.
  if ( threadID > 0 )
    {
    ++m_ThreaderFixedImageMarginalPDF[( threadID - 1 ) * m_NumberOfHistogramBins
                                      + fixedImageParzenWindowIndex];
    }
  else
    {
    ++m_FixedImageMarginalPDF[fixedImageParzenWindowIndex];
    }

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

  // Pointer to affected bin to be updated
  JointPDFValueType *pdfPtr;
  if ( threadID > 0 )
    {
    pdfPtr = m_ThreaderJointPDF[threadID - 1]
             ->GetBufferPointer()
             + ( fixedImageParzenWindowIndex
                 * m_NumberOfHistogramBins );
    }
  else
    {
    pdfPtr = m_JointPDF->GetBufferPointer()
             + ( fixedImageParzenWindowIndex
                 * m_NumberOfHistogramBins );
    }

  // Move the pointer to the fist affected bin
  int pdfMovingIndex = static_cast< int >( movingImageParzenWindowIndex ) - 1;
  pdfPtr += pdfMovingIndex;
  int pdfMovingIndexMax = static_cast< int >( movingImageParzenWindowIndex ) + 2;

  double movingImageParzenWindowArg = static_cast< double >( pdfMovingIndex )
                                      - static_cast< double >( movingImageParzenWindowTerm );

  while ( pdfMovingIndex <= pdfMovingIndexMax )
    {
    *( pdfPtr++ ) += static_cast< PDFValueType >( m_CubicBSplineKernel
                                                  ->Evaluate(
                                                    movingImageParzenWindowArg) );

    if ( this->m_UseExplicitPDFDerivatives || this->m_ImplicitDerivativesSecondPass )
      {
      // Compute the cubicBSplineDerivative for later repeated use.
      double cubicBSplineDerivativeValue =
        m_CubicBSplineDerivativeKernel->Evaluate(movingImageParzenWindowArg);

      // Compute PDF derivative contribution.
      this->ComputePDFDerivatives(threadID,
                                  fixedImageSample,
                                  pdfMovingIndex,
                                  movingImageGradientValue,
                                  cubicBSplineDerivativeValue);
      }

    movingImageParzenWindowArg += 1;
    ++pdfMovingIndex;
    }

  return true;
}

template< class TFixedImage, class TMovingImage  >
inline void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeThreadPostProcess(unsigned int threadID,
                                         bool withinSampleThread) const
{
  this->GetValueThreadPostProcess(threadID, withinSampleThread);

  if ( this->m_UseExplicitPDFDerivatives )
    {
    const unsigned int rowSize = this->m_NumberOfParameters * m_NumberOfHistogramBins;

    const unsigned int maxI =
      rowSize * ( m_ThreaderJointPDFEndBin[threadID]
                  - m_ThreaderJointPDFStartBin[threadID] + 1 );

    JointPDFDerivativesValueType *pdfDPtr;
    JointPDFDerivativesValueType *pdfDPtrStart;
    pdfDPtrStart = m_JointPDFDerivatives->GetBufferPointer()
                   + ( m_ThreaderJointPDFStartBin[threadID] * rowSize );
    JointPDFDerivativesValueType *tPdfDPtr;
    JointPDFDerivativesValueType *tPdfDPtrEnd;
    unsigned int                  tPdfDPtrOffset;
    tPdfDPtrOffset = m_ThreaderJointPDFStartBin[threadID] *  rowSize;
    for ( unsigned int t = 0; t < this->m_NumberOfThreads - 1; t++ )
      {
      pdfDPtr = pdfDPtrStart;
      tPdfDPtr = m_ThreaderJointPDFDerivatives[t]->GetBufferPointer()
                 + tPdfDPtrOffset;
      tPdfDPtrEnd = tPdfDPtr + maxI;
      // for(i = 0; i < maxI; i++)
      while ( tPdfDPtr < tPdfDPtrEnd )
        {
        *( pdfDPtr++ ) += *( tPdfDPtr++ );
        }
      }

    double nFactor = 1.0 / ( m_MovingImageBinSize
                             * this->m_NumberOfPixelsCounted );

    pdfDPtr = pdfDPtrStart;
    tPdfDPtrEnd = pdfDPtrStart + maxI;
    //for(int i = 0; i < maxI; i++)
    while ( pdfDPtr < tPdfDPtrEnd )
      {
      *( pdfDPtr++ ) *= nFactor;
      }
    }
}

/**
 * Get the both Value and Derivative Measure
 */
template< class TFixedImage, class TMovingImage  >
void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivative(const ParametersType & parameters,
                        MeasureType & value,
                        DerivativeType & derivative) const
{
  // Set output values to zero
  value = NumericTraits< MeasureType >::Zero;

  if ( this->m_UseExplicitPDFDerivatives )
    {
    // Set output values to zero
    if ( derivative.GetSize() != this->m_NumberOfParameters )
      {
      derivative = DerivativeType(this->m_NumberOfParameters);
      }
    memset( derivative.data_block(),
            0,
            this->m_NumberOfParameters * sizeof( double ) );
    }
  else
    {
    this->m_PRatioArray.Fill(0.0);
    this->m_MetricDerivative.Fill(NumericTraits< MeasureType >::Zero);
    for ( unsigned int threadID = 0; threadID < this->m_NumberOfThreads - 1; threadID++ )
      {
      this->m_ThreaderMetricDerivative[threadID].Fill(NumericTraits< MeasureType >::Zero);
      }
    this->m_ImplicitDerivativesSecondPass = false;
    }

  // Set up the parameters in the transform
  this->m_Transform->SetParameters(parameters);
  this->m_Parameters = parameters;

  // MUST BE CALLED TO INITIATE PROCESSING ON SAMPLES
  this->GetValueAndDerivativeMultiThreadedInitiate();

  // CALL IF DOING THREADED POST PROCESSING
  this->GetValueAndDerivativeMultiThreadedPostProcessInitiate();

  for ( unsigned int threadID = 0; threadID < this->m_NumberOfThreads - 1; threadID++ )
    {
    m_JointPDFSum += m_ThreaderJointPDFSum[threadID];
    }
  if ( m_JointPDFSum == 0.0 )
    {
    itkExceptionMacro("Joint PDF summed to zero");
    }

  memset( m_MovingImageMarginalPDF,
          0,
          m_NumberOfHistogramBins * sizeof( PDFValueType ) );

  JointPDFValueType *pdfPtr;
  PDFValueType *     movingMarginalPtr;
  unsigned int       i, j;
  double             fixedPDFSum = 0.0;
  const double       normalizationFactor = 1.0 / m_JointPDFSum;

  pdfPtr = m_JointPDF->GetBufferPointer();
  for ( i = 0; i < m_NumberOfHistogramBins; i++ )
    {
    fixedPDFSum += m_FixedImageMarginalPDF[i];
    movingMarginalPtr = m_MovingImageMarginalPDF;
    for ( j = 0; j < m_NumberOfHistogramBins; j++ )
      {
      *( pdfPtr ) *= normalizationFactor;
      *( movingMarginalPtr++ ) += *( pdfPtr++ );
      }
    }

  if ( this->m_NumberOfPixelsCounted <
       this->m_NumberOfFixedImageSamples / 16 )
    {
    itkExceptionMacro("Too many samples map outside moving image buffer: "
                      << this->m_NumberOfPixelsCounted << " / "
                      << this->m_NumberOfFixedImageSamples
                      << std::endl);
    }

  // Normalize the fixed image marginal PDF
  if ( fixedPDFSum == 0.0 )
    {
    itkExceptionMacro("Fixed image marginal PDF summed to zero");
    }
  for ( unsigned int bin = 0; bin < m_NumberOfHistogramBins; bin++ )
    {
    m_FixedImageMarginalPDF[bin] /= fixedPDFSum;
    }

  /**
   * Compute the metric by double summation over histogram.
   */

  // Setup pointer to point to the first bin
  JointPDFValueType *jointPDFPtr = m_JointPDF->GetBufferPointer();

  // Initialize sum to zero
  double sum = 0.0;

  const double nFactor = 1.0 / ( m_MovingImageBinSize
                                 * this->m_NumberOfPixelsCounted );

  for ( unsigned int fixedIndex = 0;
        fixedIndex < m_NumberOfHistogramBins;
        ++fixedIndex )
    {
    double fixedImagePDFValue = m_FixedImageMarginalPDF[fixedIndex];

    for ( unsigned int movingIndex = 0;
          movingIndex < m_NumberOfHistogramBins;
          ++movingIndex, jointPDFPtr++ )
      {
      double movingImagePDFValue = m_MovingImageMarginalPDF[movingIndex];
      double jointPDFValue = *( jointPDFPtr );

      // check for non-zero bin contribution
      if ( jointPDFValue > 1e-16 &&  movingImagePDFValue > 1e-16 )
        {
        double pRatio = vcl_log(jointPDFValue / movingImagePDFValue);

        if ( fixedImagePDFValue > 1e-16 )
          {
          sum += jointPDFValue * ( pRatio - vcl_log(fixedImagePDFValue) );
          }

        if ( this->m_UseExplicitPDFDerivatives )
          {
          // move joint pdf derivative pointer to the right position
          JointPDFValueType *derivPtr = m_JointPDFDerivatives->GetBufferPointer()
                                        + ( fixedIndex  * m_JointPDFDerivatives->GetOffsetTable()[2] )
                                        + ( movingIndex * m_JointPDFDerivatives->GetOffsetTable()[1] );

          for ( unsigned int parameter = 0; parameter < this->m_NumberOfParameters; ++parameter, derivPtr++ )
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

  if ( !( this->m_UseExplicitPDFDerivatives ) )
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
    for ( unsigned int t = 0; t < this->m_NumberOfThreads - 1; t++ )
      {
      DerivativeType *source = &( this->m_ThreaderMetricDerivative[t] );
      for ( unsigned int pp = 0; pp < this->m_NumberOfParameters; pp++ )
        {
        this->m_MetricDerivative[pp] += ( *source )[pp];
        }
      }

    derivative = this->m_MetricDerivative;
    }

  value = static_cast< MeasureType >( -1.0 * sum );
}

/**
 * Get the match measure derivative
 */
template< class TFixedImage, class TMovingImage  >
void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
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
template< class TFixedImage, class TMovingImage >
void
MattesMutualInformationImageToImageMetric< TFixedImage, TMovingImage >
::ComputePDFDerivatives(unsigned int threadID,
                        unsigned int sampleNumber,
                        int pdfMovingIndex,
                        const ImageDerivativesType & movingImageGradientValue,
                        double cubicBSplineDerivativeValue) const
{
  // Update bins in the PDF derivatives for the current intensity pair
  // Could pre-compute
  JointPDFDerivativesValueType *derivPtr;

  double precomputedWeight = 0.0;

  const int pdfFixedIndex = this->m_FixedImageSamples[sampleNumber].valueIndex;

  DerivativeType *derivativeHelperArray = NULL;

  if ( this->m_UseExplicitPDFDerivatives )
    {
    if ( threadID > 0 )
      {
      derivPtr = m_ThreaderJointPDFDerivatives[threadID - 1]->GetBufferPointer()
                 + ( pdfFixedIndex  * m_JointPDFDerivatives->GetOffsetTable()[2] )
                 + ( pdfMovingIndex * m_JointPDFDerivatives->GetOffsetTable()[1] );
      }
    else
      {
      derivPtr = m_JointPDFDerivatives->GetBufferPointer()
                 + ( pdfFixedIndex  * m_JointPDFDerivatives->GetOffsetTable()[2] )
                 + ( pdfMovingIndex * m_JointPDFDerivatives->GetOffsetTable()[1] );
      }
    }
  else
    {
    derivPtr = 0;
    // Recover the precomputed weight for this specific PDF bin
    precomputedWeight = this->m_PRatioArray[pdfFixedIndex][pdfMovingIndex];
    if ( threadID > 0 )
      {
      derivativeHelperArray = &( this->m_ThreaderMetricDerivative[threadID - 1] );
      }
    else
      {
      derivativeHelperArray = &( this->m_MetricDerivative );
      }
    }

  if ( !this->m_TransformIsBSpline )
    {
    /**
     * Generic version which works for all transforms.
     */

    // Compute the transform Jacobian.
    // Should pre-compute
    typedef typename TransformType::JacobianType JacobianType;

    // Need to use one of the threader transforms if we're
    // not in thread 0.
    //
    // Use a raw pointer here to avoid the overhead of smart pointers.
    // For instance, Register and UnRegister have mutex locks around
    // the reference counts.
    TransformType *transform;

    if ( threadID > 0 )
      {
      transform = this->m_ThreaderTransform[threadID - 1];
      }
    else
      {
      transform = this->m_Transform;
      }

    const JacobianType & jacobian =
      transform->GetJacobian(this->m_FixedImageSamples[sampleNumber].point);

    for ( unsigned int mu = 0; mu < this->m_NumberOfParameters; mu++ )
      {
      double innerProduct = 0.0;
      for ( unsigned int dim = 0; dim < Superclass::FixedImageDimension; dim++ )
        {
        innerProduct += jacobian[dim][mu] * movingImageGradientValue[dim];
        }

      const double derivativeContribution = innerProduct * cubicBSplineDerivativeValue;

      if ( this->m_UseExplicitPDFDerivatives )
        {
        *( derivPtr ) -= derivativeContribution;
        ++derivPtr;
        }
      else
        {
        ( *derivativeHelperArray )[mu] += precomputedWeight * derivativeContribution;
        }
      }
    }
  else
    {
    const WeightsValueType *weights = NULL;
    const IndexValueType *  indices = NULL;

    BSplineTransformWeightsType *   weightsHelper = NULL;
    BSplineTransformIndexArrayType *indicesHelper = NULL;

    if ( this->m_UseCachingOfBSplineWeights )
      {
      //
      // If the transform is of type BSplineDeformableTransform, we can obtain
      // a speed up by only processing the affected parameters.  Note that
      // these pointers are just pointing to pre-allocated rows of the caching
      // arrays. There is therefore, no need to free this memory.
      //
      weights = this->m_BSplineTransformWeightsArray[sampleNumber];
      indices = this->m_BSplineTransformIndicesArray[sampleNumber];
      }
    else
      {
      if ( threadID > 0 )
        {
        weightsHelper = &( this->m_ThreaderBSplineTransformWeights[threadID - 1] );
        indicesHelper = &( this->m_ThreaderBSplineTransformIndices[threadID - 1] );
        }
      else
        {
        weightsHelper = &( this->m_BSplineTransformWeights );
        indicesHelper = &( this->m_BSplineTransformIndices );
        }

      this->m_BSplineTransform->GetJacobian(
        this->m_FixedImageSamples[sampleNumber].point,
        *weightsHelper, *indicesHelper);
      }

    for ( unsigned int dim = 0; dim < Superclass::FixedImageDimension; dim++ )
      {
      double innerProduct;
      int    parameterIndex;

      for ( unsigned int mu = 0; mu < this->m_NumBSplineWeights; mu++ )
        {
        /* The array weights contains the Jacobian values in a 1-D array
         * (because for each parameter the Jacobian is non-zero in only 1 of the
         * possible dimensions) which is multiplied by the moving image
         * gradient. */
        if ( this->m_UseCachingOfBSplineWeights )
          {
          innerProduct = movingImageGradientValue[dim] * weights[mu];
          parameterIndex = indices[mu] + this->m_BSplineParametersOffset[dim];
          }
        else
          {
          innerProduct = movingImageGradientValue[dim] * ( *weightsHelper )[mu];
          parameterIndex = ( *indicesHelper )[mu] + this->m_BSplineParametersOffset[dim];
          }

        const double derivativeContribution = innerProduct * cubicBSplineDerivativeValue;

        if ( this->m_UseExplicitPDFDerivatives )
          {
          JointPDFValueType *ptr = derivPtr + parameterIndex;
          *( ptr ) -= derivativeContribution;
          }
        else
          {
          ( *derivativeHelperArray )[parameterIndex] += precomputedWeight * derivativeContribution;
          }
        } //end mu for loop
      }   //end dim for loop
    }     // end if-block transform is BSpline
}
} // end namespace itk

#endif
