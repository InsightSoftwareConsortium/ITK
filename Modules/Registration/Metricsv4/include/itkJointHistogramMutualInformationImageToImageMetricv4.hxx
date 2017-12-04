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
#ifndef itkJointHistogramMutualInformationImageToImageMetricv4_hxx
#define itkJointHistogramMutualInformationImageToImageMetricv4_hxx

#include "itkCompensatedSummation.h"
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkImageIterator.h"
#include "itkDiscreteGaussianImageFilter.h"

namespace itk
{

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>
::JointHistogramMutualInformationImageToImageMetricv4():
  m_JointHistogramTotalCount(0)
{
  // Initialize histogram properties
  this->m_NumberOfHistogramBins = 20;
  this->m_FixedImageTrueMin     = NumericTraits< TInternalComputationValueType >::ZeroValue();
  this->m_FixedImageTrueMax     = NumericTraits< TInternalComputationValueType >::ZeroValue();
  this->m_MovingImageTrueMin    = NumericTraits< TInternalComputationValueType >::ZeroValue();
  this->m_MovingImageTrueMax    = NumericTraits< TInternalComputationValueType >::ZeroValue();
  this->m_FixedImageBinSize     = NumericTraits< TInternalComputationValueType >::ZeroValue();
  this->m_MovingImageBinSize    = NumericTraits< TInternalComputationValueType >::ZeroValue();
  this->m_Padding = 2;
  this->m_JointPDFSum = NumericTraits< TInternalComputationValueType >::ZeroValue();
  this->m_Log2 = std::log(2.0);
  this->m_VarianceForJointPDFSmoothing = 1.5;

  // We have our own GetValueAndDerivativeThreader's that we want
  // ImageToImageMetricv4 to use.
  this->m_DenseGetValueAndDerivativeThreader  = JointHistogramMutualInformationDenseGetValueAndDerivativeThreaderType::New();
  this->m_SparseGetValueAndDerivativeThreader = JointHistogramMutualInformationSparseGetValueAndDerivativeThreaderType::New();

  this->m_JointHistogramMutualInformationDenseComputeJointPDFThreader  = JointHistogramMutualInformationDenseComputeJointPDFThreaderType::New();
  this->m_JointHistogramMutualInformationSparseComputeJointPDFThreader = JointHistogramMutualInformationSparseComputeJointPDFThreaderType::New();
  this->m_JointPDF             = JointPDFType::New();
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>
::~JointHistogramMutualInformationImageToImageMetricv4()
{
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>
::Initialize()
{
  Superclass::Initialize();

  /** Get the fixed and moving image true max's and mins.
   *  Initialize them to the PixelType min and max. */
  this->m_FixedImageTrueMin =
                    NumericTraits<typename TFixedImage::PixelType>::max();
  this->m_FixedImageTrueMax =
                    NumericTraits<typename TFixedImage::PixelType>::NonpositiveMin();
  this->m_MovingImageTrueMin =
                    NumericTraits<typename TMovingImage::PixelType>::max();
  this->m_MovingImageTrueMax =
                    NumericTraits<typename TMovingImage::PixelType>::NonpositiveMin();

  /** Iterate through the fixed image and set the true
   *  max and min for the fixed image. */
  itk::ImageRegionConstIteratorWithIndex<TFixedImage>
                fi(this->m_FixedImage,this->m_FixedImage->GetRequestedRegion());

  /** \todo multi-thread me */
  while( !fi.IsAtEnd() )
    {
    typename TFixedImage::PointType fixedSpacePhysicalPoint;
    this->m_FixedImage->TransformIndexToPhysicalPoint(fi.GetIndex(), fixedSpacePhysicalPoint);
    if ( this->m_FixedImageMask.IsNull()  /* A null mask implies entire space is to be used.*/
         || this->m_FixedImageMask->IsInside(fixedSpacePhysicalPoint) )
       {
       const typename TFixedImage::PixelType currentValue = fi.Get();
       // update the Fixed Image true min accordingly
       if ( currentValue < this->m_FixedImageTrueMin )
         {
         this->m_FixedImageTrueMin = currentValue;
         }
       // update the Fixed Image true max accordingly
       if ( currentValue > this->m_FixedImageTrueMax )
         {
         this->m_FixedImageTrueMax = currentValue;
         }
       }
      ++fi;
    }
  /** Iterate through the moving image and set the true
   * max and min for the moving image. */
  itk::ImageRegionConstIteratorWithIndex<TMovingImage>
              mi(this->m_MovingImage,this->m_MovingImage->GetBufferedRegion());

  while( !mi.IsAtEnd() )
    {
    typename TMovingImage::PointType movingSpacePhysicalPoint;
    this->m_MovingImage->TransformIndexToPhysicalPoint
                                      (mi.GetIndex(), movingSpacePhysicalPoint);

    if ( this->m_MovingImageMask.IsNull() /* A null mask implies entire space is to be used.*/
         || this->m_MovingImageMask->IsInside(movingSpacePhysicalPoint) )
       {
       const typename TMovingImage::PixelType currentValue=mi.Get();
       // update the Moving Image true min accordingly
       if ( currentValue < this->m_MovingImageTrueMin )
         {
         this->m_MovingImageTrueMin = currentValue;
         }
       // update the Moving Image true max accordingly
       if ( currentValue > this->m_MovingImageTrueMax )
         {
         this->m_MovingImageTrueMax = currentValue;
         }
       }
      ++mi;
    }
  itkDebugMacro(" FixedImageMin: " << this->m_FixedImageTrueMin
                                   << " FixedImageMax: "
                                   << this->m_FixedImageTrueMax << std::endl);
  itkDebugMacro(" MovingImageMin: " << this->m_MovingImageTrueMin
                                    << " MovingImageMax: "
                                    << this->m_MovingImageTrueMax << std::endl);


  // Allocate memory for the joint PDF.

  // Instantiate a region, index, size
  JointPDFRegionType jointPDFRegion;
  JointPDFIndexType  jointPDFIndex;
  JointPDFSizeType   jointPDFSize;

  // the jointPDF is of size NumberOfBins x NumberOfBins
  jointPDFSize.Fill(m_NumberOfHistogramBins);
  jointPDFIndex.Fill(0);
  jointPDFRegion.SetIndex(jointPDFIndex);
  jointPDFRegion.SetSize(jointPDFSize);

  // Set the regions and allocate
  this->m_JointPDF->SetRegions(jointPDFRegion);

  //By setting these values, the joint histogram physical locations will correspond to intensity values.
  JointPDFSpacingType spacing;
  spacing[0]=1/(TInternalComputationValueType)(this->m_NumberOfHistogramBins-(TInternalComputationValueType)this->m_Padding*2-1);
  spacing[1]=spacing[0];
  this->m_JointPDF->SetSpacing(spacing);
  this->m_JointPDFSpacing=this->m_JointPDF->GetSpacing();
  JointPDFPointType origin;
  origin[0]=this->m_JointPDFSpacing[0]*(TInternalComputationValueType)this->m_Padding*(-1.0);
  origin[1]=origin[0];
  this->m_JointPDF->SetOrigin(origin);
  this->m_JointPDF->Allocate();

  // do the same thing for the marginal pdfs
  this->m_FixedImageMarginalPDF = MarginalPDFType::New();
  this->m_MovingImageMarginalPDF = MarginalPDFType::New();

  // Instantiate a region, index, size
  typedef typename MarginalPDFType::RegionType  MarginalPDFRegionType;
  typedef typename MarginalPDFType::SizeType    MarginalPDFSizeType;
  MarginalPDFRegionType marginalPDFRegion;
  MarginalPDFIndexType  marginalPDFIndex;
  MarginalPDFSizeType   marginalPDFSize;

  // the marginalPDF is of size NumberOfBins x NumberOfBins
  marginalPDFSize.Fill(m_NumberOfHistogramBins);
  marginalPDFIndex.Fill(0);
  marginalPDFRegion.SetIndex(marginalPDFIndex);
  marginalPDFRegion.SetSize(marginalPDFSize);

  // Set the regions and allocate
  this->m_FixedImageMarginalPDF->SetRegions(marginalPDFRegion);
  this->m_MovingImageMarginalPDF->SetRegions(marginalPDFRegion);

  //By setting these values, the marginal histogram physical locations will correspond to intensity values.
  typename MarginalPDFType::PointType fixedorigin;
  typename MarginalPDFType::PointType movingorigin;
  fixedorigin[0]=origin[0];
  movingorigin[0]=origin[1];
  this->m_FixedImageMarginalPDF->SetOrigin(fixedorigin);
  this->m_MovingImageMarginalPDF->SetOrigin(movingorigin);
  typename MarginalPDFType::SpacingType mspacing;
  mspacing[0]=spacing[0];
  this->m_FixedImageMarginalPDF->SetSpacing(mspacing);
  mspacing[0]=spacing[1];
  this->m_MovingImageMarginalPDF->SetSpacing(mspacing);
  this->m_FixedImageMarginalPDF->Allocate();
  this->m_MovingImageMarginalPDF->Allocate();
}


template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>
::InitializeForIteration() const
{
  Superclass::InitializeForIteration();

  /* Prepare histograms for use in GetValueAndDerivative */

  // Initialize the joint pdf and the fixed and moving image marginal pdfs
  PDFValueType pdfzero = NumericTraits< PDFValueType >::ZeroValue();
  this->m_JointPDF->FillBuffer(pdfzero);
  this->m_FixedImageMarginalPDF->FillBuffer(pdfzero);
  this->m_MovingImageMarginalPDF->FillBuffer(pdfzero);

  /**
   * First, we compute the joint histogram
   */
  if( this->m_UseFixedSampledPointSet )
    {
    SizeValueType numberOfPoints = this->GetNumberOfDomainPoints();
    if( numberOfPoints < 1 )
      {
      itkExceptionMacro("VirtualSampledPointSet must have 1 or more points.");
      }
    typename JointHistogramMutualInformationSparseComputeJointPDFThreaderType::DomainType sampledRange;
    sampledRange[0] = 0;
    sampledRange[1] = numberOfPoints - 1;
    this->m_JointHistogramMutualInformationSparseComputeJointPDFThreader->Execute( const_cast<Self *>(this), sampledRange );
    }
  else
    {
    this->m_JointHistogramMutualInformationDenseComputeJointPDFThreader->Execute( const_cast<Self *>(this), this->GetVirtualRegion() );
    }

  // Optionally smooth the joint pdf
  if (this->m_VarianceForJointPDFSmoothing > NumericTraits< JointPDFValueType >::ZeroValue() )
    {
    typedef DiscreteGaussianImageFilter<JointPDFType,JointPDFType> DgType;
    typename DgType::Pointer dg = DgType::New();
    dg->SetInput(this->m_JointPDF);
    dg->SetVariance(this->m_VarianceForJointPDFSmoothing);
    dg->SetUseImageSpacingOff();
    dg->SetMaximumError(.01f);
    dg->Update();
    this->m_JointPDF = ( dg->GetOutput() );
    }

  // Compute moving image marginal PDF by summing over fixed image bins.
  typedef ImageLinearIteratorWithIndex<JointPDFType> JointPDFLinearIterator;
  JointPDFLinearIterator linearIter(m_JointPDF, m_JointPDF->GetBufferedRegion() );
  linearIter.SetDirection( 0 );
  linearIter.GoToBegin();
  unsigned int fixedIndex = 0;
  CompensatedSummation< TInternalComputationValueType > sum;
  while( !linearIter.IsAtEnd() )
    {
    sum.ResetToZero();
    while( !linearIter.IsAtEndOfLine() )
      {
      sum += linearIter.Get();
      ++linearIter;
      }
    MarginalPDFIndexType mind;
    mind[0] = fixedIndex;
    m_FixedImageMarginalPDF->SetPixel(mind,static_cast<PDFValueType>(sum.GetSum()));
    linearIter.NextLine();
    ++fixedIndex;
    }

  linearIter.SetDirection( 1 );
  linearIter.GoToBegin();
  unsigned int movingIndex = 0;
  while( !linearIter.IsAtEnd() )
    {
    sum.ResetToZero();
    while( !linearIter.IsAtEndOfLine() )
      {
      sum += linearIter.Get();
      ++linearIter;
      }
    MarginalPDFIndexType mind;
    mind[0] = movingIndex;
    m_MovingImageMarginalPDF->SetPixel(mind,static_cast<PDFValueType>(sum.GetSum()));
    linearIter.NextLine();
    ++movingIndex;
    }
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
typename JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>::MeasureType
JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>
::GetValue() const
{
  DerivativeType dummyDeriviative;
  this->m_DerivativeResult = &dummyDeriviative;
  this->InitializeForIteration();
  this->m_NumberOfValidPoints = this->m_JointHistogramTotalCount;
  MeasureType value;
  /* This checks for the default minimum number of valid points.
   * It returns true if minimum is met, otherwise is put default
   * values into value and dummyDerivative and returns false. */
  if( this->VerifyNumberOfValidPoints( value, dummyDeriviative ) )
    {
    this->m_Value = this->ComputeValue();
    }
  return this->m_Value;
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
typename JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>::MeasureType
JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>
::ComputeValue() const
{
  /**
  1- The padding is 2 in this implementation.
  2- The MI energy is bounded in the range of [0  min(H(x),H(y))].
  3- The ComputeMutualInformation() iterator range should cover the entire PDF.
  4- The normalization is done based on NumberOfHistogramBins-1 instead of NumberOfHistogramBins. */
  TInternalComputationValueType px,py,pxy;
  CompensatedSummation< TInternalComputationValueType > total_mi;
  TInternalComputationValueType local_mi;
  TInternalComputationValueType eps = NumericTraits<TInternalComputationValueType>::epsilon();
  typename JointPDFType::IndexType index;
  for (SizeValueType ii = 0; ii<m_NumberOfHistogramBins; ii++)
    {
    MarginalPDFIndexType mind;
    mind[0] = ii;
    px = this->m_FixedImageMarginalPDF->GetPixel(mind);
    for (SizeValueType jj = 0; jj<m_NumberOfHistogramBins; jj++)
      {
      mind[0] = jj;
      py = this->m_MovingImageMarginalPDF->GetPixel(mind);
      TInternalComputationValueType denom = px * py;
      index[0] = ii;
      index[1] = jj;
      pxy = m_JointPDF->GetPixel(index);
      local_mi = 0;
      if ( fabs(denom) > eps )
        {
        if (pxy / denom > eps )
          {
          //the classic mi calculation
          local_mi = pxy * std::log( pxy / denom );
          }
        }
      total_mi += local_mi;
      } // over jh bins 2
    } // over jh bins 1
  return ( -1.0 * total_mi.GetSum() / this->m_Log2  );
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>
::ComputeJointPDFPoint( const FixedImagePixelType fixedImageValue,
                        const MovingImagePixelType movingImageValue,
                        JointPDFPointType& jointPDFpoint ) const
{
    TInternalComputationValueType a = ( fixedImageValue - this->m_FixedImageTrueMin ) / ( this->m_FixedImageTrueMax - this->m_FixedImageTrueMin );
    TInternalComputationValueType b = ( movingImageValue - this->m_MovingImageTrueMin ) / ( this->m_MovingImageTrueMax - this->m_MovingImageTrueMin );
    jointPDFpoint[0] = a;
    jointPDFpoint[1] = b;
}

template <typename TFixedImage, typename TMovingImage, typename TVirtualImage, typename TInternalComputationValueType, typename TMetricTraits>
void
JointHistogramMutualInformationImageToImageMetricv4<TFixedImage,TMovingImage,TVirtualImage,TInternalComputationValueType, TMetricTraits>
::PrintSelf (std::ostream & os, Indent indent) const
{
  // Print the superclass
  Superclass::PrintSelf(os,indent);

  os << indent << "NumberOfHistogramBins: ";
  os << this->m_NumberOfHistogramBins << std::endl;
  os << indent << "MovingImageTrueMin: ";
  os << this->m_MovingImageTrueMin << std::endl;
  os << indent << "MovingImageTrueMax: ";
  os << this->m_MovingImageTrueMax << std::endl;
  os << indent << "FixedImageBinSize: ";
  os << this->m_FixedImageBinSize << std::endl;
  os << indent << "MovingImageBinSize: ";
  os << this->m_MovingImageBinSize << std::endl;

  if( this->m_JointPDF.IsNotNull() )
    {
    os << indent << "JointPDF: ";
    os << this->m_JointPDF << std::endl;
    }
}

} // end namespace itk

#endif
