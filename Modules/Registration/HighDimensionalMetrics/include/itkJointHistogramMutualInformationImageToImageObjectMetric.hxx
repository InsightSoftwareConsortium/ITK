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

#ifndef __itkJointHistogramMutualInformationImageToImageObjectMetric_hxx
#define __itkJointHistogramMutualInformationImageToImageObjectMetric_hxx

#include "itkJointHistogramMutualInformationImageToImageObjectMetric.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageIterator.h"
#include "itkDiscreteGaussianImageFilter.h"

namespace itk
{
/**
  Constructor
 */
template <class TFixedImage,class TMovingImage,class TVirtualImage>
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::JointHistogramMutualInformationImageToImageObjectMetric()
{
  // Initialize histogram properties
  this->m_NumberOfHistogramBins = 20;
  this->m_FixedImageTrueMin = (0.0);
  this->m_FixedImageTrueMax = (0.0);
  this->m_MovingImageTrueMin = (0.0);
  this->m_MovingImageTrueMax = (0.0);
  this->m_FixedImageBinSize = (0.0);
  this->m_MovingImageBinSize = (0.0);
  this->m_Padding = 2;
  this->m_JointPDFSum = (0.0);
  this->m_ThreaderJointPDFInterpolator = NULL;
  this->m_ThreaderMovingImageMarginalPDFInterpolator = NULL;
  this->m_ThreaderFixedImageMarginalPDFInterpolator = NULL;
  this->m_Log2 = vcl_log(2.0);
  this->m_VarianceForJointPDFSmoothing = 1.5;
}

/**
 * Destructor
 */
template <class TFixedImage, class TMovingImage, class TVirtualImage>
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
::~JointHistogramMutualInformationImageToImageObjectMetric()
{
  if ( this->m_ThreaderFixedImageMarginalPDFInterpolator != NULL )
    {
    delete[] this->m_ThreaderFixedImageMarginalPDFInterpolator;
    }
  if ( this->m_ThreaderMovingImageMarginalPDFInterpolator != NULL )
    {
    delete[] this->m_ThreaderMovingImageMarginalPDFInterpolator;
    }
  if ( this->m_ThreaderJointPDFInterpolator != NULL )
    {
    delete[] this->m_ThreaderJointPDFInterpolator;
    }

}

/** Initialize the metric */
template <class TFixedImage,class TMovingImage,class TVirtualImage>
void
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::Initialize() throw (itk::ExceptionObject)
{
  Superclass::Initialize();
  /** Get the fixed and moving image true max's and mins.
   *  Initialize them to the PixelType min and max. */
  this->m_FixedImageTrueMin =
                    vcl_numeric_limits<typename TFixedImage::PixelType>::max();
  this->m_FixedImageTrueMax =
                    vcl_numeric_limits<typename TFixedImage::PixelType>::min();
  this->m_MovingImageTrueMin =
                    vcl_numeric_limits<typename TMovingImage::PixelType>::max();
  this->m_MovingImageTrueMax =
                    vcl_numeric_limits<typename TMovingImage::PixelType>::min();

  /** Iterate through the fixed image and set the true
   *  max and min for the fixed image. */
  itk::ImageRegionConstIteratorWithIndex<TFixedImage>
                fi(this->m_FixedImage,this->m_FixedImage->GetRequestedRegion());

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
  this->m_JointPDF = JointPDFType::New();

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
  spacing[0]=1/(InternalComputationValueType)(this->m_NumberOfHistogramBins-(InternalComputationValueType)this->m_Padding*2-1);
  spacing[1]=spacing[0];
  this->m_JointPDF->SetSpacing(spacing);
  this->m_JointPDFSpacing=this->m_JointPDF->GetSpacing();
  JointPDFPointType origin;
  origin[0]=this->m_JointPDFSpacing[0]*(InternalComputationValueType)this->m_Padding*(-1.0);
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

   // Threaded data
   if (this->m_ThreaderJointPDFInterpolator != NULL)
    {
    delete[] this->m_ThreaderJointPDFInterpolator;
    }
   this->m_ThreaderJointPDFInterpolator = new JointPDFInterpolatorPointer[this->GetNumberOfThreads() ];
   if (this->m_ThreaderFixedImageMarginalPDFInterpolator != NULL)
    {
    delete[] this->m_ThreaderFixedImageMarginalPDFInterpolator;
    }
   this->m_ThreaderFixedImageMarginalPDFInterpolator = new MarginalPDFInterpolatorPointer[this->GetNumberOfThreads() ];

   if (this->m_ThreaderMovingImageMarginalPDFInterpolator != NULL)
    {
    delete[] this->m_ThreaderMovingImageMarginalPDFInterpolator;
    }
   this->m_ThreaderMovingImageMarginalPDFInterpolator = new MarginalPDFInterpolatorPointer[this->GetNumberOfThreads() ];

   for (ThreadIdType threadID = 0; threadID < this->GetNumberOfThreads(); threadID++)
     {
     this->m_ThreaderJointPDFInterpolator[threadID] = JointPDFInterpolatorType::New();
     this->m_ThreaderJointPDFInterpolator[threadID]->SetInputImage(this->m_JointPDF);
     this->m_ThreaderFixedImageMarginalPDFInterpolator[threadID] = MarginalPDFInterpolatorType::New();
     this->m_ThreaderFixedImageMarginalPDFInterpolator[threadID]->SetInputImage(this->m_FixedImageMarginalPDF);
     this->m_ThreaderMovingImageMarginalPDFInterpolator[threadID] = MarginalPDFInterpolatorType::New();
     this->m_ThreaderMovingImageMarginalPDFInterpolator[threadID]->SetInputImage(this->m_MovingImageMarginalPDF);
     }

}


/**
 * Prepare histograms for use in GetValueAndDerivative
 */
template <class TFixedImage, class TMovingImage, class TVirtualImage>
void
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::UpdateHistograms() const
{
  // Initialize the joint pdf and the fixed and moving image marginal pdfs
  PDFValueType pdfzero = NumericTraits< PDFValueType >::Zero;
  this->m_JointPDF->FillBuffer(pdfzero);
  this->m_FixedImageMarginalPDF->FillBuffer(pdfzero);
  this->m_MovingImageMarginalPDF->FillBuffer(pdfzero);

  /**
   * First, we compute the joint histogram
   */

  /* Create an iterator over the virtual sub region */
  ImageRegionConstIteratorWithIndex<typename Superclass::VirtualImageType>
    ItV( this->GetVirtualDomainImage(), this->GetVirtualDomainRegion() );

  typename Superclass::VirtualPointType            virtualPoint;
  typename Superclass::VirtualIndexType            virtualIndex;
  typename Superclass::FixedOutputPointType        mappedFixedPoint;
  typename Superclass::FixedImagePixelType         fixedImageValue;
  FixedImageGradientType   fixedImageGradients;
  typename Superclass::MovingOutputPointType       mappedMovingPoint;
  typename Superclass::MovingImagePixelType        movingImageValue;
  MovingImageGradientType  movingImageGradients;
  bool                                             pointIsValid = false;

  /* Iterate over the sub region */
  /* FIXME - do this w/out using a raw pointer. Probably need separate
   * Set accessors and/or Initialize within SamplingIteratorHelper. */
  typedef typename Superclass::SamplingIteratorHelper SamplingIteratorHelperType;
  SamplingIteratorHelperType * iterator;
  if( this->m_UseFixedSampledPointSet )
    {
    typename Superclass::SampledThreaderInputObjectType sampledRange;
    sampledRange[0] = 0;
    sampledRange[1] = this->m_VirtualSampledPointSet->GetNumberOfPoints() - 1;
    iterator = new SamplingIteratorHelperType( this->m_VirtualDomainImage,
      this->m_VirtualSampledPointSet, sampledRange );
    }
  else
    {
    iterator = new SamplingIteratorHelperType( this->m_VirtualDomainImage,
                                               this->GetVirtualDomainRegion() );
    }

  while( iterator->GetNext( virtualIndex, virtualPoint ) )
    {
    try
      {
      this->TransformAndEvaluateFixedPoint( virtualIndex,
                                            virtualPoint,
                                            false /*compute gradient*/,
                                            mappedFixedPoint,
                                            fixedImageValue,
                                            fixedImageGradients,
                                            pointIsValid );
      if( pointIsValid )
        {
        this->TransformAndEvaluateMovingPoint( virtualIndex,
                                              virtualPoint,
                                              false /*compute gradient*/,
                                              mappedMovingPoint,
                                              movingImageValue,
                                              movingImageGradients,
                                              pointIsValid );
        }
      }
    catch( ExceptionObject & exc )
      {
      //NOTE: there must be a cleaner way to do this:
      std::string msg("Caught exception: \n");
      msg += exc.what();
      ExceptionObject err(__FILE__, __LINE__, msg);
      throw err;
      }
    /** add the paired intensity points to the joint histogram */
    JointPDFPointType jointPDFpoint;
    this->ComputeJointPDFPoint(fixedImageValue,movingImageValue, jointPDFpoint,0);
    JointPDFIndexType  jointPDFIndex;
    jointPDFIndex.Fill( 0 );
    this->m_JointPDF->TransformPhysicalPointToIndex( jointPDFpoint, jointPDFIndex);
    this->m_JointPDF->SetPixel( jointPDFIndex, this->m_JointPDF->GetPixel(jointPDFIndex)+1);
    }

  delete iterator;

  /**
   * Normalize the PDFs, compute moving image marginal PDF
   *
   */
  typedef ImageRegionIterator<JointPDFType> JointPDFIteratorType;
  JointPDFIteratorType jointPDFIterator ( m_JointPDF, m_JointPDF->GetBufferedRegion() );

  // Compute joint PDF normalization factor (to ensure joint PDF sum adds to 1.0)
  InternalComputationValueType jointPDFSum = 0.0;
  jointPDFIterator.GoToBegin();
  while( !jointPDFIterator.IsAtEnd() )
    {
    float temp = jointPDFIterator.Get();
    jointPDFSum += temp;
    ++jointPDFIterator;
    }

  if ( jointPDFSum == NumericTraits< JointPDFValueType >::Zero )
    {
    itkExceptionMacro( "Joint PDF summed to zero" );
    }

  // Normalize the PDF bins
  jointPDFIterator.GoToEnd();
  while( !jointPDFIterator.IsAtBegin() )
    {
    --jointPDFIterator;
    jointPDFIterator.Value() /= static_cast<PDFValueType>( jointPDFSum );
    }

  // Optionally smooth the joint pdf
  if (this->m_VarianceForJointPDFSmoothing > NumericTraits< JointPDFValueType >::Zero )
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
  while( !linearIter.IsAtEnd() )
    {
    InternalComputationValueType sum = NumericTraits< InternalComputationValueType >::Zero;
    while( !linearIter.IsAtEndOfLine() )
      {
      sum += linearIter.Get();
      ++linearIter;
      }
    MarginalPDFIndexType mind;
    mind[0] = fixedIndex;
    m_FixedImageMarginalPDF->SetPixel(mind,static_cast<PDFValueType>(sum));
    linearIter.NextLine();
    ++fixedIndex;
    }

  linearIter.SetDirection( 1 );
  linearIter.GoToBegin();
  unsigned int movingIndex = 0;
  while( !linearIter.IsAtEnd() )
    {
    InternalComputationValueType sum = NumericTraits< InternalComputationValueType >::Zero;
    while( !linearIter.IsAtEndOfLine() )
      {
      sum += linearIter.Get();
      ++linearIter;
      }
    MarginalPDFIndexType mind;
    mind[0] = movingIndex;
    m_MovingImageMarginalPDF->SetPixel(mind,static_cast<PDFValueType>(sum));
    linearIter.NextLine();
    ++movingIndex;
    }

}

/** Get the value and derivative */
template <class TFixedImage, class TMovingImage, class TVirtualImage>
void
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const
{
  // Prepare the histograms
  this->UpdateHistograms();

  // Calculate value
  this->m_Value = this->GetValue();
  itkDebugMacro(" Mutual information value " << this->m_Value );

  // Multithreaded initiate and process sample.
  // This will put results in 'derivative'.
  this->GetValueAndDerivativeThreadedExecute( derivative );

  // Post processing
  this->GetValueAndDerivativeThreadedPostProcess( true /*doAverage*/ );

  // Return value.
  value = this->m_Value;
}


/** Process the sample point*/
template <class TFixedImage,class TMovingImage,class TVirtualImage>
bool
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
::GetValueAndDerivativeProcessPoint(
                    const VirtualPointType &,
                    const FixedImagePointType &,
                    const FixedImagePixelType &        fixedImageValue,
                    const FixedImageGradientType &,
                    const MovingImagePointType &       mappedMovingPoint,
                    const MovingImagePixelType &       movingImageValue,
                    const MovingImageGradientType &   movingImageGradient,
                    MeasureType &,
                    DerivativeType &                   localDerivativeReturn,
                    const ThreadIdType                 threadID) const
{
  // check that the moving image sample is within the range of the true min
  // and max, hence being within the moving image mask
  if ( movingImageValue < this->m_MovingImageTrueMin )
    {
    return false;
    }
  else if ( movingImageValue > this->m_MovingImageTrueMax )
    {
    return false;
    }
  /** the scalingfactor is the MI specific scaling of the image gradient and jacobian terms */
  InternalComputationValueType scalingfactor = 0; // for scaling the jacobian terms

  JointPDFPointType jointPDFpoint;
  bool pointok = this->ComputeJointPDFPoint( fixedImageValue,movingImageValue, jointPDFpoint,threadID);
  if ( !pointok )
    {
    return false;
    }
  InternalComputationValueType jointPDFValue =
    this->m_ThreaderJointPDFInterpolator[threadID]->Evaluate(jointPDFpoint);
  SizeValueType ind = 1;
  InternalComputationValueType dJPDF = this->ComputeJointPDFDerivative( jointPDFpoint, threadID , ind );
  typename MarginalPDFType::PointType mind;
  mind[0] = jointPDFpoint[ind];
  InternalComputationValueType movingImagePDFValue =
    this->m_ThreaderMovingImageMarginalPDFInterpolator[threadID]->Evaluate(mind);
  InternalComputationValueType dMmPDF =
    this->ComputeMovingImageMarginalPDFDerivative( mind , threadID );

  InternalComputationValueType term1 = NumericTraits< InternalComputationValueType >::Zero;
  InternalComputationValueType term2 = NumericTraits< InternalComputationValueType >::Zero;
  InternalComputationValueType eps = 1.e-16;
  if( jointPDFValue > eps &&  (movingImagePDFValue) > eps )
    {
    const InternalComputationValueType pRatio =
                            vcl_log(jointPDFValue)-vcl_log(movingImagePDFValue);
    term1 = dJPDF*pRatio;
    term2 = vcl_log(2.0) * dMmPDF * jointPDFValue / movingImagePDFValue;
    scalingfactor =  ( term2 - term1 );
    }  // end if-block to check non-zero bin contribution
  else
    {
    scalingfactor = 0;
    }

  /* Use a pre-allocated jacobian object for efficiency */
  FixedTransformJacobianType & jacobian =
    const_cast< FixedTransformJacobianType &   >(this->m_MovingTransformJacobianPerThread[threadID]);

  /** For dense transforms, this returns identity */
  this->m_MovingTransform->ComputeJacobianWithRespectToParameters(
                                                            mappedMovingPoint,
                                                            jacobian);

  // this correction is necessary for consistent derivatives across N threads
  typedef typename DerivativeType::ValueType    DerivativeValueType;
  DerivativeValueType floatingpointcorrectionresolution = 10000.0;
  // NOTE: change 'unsigned int' here when we have NumberOfParametersType
  // defined in metric base.
  for ( unsigned int par = 0; par < this->GetNumberOfLocalParameters(); par++ )
    {
    InternalComputationValueType sum = NumericTraits< InternalComputationValueType >::Zero;
    for ( SizeValueType dim = 0; dim < this->MovingImageDimension; dim++ )
      {
      sum += scalingfactor * jacobian(dim, par) * movingImageGradient[dim];
      }
    localDerivativeReturn[par] = sum;
    intmax_t test = static_cast<intmax_t>
             ( localDerivativeReturn[par] * floatingpointcorrectionresolution );
    localDerivativeReturn[par] = static_cast<DerivativeValueType>
                                   ( test / floatingpointcorrectionresolution );
    }
  return true;
}


// Get the value
template <class TFixedImage, class TMovingImage, class TVirtualImage>
typename JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>::MeasureType
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::GetValue() const
{
  /**
  1- The padding is 2 in this implementation.
  2- The MI energy is bounded in the range of [0  min(H(x),H(y))].
  3- The ComputeMutualInformation() iterator range should cover the entire PDF.
  4- The normalization is done based on NumberOfHistogramBins-1 instead of NumberOfHistogramBins. */
  InternalComputationValueType px,py,pxy;
  InternalComputationValueType total_mi = NumericTraits< InternalComputationValueType >::Zero;
  InternalComputationValueType local_mi;
  InternalComputationValueType eps =
                        NumericTraits<InternalComputationValueType>::epsilon();
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
      InternalComputationValueType denom = px * py;
      index[0] = ii;
      index[1] = jj;
      pxy = m_JointPDF->GetPixel(index);
      local_mi = 0;
      if ( fabs(denom) > eps )
        {
        if (pxy / denom > eps )
          {
          //the classic mi calculation
          local_mi = pxy * vcl_log( pxy / denom );
          }
        }
      total_mi += local_mi;
      } // over jh bins 2
    } // over jh bins 1
  return ( -1.0 * total_mi / this->m_Log2  );
}

template <class TFixedImage, class TMovingImage, class TVirtualImage>
bool
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
::ComputeJointPDFPoint( const FixedImagePixelType fixedImageValue,
                        const MovingImagePixelType movingImageValue,
                        JointPDFPointType& jointPDFpoint,
                        const ThreadIdType threadID ) const
{
    InternalComputationValueType a =
        ( fixedImageValue - this->m_FixedImageTrueMin ) /
          ( this->m_FixedImageTrueMax - this->m_FixedImageTrueMin );
    InternalComputationValueType b =
        ( movingImageValue - this->m_MovingImageTrueMin ) /
           ( this->m_MovingImageTrueMax - this->m_MovingImageTrueMin );
    jointPDFpoint[0] = a;
    jointPDFpoint[1] = b;
    bool isInsideBuffer = this->m_ThreaderJointPDFInterpolator[threadID]->
                                                IsInsideBuffer(jointPDFpoint );
    return isInsideBuffer;
}

template <class TFixedImage, class TMovingImage, class TVirtualImage>
typename JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>::InternalComputationValueType
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
::ComputeFixedImageMarginalPDFDerivative(
                                        const MarginalPDFPointType & margPDFpoint,
                                        const ThreadIdType threadID ) const
{
  InternalComputationValueType offset = 0.5*this->m_JointPDFSpacing[0];
  InternalComputationValueType eps = this->m_JointPDFSpacing[0];
  MarginalPDFPointType         leftpoint = margPDFpoint;
  leftpoint[0] -= offset;
  MarginalPDFPointType  rightpoint = margPDFpoint;
  rightpoint[0] += offset;
  if (leftpoint[0] < eps )
    {
    leftpoint[0] = eps;
    }
  if (rightpoint[0] < eps )
    {
    rightpoint[0] = eps;
    }
  if (leftpoint[0] > 1 )
    {
    leftpoint[0] = 1;
    }
  if (rightpoint[0] > 1  )
    {
    rightpoint[0] = 1;
    }
  InternalComputationValueType delta = rightpoint[0]-leftpoint[0];
  if ( delta > 0 )
    {
    InternalComputationValueType deriv = this->m_ThreaderFixedImageMarginalPDFInterpolator[threadID]->Evaluate(rightpoint) -
      this->m_ThreaderFixedImageMarginalPDFInterpolator[threadID]->Evaluate(leftpoint);
    return deriv/delta;
    }
  else
    {
    return 0;
    }
}

template <class TFixedImage, class TMovingImage, class TVirtualImage>
typename JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>::InternalComputationValueType
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
::ComputeMovingImageMarginalPDFDerivative(
                                        const MarginalPDFPointType & margPDFpoint,
                                        const ThreadIdType threadID ) const
{
  InternalComputationValueType offset = 0.5*this->m_JointPDFSpacing[0];
  InternalComputationValueType eps = this->m_JointPDFSpacing[0];
  MarginalPDFPointType  leftpoint = margPDFpoint;
  leftpoint[0] -= offset;
  MarginalPDFPointType  rightpoint = margPDFpoint;
  rightpoint[0] += offset;
  if( leftpoint[0] < eps )
    {
    leftpoint[0] = eps;
    }
  if( rightpoint[0] < eps )
    {
    rightpoint[0] = eps;
    }
  if( leftpoint[0] > 1 )
    {
    leftpoint[0] = 1;
    }
  if( rightpoint[0] > 1  )
    {
    rightpoint[0] = 1;
    }
  InternalComputationValueType delta = rightpoint[0] - leftpoint[0];
  if ( delta > 0 )
    {
    InternalComputationValueType deriv =
      this->m_ThreaderMovingImageMarginalPDFInterpolator[threadID]->Evaluate(rightpoint) -
      this->m_ThreaderMovingImageMarginalPDFInterpolator[threadID]->Evaluate(leftpoint);
    return deriv/delta;
    }
  else
    {
    return 0;
    }
}

template <class TFixedImage, class TMovingImage, class TVirtualImage>
typename JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>::InternalComputationValueType
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
::ComputeJointPDFDerivative( const JointPDFPointType & jointPDFpoint,
                             const ThreadIdType threadID,
                             const SizeValueType ind ) const
{
  InternalComputationValueType offset = 0.5*this->m_JointPDFSpacing[ind];
  InternalComputationValueType eps = this->m_JointPDFSpacing[ind];
  JointPDFPointType  leftpoint = jointPDFpoint;
  leftpoint[ind] -= offset;
  JointPDFPointType  rightpoint = jointPDFpoint;
  rightpoint[ind] += offset;

  if (leftpoint[ind] < eps )
    {
    leftpoint[ind] = eps;
    }

  if (rightpoint[ind] < eps )
    {
    rightpoint[ind] = eps;
    }

  if (leftpoint[ind] > 1 )
    {
    leftpoint[ind] = 1;
    }

  if (rightpoint[ind] > 1 )
    {
    rightpoint[ind] = 1;
    }

  InternalComputationValueType delta = rightpoint[ind] - leftpoint[ind];
  InternalComputationValueType deriv = 0;
  if ( delta > 0 )
    {
    deriv = this->m_ThreaderJointPDFInterpolator[threadID]->Evaluate(rightpoint)-
          this->m_ThreaderJointPDFInterpolator[threadID]->Evaluate(leftpoint);
    return deriv/delta;
    }
  else
    {
    return deriv;
    }
}

/** Print function */
template <class TFixedImage, class TMovingImage, class TVirtualImage>
void
JointHistogramMutualInformationImageToImageObjectMetric<TFixedImage, TMovingImage, TVirtualImage>
::PrintSelf (std::ostream & os, Indent indent) const
{
  // Print the superclass
  Superclass::Print(os);

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
