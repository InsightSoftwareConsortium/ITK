/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMattesMutualInformationImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMattesMutualInformationImageToImageMetric_txx
#define _itkMattesMutualInformationImageToImageMetric_txx

#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkCovariantVector.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkImageIterator.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_numeric_limits.h"
#include "itkBSplineDeformableTransform.h"


namespace itk
{


/**
 * Constructor
 */
template < class TFixedImage, class TMovingImage >
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::MattesMutualInformationImageToImageMetric()
{

  m_NumberOfSpatialSamples = 500;
  m_NumberOfHistogramBins = 50;

  m_ComputeGradient = false; // don't use the default gradient for now

  m_InterpolatorIsBSpline = false;
  m_TransformIsBSpline    = false;

  // Initialize PDFs to NULL
  m_JointPDF = NULL;
  m_JointPDFDerivatives = NULL;

  typename BSplineTransformType::Pointer transformer = BSplineTransformType::New();
  this->SetTransform (transformer);

  typename BSplineInterpolatorType::Pointer interpolator = BSplineInterpolatorType::New();
  this->SetInterpolator (interpolator);

  // Initialize memory
  m_MovingImageNormalizedMin = 0.0;
  m_FixedImageNormalizedMin = 0.0;
  m_MovingImageTrueMin = 0.0;
  m_MovingImageTrueMax = 0.0;
  m_FixedImageBinSize = 0.0;
  m_MovingImageBinSize = 0.0;
  m_CubicBSplineDerivativeKernel = NULL;
  m_BSplineInterpolator = NULL;
  m_DerivativeCalculator = NULL;
  m_NumParametersPerDim = 0;
  m_NumBSplineWeights = 0;
  m_BSplineTransform = NULL;
  m_NumberOfParameters = 0;

}


/**
 * Print out internal information about this class
 */
template < class TFixedImage, class TMovingImage  >
void
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfSpatialSamples: ";
  os << m_NumberOfSpatialSamples << std::endl;
  os << indent << "NumberOfHistogramBins: ";
  os << m_NumberOfHistogramBins << std::endl;

  // Debugging information
  os << indent << "NumberOfParameters: ";
  os << m_NumberOfParameters << std::endl;
  os << indent << "FixedImageNormalizedMin: ";
  os << m_FixedImageNormalizedMin << std::endl;
  os << indent << "MovingImageNormalizedMin: ";
  os << m_MovingImageNormalizedMin << std::endl;
  os << indent << "MovingImageTrueMin: ";
  os << m_MovingImageTrueMin << std::endl;
  os << indent << "MovingImageTrueMax: ";
  os << m_MovingImageTrueMax << std::endl;
  os << indent << "FixedImageBinSize: "; 
  os << m_FixedImageBinSize << std::endl;
  os << indent << "MovingImageBinSize: ";
  os << m_MovingImageBinSize << std::endl;
  os << indent << "InterpolatorIsBSpline: ";
  os << m_InterpolatorIsBSpline << std::endl;
  os << indent << "TransformIsBSpline: ";
  os << m_TransformIsBSpline << std::endl;
  
}


/**
 * Initialize
 */
template <class TFixedImage, class TMovingImage> 
void
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::Initialize(void) throw ( ExceptionObject )
{

  this->Superclass::Initialize();
  
  // Cache the number of transformation parameters
  m_NumberOfParameters = m_Transform->GetNumberOfParameters();

  /**
   * Compute the minimum and maximum for the FixedImage over
   * the FixedImageRegion.
   *
   * NB: We can't use StatisticsImageFilter to do this because
   * the filter computes the min/max for the largest possible region
   */
  double fixedImageMin = NumericTraits<double>::max();
  double fixedImageMax = NumericTraits<double>::NonpositiveMin();

  typedef ImageRegionConstIterator<FixedImageType> FixedIteratorType;
  FixedIteratorType fixedImageIterator( 
    m_FixedImage, this->GetFixedImageRegion() );

  for ( fixedImageIterator.GoToBegin(); 
        !fixedImageIterator.IsAtEnd(); ++fixedImageIterator )
    {

    double sample = static_cast<double>( fixedImageIterator.Get() );

    if ( sample < fixedImageMin )
      {
      fixedImageMin = sample;
      }

    if ( sample > fixedImageMax )
      {
      fixedImageMax = sample;
      }
    }


  /**
   * Compute the minimum and maximum for the entire moving image
   * in the buffer.
   */
  double movingImageMin = NumericTraits<double>::max();
  double movingImageMax = NumericTraits<double>::NonpositiveMin();

  typedef ImageRegionConstIterator<MovingImageType> MovingIteratorType;
  MovingIteratorType movingImageIterator(
    m_MovingImage, m_MovingImage->GetBufferedRegion() );

  for ( movingImageIterator.GoToBegin(); 
        !movingImageIterator.IsAtEnd(); ++movingImageIterator)
    {
    double sample = static_cast<double>( movingImageIterator.Get() );

    if ( sample < movingImageMin )
      {
      movingImageMin = sample;
      }

    if ( sample > movingImageMax )
      {
      movingImageMax = sample;
      }
    }

  m_MovingImageTrueMin = movingImageMin;
  m_MovingImageTrueMax = movingImageMax;

  itkDebugMacro( " FixedImageMin: " << fixedImageMin << 
                 " FixedImageMax: " << fixedImageMax << std::endl );
  itkDebugMacro( " MovingImageMin: " << movingImageMin << 
                 " MovingImageMax: " << movingImageMax << std::endl );


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

  m_FixedImageBinSize = ( fixedImageMax - fixedImageMin ) /
    static_cast<double>( m_NumberOfHistogramBins - 2 * padding );
  m_FixedImageNormalizedMin = fixedImageMin / m_FixedImageBinSize - 
    static_cast<double>( padding );

  m_MovingImageBinSize = ( movingImageMax - movingImageMin ) /
    static_cast<double>( m_NumberOfHistogramBins - 2 * padding );
  m_MovingImageNormalizedMin = movingImageMin / m_MovingImageBinSize -
    static_cast<double>( padding );


  itkDebugMacro( "FixedImageNormalizedMin: " << m_FixedImageNormalizedMin );
  itkDebugMacro( "MovingImageNormalizedMin: " << m_MovingImageNormalizedMin );
  itkDebugMacro( "FixedImageBinSize: " << m_FixedImageBinSize );
  itkDebugMacro( "MovingImageBinSize; " << m_MovingImageBinSize );
  
  /**
   * Allocate memory for the fixed image sample container.
   */
  m_FixedImageSamples.resize( m_NumberOfSpatialSamples);

  /**
   * Allocate memory for the marginal PDF and initialize values
   * to zero. The marginal PDFs are stored as std::vector.
   */
  m_FixedImageMarginalPDF.resize( m_NumberOfHistogramBins, 0.0 );
  m_MovingImageMarginalPDF.resize( m_NumberOfHistogramBins, 0.0 );

  /**
   * Allocate memory for the joint PDF and joint PDF derivatives.
   * The joint PDF and joint PDF derivatives are store as itk::Image.
   */
  m_JointPDF = JointPDFType::New();
  m_JointPDFDerivatives = JointPDFDerivativesType::New();

  // Instantiate a region, index, size
  JointPDFRegionType            jointPDFRegion;
  JointPDFIndexType              jointPDFIndex;
  JointPDFSizeType              jointPDFSize;

  JointPDFDerivativesRegionType  jointPDFDerivativesRegion;
  JointPDFDerivativesIndexType  jointPDFDerivativesIndex;
  JointPDFDerivativesSizeType    jointPDFDerivativesSize;

  // For the joint PDF define a region starting from {0,0} 
  // with size {m_NumberOfHistogramBins, m_NumberOfHistogramBins}
  jointPDFIndex.Fill( 0 ); 
  jointPDFSize.Fill( m_NumberOfHistogramBins ); 

  jointPDFRegion.SetIndex( jointPDFIndex );
  jointPDFRegion.SetSize( jointPDFSize );

  // Set the regions and allocate
  m_JointPDF->SetRegions( jointPDFRegion );
  m_JointPDF->Allocate();

  // For the derivatives of the joint PDF define a region starting from {0,0,0} 
  // with size {m_NumberOfHistogramBins, 
  // m_NumberOfHistogramBins, m_NumberOfParameters}
  jointPDFDerivativesIndex.Fill( 0 ); 
  jointPDFDerivativesSize[0] = m_NumberOfHistogramBins;
  jointPDFDerivativesSize[1] = m_NumberOfHistogramBins;
  jointPDFDerivativesSize[2] = m_NumberOfParameters;

  jointPDFDerivativesRegion.SetIndex( jointPDFDerivativesIndex );
  jointPDFDerivativesRegion.SetSize( jointPDFDerivativesSize );

  // Set the regions and allocate
  m_JointPDFDerivatives->SetRegions( jointPDFDerivativesRegion );
  m_JointPDFDerivatives->Allocate();


  /**
   * Setup the kernels used for the Parzen windows.
   */
  m_CubicBSplineKernel = CubicBSplineFunctionType::New();
  m_CubicBSplineDerivativeKernel = CubicBSplineDerivativeFunctionType::New();    


  /** 
   * Uniformly sample the fixed image (within the fixed image region)
   * to create the sample points list.
   */
  this->SampleFixedImageDomain( m_FixedImageSamples );

  
  /**
   * Check if the interpolator is of type BSplineInterpolateImageFunction.
   * If so, we can make use of its EvaluateDerivatives method.
   * Otherwise, we instantiate an external central difference
   * derivative calculator.
   *
   * TODO: Also add it the possibility of using the default gradient
   * provided by the superclass.
   *
   */
  m_InterpolatorIsBSpline = true;

  BSplineInterpolatorType * testPtr = dynamic_cast<BSplineInterpolatorType *>(
    m_Interpolator.GetPointer() );
  if ( !testPtr )
    {
    m_InterpolatorIsBSpline = false;

    m_DerivativeCalculator = DerivativeFunctionType::New();
    m_DerivativeCalculator->SetInputImage( m_MovingImage );

    m_BSplineInterpolator = NULL;
    itkDebugMacro( "Interpolator is not BSpline" );
    } 
  else
    {
    m_BSplineInterpolator = testPtr;
    m_DerivativeCalculator = NULL;
    itkDebugMacro( "Interpolator is BSpline" );
    }

  /** 
   * Check if the transform is of type BSplineDeformableTransform.
   * If so, we can speed up derivative calculations by only inspecting
   * the parameters in the support region of a point.
   *
   */
  m_TransformIsBSpline = true;

  BSplineTransformType * testPtr2 = dynamic_cast<BSplineTransformType *>(
    m_Transform.GetPointer() );
  if( !testPtr2 )
    {
    m_TransformIsBSpline = false;
    m_BSplineTransform = NULL;
    itkDebugMacro( "Transform is not BSplineDeformable" );
    }
  else
    {
    m_BSplineTransform = testPtr2;
    m_NumParametersPerDim = m_BSplineTransform->GetNumberOfParametersPerDimension();
    m_NumBSplineWeights = m_BSplineTransform->GetNumberOfWeights();
    itkDebugMacro( "Transform is BSplineDeformable" );
    }

  if ( m_TransformIsBSpline )
    {
    m_BSplineTransformWeights = BSplineTransformWeightsType( m_NumBSplineWeights );
    m_BSplineTransformIndices = BSplineTransformIndexArrayType( m_NumBSplineWeights );
    }

}


/**
 * Uniformly sample the fixed image domain using a random walk
 */
template < class TFixedImage, class TMovingImage >
void
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::SampleFixedImageDomain( FixedImageSpatialSampleContainer& samples ) const
{

  // Set up a random interator within the user specified fixed image region.
  typedef ImageRandomConstIteratorWithIndex<FixedImageType> RandomIterator;
  RandomIterator randIter( m_FixedImage, this->GetFixedImageRegion() );

  randIter.SetNumberOfSamples( m_NumberOfSpatialSamples );
  randIter.GoToBegin();

  typename FixedImageSpatialSampleContainer::iterator iter;
  typename FixedImageSpatialSampleContainer::const_iterator end=samples.end();

  for( iter=samples.begin(); iter != end; ++iter )
    {

    // Get sampled index
    FixedImageIndexType index = randIter.GetIndex();
    // Get sampled fixed image value
    (*iter).FixedImageValue = randIter.Get();
    // Translate index to point
    m_FixedImage->TransformIndexToPhysicalPoint( index,
                                                 (*iter).FixedImagePointValue );
    // Jump to random position
    ++randIter;

    }
}


/**
 * Get the match Measure
 */
template < class TFixedImage, class TMovingImage  >
typename MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::MeasureType
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::GetValue( const ParametersType& parameters ) const
{

  // Reset marginal pdf to all zeros.
  // Assumed the size has already been set to NumberOfHistogramBins
  // in Initialize().
  for ( unsigned int j = 0; j < m_NumberOfHistogramBins; j++ )
    {
    m_FixedImageMarginalPDF[j]  = 0.0;
    m_MovingImageMarginalPDF[j] = 0.0;
    }

  // Reset the joint pdfs to zero
  m_JointPDF->FillBuffer( 0.0 );

  // Set up the parameters in the transform
  m_Transform->SetParameters( parameters );


  // Declare iterators for iteration over the sample container
  typename FixedImageSpatialSampleContainer::const_iterator fiter;
  typename FixedImageSpatialSampleContainer::const_iterator fend = 
    m_FixedImageSamples.end();

  unsigned long nSamples=0;
  unsigned long nFixedImageSamples=0;

  // Declare variables for accessing the joint pdf
  JointPDFIndexType                jointPDFIndex;

  for ( fiter = m_FixedImageSamples.begin(); fiter != fend; ++fiter )
    {

    ++nFixedImageSamples;

    // Get moving image value
    MovingImagePointType mappedPoint;
    bool sampleOk;
    double movingImageValue;

    this->TransformPoint( (*fiter).FixedImagePointValue, mappedPoint, 
                          sampleOk, movingImageValue );

    if( sampleOk )
      {

      ++nSamples; 

      /**
       * Compute this sample's contribution to the marginal and joint distributions.
       *
       */

      // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).    
      double movingImageParzenWindowTerm =
        movingImageValue / m_MovingImageBinSize - m_MovingImageNormalizedMin;
      unsigned int movingImageParzenWindowIndex = 
        static_cast<unsigned int>( floor( movingImageParzenWindowTerm ) );

      double fixedImageParzenWindowTerm = 
        static_cast<double>( (*fiter).FixedImageValue ) / m_FixedImageBinSize -
        m_FixedImageNormalizedMin;
      unsigned int fixedImageParzenWindowIndex =
        static_cast<unsigned int>( floor( fixedImageParzenWindowTerm ) );
      

      // Make sure the extreme values are in valid bins
      if ( fixedImageParzenWindowIndex < 2 )
        {
        fixedImageParzenWindowIndex = 2;
        }
      else if ( fixedImageParzenWindowIndex > (m_NumberOfHistogramBins - 3) )
        {
        fixedImageParzenWindowIndex = m_NumberOfHistogramBins - 3;
        }
      
      if ( movingImageParzenWindowIndex < 2 )
        {
        movingImageParzenWindowIndex = 2;
        }
      else if ( movingImageParzenWindowIndex > (m_NumberOfHistogramBins - 3) )
        {
        movingImageParzenWindowIndex = m_NumberOfHistogramBins - 3;
        }


      // Since a zero-order BSpline (box car) kernel is used for
      // the fixed image marginal pdf, we need only increment the
      // fixedImageParzenWindowIndex by value of 1.0.
      m_FixedImageMarginalPDF[fixedImageParzenWindowIndex] =
        m_FixedImageMarginalPDF[fixedImageParzenWindowIndex] + 
        static_cast<PDFValueType>( 1 );
        
      /**
        * The region of support of the parzen window determines which bins
        * of the joint PDF are effected by the pair of image values.
        * Since we are using a cubic spline for the moving image parzen
        * window, four bins are effected.  The fixed image parzen window is
        * a zero-order spline (box car) and thus effects only one bin.
        *
        *  The PDF is arranged so that fixed image bins corresponds to the 
        * zero-th (column) dimension and the moving image bins corresponds
        * to the first (row) dimension.
        *
        */
      for ( int pdfMovingIndex = static_cast<int>( movingImageParzenWindowIndex ) - 1;
            pdfMovingIndex <= static_cast<int>( movingImageParzenWindowIndex ) + 2;
            pdfMovingIndex++ )
        {

        double movingImageParzenWindowArg = 
          static_cast<double>( pdfMovingIndex ) - 
          static_cast<double>(movingImageParzenWindowTerm);

        jointPDFIndex[0] = fixedImageParzenWindowIndex;
        jointPDFIndex[1] = pdfMovingIndex;

        // Update PDF for the current intensity pair
        JointPDFValueType & pdfValue = m_JointPDF->GetPixel( jointPDFIndex );
        pdfValue += static_cast<PDFValueType>( 
          m_CubicBSplineKernel->Evaluate( movingImageParzenWindowArg ) );


        }  //end parzen windowing for loop

      } //end if-block check sampleOk

    } // end iterating over fixed image spatial sample container for loop

  itkDebugMacro( "Ratio of voxels mapping into moving image buffer: " 
                 << nSamples << " / " << m_NumberOfSpatialSamples << std::endl );

  if( nSamples < m_NumberOfSpatialSamples / 4 )
    {
    itkExceptionMacro( "Too many samples map outside moving image buffer: "
                       << nSamples << " / " << m_NumberOfSpatialSamples << std::endl );
    }


  /**
   * Normalize the PDFs, compute moving image marginal PDF
   *
   */
  typedef ImageRegionIterator<JointPDFType> JointPDFIteratorType;
  JointPDFIteratorType jointPDFIterator ( m_JointPDF, m_JointPDF->GetBufferedRegion() );

  jointPDFIterator.GoToBegin();
  

  // Compute joint PDF normalization factor (to ensure joint PDF sum adds to 1.0)
  double jointPDFSum = 0.0;

  while( !jointPDFIterator.IsAtEnd() )
    {
    jointPDFSum += jointPDFIterator.Get();
    ++jointPDFIterator;
    }

  if ( jointPDFSum == 0.0 )
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


  // Normalize the fixed image marginal PDF
  double fixedPDFSum = 0.0;
  for( unsigned int bin = 0; bin < m_NumberOfHistogramBins; bin++ )
    {
    fixedPDFSum += m_FixedImageMarginalPDF[bin];
    }

  if ( fixedPDFSum == 0.0 )
    {
    itkExceptionMacro( "Fixed image marginal PDF summed to zero" );
    }

  for( unsigned int bin=0; bin < m_NumberOfHistogramBins; bin++ )
    {
    m_FixedImageMarginalPDF[bin] /= static_cast<PDFValueType>( fixedPDFSum );
    }


  // Compute moving image marginal PDF by summing over fixed image bins.
  typedef ImageLinearIteratorWithIndex<JointPDFType> JointPDFLinearIterator;
  JointPDFLinearIterator linearIter( 
    m_JointPDF, m_JointPDF->GetBufferedRegion() );

  linearIter.SetDirection( 0 );
  linearIter.GoToBegin();
  unsigned int movingIndex = 0;

  while( !linearIter.IsAtEnd() )
    {

    double sum = 0.0;

    while( !linearIter.IsAtEndOfLine() )
      {
      sum += linearIter.Get();
      ++linearIter;
      }

    m_MovingImageMarginalPDF[movingIndex] = static_cast<PDFValueType>(sum);

    linearIter.NextLine();
    ++movingIndex;

    }

  /**
   * Compute the metric by double summation over histogram.
   * TODO: We might be able to optimize this part with Iterators.
   *
   */
  double sum = 0.0;
  for( unsigned int fixedIndex = 0; fixedIndex < m_NumberOfHistogramBins; ++fixedIndex )
    {
    jointPDFIndex[0] = fixedIndex;
    double fixedImagePDFValue = m_FixedImageMarginalPDF[fixedIndex];
    for( unsigned int movingIndex = 0; movingIndex < m_NumberOfHistogramBins; ++movingIndex )      
      {
      double movingImagePDFValue = m_MovingImageMarginalPDF[movingIndex];
      jointPDFIndex[1] = movingIndex;

      double jointPDFValue = m_JointPDF->GetPixel( jointPDFIndex );

      if( jointPDFValue > 1e-16 &&  movingImagePDFValue > 1e-16 )
        {

        double pRatio = log( jointPDFValue / movingImagePDFValue );
        if( fixedImagePDFValue > 1e-16)
          sum += jointPDFValue * ( pRatio - log( fixedImagePDFValue ) );

        }  // end if-block to check non-zero bin contribution
      }  // end for-loop over moving index
    }  // end for-loop over fixed index

  return static_cast<MeasureType>( -1.0 * sum );

}


/**
 * Get the both Value and Derivative Measure
 */
template < class TFixedImage, class TMovingImage  >
void
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivative(
  const ParametersType& parameters,
  MeasureType& value,
  DerivativeType& derivative) const
{

  // Set output values to zero
  value = NumericTraits< MeasureType >::Zero;
  derivative = DerivativeType( this->GetNumberOfParameters() );
  derivative.Fill( NumericTraits< MeasureType >::Zero );


  // Reset marginal pdf to all zeros.
  // Assumed the size has already been set to NumberOfHistogramBins
  // in Initialize().
  for ( unsigned int j = 0; j < m_NumberOfHistogramBins; j++ )
    {
    m_FixedImageMarginalPDF[j]  = 0.0;
    m_MovingImageMarginalPDF[j] = 0.0;
    }

  // Reset the joint pdfs to zero
  m_JointPDF->FillBuffer( 0.0 );
  m_JointPDFDerivatives->FillBuffer( 0.0 );


  // Set up the parameters in the transform
  m_Transform->SetParameters( parameters );


  // Declare iterators for iteration over the sample container
  typename FixedImageSpatialSampleContainer::const_iterator fiter;
  typename FixedImageSpatialSampleContainer::const_iterator fend = 
    m_FixedImageSamples.end();

  unsigned long nSamples=0;
  unsigned long nFixedImageSamples=0;

  // Declare variables for accessing the joint pdf and derivatives
  JointPDFIndexType                jointPDFIndex;
  JointPDFDerivativesIndexType    jointPDFDerivativesIndex;

  for ( fiter = m_FixedImageSamples.begin(); fiter != fend; ++fiter )
    {

    ++nFixedImageSamples;

    // Get moving image value
    MovingImagePointType mappedPoint;
    bool sampleOk;
    double movingImageValue;

    this->TransformPoint( (*fiter).FixedImagePointValue, mappedPoint, 
                          sampleOk, movingImageValue );


    if( sampleOk )
      {

      ++nSamples; 

      ImageDerivativesType movingImageGradientValue;
      this->ComputeImageDerivatives( mappedPoint, movingImageGradientValue );


      /**
       * Compute this sample's contribution to the marginal and joint distributions.
       *
       */

      // Determine parzen window arguments (see eqn 6 of Mattes paper [2]).    
      double movingImageParzenWindowTerm =
        movingImageValue / m_MovingImageBinSize - m_MovingImageNormalizedMin;
      unsigned int movingImageParzenWindowIndex = 
        static_cast<unsigned int>( floor( movingImageParzenWindowTerm ) );

      double fixedImageParzenWindowTerm = 
        static_cast<double>( (*fiter).FixedImageValue ) / m_FixedImageBinSize -
        m_FixedImageNormalizedMin;
      unsigned int fixedImageParzenWindowIndex =
        static_cast<unsigned int>( floor( fixedImageParzenWindowTerm ) );


      // Make sure the extreme values are in valid bins
      if ( fixedImageParzenWindowIndex < 2 )
        {
        fixedImageParzenWindowIndex = 2;
        }
      else if ( fixedImageParzenWindowIndex > (m_NumberOfHistogramBins - 3) )
        {
        fixedImageParzenWindowIndex = m_NumberOfHistogramBins - 3;
        }
      
      if ( movingImageParzenWindowIndex < 2 )
        {
        movingImageParzenWindowIndex = 2;
        }
      else if ( movingImageParzenWindowIndex > (m_NumberOfHistogramBins - 3) )
        {
        movingImageParzenWindowIndex = m_NumberOfHistogramBins - 3;
        }


      // Since a zero-order BSpline (box car) kernel is used for
      // the fixed image marginal pdf, we need only increment the
      // fixedImageParzenWindowIndex by value of 1.0.
      m_FixedImageMarginalPDF[fixedImageParzenWindowIndex] =
        m_FixedImageMarginalPDF[fixedImageParzenWindowIndex] + 
        static_cast<PDFValueType>( 1 );
        
      /**
        * The region of support of the parzen window determines which bins
        * of the joint PDF are effected by the pair of image values.
        * Since we are using a cubic spline for the moving image parzen
        * window, four bins are effected.  The fixed image parzen window is
        * a zero-order spline (box car) and thus effects only one bin.
        *
        *  The PDF is arranged so that fixed image bins corresponds to the 
        * zero-th (column) dimension and the moving image bins corresponds
        * to the first (row) dimension.
        *
        */
      for ( int pdfMovingIndex = static_cast<int>( movingImageParzenWindowIndex ) - 1;
            pdfMovingIndex <= static_cast<int>( movingImageParzenWindowIndex ) + 2;
            pdfMovingIndex++ )
        {

        double movingImageParzenWindowArg = 
          static_cast<double>( pdfMovingIndex ) - 
          static_cast<double>(movingImageParzenWindowTerm);

        jointPDFIndex[0] = fixedImageParzenWindowIndex;
        jointPDFIndex[1] = pdfMovingIndex;

        // Update PDF for the current intensity pair
        JointPDFValueType & pdfValue = m_JointPDF->GetPixel( jointPDFIndex );
        pdfValue += static_cast<PDFValueType>( 
          m_CubicBSplineKernel->Evaluate( movingImageParzenWindowArg ) );

        // Compute the cubicBSplineDerivative for later repeated use.
        double cubicBSplineDerivativeValue = 
          m_CubicBSplineDerivativeKernel->Evaluate( movingImageParzenWindowArg );

        // Compute PDF derivative contribution.
        this->ComputePDFDerivatives( (*fiter).FixedImagePointValue, fixedImageParzenWindowIndex,
                                     pdfMovingIndex, movingImageGradientValue, cubicBSplineDerivativeValue );


        }  //end parzen windowing for loop

      } //end if-block check sampleOk

    } // end iterating over fixed image spatial sample container for loop

  itkDebugMacro( "Ratio of voxels mapping into moving image buffer: " 
                 << nSamples << " / " << m_NumberOfSpatialSamples << std::endl );

  if( nSamples < m_NumberOfSpatialSamples / 4 )
    {
    itkExceptionMacro( "Too many samples map outside moving image buffer: "
                       << nSamples << " / " << m_NumberOfSpatialSamples << std::endl );
    }


  /**
   * Normalize the PDFs, compute moving image marginal PDF
   *
   */
  typedef ImageRegionIterator<JointPDFType> JointPDFIteratorType;
  JointPDFIteratorType jointPDFIterator ( m_JointPDF, m_JointPDF->GetBufferedRegion() );

  jointPDFIterator.GoToBegin();
  

  // Compute joint PDF normalization factor (to ensure joint PDF sum adds to 1.0)
  double jointPDFSum = 0.0;

  while( !jointPDFIterator.IsAtEnd() )
    {
    jointPDFSum += jointPDFIterator.Get();
    ++jointPDFIterator;
    }

  if ( jointPDFSum == 0.0 )
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


  // Normalize the fixed image marginal PDF
  double fixedPDFSum = 0.0;
  for( unsigned int bin = 0; bin < m_NumberOfHistogramBins; bin++ )
    {
    fixedPDFSum += m_FixedImageMarginalPDF[bin];
    }

  if ( fixedPDFSum == 0.0 )
    {
    itkExceptionMacro( "Fixed image marginal PDF summed to zero" );
    }

  for( unsigned int bin=0; bin < m_NumberOfHistogramBins; bin++ )
    {
    m_FixedImageMarginalPDF[bin] /= static_cast<PDFValueType>( fixedPDFSum );
    }


  // Compute moving image marginal PDF by summing over fixed image bins.
  typedef ImageLinearIteratorWithIndex<JointPDFType> JointPDFLinearIterator;
  JointPDFLinearIterator linearIter( 
    m_JointPDF, m_JointPDF->GetBufferedRegion() );

  linearIter.SetDirection( 0 );
  linearIter.GoToBegin();
  unsigned int movingIndex = 0;

  while( !linearIter.IsAtEnd() )
    {

    double sum = 0.0;

    while( !linearIter.IsAtEndOfLine() )
      {
      sum += linearIter.Get();
      ++linearIter;
      }

    m_MovingImageMarginalPDF[movingIndex] = static_cast<PDFValueType>(sum);

    linearIter.NextLine();
    ++movingIndex;

    }


  // Normalize the joint PDF derivatives by the test image binsize and nSamples
  typedef ImageRegionIterator<JointPDFDerivativesType> JointPDFDerivativesIteratorType;
  JointPDFDerivativesIteratorType jointPDFDerivativesIterator (
    m_JointPDFDerivatives, m_JointPDFDerivatives->GetBufferedRegion() );

  jointPDFDerivativesIterator.GoToBegin();
  
  double nFactor = 1.0 / ( m_MovingImageBinSize * static_cast<double>( nSamples ) );

  while( !jointPDFDerivativesIterator.IsAtEnd() )
    {
    jointPDFDerivativesIterator.Value() *= nFactor;
    ++jointPDFDerivativesIterator;
    }


  /**
   * Compute the metric by double summation over histogram.
   * TODO: We might be able to optimize this part with Iterators.
   *
   */
  double sum = 0.0;
  for( unsigned int fixedIndex = 0; fixedIndex < m_NumberOfHistogramBins; ++fixedIndex )
    {
    jointPDFIndex[0] = fixedIndex;
    double fixedImagePDFValue = m_FixedImageMarginalPDF[fixedIndex];
    for( unsigned int movingIndex = 0; movingIndex < m_NumberOfHistogramBins; ++movingIndex )      
      {
      double movingImagePDFValue = m_MovingImageMarginalPDF[movingIndex];
      jointPDFIndex[1] = movingIndex;

      double jointPDFValue = m_JointPDF->GetPixel( jointPDFIndex );

      if( jointPDFValue > 1e-16 &&  movingImagePDFValue > 1e-16 )
        {

        double pRatio = log( jointPDFValue / movingImagePDFValue );
        if( fixedImagePDFValue > 1e-16)
          sum += jointPDFValue * ( pRatio - log( fixedImagePDFValue ) );

        for( unsigned int parameter=0; parameter < m_NumberOfParameters; ++parameter )
          {
          jointPDFDerivativesIndex[0] = fixedIndex;
          jointPDFDerivativesIndex[1] = movingIndex;
          jointPDFDerivativesIndex[2] = parameter;
          double jointPDFDerivativesValue = 
            m_JointPDFDerivatives->GetPixel( jointPDFDerivativesIndex );

          // Ref: eqn 23 of Thevenaz & Unser paper [3]
          derivative[parameter] -= jointPDFDerivativesValue * pRatio;

          }  // end for-loop over parameters
        }  // end if-block to check non-zero bin contribution
      }  // end for-loop over moving index
    }  // end for-loop over fixed index

  value = static_cast<MeasureType>( -1.0 * sum );

}


/**
 * Get the match measure derivative
 */
template < class TFixedImage, class TMovingImage  >
void
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::GetDerivative( const ParametersType& parameters, DerivativeType & derivative ) const
{
  MeasureType value;
  // call the combined version
  this->GetValueAndDerivative( parameters, value, derivative );
}


/**
 * Compute image derivatives using a central difference function
 * if we are not using a BSplineInterpolator, which includes
 * derivatives.
 */
template < class TFixedImage, class TMovingImage >
void
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::ComputeImageDerivatives( 
  const MovingImagePointType& mappedPoint, 
  ImageDerivativesType& gradient ) const
{
  
  if( m_InterpolatorIsBSpline )
    {
    // Computed moving image gradient using derivative BSpline kernel.
    gradient = m_BSplineInterpolator->EvaluateDerivative( mappedPoint );
    }
  else
    {
    // For all generic interpolator use central differencing.
    typedef typename MovingImagePointType::CoordRepType CoordRepType;
    typedef ContinuousIndex<CoordRepType,MovingImageDimension> 
      MovingImageContinuousIndexType;

    MovingImageContinuousIndexType tempIndex;

    m_MovingImage->TransformPhysicalPointToContinuousIndex( mappedPoint, tempIndex );

    MovingImageIndexType mappedIndex;
    for( unsigned int j = 0; j < MovingImageDimension; ++j )
      {
      mappedIndex[j] = static_cast<long>( vnl_math_rnd( tempIndex[j] ) );
      }

    for( unsigned int j = 0; j < MovingImageDimension; ++j )
      {
      gradient[j] = 
        m_DerivativeCalculator->EvaluateAtIndex( mappedIndex, j );
      }
    }

}


/**
 * Transform a point from FixedImage domain to MovingImage domain.
 * This function also checks if mapped point is within support region. 
 */
template < class TFixedImage, class TMovingImage >
void
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::TransformPoint( 
  const FixedImagePointType& fixedImagePoint, 
  MovingImagePointType& mappedPoint,
  bool& sampleOk,
  double& movingImageValue ) const
{

  bool insideBSValidRegion;

  if ( !m_TransformIsBSpline )
    {
    mappedPoint = m_Transform->TransformPoint( fixedImagePoint );
    }
  else
    {
    m_BSplineTransform->TransformPoint( fixedImagePoint,
                                        mappedPoint,
                                        m_BSplineTransformWeights,
                                        m_BSplineTransformIndices,
                                        insideBSValidRegion );
    }

  // Check if mapped point inside image buffer
  sampleOk = m_Interpolator->IsInsideBuffer( mappedPoint );

  if ( m_TransformIsBSpline )
    {
    // Check if mapped point is within the support region of a grid point.
    // This is neccessary for computing the metric gradient
    sampleOk = sampleOk && insideBSValidRegion;
    }

  if ( sampleOk )
    {
    movingImageValue = m_Interpolator->Evaluate( mappedPoint );

    if ( movingImageValue < m_MovingImageTrueMin || 
         movingImageValue > m_MovingImageTrueMax )
      {
      // need to throw out this sample as it will not fall into a valid bin
      sampleOk = false;
      }
    }
}


/**
 * Compute PDF derivatives contribution for each parameter
 */
template < class TFixedImage, class TMovingImage >
void
MattesMutualInformationImageToImageMetric<TFixedImage,TMovingImage>
::ComputePDFDerivatives( 
  const FixedImagePointType& fixedImagePoint, 
  int fixedImageParzenWindowIndex,
  int pdfMovingIndex,
  const ImageDerivativesType& movingImageGradientValue,
  double cubicBSplineDerivativeValue ) const
{

  JointPDFDerivativesIndexType    jointPDFDerivativesIndex;

  // Update bins in the PDF derivatives for the current intensity pair
  jointPDFDerivativesIndex[0] = fixedImageParzenWindowIndex;
  jointPDFDerivativesIndex[1] = pdfMovingIndex;

  if( !m_TransformIsBSpline )
    {

    /**
   * Generic version which works for all transforms.
   */

    // Compute the transform Jacobian.
    typedef typename TransformType::JacobianType JacobianType;
    const JacobianType& jacobian = 
      m_Transform->GetJacobian( fixedImagePoint );

    for ( unsigned int mu = 0; mu < m_NumberOfParameters; mu++ )
      {
      double innerProduct = 0.0;
      for ( unsigned int dim = 0; dim < FixedImageDimension; dim++ )
        {
        innerProduct += jacobian[dim][mu] * 
          movingImageGradientValue[dim];
        }

      // Index into the correct parameter slice of 
      // the jointPDFDerivative volume.
      jointPDFDerivativesIndex[2] = mu;

      JointPDFDerivativesValueType & pdfDerivative =
        m_JointPDFDerivatives->GetPixel( jointPDFDerivativesIndex );
      pdfDerivative -= innerProduct * cubicBSplineDerivativeValue;

      }

    }
  else
    {

    /**
   * If the transform is of type BSplineDeformableTransform,
   * we can obtain a speed up by only processing the affected parameters.
   */
    for( unsigned int dim = 0; dim < FixedImageDimension; dim++ )
      {

      /* Get correct index in parameter space */
      long offset = dim * m_NumParametersPerDim;

      for( unsigned int mu = 0; mu < m_NumBSplineWeights; mu++ )
        {

        /* The array weights contains the Jacobian values in a 1-D array 
       * (because for each parameter the Jacobian is non-zero in only 1 of the
       * possible dimensions) which is multiplied by the moving image gradient. */
        double innerProduct = movingImageGradientValue[dim] * m_BSplineTransformWeights[mu];

        /* Index into the correct parameter slices of the jointPDFDerivative volume. */
        jointPDFDerivativesIndex[2] = m_BSplineTransformIndices[mu] + offset;

        JointPDFDerivativesValueType & pdfDerivative =
          m_JointPDFDerivatives->GetPixel( jointPDFDerivativesIndex );
        pdfDerivative -= innerProduct * cubicBSplineDerivativeValue;
            
        } //end mu for loop
      } //end dim for loop

    } // end if-block transform is BSpline

}


} // end namespace itk


#endif

