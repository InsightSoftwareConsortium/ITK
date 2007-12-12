/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptImageToImageMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOptImageToImageMetric_txx
#define __itkOptImageToImageMetric_txx


#include "itkOptImageToImageMetric.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

namespace itk
{

/**
 * Constructor
 */
template <class TFixedImage, class TMovingImage> 
ImageToImageMetric<TFixedImage,TMovingImage>
::ImageToImageMetric()
{
  m_NumberOfFixedImageSamples = 50000;
  m_UseAllPixels = false;
  m_UseFixedImageIndexes = false;
  m_ReseedIterator = false;
  m_RandomSeed = -1;

  m_TransformIsBSpline    = false;
  m_NumBSplineWeights     = 0;
  m_BSplineTransform      = NULL;

  m_Threader = MultiThreaderType::New();
  m_ThreaderParameter.metric = this;
  m_ThreaderChunkSize = 0;
  m_ThreaderSizeOfLastChunk = 0;
  m_ThreaderNumberOfMovingImageSamples = NULL;
  m_WithinThreadPreProcess = false;
  m_WithinThreadPostProcess = false;

  m_FixedImage    = 0; // has to be provided by the user.
  m_FixedImageMask = 0;

  m_MovingImage   = 0; // has to be provided by the user.
  m_MovingImageMask = 0;

  m_Transform         = NULL; // has to be provided by the user.
  m_ThreaderTransform = NULL; // constructed at initialization.

  m_Interpolator  = 0; // has to be provided by the user.

  m_GradientImage = 0; // will receive the output of the filter;
  m_ComputeGradient = true; // metric computes gradient by default
  m_GradientImage = NULL; // computed at initialization

  m_InterpolatorIsBSpline = false;
  m_BSplineInterpolator = NULL;
  m_DerivativeCalculator = NULL;

  m_NumberOfThreads = m_Threader->GetNumberOfThreads();

  /* if 100% backward compatible, we should include this...but...
  typename BSplineTransformType::Pointer transformer =
           BSplineTransformType::New();
  this->SetTransform (transformer);

  typename BSplineInterpolatorType::Pointer interpolator =
           BSplineInterpolatorType::New();
  this->SetInterpolator (interpolator);
  */

}

template <class TFixedImage, class TMovingImage> 
ImageToImageMetric<TFixedImage,TMovingImage>
::~ImageToImageMetric()
{

  if(m_ThreaderNumberOfMovingImageSamples != NULL)
    {
    delete [] m_ThreaderNumberOfMovingImageSamples;
    }
  m_ThreaderNumberOfMovingImageSamples = NULL;

  if(m_ThreaderTransform != NULL)
    {
    delete [] m_ThreaderTransform;
    }
  m_ThreaderTransform = NULL;
}


/**
 * Set the parameters that define a unique transform
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::SetTransformParameters( const ParametersType & parameters ) const
{
  if( !m_Transform )
    {
    itkExceptionMacro(<<"Transform has not been assigned");
    }
  m_Transform->SetParameters( parameters );

  m_Parameters = parameters;
}


template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::SetFixedImageIndexes( const FixedImageIndexContainer & indexes )
{
  m_UseFixedImageIndexes = true;
  m_NumberOfFixedImageSamples = indexes.size();
  this->NumberOfFixedImageSamplesUpdated();
  m_FixedImageIndexes.resize( m_NumberOfFixedImageSamples );
  for(unsigned int i=0; i<m_NumberOfFixedImageSamples; i++)
    {
    m_FixedImageIndexes[i] = indexes[i];
    }
}

/**
 * Initialize
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::Initialize(void) throw ( ExceptionObject )
{

  if( !m_Transform )
    {
    itkExceptionMacro(<<"Transform is not present");
    }
  m_NumberOfParameters = m_Transform->GetNumberOfParameters();

  if( !m_Interpolator )
    {
    itkExceptionMacro(<<"Interpolator is not present");
    }

  if( !m_MovingImage )
    {
    itkExceptionMacro(<<"MovingImage is not present");
    }

  if( !m_FixedImage )
    {
    itkExceptionMacro(<<"FixedImage is not present");
    }

  if( m_FixedImageRegion.GetNumberOfPixels() == 0 )
    {
    itkExceptionMacro(<<"FixedImageRegion is empty");
    }

  // If the image is provided by a source, update the source.
  if( m_MovingImage->GetSource() )
    {
    m_MovingImage->GetSource()->Update();
    }

  // If the image is provided by a source, update the source.
  if( m_FixedImage->GetSource() )
    {
    m_FixedImage->GetSource()->Update();
    }

  // Make sure the FixedImageRegion is within the FixedImage buffered region
  if ( !m_FixedImageRegion.Crop( m_FixedImage->GetBufferedRegion() ) )
    {
    itkExceptionMacro(
      <<"FixedImageRegion does not overlap the fixed image buffered region" );
    }

  m_Interpolator->SetInputImage( m_MovingImage );
 
  if ( m_ComputeGradient )
    {
    ComputeGradient();
    }

  // If there are any observers on the metric, call them to give the
  // user code a chance to set parameters on the metric
  this->InvokeEvent( InitializeEvent() );

  m_Threader->SetNumberOfThreads( m_NumberOfThreads );

  if( m_UseAllPixels )
    {
    m_NumberOfFixedImageSamples = GetFixedImageRegion().GetNumberOfPixels();
    // NumberOfFixedImageSamplesUpdated called below.
    }
  
  this->NumberOfFixedImageSamplesUpdated();

  if(m_ThreaderNumberOfMovingImageSamples != NULL)
    {
    delete [] m_ThreaderNumberOfMovingImageSamples;
    }
  m_ThreaderNumberOfMovingImageSamples = new unsigned int[m_NumberOfThreads-1];

  // Allocate the array of transform clones to be used in every thread
  if(m_ThreaderTransform != NULL)
    {
    delete [] m_ThreaderTransform;
    }
  m_ThreaderTransform = new TransformPointer[m_NumberOfThreads-1];
  for( unsigned int ithread=0; ithread < m_NumberOfThreads-1; ++ithread)
    {
    // Create a copy of the main transform to be used in this thread.
    itk::LightObject::Pointer anotherTransform = this->m_Transform->CreateAnother();
    // This static_cast should always work since the pointer was created by
    // CreateAnother() called from the transform itself.
    TransformType * transformCopy = static_cast< TransformType * >( anotherTransform.GetPointer() );
    /** Set the fixed parameters first. Some transforms have parameters which depend on 
        the values of the fixed parameters. For instance, the BSplineDeformableTransform
        checks the grid size (part of the fixed parameters) before setting the parameters. */
    transformCopy->SetFixedParameters( this->m_Transform->GetFixedParameters() );
    transformCopy->SetParameters( this->m_Transform->GetParameters() );
    this->m_ThreaderTransform[ithread] = transformCopy;
    }

  m_FixedImageSamples.resize(m_NumberOfFixedImageSamples);
  if( m_UseAllPixels )
    {
    /* 
     * Take all the pixels within the fixed image region)
     * to create the sample points list.
     */
    SampleFullFixedImageDomain( m_FixedImageSamples );
    }
  else
    {
    if( m_UseFixedImageIndexes )
      {
      SampleFixedImageIndexes( m_FixedImageSamples );
      }
    else
      {
      /*
       * Uniformly sample the fixed image (within the fixed image region)
       * to create the sample points list.
       */
      SampleFixedImageDomain( m_FixedImageSamples );
      }
    }

  /*
   * Check if the interpolator is of type BSplineInterpolateImageFunction.
   * If so, we can make use of its EvaluateDerivatives method.
   * Otherwise, we instantiate an external central difference
   * derivative calculator.
   */
  m_InterpolatorIsBSpline = true;

  BSplineInterpolatorType * testPtr = dynamic_cast<BSplineInterpolatorType *>(
                                           m_Interpolator.GetPointer() );
  if ( !testPtr )
    {
    m_InterpolatorIsBSpline = false;

    m_DerivativeCalculator = DerivativeFunctionType::New();
    m_DerivativeCalculator->SetInputImage( m_MovingImage );

#ifdef ITK_USE_ORIENTED_IMAGE_DIRECTION
    m_DerivativeCalculator->UseImageDirectionOn();
#endif

    m_BSplineInterpolator = NULL;
    itkDebugMacro( "Interpolator is not BSpline" );
    } 
  else
    {
    m_BSplineInterpolator = testPtr;
    m_BSplineInterpolator->SetNumberOfThreads( m_NumberOfThreads );

#ifdef ITK_USE_ORIENTED_IMAGE_DIRECTION
  m_BSplineInterpolator->SetUseImageDirection( true );
#endif
  
    m_DerivativeCalculator = NULL;
    itkDebugMacro( "Interpolator is BSpline" );
    }

  //  
  //  Check if the transform is of type BSplineDeformableTransform.
  //  
  //  If so, several speed up features are implemented.
  //  [1] Precomputing the results of bulk transform for each sample point.
  //  [2] Precomputing the BSpline weights for each sample point,
  //      to be used later to directly compute the deformation vector
  //  [3] Precomputing the indices of the parameters within the 
  //      the support region of each sample point.
  //  
  m_TransformIsBSpline = true;

  BSplineTransformType * testPtr2 = dynamic_cast<BSplineTransformType *>(
                                               m_Transform.GetPointer() );
  if( !testPtr2 )
    {
    m_TransformIsBSpline = false;
    m_BSplineTransform = NULL;
    }
  else
    {
    m_BSplineTransform = testPtr2;
    m_NumBSplineWeights = m_BSplineTransform->GetNumberOfWeights();

    m_BSplineTransformWeightsArray.SetSize( m_NumberOfFixedImageSamples, 
                                            m_NumBSplineWeights );
    m_BSplineTransformIndicesArray.SetSize( m_NumberOfFixedImageSamples,
                                            m_NumBSplineWeights );
    m_BSplinePreTransformPointsArray.resize( m_NumberOfFixedImageSamples );
    m_WithinBSplineSupportRegionArray.resize( m_NumberOfFixedImageSamples );

    this->PreComputeTransformValues();

    for ( unsigned int j = 0; j < FixedImageDimension; j++ )
      {
      m_BSplineParametersOffset[j] = j * 
                       m_BSplineTransform->GetNumberOfParametersPerDimension(); 
      }
    }

}


/**
 * Uniformly sample the fixed image domain using a random walk
 */
template < class TFixedImage, class TMovingImage >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::SampleFixedImageIndexes( FixedImageSampleContainer & samples )
{
  typename FixedImageSampleContainer::iterator iter;

  int len = m_FixedImageIndexes.size();
  m_NumberOfFixedImageSamples = len;
  this->NumberOfFixedImageSamplesUpdated();

  samples.resize(len);
  iter=samples.begin();

  for(unsigned int i=0; i<len; i++)
    {
    // Get sampled index
    FixedImageIndexType index = m_FixedImageIndexes[i];
    // Translate index to point
    m_FixedImage->TransformIndexToPhysicalPoint( index, (*iter).point );

    // Get sampled fixed image value
    (*iter).value = m_FixedImage->GetPixel( index );
    (*iter).valueIndex = 0;

    ++iter;
    }
}

template < class TFixedImage, class TMovingImage >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::SampleFixedImageDomain( FixedImageSampleContainer & samples )
{
  // Set up a random interator within the user specified fixed image region.
  typedef ImageRandomConstIteratorWithIndex<FixedImageType> RandomIterator;
  RandomIterator randIter( m_FixedImage, GetFixedImageRegion() );

  randIter.SetNumberOfSamples( m_NumberOfFixedImageSamples );
  randIter.GoToBegin();

  typename FixedImageSampleContainer::iterator iter;
  typename FixedImageSampleContainer::const_iterator end=samples.end();

  if( m_FixedImageMask )
    {
    InputPointType inputPoint;

    iter=samples.begin();
    int count = 0;
    int samples_found = 0;
    int maxcount = m_NumberOfFixedImageSamples * 10;
    while( iter != end )
      {

      if ( count > maxcount )
        {
        samples.resize(samples_found);
        break;
        }
      count++;
      
      // Get sampled index
      FixedImageIndexType index = randIter.GetIndex();
      // Check if the Index is inside the mask, translate index to point
      m_FixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

      // If not inside the mask, ignore the point
      if( !m_FixedImageMask->IsInside( inputPoint ) )
        {
        ++randIter; // jump to another random position
        continue;
        }

      // Translate index to point
      (*iter).point = inputPoint;
      // Get sampled fixed image value
      (*iter).value = randIter.Get();
      (*iter).valueIndex = 0;

      ++samples_found;
      // Jump to random position
      ++randIter;
      ++iter;
      }
    }
  else
    {
    for( iter=samples.begin(); iter != end; ++iter )
      {
      // Get sampled index
      FixedImageIndexType index = randIter.GetIndex();
      // Translate index to point
      m_FixedImage->TransformIndexToPhysicalPoint( index,
                                                (*iter).point );
      // Get sampled fixed image value
      (*iter).value = randIter.Get();
      (*iter).valueIndex = 0;

      // Jump to random position
      ++randIter;
      }
    }
}

/**
 * Sample the fixed image domain using all pixels in the Fixed image region
 */
template < class TFixedImage, class TMovingImage >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::SampleFullFixedImageDomain( FixedImageSampleContainer& samples )
{
  // Set up a region interator within the user specified fixed image region.
  typedef ImageRegionConstIteratorWithIndex<FixedImageType> RegionIterator;
  RegionIterator regionIter( m_FixedImage, GetFixedImageRegion() );

  regionIter.GoToBegin();

  typename FixedImageSampleContainer::iterator iter;
  typename FixedImageSampleContainer::const_iterator end=samples.end();

  if( m_FixedImageMask )
    {
    InputPointType inputPoint;

    iter=samples.begin();
    unsigned long nSamplesPicked = 0;

    while( iter != end && !regionIter.IsAtEnd() )
      {
      // Get sampled index
      FixedImageIndexType index = regionIter.GetIndex();
      // Check if the Index is inside the mask, translate index to point
      m_FixedImage->TransformIndexToPhysicalPoint( index, inputPoint );

      // If not inside the mask, ignore the point
      if( !m_FixedImageMask->IsInside( inputPoint ) )
        {
        ++regionIter; // jump to next pixel
        continue;
        }

      // Translate index to point
      (*iter).point = inputPoint;
      // Get sampled fixed image value
      (*iter).value = regionIter.Get();
      (*iter).valueIndex = 0;

      ++regionIter;
      ++iter;
      ++nSamplesPicked;
      }

    // If we picked fewer samples than the desired number, 
    // resize the container
    if (nSamplesPicked != m_NumberOfFixedImageSamples)
      {
      m_NumberOfFixedImageSamples = nSamplesPicked;
      this->NumberOfFixedImageSamplesUpdated();
      samples.resize(m_NumberOfFixedImageSamples);
      }
    }
  else // not restricting sample throwing to a mask
    {
    // cannot sample more than the number of pixels in the image region
    if (  m_NumberOfFixedImageSamples 
          > GetFixedImageRegion().GetNumberOfPixels())
      {
      m_NumberOfFixedImageSamples = GetFixedImageRegion().GetNumberOfPixels();
      this->NumberOfFixedImageSamplesUpdated();
      samples.resize(m_NumberOfFixedImageSamples);
      }
      
    for( iter=samples.begin(); iter != end; ++iter )
      {
      // Get sampled index
      FixedImageIndexType index = regionIter.GetIndex();

      // Translate index to point
      m_FixedImage->TransformIndexToPhysicalPoint( index,
                                               (*iter).point );
      // Get sampled fixed image value
      (*iter).value = regionIter.Get();
      (*iter).valueIndex = 0;

      ++regionIter;
      }
    }
}

/**
 * Compute the gradient image and assign it to m_GradientImage.
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::ComputeGradient() 
{
  GradientImageFilterPointer gradientFilter = GradientImageFilterType::New();

  gradientFilter->SetInput( m_MovingImage );

  const typename MovingImageType::SpacingType & spacing = m_MovingImage
                                                          ->GetSpacing();
  double maximumSpacing=0.0;
  for(unsigned int i=0; i<MovingImageDimension; i++)
    {
    if( spacing[i] > maximumSpacing )
      {
      maximumSpacing = spacing[i];
      }
    }
  gradientFilter->SetSigma( maximumSpacing );
  gradientFilter->SetNormalizeAcrossScale( true );
  gradientFilter->SetNumberOfThreads( m_NumberOfThreads );
  
#ifdef ITK_USE_ORIENTED_IMAGE_DIRECTION
  gradientFilter->SetUseImageDirection( true );
#endif
  
  gradientFilter->Update();
  
  m_GradientImage = gradientFilter->GetOutput();
}

// Method to reinitialize the seed of the random number generator
template < class TFixedImage, class TMovingImage  > void
ImageToImageMetric<TFixedImage,TMovingImage>
::ReinitializeSeed()
{
  Statistics::MersenneTwisterRandomVariateGenerator::GetInstance()->SetSeed();
}

// Method to reinitialize the seed of the random number generator
template < class TFixedImage, class TMovingImage  > void
ImageToImageMetric<TFixedImage,TMovingImage>
::ReinitializeSeed(int seed)
{
  Statistics::MersenneTwisterRandomVariateGenerator::GetInstance()->SetSeed(
                                                                         seed);
}


/**
 * Cache pre-transformed points, weights and indices.
 */
template < class TFixedImage, class TMovingImage >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::PreComputeTransformValues()
{
  // Create all zero dummy transform parameters
  ParametersType dummyParameters( m_NumberOfParameters );
  dummyParameters.Fill( 0.0 );
  m_Transform->SetParameters( dummyParameters );

  // Cycle through each sampled fixed image point
  BSplineTransformWeightsType weights( m_NumBSplineWeights );
  BSplineTransformIndexArrayType indices( m_NumBSplineWeights );
  bool valid;
  MovingImagePointType mappedPoint;

  // Declare iterators for iteration over the sample container
  typename FixedImageSampleContainer::const_iterator fiter;
  typename FixedImageSampleContainer::const_iterator fend = 
                                                     m_FixedImageSamples.end();
  unsigned long counter = 0;

  for( fiter = m_FixedImageSamples.begin(); fiter != fend; ++fiter, counter++ )
    {
    m_BSplineTransform->TransformPoint( m_FixedImageSamples[counter].point,
                                        mappedPoint, weights, indices, valid );

    for( unsigned long k = 0; k < m_NumBSplineWeights; k++ )
      {
      m_BSplineTransformWeightsArray[counter][k] = weights[k];
      m_BSplineTransformIndicesArray[counter][k] = indices[k];
      }

    m_BSplinePreTransformPointsArray[counter]      = mappedPoint;
    m_WithinBSplineSupportRegionArray[counter]     = valid;
    }

}


/**
 * Transform a point from FixedImage domain to MovingImage domain.
 * This function also checks if mapped point is within support region. 
 */
template < class TFixedImage, class TMovingImage >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::TransformPoint( unsigned int sampleNumber, 
                  MovingImagePointType& mappedPoint,
                  bool& sampleOk,
                  double& movingImageValue,
                  unsigned int threadID ) const
{
  TransformType * transform;
  
  if( threadID > 0 )
    {
    transform = this->m_ThreaderTransform[threadID-1];
    }
  else
    {
    transform = this->m_Transform;
    }

  /** Useful for debugging */
  if (sampleNumber >= m_FixedImageSamples.size())
    {
    itkExceptionMacro( << "sampleNumber " << sampleNumber << " exceeds " << m_FixedImageSamples.size() << " which is the container size of m_FixedImageSamples" << std::endl);
    }

  if ( !m_TransformIsBSpline )
    {
    // Use generic transform to compute mapped position
    mappedPoint = transform->TransformPoint( m_FixedImageSamples[sampleNumber].point );
    sampleOk = true;
    }
  else
    {
    sampleOk = m_WithinBSplineSupportRegionArray[sampleNumber];

    if(sampleOk)
      {
      // If the transform is BSplineDeformable, we can use the precomputed
      // weights and indices to obtained the mapped position
      const WeightsValueType * weights = 
                                   m_BSplineTransformWeightsArray[sampleNumber];
      const IndexValueType   * indices = 
                                   m_BSplineTransformIndicesArray[sampleNumber];

      for( unsigned int j = 0; j < FixedImageDimension; j++ )
        {
        mappedPoint[j] = m_BSplinePreTransformPointsArray[sampleNumber][j];
        }
  
      for ( unsigned int k = 0; k < m_NumBSplineWeights; k++ )
        {
        for ( unsigned int j = 0; j < FixedImageDimension; j++ )
          {
          mappedPoint[j] += weights[k] * m_Parameters[ indices[k] 
                                               + m_BSplineParametersOffset[j] ];
          }
        }
      }
    }
  
  if(sampleOk)
    {
    // If user provided a mask over the Moving image
    if ( m_MovingImageMask )
      {
      // Check if mapped point is within the support region of the moving image
      // mask
      sampleOk = m_MovingImageMask->IsInside( mappedPoint );
      }
    else
      {
      // Check if mapped point inside image buffer
      sampleOk = m_Interpolator->IsInsideBuffer( mappedPoint );
      }
  
    if ( sampleOk )
      {
      if(m_InterpolatorIsBSpline)
        {
        movingImageValue = m_BSplineInterpolator->Evaluate( mappedPoint, 
                                                            threadID );
        }
      else
        {
        movingImageValue = m_Interpolator->Evaluate( mappedPoint );
        }
      }
    }
}


/**
 * Transform a point from FixedImage domain to MovingImage domain.
 * This function also checks if mapped point is within support region. 
 */
template < class TFixedImage, class TMovingImage >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::TransformPointWithDerivatives( unsigned int sampleNumber, 
                  MovingImagePointType& mappedPoint,
                  bool& sampleOk,
                  double& movingImageValue,
                  ImageDerivativesType & movingImageGradient,
                  unsigned int threadID ) const
{
  TransformType * transform;
  
  if( threadID > 0 )
    {
    transform = this->m_ThreaderTransform[threadID-1];
    }
  else
    {
    transform = this->m_Transform;
    }

  /** Useful for debugging */
  if (sampleNumber >= m_FixedImageSamples.size())
    {
    itkExceptionMacro( << "sampleNumber " << sampleNumber << " exceeds " << m_FixedImageSamples.size() << " which is the container size of m_FixedImageSamples" << std::endl);
    }

  if ( !m_TransformIsBSpline )
    {
    // Use generic transform to compute mapped position
    mappedPoint = transform->TransformPoint( m_FixedImageSamples[sampleNumber].point );
    sampleOk = true;
    }
  else
    {
    sampleOk = m_WithinBSplineSupportRegionArray[sampleNumber];

    if(sampleOk)
      {
      // If the transform is BSplineDeformable, we can use the precomputed
      // weights and indices to obtained the mapped position
      const WeightsValueType * weights = 
                                   m_BSplineTransformWeightsArray[sampleNumber];
      const IndexValueType   * indices = 
                                   m_BSplineTransformIndicesArray[sampleNumber];

      for( unsigned int j = 0; j < FixedImageDimension; j++ )
        {
        mappedPoint[j] = m_BSplinePreTransformPointsArray[sampleNumber][j];
        }
  
      for ( unsigned int k = 0; k < m_NumBSplineWeights; k++ )
        {
        for ( unsigned int j = 0; j < FixedImageDimension; j++ )
          {
          mappedPoint[j] += weights[k] * m_Parameters[ indices[k] 
                                               + m_BSplineParametersOffset[j] ];
          }
        }
      }
    }
  
  if(sampleOk)
    {
    // If user provided a mask over the Moving image
    if ( m_MovingImageMask )
      {
      // Check if mapped point is within the support region of the moving image
      // mask
      sampleOk = m_MovingImageMask->IsInside( mappedPoint );
      }
    else
      {
      // Check if mapped point inside image buffer
      sampleOk = m_Interpolator->IsInsideBuffer( mappedPoint );
      }
  
    if ( sampleOk )
      {
      if(m_InterpolatorIsBSpline)
        {
        m_BSplineInterpolator->EvaluateValueAndDerivative(mappedPoint,
                                                          movingImageValue,
                                                          movingImageGradient,
                                                          threadID);
        }
      else
        {
        this->ComputeImageDerivatives( mappedPoint, movingImageGradient,
                                       threadID );
        movingImageValue = this->m_Interpolator->Evaluate( mappedPoint );
        }
      }
    }
}

/**
 * Compute image derivatives using a central difference function
 * if we are not using a BSplineInterpolator, which includes
 * derivatives.
 */
template < class TFixedImage, class TMovingImage >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::ComputeImageDerivatives( const MovingImagePointType & mappedPoint, 
                           ImageDerivativesType & gradient,
                           unsigned int threadID) const
{
  
  if( m_InterpolatorIsBSpline )
    {
    // Computed moving image gradient using derivative BSpline kernel.
    gradient = m_BSplineInterpolator->EvaluateDerivative( mappedPoint,
                                                          threadID );
    }
  else
    {
    if ( m_ComputeGradient )
      {
      itk::ContinuousIndex<double, MovingImageDimension> tempIndex;
      m_MovingImage->TransformPhysicalPointToContinuousIndex( mappedPoint,
                                                              tempIndex );
      MovingImageIndexType mappedIndex;
      mappedIndex.CopyWithRound( tempIndex );
      gradient = m_GradientImage->GetPixel( mappedIndex );
      }
    else
      {
      // if not using the gradient image
      gradient = m_DerivativeCalculator->Evaluate( mappedPoint );
      }
    }

}

template < class TFixedImage, class TMovingImage  >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueMultiThreadedPreProcessInitiate( void ) const
{
  this->SynchronizeTransforms();

  m_Threader->SetSingleMethod(GetValueMultiThreadedPreProcess,
                              (void *)(&m_ThreaderParameter));
  m_Threader->SingleMethodExecute();
}

template < class TFixedImage, class TMovingImage  >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueMultiThreadedInitiate( void ) const
{
  this->SynchronizeTransforms();

  m_Threader->SetSingleMethod(GetValueMultiThreaded,
                              (void *)(&m_ThreaderParameter));
  m_Threader->SingleMethodExecute();

  for( unsigned int threadID = 0; threadID<m_NumberOfThreads-1; threadID++ )
    {
    this->m_NumberOfMovingImageSamples += m_ThreaderNumberOfMovingImageSamples[threadID];
    }
}

template < class TFixedImage, class TMovingImage  >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueMultiThreadedPostProcessInitiate( void ) const
{
  m_Threader->SetSingleMethod(GetValueMultiThreadedPostProcess,
                              (void *)(&m_ThreaderParameter));
  m_Threader->SingleMethodExecute();
}

/**
 * Get the match Measure
 */
template < class TFixedImage, class TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueMultiThreadedPreProcess( void * arg ) 
{
  int threadID;
  MultiThreaderParameterType * mtParam;

  threadID = ((MultiThreaderType::ThreadInfoStruct *)(arg))->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            (((MultiThreaderType::ThreadInfoStruct *)(arg))->UserData);

  mtParam->metric->GetValueThreadPreProcess(threadID, false);

  return ITK_THREAD_RETURN_VALUE;
}


template < class TFixedImage, class TMovingImage  >
inline void
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueThreadPreProcess( unsigned int itkNotUsed(threadID),
                            bool itkNotUsed(withinSampleThread) ) const
{
  // intended to be overloaded in derived classes.
}


/**
 * Get the match Measure
 */
template < class TFixedImage, class TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueMultiThreaded( void * arg ) 
{
  int threadID;
  MultiThreaderParameterType * mtParam;

  threadID = ((MultiThreaderType::ThreadInfoStruct *)(arg))->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            (((MultiThreaderType::ThreadInfoStruct *)(arg))->UserData);

  mtParam->metric->GetValueThread(threadID);

  return ITK_THREAD_RETURN_VALUE;
}
/**
 * Get the match Measure
 */
template < class TFixedImage, class TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueMultiThreadedPostProcess( void * arg ) 
{
  int threadID;
  MultiThreaderParameterType * mtParam;

  threadID = ((MultiThreaderType::ThreadInfoStruct *)(arg))->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            (((MultiThreaderType::ThreadInfoStruct *)(arg))->UserData);

  mtParam->metric->GetValueThreadPostProcess(threadID, false);

  return ITK_THREAD_RETURN_VALUE;
}


template < class TFixedImage, class TMovingImage  >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueThread( unsigned int threadID ) const
{
  // Skip to this thread's samples to process
  unsigned int fixedImageSample = threadID * m_ThreaderChunkSize;

  // Figure out how many samples to process
  unsigned int chunkSize = m_ThreaderChunkSize;
  if(threadID == m_NumberOfThreads - 1)
    {
    chunkSize = m_ThreaderSizeOfLastChunk;
    }

  int numSamples = 0;

  if(m_WithinThreadPreProcess)
    {
    this->GetValueThreadPreProcess(threadID, true);
    }

  // Process the samples
  MovingImagePointType mappedPoint;
  bool sampleOk;
  double movingImageValue;
  for( unsigned int count=0; count < chunkSize; ++count, ++fixedImageSample )
    {
    // Get moving image value
    this->TransformPoint( fixedImageSample, mappedPoint, sampleOk, movingImageValue,
                    threadID );

    if( sampleOk )
      {
      // CALL USER FUNCTION
      if(GetValueThreadProcessSample(threadID, fixedImageSample,
                                     mappedPoint, movingImageValue))
        {
        ++numSamples;
        }
      }
    }

  if(threadID > 0)
    {
    m_ThreaderNumberOfMovingImageSamples[threadID-1] = numSamples;
    }
  else
    {
    m_NumberOfMovingImageSamples = numSamples;
    }

  if(m_WithinThreadPostProcess)
    {
    this->GetValueThreadPostProcess(threadID, true);
    }
}

template < class TFixedImage, class TMovingImage  >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivativeMultiThreadedPreProcessInitiate( void ) const
{
  this->SynchronizeTransforms();

  m_Threader->SetSingleMethod(GetValueAndDerivativeMultiThreadedPreProcess,
                              (void *)(&m_ThreaderParameter));
  m_Threader->SingleMethodExecute();
}

template < class TFixedImage, class TMovingImage  >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivativeMultiThreadedInitiate( void ) const
{
  this->SynchronizeTransforms();

  m_Threader->SetSingleMethod(GetValueAndDerivativeMultiThreaded,
                              (void *)(&m_ThreaderParameter));
  m_Threader->SingleMethodExecute();

  for( unsigned int threadID = 0; threadID<m_NumberOfThreads-1; threadID++ )
    {
    this->m_NumberOfMovingImageSamples += m_ThreaderNumberOfMovingImageSamples[threadID];
    }
}

template < class TFixedImage, class TMovingImage  >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivativeMultiThreadedPostProcessInitiate( void ) const
{
  m_Threader->SetSingleMethod(GetValueAndDerivativeMultiThreadedPostProcess,
                              (void *)(&m_ThreaderParameter));
  m_Threader->SingleMethodExecute();
}

/**
 * Get the match Measure
 */
template < class TFixedImage, class TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivativeMultiThreadedPreProcess( void * arg ) 
{
  int threadID;
  MultiThreaderParameterType * mtParam;

  threadID = ((MultiThreaderType::ThreadInfoStruct *)(arg))->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            (((MultiThreaderType::ThreadInfoStruct *)(arg))->UserData);

  mtParam->metric->GetValueAndDerivativeThreadPreProcess(threadID, false);

  return ITK_THREAD_RETURN_VALUE;
}

/**
 * Get the match Measure
 */
template < class TFixedImage, class TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivativeMultiThreaded( void * arg ) 
{
  int threadID;
  MultiThreaderParameterType * mtParam;

  threadID = ((MultiThreaderType::ThreadInfoStruct *)(arg))->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            (((MultiThreaderType::ThreadInfoStruct *)(arg))->UserData);

  mtParam->metric->GetValueAndDerivativeThread(threadID);

  return ITK_THREAD_RETURN_VALUE;
}

/**
 * Get the match Measure
 */
template < class TFixedImage, class TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivativeMultiThreadedPostProcess( void * arg ) 
{
  int threadID;
  MultiThreaderParameterType * mtParam;

  threadID = ((MultiThreaderType::ThreadInfoStruct *)(arg))->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            (((MultiThreaderType::ThreadInfoStruct *)(arg))->UserData);

  mtParam->metric->GetValueAndDerivativeThreadPostProcess(threadID, false);

  return ITK_THREAD_RETURN_VALUE;
}

template < class TFixedImage, class TMovingImage  >
void
ImageToImageMetric<TFixedImage,TMovingImage>
::GetValueAndDerivativeThread( unsigned int threadID ) const
{
  // Skip to this thread's samples to process
  unsigned int fixedImageSample = threadID * m_ThreaderChunkSize;

  // Figure out how many samples to process
  unsigned int chunkSize = m_ThreaderChunkSize;
  if(threadID == m_NumberOfThreads - 1)
    {
    chunkSize = m_ThreaderSizeOfLastChunk;
    }

  int numSamples = 0;

  if(m_WithinThreadPreProcess)
    {
    this->GetValueAndDerivativeThreadPreProcess(threadID, true);
    }

  // Process the samples
  MovingImagePointType mappedPoint;
  bool sampleOk;
  double movingImageValue;
  ImageDerivativesType movingImageGradientValue;
  for( unsigned int count=0; count < chunkSize; ++count, ++fixedImageSample )
    {
    // Get moving image value
    TransformPointWithDerivatives( fixedImageSample, mappedPoint, sampleOk,
                                   movingImageValue, movingImageGradientValue,
                                   threadID );

    if( sampleOk )
      {
      // CALL USER FUNCTION
      if( this->GetValueAndDerivativeThreadProcessSample( 
            threadID,
            fixedImageSample,
            mappedPoint,
            movingImageValue,
            movingImageGradientValue ))
        {
        ++numSamples;
        }
      }
    }

  if(threadID > 0)
    {
    m_ThreaderNumberOfMovingImageSamples[threadID-1] = numSamples;
    }
  else
    {
    m_NumberOfMovingImageSamples = numSamples;
    }

  if(m_WithinThreadPostProcess)
    {
    this->GetValueAndDerivativeThreadPostProcess(threadID, true);
    }
}


/**
 * PrintSelf
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "NumberOfFixedImageSamples: ";
  os << m_NumberOfFixedImageSamples << std::endl;

  os << indent << "UseAllPixels: ";
  os << m_UseAllPixels << std::endl;

  os << indent << "Threader: " << m_Threader << std::endl;
  os << indent << "Number of Threads: " << m_NumberOfThreads << std::endl;
  os << indent << "ThreaderParameter: " << std::endl;
//  os << (unsigned int)(m_ThreaderParameter.metric) << std::endl;
  os << indent << "ThreaderChunkSize: " << m_ThreaderChunkSize << std::endl;
  os << indent << "ThreaderSizeOfLastChunk: " << m_ThreaderSizeOfLastChunk 
     << std::endl;
  os << indent << "ThreaderNumberOfMovingImageSamples: " << std::endl;
//  os << (unsigned int)m_ThreaderNumberOfMovingImageSamples << std::endl;

  os << indent << "ComputeGradient: "
     << static_cast<typename NumericTraits<bool>::PrintType>(m_ComputeGradient)
     << std::endl;
  os << indent << "Moving Image: " << m_MovingImage.GetPointer()  << std::endl;
  os << indent << "Fixed  Image: " << m_FixedImage.GetPointer()   << std::endl;
  os << indent << "Gradient Image: " << m_GradientImage.GetPointer() 
     << std::endl;
  os << indent << "Transform:    " << m_Transform.GetPointer()    << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "FixedImageRegion: " << m_FixedImageRegion << std::endl;
  os << indent << "Moving Image Mask: " << m_MovingImageMask.GetPointer() 
     << std::endl;
  os << indent << "Fixed Image Mask: " << m_FixedImageMask.GetPointer() 
     << std::endl;
  os << indent << "Number of Moving Image Samples: " << m_NumberOfMovingImageSamples 
     << std::endl;
  os << indent << "Number of Pixels Counted: " << m_NumberOfPixelsCounted 
     << std::endl;

}

/** This method can be const because we are not altering the m_ThreaderTransform
 *  pointer. We are altering the object that m_ThreaderTransform[idx] points at.
 *  This is allowed under C++ const rules.
 */
template <class TFixedImage, class TMovingImage> 
void
ImageToImageMetric<TFixedImage,TMovingImage>
::SynchronizeTransforms() const
{
  for( unsigned int threadID = 0; threadID<m_NumberOfThreads-1; threadID++ )
    {
    /** Set the fixed parameters first. Some transforms have parameters which depend on 
        the values of the fixed parameters. For instance, the BSplineDeformableTransform
        checks the grid size (part of the fixed parameters) before setting the parameters. */
    this->m_ThreaderTransform[threadID]->SetFixedParameters( this->m_Transform->GetFixedParameters() );
    this->m_ThreaderTransform[threadID]->SetParameters( this->m_Transform->GetParameters() );
    }
}

template <class TFixedImage, class TMovingImage>
void
ImageToImageMetric<TFixedImage,TMovingImage>
::NumberOfFixedImageSamplesUpdated()
{
  m_ThreaderChunkSize = m_NumberOfFixedImageSamples / m_NumberOfThreads;
  m_ThreaderSizeOfLastChunk = m_NumberOfFixedImageSamples 
                              - ((m_NumberOfThreads-1) 
                                 * m_ThreaderChunkSize);
}


} // end namespace itk

#endif
