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
#ifndef itkImageToImageMetric_hxx
#define itkImageToImageMetric_hxx

#include "itkImageToImageMetric.h"
#include "itkImageRandomConstIteratorWithIndex.h"
#include "itkMath.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TFixedImage, typename TMovingImage >
ImageToImageMetric< TFixedImage, TMovingImage >
::ImageToImageMetric():
  m_UseFixedImageIndexes(false),
  m_FixedImageIndexes(0),

  m_UseFixedImageSamplesIntensityThreshold(false),
  m_FixedImageSamplesIntensityThreshold(0),

  m_FixedImageSamples(0),
  m_NumberOfParameters(0),

  m_NumberOfFixedImageSamples(50000),

  m_NumberOfPixelsCounted(0),

  m_FixedImage(ITK_NULLPTR), // has to be provided by the user.
  m_MovingImage(ITK_NULLPTR), // has to be provided by the user.

  m_Transform(ITK_NULLPTR), // has to be provided by the user.
  m_ThreaderTransform(ITK_NULLPTR), // constructed at initialization.

  m_Interpolator(ITK_NULLPTR), // has to be provided by the user.

  m_ComputeGradient(true), // metric computes gradient by default
  m_GradientImage(ITK_NULLPTR),   // computed at initialization

  m_FixedImageMask(ITK_NULLPTR),
  m_MovingImageMask(ITK_NULLPTR),

  m_NumberOfThreads(1),

  m_UseAllPixels(false),
  m_UseSequentialSampling(false),
  m_ReseedIterator(false),
  m_RandomSeed(Statistics::MersenneTwisterRandomVariateGenerator::GetNextSeed()),

  m_TransformIsBSpline(false),
  m_NumBSplineWeights(0),

  m_BSplineTransform(ITK_NULLPTR),
  m_BSplineTransformWeightsArray(),
  m_BSplineTransformIndicesArray(),
  m_BSplinePreTransformPointsArray(0),
  m_WithinBSplineSupportRegionArray(0),
  m_BSplineParametersOffset(),

  m_UseCachingOfBSplineWeights(true),
  m_BSplineTransformWeights(),
  m_BSplineTransformIndices(),
  m_ThreaderBSplineTransformWeights(ITK_NULLPTR),
  m_ThreaderBSplineTransformIndices(ITK_NULLPTR),

  m_InterpolatorIsBSpline(false),
  m_BSplineInterpolator(ITK_NULLPTR),
  m_DerivativeCalculator(ITK_NULLPTR),

  m_Threader(MultiThreaderType::New()),
  m_ThreaderNumberOfMovingImageSamples(ITK_NULLPTR),
  m_WithinThreadPreProcess(false),
  m_WithinThreadPostProcess(false)
{
  this->m_ThreaderParameter.metric = this;
  this->m_NumberOfThreads = this->m_Threader->GetNumberOfThreads();

  /* if 100% backward compatible, we should include this...but...
  typename BSplineTransformType::Pointer transformer =
           BSplineTransformType::New();
  this->SetTransform (transformer);

  typename BSplineInterpolatorType::Pointer interpolator =
           BSplineInterpolatorType::New();
  this->SetInterpolator (interpolator);
  */
}

template< typename TFixedImage, typename TMovingImage >
ImageToImageMetric< TFixedImage, TMovingImage >
::~ImageToImageMetric()
{
  delete[] m_ThreaderNumberOfMovingImageSamples;
  m_ThreaderNumberOfMovingImageSamples = ITK_NULLPTR;

  delete[] m_ThreaderTransform;
  m_ThreaderTransform = ITK_NULLPTR;

  delete[] this->m_ThreaderBSplineTransformWeights;
  this->m_ThreaderBSplineTransformWeights = ITK_NULLPTR;

  delete[] this->m_ThreaderBSplineTransformIndices;
  this->m_ThreaderBSplineTransformIndices = ITK_NULLPTR;
}

/**
 * Set the number of threads. This will be clamped by the
 * multithreader, so we must check to see if it is accepted.
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetNumberOfThreads(ThreadIdType numberOfThreads)
{
  m_Threader->SetNumberOfThreads(numberOfThreads);
  m_NumberOfThreads = m_Threader->GetNumberOfThreads();
}

/**
 * Set the parameters that define a unique transform
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetTransformParameters(const ParametersType & parameters) const
{
  if ( !m_Transform )
    {
    itkExceptionMacro(<< "Transform has not been assigned");
    }
  m_Transform->SetParameters(parameters);

}

template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetNumberOfFixedImageSamples(SizeValueType numSamples)
{
  if ( numSamples != m_NumberOfFixedImageSamples )
    {
    m_NumberOfFixedImageSamples = numSamples;
    if ( m_NumberOfFixedImageSamples != this->m_FixedImageRegion.GetNumberOfPixels() )
      {
      this->SetUseAllPixels(false);
      }
    this->Modified();
    }
}

template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetFixedImageIndexes(const FixedImageIndexContainer & indexes)
{
  this->SetUseFixedImageIndexes(true);
  m_NumberOfFixedImageSamples = indexes.size();
  m_FixedImageIndexes.resize(m_NumberOfFixedImageSamples);
  for ( unsigned int i = 0; i < m_NumberOfFixedImageSamples; i++ )
    {
    m_FixedImageIndexes[i] = indexes[i];
    }
}

template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetUseFixedImageIndexes(bool useIndexes)
{
  if ( useIndexes != m_UseFixedImageIndexes )
    {
    m_UseFixedImageIndexes = useIndexes;
    if ( m_UseFixedImageIndexes )
      {
      this->SetUseAllPixels(false);
      }
    else
      {
      this->Modified();
      }
    }
}

template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetFixedImageSamplesIntensityThreshold(const FixedImagePixelType & thresh)
{
  if ( thresh != m_FixedImageSamplesIntensityThreshold )
    {
    m_FixedImageSamplesIntensityThreshold = thresh;
    this->SetUseFixedImageSamplesIntensityThreshold(true);
    this->Modified();
    }
}

template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetUseFixedImageSamplesIntensityThreshold(bool useThresh)
{
  if ( useThresh != m_UseFixedImageSamplesIntensityThreshold )
    {
    m_UseFixedImageSamplesIntensityThreshold = useThresh;
    if ( m_UseFixedImageSamplesIntensityThreshold )
      {
      this->SetUseAllPixels(false);
      }
    else
      {
      this->Modified();
      }
    }
}

template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetFixedImageRegion(const FixedImageRegionType reg)
{
  if ( reg != m_FixedImageRegion )
    {
    m_FixedImageRegion = reg;
    if ( this->GetUseAllPixels() )
      {
      this->SetNumberOfFixedImageSamples( this->m_FixedImageRegion.GetNumberOfPixels() );
      }
    }
}

template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetUseAllPixels(bool useAllPixels)
{
  if ( useAllPixels != m_UseAllPixels )
    {
    m_UseAllPixels = useAllPixels;
    if ( m_UseAllPixels )
      {
      this->SetUseFixedImageSamplesIntensityThreshold(false);
      this->SetNumberOfFixedImageSamples( this->m_FixedImageRegion.GetNumberOfPixels() );
      this->SetUseSequentialSampling(true);
      }
    else
      {
      this->SetUseSequentialSampling(false);
      this->Modified();
      }
    }
}

template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SetUseSequentialSampling(bool useSequential)
{
  if ( useSequential != m_UseSequentialSampling )
    {
    m_UseSequentialSampling = useSequential;
    if ( !m_UseSequentialSampling )
      {
      this->SetUseAllPixels(false);
      }
    else
      {
      this->Modified();
      }
    }
}

/**
 * Initialize
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::Initialize(void)
{
  if ( !m_Transform )
    {
    itkExceptionMacro(<< "Transform is not present");
    }
  m_NumberOfParameters = m_Transform->GetNumberOfParameters();

  if ( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator is not present");
    }

  if ( !m_MovingImage )
    {
    itkExceptionMacro(<< "MovingImage is not present");
    }

  if ( !m_FixedImage )
    {
    itkExceptionMacro(<< "FixedImage is not present");
    }

  // If the image is provided by a source, update the source.
  if ( m_MovingImage->GetSource() )
    {
    m_MovingImage->GetSource()->Update();
    }

  // If the image is provided by a source, update the source.
  if ( m_FixedImage->GetSource() )
    {
    m_FixedImage->GetSource()->Update();
    }

  //The use of FixedImageIndexes and the use of FixedImageRegion
  //are mutually exclusive, so they should not both be checked.
  if ( this->m_UseFixedImageIndexes  == true )
    {
    if( this->m_FixedImageIndexes.size() == 0 )
      {
      itkExceptionMacro(<< "FixedImageIndexes list is empty");
      }
    }
  else
    {
    // Make sure the FixedImageRegion is within the FixedImage buffered region
    if ( m_FixedImageRegion.GetNumberOfPixels() == 0 )
      {
      itkExceptionMacro(<< "FixedImageRegion is empty");
      }

    if ( !m_FixedImageRegion.Crop( m_FixedImage->GetBufferedRegion() ) )
      {
      itkExceptionMacro(
        << "FixedImageRegion does not overlap the fixed image buffered region");
      }
    }

  m_Interpolator->SetInputImage(m_MovingImage);

  if ( m_ComputeGradient )
    {
    ComputeGradient();
    }

  // If there are any observers on the metric, call them to give the
  // user code a chance to set parameters on the metric
  this->InvokeEvent( InitializeEvent() );
}

/**
 * MultiThreading Initialize
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::MultiThreadingInitialize(void)
{
  this->SetNumberOfThreads(m_NumberOfThreads);

  delete[] m_ThreaderNumberOfMovingImageSamples;
  m_ThreaderNumberOfMovingImageSamples = new unsigned int[m_NumberOfThreads - 1];

  // Allocate the array of transform clones to be used in every thread
  delete[] m_ThreaderTransform;
  m_ThreaderTransform = new TransformPointer[m_NumberOfThreads - 1];
  for ( ThreadIdType ithread = 0; ithread < m_NumberOfThreads - 1; ++ithread )
    {
    this->m_ThreaderTransform[ithread] = this->m_Transform->Clone();
    }

  m_FixedImageSamples.resize(m_NumberOfFixedImageSamples);
  if ( m_UseSequentialSampling )
    {
    //
    // Take all the pixels within the fixed image region)
    // to create the sample points list.
    //
    SampleFullFixedImageRegion(m_FixedImageSamples);
    }
  else
    {
    if ( m_UseFixedImageIndexes )
      {
      //
      //  Use the list of indexes passed to the SetFixedImageIndexes
      //  member function .
      //
      SampleFixedImageIndexes(m_FixedImageSamples);
      }
    else
      {
      //
      // Uniformly sample the fixed image (within the fixed image region)
      // to create the sample points list.
      //
      SampleFixedImageRegion(m_FixedImageSamples);
      }
    }

  //
  //  Check if the interpolator is of type BSplineInterpolateImageFunction.
  //  If so, we can make use of its EvaluateDerivatives method.
  //  Otherwise, we instantiate an external central difference
  //  derivative calculator.
  //
  m_InterpolatorIsBSpline = true;

  BSplineInterpolatorType *testPtr = dynamic_cast< BSplineInterpolatorType * >(
    this->m_Interpolator.GetPointer() );
  if ( !testPtr )
    {
    m_InterpolatorIsBSpline = false;

    m_DerivativeCalculator = DerivativeFunctionType::New();
    m_DerivativeCalculator->UseImageDirectionOn();

    m_DerivativeCalculator->SetInputImage(this->m_MovingImage);

    m_BSplineInterpolator = ITK_NULLPTR;
    itkDebugMacro("Interpolator is not BSpline");
    }
  else
    {
    m_BSplineInterpolator = testPtr;
    m_BSplineInterpolator->SetNumberOfThreads(m_NumberOfThreads);
    m_BSplineInterpolator->UseImageDirectionOn();

    m_DerivativeCalculator = ITK_NULLPTR;
    itkDebugMacro("Interpolator is BSpline");
    }

  //
  //  Check if the transform is of type BSplineTransform.
  //
  //  If so, several speed up features are implemented.
  //  [1] Precomputing the results of bulk transform for each sample point.
  //  [2] Precomputing the BSpline weights for each sample point,
  //      to be used later to directly compute the deformation vector
  //  [3] Precomputing the indices of the parameters within the
  //      the support region of each sample point.
  //
  m_TransformIsBSpline = true;

  BSplineTransformType *testPtr2 = dynamic_cast< BSplineTransformType * >(
    this->m_Transform.GetPointer() );
  if ( !testPtr2 )
    {
    m_TransformIsBSpline = false;
    m_BSplineTransform = ITK_NULLPTR;
    itkDebugMacro("Transform is not BSplineDeformable");
    }
  else
    {
    m_BSplineTransform = testPtr2;
    m_NumBSplineWeights = m_BSplineTransform->GetNumberOfWeights();
    itkDebugMacro("Transform is BSplineDeformable");
    }

  if ( this->m_TransformIsBSpline )
    {
    // First, deallocate memory that may have been used from previous run of the Metric
    this->m_BSplineTransformWeightsArray.SetSize(1, 1);
    this->m_BSplineTransformIndicesArray.SetSize(1, 1);
    this->m_BSplinePreTransformPointsArray.resize(1);
    this->m_WithinBSplineSupportRegionArray.resize(1);
    this->m_BSplineTransformWeights.SetSize(1);
    this->m_BSplineTransformIndices.SetSize(1);

    delete[] this->m_ThreaderBSplineTransformWeights;
    this->m_ThreaderBSplineTransformWeights = ITK_NULLPTR;

    delete[] this->m_ThreaderBSplineTransformIndices;
    this->m_ThreaderBSplineTransformIndices = ITK_NULLPTR;

    if ( this->m_UseCachingOfBSplineWeights )
      {
      m_BSplineTransformWeightsArray.SetSize(
        m_NumberOfFixedImageSamples, m_NumBSplineWeights);
      m_BSplineTransformIndicesArray.SetSize(
        m_NumberOfFixedImageSamples, m_NumBSplineWeights);
      m_BSplinePreTransformPointsArray.resize(m_NumberOfFixedImageSamples);
      m_WithinBSplineSupportRegionArray.resize(m_NumberOfFixedImageSamples);

      this->PreComputeTransformValues();
      }
    else
      {
      this->m_BSplineTransformWeights.SetSize(this->m_NumBSplineWeights);
      this->m_BSplineTransformIndices.SetSize(this->m_NumBSplineWeights);

      this->m_ThreaderBSplineTransformWeights = new BSplineTransformWeightsType[m_NumberOfThreads - 1];
      this->m_ThreaderBSplineTransformIndices = new BSplineTransformIndexArrayType[m_NumberOfThreads - 1];

      for ( ThreadIdType ithread = 0; ithread < m_NumberOfThreads - 1; ++ithread )
        {
        this->m_ThreaderBSplineTransformWeights[ithread].SetSize(this->m_NumBSplineWeights);
        this->m_ThreaderBSplineTransformIndices[ithread].SetSize(this->m_NumBSplineWeights);
        }
      }

    for ( unsigned int j = 0; j < FixedImageDimension; j++ )
      {
      this->m_BSplineParametersOffset[j] = j * this->m_BSplineTransform->GetNumberOfParametersPerDimension();
      }
    }
}

/**
 * Use the indexes that have been passed to the metric
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SampleFixedImageIndexes(FixedImageSampleContainer & samples) const
{
  typename FixedImageSampleContainer::iterator iter;

  SizeValueType len = static_cast<SizeValueType>( m_FixedImageIndexes.size() );
  if ( len != m_NumberOfFixedImageSamples
       || samples.size() != m_NumberOfFixedImageSamples )
    {
    throw ExceptionObject(__FILE__, __LINE__,
                          "Index list size does not match desired number of samples");
    }

  iter = samples.begin();
  for ( SizeValueType i = 0; i < len; i++ )
    {
    // Get sampled index
    FixedImageIndexType index = m_FixedImageIndexes[i];
    // Translate index to point
    m_FixedImage->TransformIndexToPhysicalPoint(index, ( *iter ).point);

    // Get sampled fixed image value
    ( *iter ).value = m_FixedImage->GetPixel(index);
    ( *iter ).valueIndex = 0;

    ++iter;
    }
}

/**
 * Sample the fixed image using a random walk
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SampleFixedImageRegion(FixedImageSampleContainer & samples) const
{
  if ( samples.size() != m_NumberOfFixedImageSamples )
    {
    throw ExceptionObject(__FILE__, __LINE__,
                          "Sample size does not match desired number of samples");
    }

  // Set up a random interator within the user specified fixed image region.
  typedef ImageRandomConstIteratorWithIndex< FixedImageType > RandomIterator;
  RandomIterator randIter( m_FixedImage, GetFixedImageRegion() );
  randIter.ReinitializeSeed(Statistics::MersenneTwisterRandomVariateGenerator::GetInstance()->GetSeed());
  if (m_ReseedIterator)
    {
    randIter.ReinitializeSeed();
    }
  else
    {
    randIter.ReinitializeSeed(m_RandomSeed++);
    }
  typename FixedImageSampleContainer::iterator iter;
  typename FixedImageSampleContainer::const_iterator end = samples.end();

  if ( m_FixedImageMask.IsNotNull()
       || m_UseFixedImageSamplesIntensityThreshold )
    {
    InputPointType inputPoint;

    iter = samples.begin();
    SizeValueType samplesFound = 0;
    randIter.SetNumberOfSamples(m_NumberOfFixedImageSamples * 1000);
    randIter.GoToBegin();
    while ( iter != end )
      {
      if ( randIter.IsAtEnd() )
        {
        // Must be a small mask since after many random samples we don't
        // have enough to fill the desired number.   So, we will replicate
        // the samples we've found so far to fill-in the desired number
        // of samples
        SizeValueType count = 0;
        while ( iter != end )
          {
          ( *iter ).point = samples[count].point;
          ( *iter ).value = samples[count].value;
          ( *iter ).valueIndex = 0;
          ++count;
          if ( count >= samplesFound )
            {
            count = 0;
            }
          ++iter;
          }
        break;
        }

      // Get sampled index
      FixedImageIndexType index = randIter.GetIndex();
      // Check if the Index is inside the mask, translate index to point
      m_FixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

      if ( m_FixedImageMask.IsNotNull() )
        {
        double val;
        if ( m_FixedImageMask->ValueAt(inputPoint, val) )
          {
          if ( Math::AlmostEquals( val, 0.0 ) )
            {
            ++randIter; // jump to another random position
            continue;
            }
          }
        else
          {
          ++randIter; // jump to another random position
          continue;
          }
        }

      if ( m_UseFixedImageSamplesIntensityThreshold
           && randIter.Get() < m_FixedImageSamplesIntensityThreshold )
        {
        ++randIter;
        continue;
        }

      // Translate index to point
      ( *iter ).point = inputPoint;
      // Get sampled fixed image value
      ( *iter ).value = randIter.Get();
      ( *iter ).valueIndex = 0;

      ++samplesFound;
      ++randIter;
      ++iter;
      }
    }
  else
    {
    randIter.SetNumberOfSamples(m_NumberOfFixedImageSamples);
    randIter.GoToBegin();
    for ( iter = samples.begin(); iter != end; ++iter )
      {
      // Get sampled index
      FixedImageIndexType index = randIter.GetIndex();
      // Translate index to point
      m_FixedImage->TransformIndexToPhysicalPoint(index,
                                                  ( *iter ).point);
      // Get sampled fixed image value
      ( *iter ).value = randIter.Get();
      ( *iter ).valueIndex = 0;

      // Jump to random position
      ++randIter;
      }
    }
}

/**
 * Sample the fixed image domain using all pixels in the Fixed image region
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SampleFullFixedImageRegion(FixedImageSampleContainer & samples) const
{
  if ( samples.size() != m_NumberOfFixedImageSamples )
    {
    throw ExceptionObject(__FILE__, __LINE__,
                          "Sample size does not match desired number of samples");
    }

  // Set up a region interator within the user specified fixed image region.
  typedef ImageRegionConstIteratorWithIndex< FixedImageType > RegionIterator;
  RegionIterator regionIter( m_FixedImage, GetFixedImageRegion() );

  regionIter.GoToBegin();

  typename FixedImageSampleContainer::iterator iter;
  typename FixedImageSampleContainer::const_iterator end = samples.end();

  if ( m_FixedImageMask.IsNotNull()
       || m_UseFixedImageSamplesIntensityThreshold )
    {
    InputPointType inputPoint;

    // repeat until we get enough samples to fill the array
    iter = samples.begin();
    while ( iter != end )
      {
      // Get sampled index
      FixedImageIndexType index = regionIter.GetIndex();
      // Check if the Index is inside the mask, translate index to point
      m_FixedImage->TransformIndexToPhysicalPoint(index, inputPoint);

      if ( m_FixedImageMask.IsNotNull() )
        {
        // If not inside the mask, ignore the point
        if ( !m_FixedImageMask->IsInside(inputPoint) )
          {
          ++regionIter; // jump to next pixel
          if ( regionIter.IsAtEnd() )
            {
            regionIter.GoToBegin();
            }
          continue;
          }
        }

      if ( m_UseFixedImageSamplesIntensityThreshold
           && regionIter.Get() < m_FixedImageSamplesIntensityThreshold )
        {
        ++regionIter; // jump to next pixel
        if ( regionIter.IsAtEnd() )
          {
          regionIter.GoToBegin();
          }
        continue;
        }

      // Translate index to point
      ( *iter ).point = inputPoint;
      // Get sampled fixed image value
      ( *iter ).value = regionIter.Get();
      ( *iter ).valueIndex = 0;

      ++regionIter;
      if ( regionIter.IsAtEnd() )
        {
        regionIter.GoToBegin();
        }
      ++iter;
      }
    }
  else // not restricting sample throwing to a mask
    {
    for ( iter = samples.begin(); iter != end; ++iter )
      {
      // Get sampled index
      FixedImageIndexType index = regionIter.GetIndex();

      // Translate index to point
      m_FixedImage->TransformIndexToPhysicalPoint(index,
                                                  ( *iter ).point);
      // Get sampled fixed image value
      ( *iter ).value = regionIter.Get();
      ( *iter ).valueIndex = 0;

      ++regionIter;
      if ( regionIter.IsAtEnd() )
        {
        regionIter.GoToBegin();
        }
      }
    }
}

/**
 * Compute the gradient image and assign it to m_GradientImage.
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::ComputeGradient()
{
  GradientImageFilterPointer gradientFilter = GradientImageFilterType::New();

  gradientFilter->SetInput(m_MovingImage);

  const typename MovingImageType::SpacingType & spacing = m_MovingImage
                                                          ->GetSpacing();
  double maximumSpacing = 0.0;
  for ( unsigned int i = 0; i < MovingImageDimension; i++ )
    {
    if ( spacing[i] > maximumSpacing )
      {
      maximumSpacing = spacing[i];
      }
    }
  gradientFilter->SetSigma(maximumSpacing);
  gradientFilter->SetNormalizeAcrossScale(true);
  gradientFilter->SetNumberOfThreads(m_NumberOfThreads);
  gradientFilter->SetUseImageDirection(true);
  gradientFilter->Update();

  m_GradientImage = gradientFilter->GetOutput();
}

// Method to reinitialize the seed of the random number generator
template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::ReinitializeSeed()
{
  m_ReseedIterator = true;
}

// Method to reinitialize the seed of the random number generator
template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::ReinitializeSeed(int seed)
{
  m_ReseedIterator = false;
  m_RandomSeed = seed;
}

/**
 * Cache pre-transformed points, weights and indices.
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::PreComputeTransformValues()
{
  // Note: This code is specific to the b-spline deformable transform.

  // Unfortunately, the BSplineTransform stores a
  // pointer to parameters passed to SetParameters(). Since
  // we're creating a dummy set of parameters below on the
  // stack, this can cause a crash if the transform's
  // parameters are not later reset with a more properly
  // scoped set of parameters. In addition, we're overwriting
  // any previously set parameters. In order to be kinder,
  // we'll save a pointer to the current set of parameters
  // and restore them after we're done.

  // Note the address operator.
  // const TransformParametersType* previousParameters = &
  // m_Transform->GetParameters();

  // Create all zero dummy transform parameters
  ParametersType dummyParameters(m_NumberOfParameters);

  dummyParameters.Fill(0.0);
  m_Transform->SetParameters(dummyParameters);

  // Cycle through each sampled fixed image point
  BSplineTransformWeightsType    weights(m_NumBSplineWeights);
  BSplineTransformIndexArrayType indices(m_NumBSplineWeights);
  bool                           valid;
  MovingImagePointType           mappedPoint;

  // Declare iterators for iteration over the sample container
  typename FixedImageSampleContainer::const_iterator fiter;
  typename FixedImageSampleContainer::const_iterator fend =
    m_FixedImageSamples.end();
  SizeValueType counter = 0;

  for ( fiter = m_FixedImageSamples.begin(); fiter != fend; ++fiter, counter++ )
    {
    m_BSplineTransform->TransformPoint(m_FixedImageSamples[counter].point,
                                       mappedPoint, weights, indices, valid);

    for ( SizeValueType k = 0; k < m_NumBSplineWeights; k++ )
      {
      m_BSplineTransformWeightsArray[counter][k] = weights[k];
      m_BSplineTransformIndicesArray[counter][k] = indices[k];
      }

    m_BSplinePreTransformPointsArray[counter]      = mappedPoint;
    m_WithinBSplineSupportRegionArray[counter]     = valid;
    }

  // Restore the previous parameters.
  // m_Transform->SetParameters( *previousParameters );
}

/**
 * Transform a point from FixedImage domain to MovingImage domain.
 * This function also checks if mapped point is within support region.
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::TransformPoint(unsigned int sampleNumber,
                 MovingImagePointType & mappedPoint,
                 bool & sampleOk,
                 double & movingImageValue,
                 ThreadIdType threadId) const
{
  sampleOk = true;
  TransformType *transform;

  if ( threadId > 0 )
    {
    transform = this->m_ThreaderTransform[threadId - 1];
    }
  else
    {
    transform = this->m_Transform;
    }

  if ( !m_TransformIsBSpline )
    {
    // Use generic transform to compute mapped position
    mappedPoint = transform->TransformPoint(m_FixedImageSamples[sampleNumber].point);
    sampleOk = true;
    }
  else
    {
    if ( this->m_UseCachingOfBSplineWeights )
      {
      sampleOk = m_WithinBSplineSupportRegionArray[sampleNumber];

      if ( sampleOk )
        {
        // If the transform is BSplineDeformable, we can use the precomputed
        // weights and indices to obtained the mapped position
        const WeightsValueType *weights =
          m_BSplineTransformWeightsArray[sampleNumber];
        const IndexValueType *indices =
          m_BSplineTransformIndicesArray[sampleNumber];

        for ( unsigned int j = 0; j < FixedImageDimension; j++ )
          {
          mappedPoint[j] = m_BSplinePreTransformPointsArray[sampleNumber][j];
          }

        const ParametersType &LocalParameters = m_Transform->GetParameters();
        for ( unsigned int k = 0; k < m_NumBSplineWeights; k++ )
          {
          for ( unsigned int j = 0; j < FixedImageDimension; j++ )
            {
            mappedPoint[j] += weights[k] * LocalParameters[indices[k]
                                                        + m_BSplineParametersOffset[j]];
            }
          }
        }
      }
    else
      {
      BSplineTransformWeightsType *   weightsHelper;
      BSplineTransformIndexArrayType *indicesHelper;

      if ( threadId > 0 )
        {
        weightsHelper = &( this->m_ThreaderBSplineTransformWeights[threadId - 1] );
        indicesHelper = &( this->m_ThreaderBSplineTransformIndices[threadId - 1] );
        }
      else
        {
        weightsHelper = &( this->m_BSplineTransformWeights );
        indicesHelper = &( this->m_BSplineTransformIndices );
        }

      // If not caching values, we invoke the Transform to recompute the
      // mapping of the point.
      this->m_BSplineTransform->TransformPoint(
        this->m_FixedImageSamples[sampleNumber].point,
        mappedPoint, *weightsHelper, *indicesHelper, sampleOk);
      }
    }

  if ( sampleOk )
    {
    // If user provided a mask over the Moving image
    if ( m_MovingImageMask )
      {
      // Check if mapped point is within the support region of the moving image
      // mask
      sampleOk = sampleOk && m_MovingImageMask->IsInside(mappedPoint);
      }

    if ( m_InterpolatorIsBSpline )
      {
      // Check if mapped point inside image buffer
      sampleOk = sampleOk && m_BSplineInterpolator->IsInsideBuffer(mappedPoint);
      if ( sampleOk )
        {
        movingImageValue = m_BSplineInterpolator->Evaluate(mappedPoint, threadId);
        }
      }
    else
      {
      // Check if mapped point inside image buffer
      sampleOk = sampleOk && m_Interpolator->IsInsideBuffer(mappedPoint);
      if ( sampleOk )
        {
        movingImageValue = m_Interpolator->Evaluate(mappedPoint);
        }
      }
    }
}

/**
 * Transform a point from FixedImage domain to MovingImage domain.
 * This function also checks if mapped point is within support region.
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::TransformPointWithDerivatives(unsigned int sampleNumber,
                                MovingImagePointType & mappedPoint,
                                bool & sampleOk,
                                double & movingImageValue,
                                ImageDerivativesType & movingImageGradient,
                                ThreadIdType threadId) const
{
  TransformType *transform;

  sampleOk = true;

  if ( threadId > 0 )
    {
    transform = this->m_ThreaderTransform[threadId - 1];
    }
  else
    {
    transform = this->m_Transform;
    }

  if ( !m_TransformIsBSpline )
    {
    // Use generic transform to compute mapped position
    mappedPoint = transform->TransformPoint(m_FixedImageSamples[sampleNumber].point);
    sampleOk = true;
    }
  else
    {
    if ( this->m_UseCachingOfBSplineWeights )
      {
      sampleOk = m_WithinBSplineSupportRegionArray[sampleNumber];

      if ( sampleOk )
        {
        // If the transform is BSplineDeformable, we can use the precomputed
        // weights and indices to obtained the mapped position
        const WeightsValueType *weights =
          m_BSplineTransformWeightsArray[sampleNumber];
        const IndexValueType *indices =
          m_BSplineTransformIndicesArray[sampleNumber];

        const ParametersType &Local_Parameters=this->m_Transform->GetParameters();
        for ( unsigned int j = 0; j < FixedImageDimension; j++ )
          {
          mappedPoint[j] = m_BSplinePreTransformPointsArray[sampleNumber][j];
          }

        for ( unsigned int k = 0; k < m_NumBSplineWeights; k++ )
          {
          for ( unsigned int j = 0; j < FixedImageDimension; j++ )
            {
            mappedPoint[j] += weights[k] * Local_Parameters[indices[k]
                                                        + m_BSplineParametersOffset[j]];
            }
          }
        }
      }
    else
      {
      BSplineTransformWeightsType *   weightsHelper;
      BSplineTransformIndexArrayType *indicesHelper;

      if ( threadId > 0 )
        {
        weightsHelper = &( this->m_ThreaderBSplineTransformWeights[threadId - 1] );
        indicesHelper = &( this->m_ThreaderBSplineTransformIndices[threadId - 1] );
        }
      else
        {
        weightsHelper = &( this->m_BSplineTransformWeights );
        indicesHelper = &( this->m_BSplineTransformIndices );
        }

      // If not caching values, we invoke the Transform to recompute the
      // mapping of the point.
      this->m_BSplineTransform->TransformPoint(
        this->m_FixedImageSamples[sampleNumber].point,
        mappedPoint, *weightsHelper, *indicesHelper, sampleOk);
      }
    }

  if ( sampleOk )
    {
    // If user provided a mask over the Moving image
    if ( m_MovingImageMask )
      {
      // Check if mapped point is within the support region of the moving image
      // mask
      sampleOk = sampleOk && m_MovingImageMask->IsInside(mappedPoint);
      }

    if ( m_InterpolatorIsBSpline )
      {
      // Check if mapped point inside image buffer
      sampleOk = sampleOk && m_BSplineInterpolator->IsInsideBuffer(mappedPoint);
      if ( sampleOk )
        {
        this->m_BSplineInterpolator->EvaluateValueAndDerivative(mappedPoint,
                                                                movingImageValue,
                                                                movingImageGradient,
                                                                threadId);
        }
      }
    else
      {
      // Check if mapped point inside image buffer
      sampleOk = sampleOk && m_Interpolator->IsInsideBuffer(mappedPoint);
      if ( sampleOk )
        {
        this->ComputeImageDerivatives(mappedPoint, movingImageGradient, threadId);
        movingImageValue = this->m_Interpolator->Evaluate(mappedPoint);
        }
      }
    }
}

/**
 * Compute image derivatives using a central difference function
 * if we are not using a BSplineInterpolator, which includes
 * derivatives.
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::ComputeImageDerivatives(const MovingImagePointType & mappedPoint,
                          ImageDerivativesType & gradient,
                          ThreadIdType threadId) const
{
  if ( m_InterpolatorIsBSpline )
    {
    // Computed moving image gradient using derivative BSpline kernel.
    gradient = m_BSplineInterpolator->EvaluateDerivative(mappedPoint,
                                                         threadId);
    }
  else
    {
    if ( m_ComputeGradient )
      {
      ContinuousIndex< double, MovingImageDimension > tempIndex;
      m_MovingImage->TransformPhysicalPointToContinuousIndex(mappedPoint,
                                                             tempIndex);
      MovingImageIndexType mappedIndex;
      mappedIndex.CopyWithRound(tempIndex);
      gradient = m_GradientImage->GetPixel(mappedIndex);
      }
    else
      {
      // if not using the gradient image
      gradient = m_DerivativeCalculator->Evaluate(mappedPoint);
      }
    }
}

template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueMultiThreadedPreProcessInitiate(void) const
{
  this->SynchronizeTransforms();

  m_Threader->SetSingleMethod( GetValueMultiThreadedPreProcess,
                               (void *)( &m_ThreaderParameter ) );
  m_Threader->SingleMethodExecute();
}

template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueMultiThreadedInitiate(void) const
{
  this->SynchronizeTransforms();

  m_Threader->SetSingleMethod( GetValueMultiThreaded,
                               const_cast< void * >( static_cast< const void * >( &m_ThreaderParameter ) ) );
  m_Threader->SingleMethodExecute();

  for ( ThreadIdType threadId = 0; threadId < m_NumberOfThreads - 1; threadId++ )
    {
    this->m_NumberOfPixelsCounted += m_ThreaderNumberOfMovingImageSamples[threadId];
    }
}

template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueMultiThreadedPostProcessInitiate(void) const
{
  m_Threader->SetSingleMethod( GetValueMultiThreadedPostProcess,
                               const_cast< void * >( static_cast< const void * >( &m_ThreaderParameter ) ) );
  m_Threader->SingleMethodExecute();
}

/**
 * Get the match Measure
 */
template< typename TFixedImage, typename TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueMultiThreadedPreProcess(void *arg)
{
  ThreadIdType                threadId;
  MultiThreaderParameterType *mtParam;

  threadId = ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            ( ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->UserData );

  mtParam->metric->GetValueThreadPreProcess(threadId, false);

  return ITK_THREAD_RETURN_VALUE;
}

/**
 * Get the match Measure
 */
template< typename TFixedImage, typename TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueMultiThreaded(void *arg)
{
  ThreadIdType                threadId;
  MultiThreaderParameterType *mtParam;

  threadId = ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            ( ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->UserData );

  mtParam->metric->GetValueThread(threadId);

  return ITK_THREAD_RETURN_VALUE;
}

/**
 * Get the match Measure
 */
template< typename TFixedImage, typename TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueMultiThreadedPostProcess(void *arg)
{
  ThreadIdType                threadId;
  MultiThreaderParameterType *mtParam;

  threadId = ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            ( ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->UserData );

  mtParam->metric->GetValueThreadPostProcess(threadId, false);

  return ITK_THREAD_RETURN_VALUE;
}

template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueThread(ThreadIdType threadId) const
{
  // Figure out how many samples to process
  int chunkSize = m_NumberOfFixedImageSamples / m_NumberOfThreads;

  // Skip to this thread's samples to process
  unsigned int fixedImageSample = threadId * chunkSize;

  if ( threadId == m_NumberOfThreads - 1 )
    {
    chunkSize = m_NumberOfFixedImageSamples
                - ( ( m_NumberOfThreads - 1 )
                    * chunkSize );
    }


  if ( m_WithinThreadPreProcess )
    {
    this->GetValueThreadPreProcess(threadId, true);
    }

  // Process the samples
  int numSamples = 0;
  for ( int count = 0; count < chunkSize; ++count, ++fixedImageSample )
    {
    MovingImagePointType mappedPoint;
    bool                 sampleOk;
    double               movingImageValue;
    // Get moving image value
    this->TransformPoint(fixedImageSample, mappedPoint, sampleOk, movingImageValue,
                         threadId);

    if ( sampleOk )
      {
      // CALL USER FUNCTION
      if ( GetValueThreadProcessSample(threadId, fixedImageSample,
                                       mappedPoint, movingImageValue) )
        {
        ++numSamples;
        }
      }
    }

  if ( threadId > 0 )
    {
    m_ThreaderNumberOfMovingImageSamples[threadId - 1] = numSamples;
    }
  else
    {
    m_NumberOfPixelsCounted = numSamples;
    }

  if ( m_WithinThreadPostProcess )
    {
    this->GetValueThreadPostProcess(threadId, true);
    }
}

template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeMultiThreadedPreProcessInitiate(void) const
{
  this->SynchronizeTransforms();

  m_Threader->SetSingleMethod( GetValueAndDerivativeMultiThreadedPreProcess,
                               (void *)( &m_ThreaderParameter ) );
  m_Threader->SingleMethodExecute();
}

template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeMultiThreadedInitiate(void) const
{
  this->SynchronizeTransforms();

  m_Threader->SetSingleMethod( GetValueAndDerivativeMultiThreaded,
                               const_cast< void * >( static_cast< const void * >( &m_ThreaderParameter ) ) );
  m_Threader->SingleMethodExecute();

  for ( ThreadIdType threadId = 0; threadId < m_NumberOfThreads - 1; threadId++ )
    {
    this->m_NumberOfPixelsCounted += m_ThreaderNumberOfMovingImageSamples[threadId];
    }
}

template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeMultiThreadedPostProcessInitiate(void) const
{
  m_Threader->SetSingleMethod( GetValueAndDerivativeMultiThreadedPostProcess,
                               (void *)( &m_ThreaderParameter ) );
  m_Threader->SingleMethodExecute();
}

/**
 * Get the match Measure
 */
template< typename TFixedImage, typename TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeMultiThreadedPreProcess(void *arg)
{
  ThreadIdType                threadId;
  MultiThreaderParameterType *mtParam;

  threadId = ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            ( ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->UserData );

  mtParam->metric->GetValueAndDerivativeThreadPreProcess(threadId, false);

  return ITK_THREAD_RETURN_VALUE;
}

/**
 * Get the match Measure
 */
template< typename TFixedImage, typename TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeMultiThreaded(void *arg)
{
  ThreadIdType                threadId;
  MultiThreaderParameterType *mtParam;

  threadId = ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            ( ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->UserData );

  mtParam->metric->GetValueAndDerivativeThread(threadId);

  return ITK_THREAD_RETURN_VALUE;
}

/**
 * Get the match Measure
 */
template< typename TFixedImage, typename TMovingImage  >
ITK_THREAD_RETURN_TYPE
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeMultiThreadedPostProcess(void *arg)
{
  ThreadIdType                threadId;
  MultiThreaderParameterType *mtParam;

  threadId = ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->ThreadID;

  mtParam = (MultiThreaderParameterType *)
            ( ( (MultiThreaderType::ThreadInfoStruct *)( arg ) )->UserData );

  mtParam->metric->GetValueAndDerivativeThreadPostProcess(threadId, false);

  return ITK_THREAD_RETURN_VALUE;
}

template< typename TFixedImage, typename TMovingImage  >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::GetValueAndDerivativeThread(ThreadIdType threadId) const
{
  // Figure out how many samples to process
  int chunkSize = m_NumberOfFixedImageSamples / m_NumberOfThreads;

  // Skip to this thread's samples to process
  unsigned int fixedImageSample = threadId * chunkSize;

  if ( threadId == m_NumberOfThreads - 1 )
    {
    chunkSize = m_NumberOfFixedImageSamples
                - ( ( m_NumberOfThreads - 1 )
                    * chunkSize );
    }

  int numSamples = 0;

  if ( m_WithinThreadPreProcess )
    {
    this->GetValueAndDerivativeThreadPreProcess(threadId, true);
    }

  // Process the samples
  MovingImagePointType mappedPoint;
  bool                 sampleOk;
  double               movingImageValue;
  ImageDerivativesType movingImageGradientValue;
  for ( int count = 0; count < chunkSize; ++count, ++fixedImageSample )
    {
    // Get moving image value
    TransformPointWithDerivatives(fixedImageSample, mappedPoint, sampleOk,
                                  movingImageValue, movingImageGradientValue,
                                  threadId);

    if ( sampleOk )
      {
      // CALL USER FUNCTION
      if ( this->GetValueAndDerivativeThreadProcessSample(threadId,
                                                          fixedImageSample,
                                                          mappedPoint,
                                                          movingImageValue,
                                                          movingImageGradientValue) )
        {
        ++numSamples;
        }
      }
    }

  if ( threadId > 0 )
    {
    m_ThreaderNumberOfMovingImageSamples[threadId - 1] = numSamples;
    }
  else
    {
    m_NumberOfPixelsCounted = numSamples;
    }

  if ( m_WithinThreadPostProcess )
    {
    this->GetValueAndDerivativeThreadPostProcess(threadId, true);
    }
}

/**
 * PrintSelf
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfFixedImageSamples: ";
  os << m_NumberOfFixedImageSamples << std::endl;

  os << indent << "FixedImageSamplesIntensityThreshold: "
     << static_cast< typename NumericTraits< FixedImagePixelType >::PrintType >( m_FixedImageSamplesIntensityThreshold )
     << std::endl;

  os << indent << "UseFixedImageSamplesIntensityThreshold: ";
  os << m_UseFixedImageSamplesIntensityThreshold << std::endl;

  if ( m_UseFixedImageIndexes )
    {
    os << indent << "Use Fixed Image Indexes: True" << std::endl;
    os << indent << "Number of Fixed Image Indexes = "
       << m_FixedImageIndexes.size() << std::endl;
    }
  else
    {
    os << indent << "Use Fixed Image Indexes: False" << std::endl;
    }

  if ( m_UseSequentialSampling )
    {
    os << indent << "Use Sequential Sampling: True" << std::endl;
    }
  else
    {
    os << indent << "Use Sequential Sampling: False" << std::endl;
    }

  os << indent << "UseAllPixels: ";
  os << m_UseAllPixels << std::endl;

  os << indent << "ReseedIterator: " << m_ReseedIterator << std::endl;
  os << indent << "RandomSeed: " << m_RandomSeed << std::endl;

  os << indent << "Threader: " << m_Threader << std::endl;
  os << indent << "Number of Threads: " << m_NumberOfThreads << std::endl;
  os << indent << "ThreaderParameter: " << std::endl;
  os << indent << "ThreaderNumberOfMovingImageSamples: " << std::endl;
  if ( m_ThreaderNumberOfMovingImageSamples )
    {
    for ( ThreadIdType i = 0; i < m_NumberOfThreads - 1; i++ )
      {
      os << "  Thread[" << i << "]= " << (unsigned int)m_ThreaderNumberOfMovingImageSamples[i] << std::endl;
      }
    }

  os << indent << "ComputeGradient: "
     << static_cast< typename NumericTraits< bool >::PrintType >( m_ComputeGradient )
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
  os << indent << "Number of Moving Image Samples: " << m_NumberOfPixelsCounted
     << std::endl;

  os << indent << "UseCachingOfBSplineWeights: ";
  os << this->m_UseCachingOfBSplineWeights << std::endl;
}

/** This method can be const because we are not altering the m_ThreaderTransform
 *  pointer. We are altering the object that m_ThreaderTransform[idx] points at.
 *  This is allowed under C++ const rules.
 */
template< typename TFixedImage, typename TMovingImage >
void
ImageToImageMetric< TFixedImage, TMovingImage >
::SynchronizeTransforms() const
{
  for ( ThreadIdType threadId = 0; threadId < m_NumberOfThreads - 1; threadId++ )
    {
    /** Set the fixed parameters first. Some transforms have parameters which depend on
        the values of the fixed parameters. For instance, the BSplineTransform
        checks the grid size (part of the fixed parameters) before setting the parameters. */
    this->m_ThreaderTransform[threadId]->SetFixedParameters( this->m_Transform->GetFixedParameters() );
    this->m_ThreaderTransform[threadId]->SetParameters( this->m_Transform->GetParameters() );
    }
}
} // end namespace itk

#endif
