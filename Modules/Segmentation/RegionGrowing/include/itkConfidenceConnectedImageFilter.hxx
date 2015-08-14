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
#ifndef itkConfidenceConnectedImageFilter_hxx
#define itkConfidenceConnectedImageFilter_hxx

#include "itkMath.h"
#include "itkConfidenceConnectedImageFilter.h"
#include "itkMacro.h"
#include "itkImageRegionIterator.h"
#include "itkMeanImageFunction.h"
#include "itkSumOfSquaresImageFunction.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
ConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::ConfidenceConnectedImageFilter()
{
  m_Multiplier = 2.5;
  m_NumberOfIterations = 4;
  m_Seeds.clear();
  m_InitialNeighborhoodRadius = 1;
  m_ReplaceValue = NumericTraits< OutputImagePixelType >::OneValue();
  m_Mean     = NumericTraits< InputRealType >::ZeroValue();
  m_Variance = NumericTraits< InputRealType >::ZeroValue();
}

template< typename TInputImage, typename TOutputImage >
void
ConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::SetSeed(const IndexType & seed)
{
  this->m_Seeds.clear();
  this->AddSeed(seed);
}

template< typename TInputImage, typename TOutputImage >
void
ConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::ClearSeeds()
{
  if ( this->m_Seeds.size() > 0 )
    {
    this->m_Seeds.clear();
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
void
ConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::AddSeed(const IndexType & seed)
{
  this->m_Seeds.push_back(seed);
  this->Modified();
}

/** Method to access seed container */
template< typename TInputImage, typename TOutputImage >
const typename ConfidenceConnectedImageFilter<TInputImage,TOutputImage>::SeedsContainerType &
ConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::GetSeeds() const
{
  itkDebugMacro("returning Seeds");
  return this->m_Seeds;
}

/**
 * Standard PrintSelf method.
 */
template< typename TInputImage, typename TOutputImage >
void
ConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Number of iterations: " << m_NumberOfIterations
     << std::endl;
  os << indent << "Multiplier for confidence interval: " << m_Multiplier
     << std::endl;
  os << indent << "ReplaceValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_ReplaceValue )
     << std::endl;
  os << indent << "InitialNeighborhoodRadius: " << m_InitialNeighborhoodRadius
     << std::endl;
  os << indent << "Mean of the connected region: " << m_Mean
     << std::endl;
  os << indent << "Variance of the connected region: " << m_Variance
     << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void
ConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer input =
      const_cast< TInputImage * >( this->GetInput() );
    input->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TOutputImage >
void
ConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
ConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typedef BinaryThresholdImageFunction< InputImageType, double >  FunctionType;
  typedef BinaryThresholdImageFunction< OutputImageType, double > SecondFunctionType;

  typedef FloodFilledImageFunctionConditionalIterator< OutputImageType, FunctionType >           IteratorType;
  typedef FloodFilledImageFunctionConditionalConstIterator< InputImageType, SecondFunctionType > SecondIteratorType;

  unsigned int loop;

  typename Superclass::InputImageConstPointer inputImage  = this->GetInput();
  typename Superclass::OutputImagePointer outputImage = this->GetOutput();

  // Zero the output
  OutputImageRegionType region = outputImage->GetRequestedRegion();
  outputImage->SetBufferedRegion(region);
  outputImage->Allocate();
  outputImage->FillBuffer (NumericTraits< OutputImagePixelType >::ZeroValue());

  // Compute the statistics of the seed point
  typedef MeanImageFunction< InputImageType,
                             double > MeanImageFunctionType;

  typedef SumOfSquaresImageFunction< InputImageType,
                                     double > SumOfSquaresImageFunctionType;

  typename MeanImageFunctionType::Pointer meanFunction =
    MeanImageFunctionType::New();

  meanFunction->SetInputImage(inputImage);
  meanFunction->SetNeighborhoodRadius(m_InitialNeighborhoodRadius);

  typename SumOfSquaresImageFunctionType::Pointer sumOfSquaresFunction =
    SumOfSquaresImageFunctionType::New();

  sumOfSquaresFunction->SetInputImage(inputImage);
  sumOfSquaresFunction->SetNeighborhoodRadius(m_InitialNeighborhoodRadius);

  // Set up the image function used for connectivity
  typename FunctionType::Pointer function = FunctionType::New();
  function->SetInputImage (inputImage);

  InputRealType lower;
  InputRealType upper;

  m_Mean     = itk::NumericTraits< InputRealType >::ZeroValue();
  m_Variance = itk::NumericTraits< InputRealType >::ZeroValue();

  if ( m_InitialNeighborhoodRadius > 0 )
    {
    InputRealType sumOfSquares = itk::NumericTraits< InputRealType >::ZeroValue();

    typename SeedsContainerType::const_iterator si = m_Seeds.begin();
    typename SeedsContainerType::const_iterator li = m_Seeds.end();
    SizeValueType num = 0;
    while ( si != li )
      {
      if ( region.IsInside(*si) )
        {
        m_Mean += meanFunction->EvaluateAtIndex(*si);
        sumOfSquares += sumOfSquaresFunction->EvaluateAtIndex(*si);
        ++num;
        }
      si++;
      }

    if ( num == 0 )
      {
      this->UpdateProgress(1.0);
      // no seeds result in zero image
      return;
      }

    const double totalNum = num * sumOfSquaresFunction->GetNeighborhoodSize();
    m_Mean /= num;
    m_Variance = ( sumOfSquares - ( m_Mean * m_Mean * totalNum ) ) / ( totalNum - 1.0 );
    }
  else
    {
    InputRealType sum = itk::NumericTraits< InputRealType >::ZeroValue();
    InputRealType sumOfSquares = itk::NumericTraits< InputRealType >::ZeroValue();

    typename SeedsContainerType::const_iterator si = m_Seeds.begin();
    typename SeedsContainerType::const_iterator li = m_Seeds.end();
    SizeValueType num = 0;
    while ( si != li )
      {
      if ( region.IsInside(*si) )
        {
        const InputRealType value =
          static_cast< InputRealType >( inputImage->GetPixel(*si) );

        sum += value;
        sumOfSquares += value * value;
        ++num;
        }
      si++;
      }

    if ( num == 0 )
      {
      this->UpdateProgress(1.0);
      // no seeds result in zero image
      return;
      }
    m_Mean      = sum / double(num);
    m_Variance  = ( sumOfSquares - ( sum * sum / double(num) ) ) / ( double(num) - 1.0 );
    }

  lower = m_Mean - m_Multiplier *std::sqrt(m_Variance);
  upper = m_Mean + m_Multiplier *std::sqrt(m_Variance);

  // Find the highest and lowest seed intensity.
  InputRealType lowestSeedIntensity = itk::NumericTraits< InputImagePixelType >::max();
  InputRealType highestSeedIntensity = itk::NumericTraits< InputImagePixelType >::NonpositiveMin();
  typename SeedsContainerType::const_iterator si = m_Seeds.begin();
  typename SeedsContainerType::const_iterator li = m_Seeds.end();
  while ( si != li )
    {
    if ( region.IsInside(*si) )
      {
      const InputRealType seedIntensity =
        static_cast< InputRealType >( inputImage->GetPixel(*si) );

      if ( lowestSeedIntensity > seedIntensity )
        {
        lowestSeedIntensity = seedIntensity;
        }
      if ( highestSeedIntensity < seedIntensity )
        {
        highestSeedIntensity = seedIntensity;
        }
      }
    si++;
    }

  // Adjust lower and upper to always contain the seed's intensity, otherwise,
  // no pixels will be
  // returned by the iterator and a zero variance will result
  if ( lower > lowestSeedIntensity )
    {
    lower = lowestSeedIntensity;
    }
  if ( upper < highestSeedIntensity )
    {
    upper = highestSeedIntensity;
    }

  // Make sure the lower and upper limit are not outside the valid range of the
  // input
  if ( lower < static_cast< InputRealType >( NumericTraits< InputImagePixelType >::NonpositiveMin() ) )
    {
    lower = static_cast< InputRealType >( NumericTraits< InputImagePixelType >::NonpositiveMin() );
    }
  if ( upper > static_cast< InputRealType >( NumericTraits< InputImagePixelType >::max() ) )
    {
    upper = static_cast< InputRealType >( NumericTraits< InputImagePixelType >::max() );
    }

  function->ThresholdBetween( static_cast< InputImagePixelType >( lower ),
                              static_cast< InputImagePixelType >( upper ) );

  itkDebugMacro(
    << "\nLower intensity = " << lower << ", Upper intensity = " << upper << "\nmean = " << m_Mean
    << " , std::sqrt(variance) = " << std::sqrt(m_Variance) );

  // Segment the image, the iterator walks the output image (so Set()
  // writes into the output image), starting at the seed point.  As
  // the iterator walks, if the corresponding pixel in the input image
  // (accessed via the "function" assigned to the iterator) is within
  // the [lower, upper] bounds prescribed, the pixel is added to the
  // output segmentation and its neighbors become candidates for the
  // iterator to walk.
  IteratorType it = IteratorType (outputImage, function, m_Seeds);
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    it.Set(m_ReplaceValue);
    ++it;
    }

  ProgressReporter progress(this, 0, region.GetNumberOfPixels() * m_NumberOfIterations);

  for ( loop = 0; loop < m_NumberOfIterations; ++loop )
    {
    // Now that we have an initial segmentation, let's recalculate the
    // statistics.  Since we have already labelled the output, we visit
    // pixels in the input image that have been set in the output image.
    // Essentially, we flip the iterator around, so we walk the input
    // image (so Get() will get pixel values from the input) and constrain
    // iterator such it only visits pixels that were set in the output.
    typename SecondFunctionType::Pointer secondFunction = SecondFunctionType::New();
    secondFunction->SetInputImage (outputImage);
    secondFunction->ThresholdBetween(m_ReplaceValue, m_ReplaceValue);

    typename NumericTraits< typename InputImageType::PixelType >::RealType sum, sumOfSquares;
    sum = NumericTraits< InputRealType >::ZeroValue();
    sumOfSquares = NumericTraits< InputRealType >::ZeroValue();
    typename TOutputImage::SizeValueType numberOfSamples = 0;

    SecondIteratorType sit =
      SecondIteratorType (inputImage, secondFunction, m_Seeds);
    sit.GoToBegin();
    while ( !sit.IsAtEnd() )
      {
      const InputRealType value = static_cast< InputRealType >( sit.Get() );
      sum += value;
      sumOfSquares += value * value;
      ++numberOfSamples;
      ++sit;
      }
    m_Mean      = sum / double(numberOfSamples);
    m_Variance  = ( sumOfSquares - ( sum * sum / double(numberOfSamples) ) ) / ( double(numberOfSamples) - 1.0 );
    // if the variance is zero, there is no point in continuing
    if ( Math::AlmostEquals( m_Variance, 0.0 ) )
      {
      itkDebugMacro( << "\nLower intensity = " << lower
                     << ", Upper intensity = " << upper
                     << "\nmean = " << m_Mean
                     << ", variance = " << m_Variance
                     << " , std::sqrt(variance) = " << std::sqrt(m_Variance) );
      itkDebugMacro(<< "\nsum = " << sum
                    << ", sumOfSquares = "
                    << sumOfSquares << "\nnumberOfSamples = "
                    << numberOfSamples);
      break;
      }
    lower = m_Mean - m_Multiplier *std::sqrt(m_Variance);
    upper = m_Mean + m_Multiplier *std::sqrt(m_Variance);

    // Adjust lower and upper to always contain the seed's intensity, otherwise,
    // no pixels will be
    // returned by the iterator and a zero variance will result
    if ( lower > lowestSeedIntensity )
      {
      lower = lowestSeedIntensity;
      }
    if ( upper < highestSeedIntensity )
      {
      upper = highestSeedIntensity;
      }
    // Make sure the lower and upper limit are not outside the valid range of
    // the input
    if ( lower < static_cast< InputRealType >( NumericTraits< InputImagePixelType >::NonpositiveMin() ) )
      {
      lower = static_cast< InputRealType >( NumericTraits< InputImagePixelType >::NonpositiveMin() );
      }
    if ( upper > static_cast< InputRealType >( NumericTraits< InputImagePixelType >::max() ) )
      {
      upper = static_cast< InputRealType >( NumericTraits< InputImagePixelType >::max() );
      }

    function->ThresholdBetween( static_cast< InputImagePixelType >( lower ),
                                static_cast< InputImagePixelType >( upper ) );

    itkDebugMacro( << "\nLower intensity = " << lower
                   << ", Upper intensity = " << upper
                   << "\nmean = " << m_Mean
                   << ", variance = " << m_Variance
                   << " , std::sqrt(variance) = " << std::sqrt(m_Variance) );
    itkDebugMacro(<< "\nsum = " << sum << ", sumOfSquares = " << sumOfSquares << "\nnum = " << numberOfSamples);

    // Rerun the segmentation, the iterator walks the output image,
    // starting at the seed point.  As the iterator walks, if the
    // corresponding pixel in the input image (accessed via the
    // "function" assigned to the iterator) is within the [lower,
    // upper] bounds prescribed, the pixel is added to the output
    // segmentation and its neighbors become candidates for the
    // iterator to walk.
    outputImage->FillBuffer (NumericTraits< OutputImagePixelType >::ZeroValue());
    IteratorType thirdIt = IteratorType (outputImage, function, m_Seeds);
    thirdIt.GoToBegin();
    try
      {
      while ( !thirdIt.IsAtEnd() )
        {
        thirdIt.Set(m_ReplaceValue);
        ++thirdIt;
        progress.CompletedPixel();  // potential exception thrown here
        }
      }
    catch ( ProcessAborted & )
      {
      break; // interrupt the iterations loop
      }
    }  // end iteration loop

  if ( this->GetAbortGenerateData() )
    {
    ProcessAborted e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Process aborted.");
    throw ProcessAborted(__FILE__, __LINE__);
    }
}
} // end namespace itk

#endif
