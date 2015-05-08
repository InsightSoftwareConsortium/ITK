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
#ifndef itkVectorConfidenceConnectedImageFilter_hxx
#define itkVectorConfidenceConnectedImageFilter_hxx

#include "itkVectorConfidenceConnectedImageFilter.h"
#include "itkMacro.h"
#include "itkImageRegionIterator.h"
#include "itkVectorMeanImageFunction.h"
#include "itkCovarianceImageFunction.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkNumericTraitsRGBPixel.h"
#include "itkProgressReporter.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::VectorConfidenceConnectedImageFilter()
{
  m_Multiplier = 2.5;
  m_NumberOfIterations = 4;
  m_Seeds.clear();
  m_InitialNeighborhoodRadius = 1;
  m_ReplaceValue = NumericTraits< OutputImagePixelType >::OneValue();
  m_ThresholdFunction = DistanceThresholdFunctionType::New();
}

/**
 * Standard PrintSelf method.
 */
template< typename TInputImage, typename TOutputImage >
void
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
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
}

template< typename TInputImage, typename TOutputImage >
void
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::SetSeed(const IndexType & seed)
{
  this->ClearSeeds();
  this->AddSeed(seed);
}

template< typename TInputImage, typename TOutputImage >
void
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::AddSeed(const IndexType & seed)
{
  m_Seeds.push_back(seed);
  this->Modified();
}

template< typename TInputImage, typename TOutputImage >
void
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
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
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
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
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  typedef typename InputImageType::PixelType InputPixelType;

  typedef BinaryThresholdImageFunction< OutputImageType >
  SecondFunctionType;
  typedef FloodFilledImageFunctionConditionalIterator< OutputImageType, DistanceThresholdFunctionType > IteratorType;
  typedef FloodFilledImageFunctionConditionalConstIterator< InputImageType,
                                                            SecondFunctionType >        SecondIteratorType;

  unsigned int loop;

  typename Superclass::InputImageConstPointer inputImage  = this->GetInput();
  typename Superclass::OutputImagePointer outputImage = this->GetOutput();

  // Zero the output
  OutputImageRegionType region = outputImage->GetRequestedRegion();
  outputImage->SetBufferedRegion(region);
  outputImage->Allocate();
  outputImage->FillBuffer (NumericTraits< OutputImagePixelType >::ZeroValue());

  // Compute the statistics of the seed point
  typedef VectorMeanImageFunction< InputImageType > VectorMeanImageFunctionType;
  typename VectorMeanImageFunctionType::Pointer meanFunction =
    VectorMeanImageFunctionType::New();

  meanFunction->SetInputImage(inputImage);
  meanFunction->SetNeighborhoodRadius(m_InitialNeighborhoodRadius);
  typedef CovarianceImageFunction< InputImageType > CovarianceImageFunctionType;
  typename CovarianceImageFunctionType::Pointer varianceFunction =
    CovarianceImageFunctionType::New();
  varianceFunction->SetInputImage(inputImage);
  varianceFunction->SetNeighborhoodRadius(m_InitialNeighborhoodRadius);

  // Set up the image function used for connectivity
  m_ThresholdFunction->SetInputImage (inputImage);

  CovarianceMatrixType covariance;
  MeanVectorType       mean;

  typedef typename InputPixelType::ValueType                     ComponentPixelType;
  typedef typename NumericTraits< ComponentPixelType >::RealType ComponentRealType;

  const unsigned int dimension = inputImage->GetNumberOfComponentsPerPixel();

  covariance = CovarianceMatrixType(dimension, dimension);
  mean       = MeanVectorType(dimension);

  covariance.fill(NumericTraits< ComponentRealType >::ZeroValue());
  mean.fill(NumericTraits< ComponentRealType >::ZeroValue());

  typedef typename VectorMeanImageFunctionType::OutputType MeanFunctionVectorType;
  typedef typename CovarianceImageFunctionType::OutputType CovarianceFunctionMatrixType;

  typename SeedsContainerType::const_iterator si = m_Seeds.begin();
  typename SeedsContainerType::const_iterator li = m_Seeds.end();
  SizeValueType seed_cnt = 0;
  while ( si != li )
    {
    if ( region.IsInside(*si) )
      {
      ++seed_cnt;
      const MeanFunctionVectorType       meanContribution       = meanFunction->EvaluateAtIndex(*si);
      const CovarianceFunctionMatrixType covarianceContribution = varianceFunction->EvaluateAtIndex(*si);
      for ( unsigned int ii = 0; ii < dimension; ii++ )
        {
        mean[ii] += meanContribution[ii];
        for ( unsigned int jj = 0; jj < dimension; jj++ )
          {
          covariance[ii][jj] += covarianceContribution[ii][jj];
          }
        }
      }
    si++;
    }

  if ( seed_cnt == 0 )
    {
      this->UpdateProgress(1.0);
      // no seeds result in zero image
      return;
    }

  for ( unsigned int ik = 0; ik < dimension; ik++ )
    {
    mean[ik] /= seed_cnt;
    for ( unsigned int jk = 0; jk < dimension; jk++ )
      {
      covariance[ik][jk] /= seed_cnt;
      }
    }

  m_ThresholdFunction->SetMean(mean);
  m_ThresholdFunction->SetCovariance(covariance);

  m_ThresholdFunction->SetThreshold(m_Multiplier);

  itkDebugMacro(<< "\nMultiplier originally = " << m_Multiplier);

  // Make sure that the multiplier is large enough to include the seed points
  // themselves.
  // This is a pragmatic fix, but a questionable practice because it means that
  // the actual
  // region may be grown using a multiplier different from the one specified by
  // the user.

  si = m_Seeds.begin();
  li = m_Seeds.end();
  while ( si != li )
    {
    if ( region.IsInside(*si) )
      {
      const double distance =
        m_ThresholdFunction->EvaluateDistanceAtIndex(*si);
      if ( distance >  m_Multiplier )
        {
        m_Multiplier = distance;
        }
      }
    si++;
    }

  // Finally setup the eventually modified multiplier. That is actually the
  // threshold itself.
  m_ThresholdFunction->SetThreshold(m_Multiplier);

  itkDebugMacro(<< "\nMultiplier after verifying seeds inclusion = " << m_Multiplier);

  // Segment the image, the iterator walks the output image (so Set()
  // writes into the output image), starting at the seed point.  As
  // the iterator walks, if the corresponding pixel in the input image
  // (accessed via the "m_ThresholdFunction" assigned to the iterator) is within
  // the [lower, upper] bounds prescribed, the pixel is added to the
  // output segmentation and its neighbors become candidates for the
  // iterator to walk.
  IteratorType it = IteratorType (outputImage, m_ThresholdFunction, m_Seeds);
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

    covariance = CovarianceMatrixType(dimension, dimension);
    mean       = MeanVectorType(dimension);

    covariance.fill(NumericTraits< ComponentRealType >::ZeroValue());
    mean.fill(NumericTraits< ComponentRealType >::ZeroValue());

    SizeValueType num = NumericTraits< SizeValueType >::ZeroValue();

    SecondIteratorType sit =
      SecondIteratorType (inputImage, secondFunction, m_Seeds);
    sit.GoToBegin();
    while ( !sit.IsAtEnd() )
      {
      const InputPixelType pixelValue = sit.Get();
      for ( unsigned int i = 0; i < dimension; i++ )
        {
        const ComponentRealType pixelValueI = static_cast< ComponentRealType >( pixelValue[i] );
        covariance[i][i] += pixelValueI * pixelValueI;
        mean[i] += pixelValueI;
        for ( unsigned int j = i + 1; j < dimension; j++ )
          {
          const ComponentRealType pixelValueJ = static_cast< ComponentRealType >( pixelValue[j] );
          const ComponentRealType product = pixelValueI * pixelValueJ;
          covariance[i][j] += product;
          covariance[j][i] += product;
          }
        }
      ++num;
      ++sit;
      }
    for ( unsigned int ii = 0; ii < dimension; ii++ )
      {
      mean[ii] /= static_cast< double >( num );
      for ( unsigned int jj = 0; jj < dimension; jj++ )
        {
        covariance[ii][jj] /= static_cast< double >( num );
        }
      }

    for ( unsigned int ik = 0; ik < dimension; ik++ )
      {
      for ( unsigned int jk = 0; jk < dimension; jk++ )
        {
        covariance[ik][jk] -= mean[ik] * mean[jk];
        }
      }

    m_ThresholdFunction->SetMean(mean);
    m_ThresholdFunction->SetCovariance(covariance);

    // Rerun the segmentation, the iterator walks the output image,
    // starting at the seed point.  As the iterator walks, if the
    // corresponding pixel in the input image (accessed via the
    // "m_ThresholdFunction" assigned to the iterator) is within the [lower,
    // upper] bounds prescribed, the pixel is added to the output
    // segmentation and its neighbors become candidates for the
    // iterator to walk.
    outputImage->FillBuffer (NumericTraits< OutputImagePixelType >::ZeroValue());
    IteratorType thirdIt = IteratorType (outputImage, m_ThresholdFunction, m_Seeds);
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
    e.SetDescription("Process aborted.");
    e.SetLocation(ITK_LOCATION);
    throw e;
    }
}

template< typename TInputImage, typename TOutputImage >
const typename
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >::CovarianceMatrixType &
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::GetCovariance() const
{
  return m_ThresholdFunction->GetCovariance();
}

template< typename TInputImage, typename TOutputImage >
const typename
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >::MeanVectorType &
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::GetMean() const
{
  return m_ThresholdFunction->GetMean();
}

template< typename TInputImage, typename TOutputImage >
const typename VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >::SeedsContainerType &
VectorConfidenceConnectedImageFilter< TInputImage, TOutputImage >
::GetSeeds() const
{
  itkDebugMacro("returning Seeds");
  return this->m_Seeds;
}

} // end namespace itk

#endif
