/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkCoocurrenceTextureFeaturesImageFilter_hxx
#define itkCoocurrenceTextureFeaturesImageFilter_hxx

#include "itkCoocurrenceTextureFeaturesImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkBinaryFunctorImageFilter.h"
#include "itkDigitizerFunctor.h"

namespace itk
{
namespace Statistics
{
template <typename TInputImage, typename TOutputImage, typename TMaskImage>
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::CoocurrenceTextureFeaturesImageFilter()
  : m_NumberOfBinsPerAxis(itkGetStaticConstMacro(DefaultBinsPerAxis))
  , m_HistogramMinimum(NumericTraits<PixelType>::NonpositiveMin())
  , m_HistogramMaximum(NumericTraits<PixelType>::max())
  , m_InsidePixelValue(NumericTraits<MaskPixelType>::OneValue())
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  // Mark the "MaskImage" as an optional named input. First it has to
  // be added to the list of named inputs then removed from the
  // required list.
  Self::AddRequiredInputName("MaskImage");
  Self::RemoveRequiredInputName("MaskImage");

  // Set the offset directions to their defaults: half of all the possible
  // directions 1 pixel away. (The other half is included by symmetry.)
  // We use a neighborhood iterator to calculate the appropriate offsets.
  using NeighborhoodType = Neighborhood<typename InputImageType::PixelType, InputImageType::ImageDimension>;
  NeighborhoodType hood;
  hood.SetRadius(1);

  // Select all "previous" neighbors that are face+edge+vertex
  // connected to the iterated pixel. Do not include the currentInNeighborhood pixel.
  unsigned int        centerIndex = hood.GetCenterNeighborhoodIndex();
  OffsetVectorPointer offsets = OffsetVector::New();
  for (unsigned int d = 0; d < centerIndex; ++d)
  {
    OffsetType offset = hood.GetOffset(d);
    offsets->push_back(offset);
  }
  this->SetOffsets(offsets);
  NeighborhoodType nhood;
  nhood.SetRadius(2);
  this->m_NeighborhoodRadius = nhood.GetRadius();

  this->m_Normalize = false;
  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::SetOffset(const OffsetType offset)
{
  OffsetVectorPointer offsetVector = OffsetVector::New();
  offsetVector->push_back(offset);
  this->SetOffsets(offsetVector);
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::BeforeThreadedGenerateData()
{

  typename TInputImage::Pointer input = InputImageType::New();
  input->Graft(const_cast<TInputImage *>(this->GetInput()));

  using DigitizerFunctorType = Digitizer<PixelType, PixelType, typename DigitizedImageType::PixelType>;

  DigitizerFunctorType digitalizer(m_NumberOfBinsPerAxis, m_InsidePixelValue, m_HistogramMinimum, m_HistogramMaximum);

  using FilterType = BinaryFunctorImageFilter<MaskImageType, InputImageType, DigitizedImageType, DigitizerFunctorType>;
  typename FilterType::Pointer filter = FilterType::New();
  if (this->GetMaskImage() != nullptr)
  {
    typename TMaskImage::Pointer mask = MaskImageType::New();
    mask->Graft(const_cast<TMaskImage *>(this->GetMaskImage()));
    filter->SetInput1(mask);
  }
  else
  {
    filter->SetConstant1(m_InsidePixelValue);
  }
  filter->SetInput2(input);
  filter->SetFunctor(digitalizer);
  filter->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());

  filter->Update();
  m_DigitizedInputImage = filter->GetOutput();
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::AfterThreadedGenerateData()
{
  // Free internal image
  this->m_DigitizedInputImage = nullptr;
}


template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::DynamicThreadedGenerateData(
  const OutputRegionType & outputRegionForThread)
{
  // Recuperation of the different inputs/outputs
  OutputImageType * outputPtr = this->GetOutput();

  // Creation of the output pixel type
  typename TOutputImage::PixelType outputPixel;
  NumericTraits<typename TOutputImage::PixelType>::SetLength(outputPixel, outputPtr->GetNumberOfComponentsPerPixel());

  // Separation of the non-boundary region that will be processed in a different way
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<DigitizedImageType> boundaryFacesCalculator;
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<DigitizedImageType>::FaceListType faceList =
    boundaryFacesCalculator(this->m_DigitizedInputImage, outputRegionForThread, m_NeighborhoodRadius);
  auto fit = faceList.begin();

  // Declaration of the variables useful to iterate over the all image region
  bool                                 isInImage;
  typename OffsetVector::ConstIterator offsets;

  // Declaration of the variables useful to iterate over the all the offsets
  OffsetType   offset;
  unsigned int totalNumberOfFreq;


  vnl_matrix<unsigned int> hist(m_NumberOfBinsPerAxis, m_NumberOfBinsPerAxis);

  // Declaration of the variables useful to iterate over the all neighborhood region
  HistogramIndexType currentInNeighborhoodPixelIntensity;

  // Declaration of the variables useful to iterate over the run
  HistogramIndexType pixelIntensity(NumericTraits<HistogramIndexType>::ZeroValue());
  OffsetType         tempOffset;

  /// ***** Non-boundary Region *****
  for (; fit != faceList.end(); ++fit)
  {
    NeighborhoodIteratorType inputNIt(m_NeighborhoodRadius, this->m_DigitizedInputImage, *fit);
    using IteratorType = itk::ImageRegionIterator<OutputImageType>;
    IteratorType outputIt(outputPtr, *fit);

    // Iteration over the all image region
    while (!inputNIt.IsAtEnd())
    {
      // If the voxel is outside of the mask, don't treat it
      if (inputNIt.GetCenterPixel() < (-5)) // the pixel is outside of the mask
      {
        outputPixel.Fill(0);
        outputIt.Set(outputPixel);
        ++inputNIt;
        ++outputIt;
        continue;
      }
      // Initialisation of the histogram
      hist.fill(0);

      totalNumberOfFreq = 0;
      // Iteration over all the offsets
      for (offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); ++offsets)
      {
        offset = offsets.Value();
        // Iteration over the all neighborhood region
        for (NeighborIndexType nb = 0; nb < inputNIt.Size(); ++nb)
        {
          // Test if the current voxel is in the mask and is the range of the image intensity specified
          currentInNeighborhoodPixelIntensity = inputNIt.GetPixel(nb);
          if (currentInNeighborhoodPixelIntensity < 0)
          {
            continue;
          }

          // Test if the current offset is still pointing to a voxel inside th neighborhood
          tempOffset = inputNIt.GetOffset(nb) + offset;
          if (!(this->IsInsideNeighborhood(tempOffset)))
          {
            continue;
          }

          // Test if the part of the neighborhood pointed by the offset is still part of the image
          if (fit == faceList.begin())
          {
            inputNIt.GetPixel(tempOffset, isInImage);
            if (!isInImage)
            {
              break;
            }
          }

          // Test if the pointed voxel is in the mask and is the range of the image intensity specified
          pixelIntensity = inputNIt.GetPixel(tempOffset);
          if (pixelIntensity < 0)
          {
            continue;
          }

          // Increase the corresponding bin in the histogram
          ++totalNumberOfFreq;
          ++hist[currentInNeighborhoodPixelIntensity][pixelIntensity];
        }
      }

      // No coocurrences means we are computing the texture of a single pixel, which is undefined
      if (totalNumberOfFreq != 0)
      {
        // Compute the run length features
        this->ComputeFeatures(hist, totalNumberOfFreq, outputPixel);
        outputIt.Set(outputPixel);
      }

      ++inputNIt;
      ++outputIt;
    }
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::GenerateOutputInformation()
{
  // Call superclass's version
  Superclass::GenerateOutputInformation();

  OutputImageType * output = this->GetOutput();
  // If the output image type is a VectorImage the number of
  // components will be properly sized if before allocation, if the
  // output is a fixed width vector and the wrong number of
  // components, then an exception will be thrown.
  if (output->GetNumberOfComponentsPerPixel() != 8)
  {
    output->SetNumberOfComponentsPerPixel(8);
  }
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
bool
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::IsInsideNeighborhood(
  const OffsetType & iteratedOffset)
{
  bool insideNeighborhood = true;
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; ++i)
  {
    int boundDistance = m_NeighborhoodRadius[i] - Math::abs(iteratedOffset[i]);
    if (boundDistance < 0)
    {
      insideNeighborhood = false;
      break;
    }
  }
  return insideNeighborhood;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::ComputeFeatures(
  const vnl_matrix<unsigned int> &   hist,
  const unsigned int                 totalNumberOfFreq,
  typename TOutputImage::PixelType & outputPixel)
{
  // Now get the various means and variances. This is takes two passes
  // through the histogram.
  double pixelMean;
  double marginalMean;
  double marginalDevSquared;
  double pixelVariance;

  this->ComputeMeansAndVariances(hist, totalNumberOfFreq, pixelMean, marginalMean, marginalDevSquared, pixelVariance);

  // Finally compute the texture features. Another pass.
  MeasurementType energy = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType entropy = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType correlation = NumericTraits<MeasurementType>::ZeroValue();

  MeasurementType inverseDifferenceMoment = NumericTraits<MeasurementType>::ZeroValue();

  MeasurementType inertia = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType clusterShade = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType clusterProminence = NumericTraits<MeasurementType>::ZeroValue();
  MeasurementType haralickCorrelation = NumericTraits<MeasurementType>::ZeroValue();

  double pixelVarianceSquared = pixelVariance * pixelVariance;
  // Variance is only used in correlation. If variance is 0, then
  //   (index[0] - pixelMean) * (index[1] - pixelMean)
  // should be zero as well. In this case, set the variance to 1. in
  // order to avoid NaN correlation.
  if (Math::FloatAlmostEqual(pixelVarianceSquared, 0.0, 4, 2 * NumericTraits<double>::epsilon()))
  {
    pixelVarianceSquared = 1.;
  }
  const double log2 = std::log(2.0);

  for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; ++a)
  {
    for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; ++b)
    {
      float frequency = hist[a][b] / (float)totalNumberOfFreq;
      if (Math::AlmostEquals(frequency, NumericTraits<float>::ZeroValue()))
      {
        continue; // no use doing these calculations if we're just multiplying by
                  // zero.
      }

      energy += frequency * frequency;
      entropy -= (frequency > 0.0001) ? frequency * std::log(frequency) / log2 : 0;
      correlation += ((a - pixelMean) * (b - pixelMean) * frequency) / pixelVarianceSquared;
      inverseDifferenceMoment += frequency / (1.0 + (a - b) * (a - b));
      inertia += (a - b) * (a - b) * frequency;
      clusterShade += std::pow((a - pixelMean) + (b - pixelMean), 3) * frequency;
      clusterProminence += std::pow((a - pixelMean) + (b - pixelMean), 4) * frequency;
      haralickCorrelation += a * b * frequency;
    }
  }

  haralickCorrelation = (haralickCorrelation - marginalMean * marginalMean) / marginalDevSquared;

  outputPixel[0] = energy;
  outputPixel[1] = entropy;
  outputPixel[2] = correlation;
  outputPixel[3] = inverseDifferenceMoment;
  outputPixel[4] = inertia;
  outputPixel[5] = clusterShade;
  outputPixel[6] = clusterProminence;
  outputPixel[7] = haralickCorrelation;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::ComputeMeansAndVariances(
  const vnl_matrix<unsigned int> & hist,
  const unsigned int               totalNumberOfFreq,
  double &                         pixelMean,
  double &                         marginalMean,
  double &                         marginalDevSquared,
  double &                         pixelVariance)
{
  // This function takes two passes through the histogram and two passes through
  // an array of the same length as a histogram axis. This could probably be
  // cleverly compressed to one pass, but it's not clear that that's necessary.

  // Initialize everything
  auto * marginalSums = new double[m_NumberOfBinsPerAxis];

  for (double * ms_It = marginalSums; ms_It < marginalSums + m_NumberOfBinsPerAxis; ms_It++)
  {
    *ms_It = 0;
  }
  pixelMean = 0;

  // Ok, now do the first pass through the histogram to get the marginal sums
  // and compute the pixel mean
  for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; a++)
  {
    for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; b++)
    {
      float frequency = hist[a][b] / (float)totalNumberOfFreq;
      pixelMean += a * frequency;
      marginalSums[a] += frequency;
    }
  }

  /*  Now get the mean and deviaton of the marginal sums.
      Compute incremental mean and SD, a la Knuth, "The  Art of Computer
      Programming, Volume 2: Seminumerical Algorithms",  section 4.2.2.
      Compute mean and standard deviation using the recurrence relation:
      M(1) = x(1), M(k) = M(k-1) + (x(k) - M(k-1) ) / k
      S(1) = 0, S(k) = S(k-1) + (x(k) - M(k-1)) * (x(k) - M(k))
      for 2 <= k <= n, then
      sigma = std::sqrt(S(n) / n) (or divide by n-1 for sample SD instead of
      population SD).
  */
  marginalMean = marginalSums[0];
  marginalDevSquared = 0;
  for (unsigned int arrayIndex = 1; arrayIndex < m_NumberOfBinsPerAxis; arrayIndex++)
  {
    int    k = arrayIndex + 1;
    double M_k_minus_1 = marginalMean;
    double S_k_minus_1 = marginalDevSquared;
    double x_k = marginalSums[arrayIndex];

    double M_k = M_k_minus_1 + (x_k - M_k_minus_1) / k;
    double S_k = S_k_minus_1 + (x_k - M_k_minus_1) * (x_k - M_k);

    marginalMean = M_k;
    marginalDevSquared = S_k;
  }
  marginalDevSquared = marginalDevSquared / m_NumberOfBinsPerAxis;

  // OK, now compute the pixel variances.
  pixelVariance = 0;
  for (unsigned int a = 0; a < m_NumberOfBinsPerAxis; a++)
  {
    for (unsigned int b = 0; b < m_NumberOfBinsPerAxis; b++)
    {
      float frequency = hist[a][b] / (float)totalNumberOfFreq;
      pixelVariance += (a - pixelMean) * (a - pixelMean) * (frequency);
    }
  }

  delete[] marginalSums;
}

template <typename TInputImage, typename TOutputImage, typename TMaskImage>
void
CoocurrenceTextureFeaturesImageFilter<TInputImage, TOutputImage, TMaskImage>::PrintSelf(std::ostream & os,
                                                                                        Indent         indent) const
{

  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(DigitizedInputImage);

  os << indent << "NeighborhoodRadius: "
     << static_cast<typename NumericTraits<NeighborhoodRadiusType>::PrintType>(m_NeighborhoodRadius) << std::endl;

  itkPrintSelfObjectMacro(Offsets);

  os << indent << "NumberOfBinsPerAxis: " << m_NumberOfBinsPerAxis << std::endl;
  os << indent << "Min: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_HistogramMinimum) << std::endl;
  os << indent << "Max: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_HistogramMaximum) << std::endl;
  os << indent << "InsidePixelValue: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_InsidePixelValue)
     << std::endl;
  os << indent << "Normalize: " << m_Normalize << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
