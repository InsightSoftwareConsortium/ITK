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
#ifndef itkScalarImageToRunLengthFeaturesImageFilter_hxx
#define itkScalarImageToRunLengthFeaturesImageFilter_hxx

#include "itkScalarImageToRunLengthFeaturesImageFilter.h"
#include "itkNeighborhood.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkNeighborhoodAlgorithm.h"
#include <stdio.h>

namespace itk
{
namespace Statistics
{
template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  ScalarImageToRunLengthFeaturesImageFilter()
  : m_NumberOfBinsPerAxis(itkGetStaticConstMacro(DefaultBinsPerAxis))
  , m_Min(NumericTraits<PixelType>::NonpositiveMin())
  , m_Max(NumericTraits<PixelType>::max())
  , m_MinDistance(NumericTraits<RealType>::ZeroValue())
  , m_MaxDistance(NumericTraits<RealType>::max())
  , m_InsidePixelValue(NumericTraits<PixelType>::OneValue())
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  for (int i = 1; i < 2; ++i)
  {
    this->ProcessObject::SetNthOutput(i, this->MakeOutput(i));
  }

  // Set the requested features to the default value:
  // {Energy, Entropy, InverseDifferenceMoment, Inertia, ClusterShade,
  // ClusterProminence}
  typename FeatureNameVector::Pointer requestedFeatures = FeatureNameVector::New();
  // can't directly set this->m_RequestedFeatures since it is const!

  requestedFeatures->push_back(RunLengthFeaturesFilterType::ShortRunEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::LongRunEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::GreyLevelNonuniformity);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::RunLengthNonuniformity);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::LowGreyLevelRunEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::HighGreyLevelRunEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::ShortRunLowGreyLevelEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::ShortRunHighGreyLevelEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::LongRunLowGreyLevelEmphasis);
  requestedFeatures->push_back(RunLengthFeaturesFilterType::LongRunHighGreyLevelEmphasis);

  this->SetRequestedFeatures(requestedFeatures);

  // Set the offset directions to their defaults: half of all the possible
  // directions 1 pixel away. (The other half is included by symmetry.)
  // We use a neighborhood iterator to calculate the appropriate offsets.
  typedef Neighborhood<typename InputImageType::PixelType, InputImageType::ImageDimension> NeighborhoodType;
  NeighborhoodType                                                                         hood;
  hood.SetRadius(1);

  // select all "previous" neighbors that are face+edge+vertex
  // connected to the iterated pixel. do not include the curentInNeighborhood pixel.
  unsigned int        centerIndex = hood.GetCenterNeighborhoodIndex();
  OffsetVectorPointer offsets = OffsetVector::New();
  for (unsigned int d = 0; d < centerIndex; d++)
  {
    OffsetType offset = hood.GetOffset(d);
    offsets->push_back(offset);
  }
  this->SetOffsets(offsets);
  this->m_FastCalculations = false;
  NeighborhoodType Nhood;
  Nhood.SetRadius(2);

  const unsigned int measurementVectorSize = 2;

  this->m_NeighborhoodRadius = Nhood.GetRadius();

  this->m_LowerBound.SetSize(measurementVectorSize);
  this->m_UpperBound.SetSize(measurementVectorSize);

  this->m_LowerBound[0] = this->m_Min;
  this->m_LowerBound[1] = this->m_MinDistance;
  this->m_UpperBound[0] = this->m_Max;
  this->m_UpperBound[1] = this->m_MaxDistance;

  TOutputImage * outputPtr = this->GetOutput();
  outputPtr->FillBuffer(0);
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  BeforeThreadedGenerateData()
{
  this->m_LowerBound[0] = this->m_Min;
  this->m_LowerBound[1] = this->m_MinDistance;
  this->m_UpperBound[0] = this->m_Max;
  this->m_UpperBound[1] = this->m_MaxDistance;
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  ThreadedGenerateData(const OutputRegionType & outputRegionForThread, ThreadIdType threadId)
{
  // Recuperation of the different inputs/outputs
  typename TInputImage::Pointer inputPtr = TInputImage::New();
  inputPtr = const_cast<TInputImage *>(this->GetInput());
  typename TInputImage::Pointer maskPointer = TInputImage::New();
  maskPointer = const_cast<TInputImage *>(this->GetMaskImage());
  typename TOutputImage::Pointer outputPtr = TOutputImage::New();
  outputPtr = this->GetOutput();

  // Creation of the filter that will compute the Run Lenght features once the histogrham computed
  typename RunLengthFeaturesFilterType::Pointer runLengthMatrixCalculator = RunLengthFeaturesFilterType::New();
  typedef typename RunLengthFeaturesFilterType::RunLengthFeatureName InternalRunLengthFeatureName;
  typename FeatureNameVector::ConstIterator                          fnameIt;
  fnameIt = this->m_RequestedFeatures->Begin();
  fnameIt++;
  fnameIt++;
  fnameIt++;

  // Creation of a region with the sqme size than the neighborhood that
  // will be used to check if each voxel has already been visited
  InputRegionType                                  boolRegion;
  typename InputRegionType::IndexType              boolStart;
  typename InputRegionType::SizeType               boolSize;
  IndexType                                        boolCurentInNeighborhoodIndex;
  typedef Image<bool, TInputImage::ImageDimension> BoolImageType;
  typename BoolImageType::Pointer                  alreadyVisitedImage = BoolImageType::New();
  for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; i++)
  {
    boolSize[i] = this->m_NeighborhoodRadius[i] * 2 + 1;
    boolStart[i] = 0;
    boolCurentInNeighborhoodIndex[i] = m_NeighborhoodRadius[i];
  }
  boolRegion.SetIndex(boolStart);
  boolRegion.SetSize(boolSize);
  alreadyVisitedImage->CopyInformation(inputPtr);
  alreadyVisitedImage->SetRegions(boolRegion);
  alreadyVisitedImage->Allocate();

  // Creation of the histogram that will be fill and used to compute the features
  const unsigned int                measurementVectorSize = 2;
  typename HistogramType::IndexType hIndex;
  typename HistogramType::Pointer   hist = HistogramType::New();
  hist->SetMeasurementVectorSize(measurementVectorSize);
  typename HistogramType::SizeType size(2);
  size.Fill(this->m_NumberOfBinsPerAxis);
  MeasurementVectorType run(hist->GetMeasurementVectorSize());
  hist->Initialize(size, this->m_LowerBound, this->m_UpperBound);
  // Separation of the non-boundery region that will be processed in a different way
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<TInputImage>                           BoundaryFacesCalculator;
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList =
    BoundaryFacesCalculator(inputPtr, outputRegionForThread, m_NeighborhoodRadius);
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType::iterator fit =
    faceList.begin();

  /// ***** Non-boundary Region *****
  NeighborhoodIteratorType                          inputNIt(m_NeighborhoodRadius, inputPtr, *fit);
  typedef itk::ImageRegionIterator<OutputImageType> IteratorType;
  IteratorType                                      outputIt(outputPtr, *fit);

  // Iteration over the all image region
  while (!inputNIt.IsAtEnd())
  {
    if ((maskPointer && maskPointer->GetPixel(inputNIt.GetIndex()) != this->m_InsidePixelValue))
    {
      ++inputNIt;
      ++outputIt;
      continue;
    }
    hist->SetToZero();
    hist->Modified();
    // Iteration over all the offsets
    typename OffsetVector::ConstIterator offsets;
    for (offsets = m_Offsets->Begin(); offsets != m_Offsets->End(); ++offsets)
    {
      alreadyVisitedImage->FillBuffer(false);
      OffsetType offset = offsets.Value();
      this->NormalizeOffsetDirection(offset);

      // Iteration over the all neighborhood
      for (NeighborIndexType nb = 0; nb < inputNIt.Size(); ++nb)
      {
        IndexType       curentInNeighborhoodIndex = inputNIt.GetIndex(nb);
        const PixelType curentInNeighborhoodPixelIntensity = inputNIt.GetPixel(nb);
        // Cecking if the value is out-of-bounds or is outside the mask.
        if (curentInNeighborhoodPixelIntensity < this->m_Min || curentInNeighborhoodPixelIntensity > this->m_Max ||
            alreadyVisitedImage->GetPixel(boolCurentInNeighborhoodIndex + inputNIt.GetOffset(nb)) ||
            (maskPointer && maskPointer->GetPixel(curentInNeighborhoodIndex) != this->m_InsidePixelValue))
        {
          continue;
        }

        MeasurementType curentInNeighborhoodBinMin = hist->GetBinMinFromValue(0, curentInNeighborhoodPixelIntensity);
        MeasurementType curentInNeighborhoodBinMax = hist->GetBinMaxFromValue(0, curentInNeighborhoodPixelIntensity);
        MeasurementType lastBinMax = hist->GetDimensionMaxs(0)[hist->GetSize(0) - 1];

        PixelType  pixelIntensity(NumericTraits<PixelType>::ZeroValue());
        OffsetType iteratedOffset = inputNIt.GetOffset(nb) + offset;
        IndexType  lastGoodIndex = curentInNeighborhoodIndex;
        bool       runLengthSegmentAlreadyVisited = false;
        bool       insideNeighborhood = true;
        for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; ++i)
        {
          int boundDistance = m_NeighborhoodRadius[i] - std::abs(iteratedOffset[i]);
          if (boundDistance < 0)
          {
            insideNeighborhood = false;
            break;
          }
        }

        // Scan from the iterated pixel at index, following the direction of
        // offset. Run length is computed as the length of continuous pixel
        // whose pixel values are in the same bin.
        while (insideNeighborhood)
        {
          // For the same offset, each run length segment can only be visited once
          if (alreadyVisitedImage->GetPixel(boolCurentInNeighborhoodIndex + iteratedOffset))
          {
            runLengthSegmentAlreadyVisited = true;
            break;
          }
          pixelIntensity = inputNIt.GetPixel(iteratedOffset);

          // Special attention paid to boundaries of bins.
          // For the last bin, it is left close and right close (following the previous
          // gerrit patch). For all other bins, the bin is left close and right open.
          if (pixelIntensity >= curentInNeighborhoodBinMin &&
              (pixelIntensity < curentInNeighborhoodBinMax ||
               (Math::ExactlyEquals(pixelIntensity, curentInNeighborhoodBinMax) &&
                Math::ExactlyEquals(curentInNeighborhoodBinMax, lastBinMax))))
          {
            alreadyVisitedImage->SetPixel(boolCurentInNeighborhoodIndex + iteratedOffset, true);
            lastGoodIndex = inputNIt.GetIndex(iteratedOffset);
            iteratedOffset += offset;
            for (unsigned int i = 0; i < this->m_NeighborhoodRadius.Dimension; ++i)
            {
              int boundDistance = m_NeighborhoodRadius[i] - std::abs(iteratedOffset[i]);
              if (boundDistance < 0)
              {
                insideNeighborhood = false;
                break;
              }
            }
          }
          else
          {
            break;
          }
        }
        if (runLengthSegmentAlreadyVisited)
        {
          continue;
        }

        PointType curentInNeighborhoodPoint;
        inputPtr->TransformIndexToPhysicalPoint(curentInNeighborhoodIndex, curentInNeighborhoodPoint);
        PointType point;
        inputPtr->TransformIndexToPhysicalPoint(lastGoodIndex, point);

        run[0] = curentInNeighborhoodPixelIntensity;
        run[1] = curentInNeighborhoodPoint.EuclideanDistanceTo(point);
        if (run[1] >= this->m_MinDistance && run[1] <= this->m_MaxDistance)
        {
          hist->GetIndex(run, hIndex);
          hist->IncreaseFrequencyOfIndex(hIndex, 1);
        }
      }
    }

    runLengthMatrixCalculator->SetInput(hist);
    runLengthMatrixCalculator->Update();
    outputIt.Set(runLengthMatrixCalculator->GetFeature((InternalRunLengthFeatureName)fnameIt.Value()));

    ++inputNIt;
    ++outputIt;
  }

  /// ***** With-boundary Regions *****
  fit++;
  for (fit; fit != faceList.end(); ++fit)
  {
    //      NeighborhoodIteratorType boundaryInputNIt(m_NeighborhoodRadius, inputPtr, *fit );
    //      typedef itk::ImageRegionIterator< OutputImageType> IteratorType;
    //      IteratorType boundaryOutputIt( outputPtr, *fit );

    //      // Iteration over the all image region
    //      while( !boundaryInputNIt.IsAtEnd() )
    //        {
    //        boundaryOutputIt.Set( 0 );
    //        ++boundaryInputNIt;
    //        ++boundaryOutputIt;
    //        }
  }
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::SetMaskImage(
  const InputImageType * image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1, const_cast<InputImageType *>(image));
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
const TInputImage *
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::GetMaskImage() const
{
  if (this->GetNumberOfInputs() < 2)
  {
    return ITK_NULLPTR;
  }
  return static_cast<const InputImageType *>(this->ProcessObject::GetInput(1));
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::SetPixelValueMinMax(
  PixelType min,
  PixelType max)
{
  if (this->m_Min != min || this->m_Max != max)
  {
    this->m_Min = min;
    this->m_Max = max;
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  SetDistanceValueMinMax(RealType min, RealType max)
{
  if (Math::NotExactlyEquals(this->m_MinDistance, min) || Math::NotExactlyEquals(this->m_MaxDistance, max))
  {
    this->m_MinDistance = min;
    this->m_MaxDistance = max;
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::
  NormalizeOffsetDirection(OffsetType & offset)
{
  itkDebugMacro("old offset = " << offset << std::endl);
  int  sign = 1;
  bool metLastNonZero = false;
  for (int i = offset.GetOffsetDimension() - 1; i >= 0; i--)
  {
    if (metLastNonZero)
    {
      offset[i] *= sign;
    }
    else if (offset[i] != 0)
    {
      sign = (offset[i] > 0) ? 1 : -1;
      metLastNonZero = true;
      offset[i] *= sign;
    }
  }

  itkDebugMacro("new  offset = " << offset << std::endl);
}

template <typename TInputImage, typename TOutputImage, typename THistogramFrequencyContainer>
void
ScalarImageToRunLengthFeaturesImageFilter<TInputImage, TOutputImage, THistogramFrequencyContainer>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "RequestedFeatures: " << this->GetRequestedFeatures() << std::endl;
  os << indent << "FastCalculations: " << this->GetFastCalculations() << std::endl;
  os << indent << "Offsets: " << this->GetOffsets() << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
