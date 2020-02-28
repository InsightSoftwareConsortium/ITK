/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkTransformToDisplacementFieldFilter_hxx
#define itkTransformToDisplacementFieldFilter_hxx

#include "itkTransformToDisplacementFieldFilter.h"

#include "itkIdentityTransform.h"
#include "itkTotalProgressReporter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageScanlineIterator.h"

namespace itk
{

template <typename TOutputImage, typename TParametersValueType>
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::TransformToDisplacementFieldFilter()

{
  this->m_OutputSpacing.Fill(1.0);
  this->m_OutputOrigin.Fill(0.0);
  this->m_OutputDirection.SetIdentity();

  this->m_Size.Fill(0);
  this->m_OutputStartIndex.Fill(0);

  this->SetNumberOfRequiredInputs(1);
  this->SetPrimaryInputName("Transform");

  //  #1 "ReferenceImage" optional
  Self::AddOptionalInputName("ReferenceImage", 1);
  this->DynamicMultiThreadingOn();
}


template <typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::PrintSelf(std::ostream & os,
                                                                                  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Size: " << this->m_Size << std::endl;
  os << indent << "OutputStartIndex: " << this->m_OutputStartIndex << std::endl;
  os << indent << "OutputSpacing: " << this->m_OutputSpacing << std::endl;
  os << indent << "OutputOrigin: " << this->m_OutputOrigin << std::endl;
  os << indent << "OutputDirection: " << this->m_OutputDirection << std::endl;
  os << indent << "UseReferenceImage: ";
  if (this->m_UseReferenceImage)
  {
    os << "On" << std::endl;
  }
  else
  {
    os << "Off" << std::endl;
  }
}


template <typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::SetOutputSpacing(
  const SpacePrecisionType * spacing)
{
  this->SetOutputSpacing(SpacingType(spacing));
}


template <typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::SetOutputOrigin(
  const SpacePrecisionType * origin)
{
  this->SetOutputOrigin(OriginType(origin));
}

template <typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::SetInput(const TransformInputType * input)
{
  if (input != itkDynamicCastInDebugMode<TransformInputType *>(this->ProcessObject::GetPrimaryInput()))
  {
    // Process object is not const-correct so the const_cast is required here
    this->ProcessObject::SetNthInput(0, const_cast<TransformInputType *>(input));
    this->Modified();
  }
}

template <typename TOutputImage, typename TParametersValueType>
const typename TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::TransformInputType *
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::GetInput() const
{
  return itkDynamicCastInDebugMode<const TransformInputType *>(this->GetPrimaryInput());
}


template <typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::GenerateOutputInformation()
{
  OutputImageType * output = this->GetOutput();
  if (!output)
  {
    return;
  }

  const ReferenceImageBaseType * referenceImage = this->GetReferenceImage();

  // Set the size of the output region
  if (m_UseReferenceImage && referenceImage)
  {
    output->SetLargestPossibleRegion(referenceImage->GetLargestPossibleRegion());
  }
  else
  {
    typename TOutputImage::RegionType outputLargestPossibleRegion;
    outputLargestPossibleRegion.SetSize(m_Size);
    outputLargestPossibleRegion.SetIndex(m_OutputStartIndex);
    output->SetLargestPossibleRegion(outputLargestPossibleRegion);
  }

  // Set spacing and origin
  if (m_UseReferenceImage && referenceImage)
  {
    output->SetSpacing(referenceImage->GetSpacing());
    output->SetOrigin(referenceImage->GetOrigin());
    output->SetDirection(referenceImage->GetDirection());
  }
  else
  {
    output->SetSpacing(m_OutputSpacing);
    output->SetOrigin(m_OutputOrigin);
    output->SetDirection(m_OutputDirection);
  }
}


template <typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  const TransformType * transform = this->GetInput()->Get();
  // Check whether we can use a fast path for resampling. Fast path
  // can be used if the transformation is linear. Transform respond
  // to the IsLinear() call.
  if (transform->IsLinear())
  {
    this->LinearThreadedGenerateData(outputRegionForThread);
    return;
  }

  // Otherwise, we use the normal method where the transform is called
  // for computing the transformation of every point.
  this->NonlinearThreadedGenerateData(outputRegionForThread);
}


template <typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::NonlinearThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  // Get the output pointer
  OutputImageType *     output = this->GetOutput();
  const TransformType * transform = this->GetInput()->Get();

  // Create an iterator that will walk the output region for this thread.
  using OutputIteratorType = ImageScanlineIterator<TOutputImage>;
  OutputIteratorType outIt(output, outputRegionForThread);

  // Define a few variables that will be used to translate from an input pixel
  // to an output pixel
  PointType outputPoint;      // Coordinates of output pixel
  PointType transformedPoint; // Coordinates of transformed pixel
  PixelType displacement;     // the difference


  TotalProgressReporter progress(this, output->GetRequestedRegion().GetNumberOfPixels());

  // Walk the output region
  outIt.GoToBegin();
  while (!outIt.IsAtEnd())
  {
    while (!outIt.IsAtEndOfLine())
    {
      // Determine the index of the current output pixel
      output->TransformIndexToPhysicalPoint(outIt.GetIndex(), outputPoint);

      // Compute corresponding input pixel position
      transformedPoint = transform->TransformPoint(outputPoint);

      displacement = transformedPoint - outputPoint;
      outIt.Set(displacement);
      ++outIt;
    }
    outIt.NextLine();
    progress.Completed(outputRegionForThread.GetSize()[0]);
  }
}


template <typename TOutputImage, typename TParametersValueType>
void
TransformToDisplacementFieldFilter<TOutputImage, TParametersValueType>::LinearThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  // Get the output pointer
  OutputImageType *     outputPtr = this->GetOutput();
  const TransformType * transformPtr = this->GetInput()->Get();

  const OutputImageRegionType & largestPossibleRegion = outputPtr->GetLargestPossibleRegion();

  // Create an iterator that will walk the output region for this thread.
  using OutputIteratorType = ImageScanlineIterator<TOutputImage>;
  OutputIteratorType outIt(outputPtr, outputRegionForThread);

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  PointType outputPoint; // Coordinates of current output pixel
  PointType inputPoint;


  // loop over the vector image
  while (!outIt.IsAtEnd())
  {

    // Compare with the ResampleImageFilter
    // The region may be split along the fast scan-line direction, so
    // the computation is done for the beginning and end of the largest
    // possible region to improve consistent numerics.
    IndexType index = outIt.GetIndex();
    index[0] = largestPossibleRegion.GetIndex(0);

    outputPtr->TransformIndexToPhysicalPoint(index, outputPoint);
    inputPoint = transformPtr->TransformPoint(outputPoint);
    const typename PointType::VectorType startDisplacement = inputPoint - outputPoint;

    index[0] += largestPossibleRegion.GetSize(0);
    outputPtr->TransformIndexToPhysicalPoint(index, outputPoint);
    inputPoint = transformPtr->TransformPoint(outputPoint);
    const typename PointType::VectorType endDisplacement = inputPoint - outputPoint;

    IndexValueType scanlineIndex = outIt.GetIndex()[0];

    while (!outIt.IsAtEndOfLine())
    {
      // Perform linear interpolation between startIndex and endIndex
      const double alpha =
        (scanlineIndex - largestPossibleRegion.GetIndex(0)) / double(largestPossibleRegion.GetSize(0));
      const double oneMinusAlpha = 1.0 - alpha;

      PixelType displacement;
      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        displacement[i] = oneMinusAlpha * startDisplacement[i] + alpha * endDisplacement[i];
      }


      outIt.Set(displacement);
      ++outIt;
      ++scanlineIndex;
    }

    outIt.NextLine();
  }
}

} // end namespace itk

#endif
