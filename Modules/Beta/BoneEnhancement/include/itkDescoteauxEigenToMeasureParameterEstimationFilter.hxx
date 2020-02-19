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

#ifndef itkDescoteauxEigenToMeasureParameterEstimationFilter_hxx
#define itkDescoteauxEigenToMeasureParameterEstimationFilter_hxx

#include "itkDescoteauxEigenToMeasureParameterEstimationFilter.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
DescoteauxEigenToMeasureParameterEstimationFilter<TInputImage,
                                                  TOutputImage>::DescoteauxEigenToMeasureParameterEstimationFilter()
  : Superclass()
  , m_FrobeniusNormWeight(0.5)
{
  /* Set parameter size to 3 */
  ParameterArrayType parameters = this->GetParametersOutput()->Get();
  parameters.SetSize(3);
  parameters[0] = 0.5;
  parameters[1] = 0.5;
  parameters[2] = 1;
  this->GetParametersOutput()->Set(parameters);
}

template <typename TInputImage, typename TOutputImage>
void
DescoteauxEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  m_MaxFrobeniusNorm = NumericTraits<RealType>::NonpositiveMin();
}

template <typename TInputImage, typename TOutputImage>
void
DescoteauxEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::AfterThreadedGenerateData()
{
  /* Determine default parameters */
  RealType alpha, beta, c;
  alpha = 0.5f;
  beta = 0.5f;
  c = 0.0f;

  /* Scale c */
  if (m_MaxFrobeniusNorm > 0)
  {
    c = m_FrobeniusNormWeight * m_MaxFrobeniusNorm;
  }

  /* Assign outputs parameters */
  ParameterArrayType parameters;
  parameters.SetSize(3);
  parameters[0] = alpha;
  parameters[1] = beta;
  parameters[2] = c;
  this->GetParametersOutput()->Set(parameters);
}

template <typename TInputImage, typename TOutputImage>
void
DescoteauxEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  /* If size is zero, return */
  const SizeValueType size0 = outputRegionForThread.GetSize(0);
  if (size0 == 0)
  {
    return;
  }

  /* Get input and mask pointer */
  InputImageConstPointer            inputPointer = this->GetInput();
  MaskSpatialObjectTypeConstPointer maskPointer = this->GetMask();

  OutputImageType * outputPtr = this->GetOutput(0);

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  MultiThreaderBase::Pointer mt = this->GetMultiThreader();

  mt->ParallelizeImageRegion<TInputImage::ImageDimension>(
    outputRegionForThread,
    [inputPointer, maskPointer, outputPtr, this](const OutputImageRegionType region) {
      /* Keep track of the current max */
      RealType max = NumericTraits<RealType>::NonpositiveMin();

      typename InputImageType::PointType point;

      /* Setup iterator */
      ImageRegionConstIteratorWithIndex<TInputImage> inputIt(inputPointer, region);
      ImageRegionIterator<OutputImageType>           outputIt(outputPtr, region);

      /* Iterate and count */
      while (!inputIt.IsAtEnd())
      {
        // Process point
        inputPointer->TransformIndexToPhysicalPoint(inputIt.GetIndex(), point);
        if ((!maskPointer) || (maskPointer->IsInsideInObjectSpace(point)))
        {
          /* Compute max norm */
          max = std::max(max, this->CalculateFrobeniusNorm(inputIt.Get()));
        }

        // Set
        outputIt.Set(static_cast<OutputImagePixelType>(inputIt.Get()));

        // Increment
        ++inputIt;
        ++outputIt;
      }


      /* Block and store */
      std::lock_guard<std::mutex> mutexHolder(m_Mutex);
      m_MaxFrobeniusNorm = std::max(m_MaxFrobeniusNorm, max);
    },
    nullptr);
}

template <typename TInputImage, typename TOutputImage>
typename DescoteauxEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::RealType
DescoteauxEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::CalculateFrobeniusNorm(
  const InputImagePixelType & pixel) const
{
  /* Forbenius norm is given by the square root of the sum of squares
   * of the eigenvalues for real, symmetric matricies
   */
  RealType norm = 0;
  for (unsigned int i = 0; i < pixel.Length; ++i)
  {
    norm += pixel[i] * pixel[i];
  }
  return sqrt(norm);
}

template <typename TInputImage, typename TOutputImage>
void
DescoteauxEigenToMeasureParameterEstimationFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os,
                                                                                        Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "FrobeniusNormWeight: " << GetFrobeniusNormWeight() << std::endl;
}

} // namespace itk

#endif /* itkDescoteauxEigenToMeasureParameterEstimationFilter_hxx */
