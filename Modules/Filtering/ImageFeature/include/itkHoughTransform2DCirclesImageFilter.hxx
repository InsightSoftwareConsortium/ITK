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
#ifndef itkHoughTransform2DCirclesImageFilter_hxx
#define itkHoughTransform2DCirclesImageFilter_hxx

#include "itkHoughTransform2DCirclesImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkGaussianDerivativeImageFunction.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkMath.h"

namespace itk
{
template <typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType>
HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>::
  HoughTransform2DCirclesImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
}

template <typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType>
void
HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>::SetRadius(double radius)
{
  this->SetMinimumRadius(radius);
  this->SetMaximumRadius(radius);
}

template <typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType>
void
HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>::EnlargeOutputRequestedRegion(
  DataObject * output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType>
void
HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if (this->GetInput())
  {
    InputImagePointer image = const_cast<InputImageType *>(this->GetInput());
    image->SetRequestedRegionToLargestPossibleRegion();
  }
}


template <typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType>
void
HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>::VerifyPreconditions()
  ITKv5_CONST
{
  Superclass::VerifyPreconditions();

  if (!(m_GradientNormThreshold >= 0.0))
  {
    itkExceptionMacro("Failed precondition: GradientNormThreshold >= 0.");
  }
}


template <typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType>
void
HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>::GenerateData()
{
  // Get the input and output pointers
  const InputImageConstPointer inputImage = this->GetInput(0);
  const OutputImagePointer     outputImage = this->GetOutput(0);

  // Allocate the output
  this->AllocateOutputs();
  outputImage->FillBuffer(0);

  using DoGFunctionType = GaussianDerivativeImageFunction<InputImageType>;
  const auto DoGFunction = DoGFunctionType::New();
  DoGFunction->SetSigma(m_SigmaGradient);
  DoGFunction->SetUseImageSpacing(m_UseImageSpacing);
  // Set input image _after_ setting the other GaussianDerivative properties,
  // to avoid multiple kernel recomputation within GaussianDerivativeImageFunction.
  DoGFunction->SetInputImage(inputImage);

  m_RadiusImage = RadiusImageType::New();

  m_RadiusImage->SetRegions(outputImage->GetLargestPossibleRegion());
  m_RadiusImage->SetOrigin(inputImage->GetOrigin());
  m_RadiusImage->SetSpacing(inputImage->GetSpacing());
  m_RadiusImage->SetDirection(inputImage->GetDirection());
  m_RadiusImage->Allocate(true); // initialize buffer to zero

  ImageRegionConstIteratorWithIndex<InputImageType> image_it(inputImage, inputImage->GetRequestedRegion());
  image_it.GoToBegin();

  const ImageRegion<2> & region = outputImage->GetRequestedRegion();

  while (!image_it.IsAtEnd())
  {
    if (image_it.Get() > m_Threshold)
    {
      const Index<2>                             inputIndex = image_it.GetIndex();
      const typename DoGFunctionType::VectorType grad = DoGFunction->DoGFunctionType::EvaluateAtIndex(inputIndex);

      double Vx = grad[0];
      double Vy = grad[1];

      const double norm = std::sqrt(Vx * Vx + Vy * Vy);

      // if the gradient is not flat (using GradientNormThreshold to estimate flatness)
      if (norm > m_GradientNormThreshold)
      {
        Vx /= norm;
        Vy /= norm;

        for (double angle = -m_SweepAngle; angle <= m_SweepAngle; angle += 0.05)
        {
          double i = m_MinimumRadius;
          double distance;

          do
          {
            const Index<2> outputIndex = {
              { Math::Round<IndexValueType>(inputIndex[0] - i * (Vx * std::cos(angle) + Vy * std::sin(angle))),
                Math::Round<IndexValueType>(inputIndex[1] - i * (Vx * std::sin(angle) + Vy * std::cos(angle))) }
            };

            if (region.IsInside(outputIndex))
            {
              distance =
                std::sqrt(static_cast<double>((outputIndex[0] - inputIndex[0]) * (outputIndex[0] - inputIndex[0]) +
                                              (outputIndex[1] - inputIndex[1]) * (outputIndex[1] - inputIndex[1])));

              ++outputImage->GetPixel(outputIndex);
              m_RadiusImage->GetPixel(outputIndex) += distance;
            }
            else
            {
              break;
            }
            ++i;
          } while (distance < m_MaximumRadius);
        }
      }
    }
    ++image_it;
  }

  // Compute the average radius
  ImageRegionConstIterator<OutputImageType> output_it(outputImage, outputImage->GetLargestPossibleRegion());
  ImageRegionIterator<RadiusImageType>      radius_it(m_RadiusImage, m_RadiusImage->GetLargestPossibleRegion());
  output_it.GoToBegin();
  radius_it.GoToBegin();
  while (!output_it.IsAtEnd())
  {
    if (output_it.Get() > 1)
    {
      radius_it.Value() /= output_it.Get();
    }
    ++output_it;
    ++radius_it;
  }
}

template <typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType>
typename HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>::CirclesListType &
HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>::GetCircles()
{
  // Make sure that all the required inputs exist and have a non-null value
  this->VerifyPreconditions();

  if (this->GetMTime() == m_OldModifiedTime)
  {
    // If the filter has not been updated
    return m_CirclesList;
  }

  if (m_RadiusImage.IsNull())
  {
    itkExceptionMacro(<< "Update() must be called before GetCircles().");
  }

  m_CirclesList.clear();

  if (m_NumberOfCircles > 0)
  {
    // Blur the accumulator in order to find the maximum
    using InternalImageType = Image<float, 2>;

    // The variable "outputImage" is only used as input to gaussianFilter.
    // It should not be modified, because GetOutput(0) should not be changed.
    const auto outputImage = OutputImageType::New();
    outputImage->Graft(this->GetOutput(0));

    const auto gaussianFilter = DiscreteGaussianImageFilter<OutputImageType, InternalImageType>::New();

    gaussianFilter->SetInput(outputImage); // The output is the accumulator image
    gaussianFilter->SetVariance(m_Variance);
    gaussianFilter->SetUseImageSpacing(m_UseImageSpacing);

    gaussianFilter->Update();
    const InternalImageType::Pointer postProcessImage = gaussianFilter->GetOutput();

    const auto                             minMaxCalculator = MinimumMaximumImageCalculator<InternalImageType>::New();
    ImageRegionIterator<InternalImageType> it_input(postProcessImage, postProcessImage->GetLargestPossibleRegion());

    CirclesListSizeType circles = 0;

    // Find maxima
    // Break out of "forever loop" as soon as the requested number of circles is found.
    for (;;)
    {
      minMaxCalculator->SetImage(postProcessImage);
      minMaxCalculator->ComputeMaximum();

      if (minMaxCalculator->GetMaximum() <= 0)
      {
        // When all pixel values in 'postProcessImage' are zero or less, no more circles
        // should be found. Note that a zero in 'postProcessImage' might correspond to a
        // zero in the accumulator image, or it might be that the pixel is within a
        // removed disc around a previously found circle center.
        break;
      }

      const InternalImageType::IndexType indexOfMaximum = minMaxCalculator->GetIndexOfMaximum();

      // Create a Circle Spatial Object
      const auto Circle = CircleType::New();
      Circle->SetId(static_cast<int>(circles));
      Circle->SetRadiusInObjectSpace(m_RadiusImage->GetPixel(indexOfMaximum));

      CircleType::PointType center;
      center[0] = indexOfMaximum[0];
      center[1] = indexOfMaximum[1];
      Circle->SetCenterInObjectSpace(center);
      Circle->Update();

      m_CirclesList.push_back(Circle);

      circles++;
      if (circles >= m_NumberOfCircles)
      {
        break;
      }

      // Remove a black disc from the Hough space domain
      for (double angle = 0; angle <= 2 * itk::Math::pi; angle += itk::Math::pi / 1000)
      {
        for (double length = 0; length < m_DiscRadiusRatio * Circle->GetRadiusInObjectSpace()[0]; length += 1)
        {
          const Index<2> index = { { Math::Round<IndexValueType>(indexOfMaximum[0] + length * std::cos(angle)),
                                     Math::Round<IndexValueType>(indexOfMaximum[1] + length * std::sin(angle)) } };

          if (postProcessImage->GetLargestPossibleRegion().IsInside(index))
          {
            postProcessImage->SetPixel(index, 0);
          }
        }
      }
    }
  }

  m_OldModifiedTime = this->GetMTime();
  return m_CirclesList;
}

template <typename TInputPixelType, typename TOutputPixelType, typename TRadiusPixelType>
void
HoughTransform2DCirclesImageFilter<TInputPixelType, TOutputPixelType, TRadiusPixelType>::PrintSelf(std::ostream & os,
                                                                                                   Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "Gradient Norm Threshold: " << m_GradientNormThreshold << std::endl;
  os << indent << "Minimum Radius:  " << m_MinimumRadius << std::endl;
  os << indent << "Maximum Radius: " << m_MaximumRadius << std::endl;
  os << indent << "Derivative Scale : " << m_SigmaGradient << std::endl;
  os << indent << "Number Of Circles: " << m_NumberOfCircles << std::endl;
  os << indent << "Disc Radius Ratio: " << m_DiscRadiusRatio << std::endl;
  os << indent << "Accumulator blur variance: " << m_Variance << std::endl;
  os << indent << "Sweep angle : " << m_SweepAngle << std::endl;
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;

  itkPrintSelfObjectMacro(RadiusImage);

  os << indent << "CirclesList: " << std::endl;
  unsigned int i = 0;
  auto         it = m_CirclesList.begin();
  while (it != m_CirclesList.end())
  {
    os << indent << "[" << i << "]: " << *it << std::endl;
    ++it;
    ++i;
  }

  os << indent << "OldModifiedTime: " << NumericTraits<ModifiedTimeType>::PrintType(m_OldModifiedTime) << std::endl;
}
} // namespace itk

#endif
