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
#ifndef itkResampleImageFilter_hxx
#define itkResampleImageFilter_hxx

#include "itkResampleImageFilter.h"
#include "itkObjectFactory.h"
#include "itkIdentityTransform.h"
#include "itkTotalProgressReporter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageScanlineIterator.h"
#include "itkSpecialCoordinatesImage.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkImageAlgorithm.h"

#include <type_traits> // For is_same.

namespace itk
{

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  ResampleImageFilter()
  : m_Extrapolator(nullptr)
  , m_OutputSpacing(1.0)
  , m_OutputOrigin(0.0)

{

  m_Size.Fill(0);
  m_OutputStartIndex.Fill(0);

  m_OutputDirection.SetIdentity();

  // Pipeline input configuration

  // implicit:
  // #0 "Primary" required

  // #1 "ReferenceImage" optional
  Self::AddRequiredInputName("ReferenceImage", 1);
  Self::RemoveRequiredInputName("ReferenceImage");

  // "Transform" required ( not numbered )
  Self::AddRequiredInputName("Transform");
  this->InitializeTransform();

  m_Interpolator = dynamic_cast<InterpolatorType *>(LinearInterpolatorType::New().GetPointer());

  m_DefaultPixelValue = NumericTraits<PixelType>::ZeroValue(m_DefaultPixelValue);
  this->DynamicMultiThreadingOn();
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  InitializeTransform()
{
  using IdentityTransformType =
    Transform<TTransformPrecisionType, Self::OutputImageDimension, Self::OutputImageDimension>;
  typename IdentityTransformType::Pointer defaultTransform =
    IdentityTransform<TTransformPrecisionType, OutputImageDimension>::New();
  if (InputImageDimension == OutputImageDimension)
  {
    using DecoratorType = DataObjectDecorator<IdentityTransformType>;
    typename DecoratorType::Pointer decoratedInput = DecoratorType::New();
    decoratedInput->Set(defaultTransform);
    this->ProcessObject::SetInput(
      "Transform", const_cast<DataObjectDecorator<IdentityTransformType> *>(decoratedInput.GetPointer()));
  }
  else
  {
    // Initialize with rectangular identity?
  }
  this->Modified();
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  VerifyPreconditions() ITKv5_CONST
{
  this->Superclass::VerifyPreconditions();
  const ReferenceImageBaseType * const referenceImage = this->GetReferenceImage();
  if (this->m_Size[0] == 0 && referenceImage && !m_UseReferenceImage)
  {
    itkExceptionMacro("Output image size is zero in all dimensions.  Consider using SetUseReferenceImageOn()."
                      "to define the resample output from the ReferenceImage.");
  }
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::SetOutputSpacing(
  const double * spacing)
{
  SpacingType s;
  for (unsigned int i = 0; i < TOutputImage::ImageDimension; ++i)
  {
    s[i] = static_cast<typename SpacingType::ValueType>(spacing[i]);
  }
  this->SetOutputSpacing(s);
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::SetOutputOrigin(
  const double * origin)
{
  this->SetOutputOrigin(OriginPointType(origin));
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  SetOutputParametersFromImage(const ImageBaseType * image)
{
  this->SetOutputOrigin(image->GetOrigin());
  this->SetOutputSpacing(image->GetSpacing());
  this->SetOutputDirection(image->GetDirection());
  this->SetOutputStartIndex(image->GetLargestPossibleRegion().GetIndex());
  this->SetSize(image->GetLargestPossibleRegion().GetSize());
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  BeforeThreadedGenerateData()
{
  m_Interpolator->SetInputImage(this->GetInput());

  // Connect input image to extrapolator
  if (!m_Extrapolator.IsNull())
  {
    m_Extrapolator->SetInputImage(this->GetInput());
  }

  unsigned int nComponents = DefaultConvertPixelTraits<PixelType>::GetNumberOfComponents(m_DefaultPixelValue);

  if (nComponents == 0)
  {
    PixelComponentType zeroComponent = NumericTraits<PixelComponentType>::ZeroValue(zeroComponent);
    nComponents = this->GetInput()->GetNumberOfComponentsPerPixel();
    NumericTraits<PixelType>::SetLength(m_DefaultPixelValue, nComponents);
    for (unsigned int n = 0; n < nComponents; n++)
    {
      PixelConvertType::SetNthComponent(n, m_DefaultPixelValue, zeroComponent);
    }
  }
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  AfterThreadedGenerateData()
{
  // Disconnect input image from the interpolator
  m_Interpolator->SetInputImage(nullptr);
  if (!m_Extrapolator.IsNull())
  {
    // Disconnect input image from the extrapolator
    m_Extrapolator->SetInputImage(nullptr);
  }
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread)
{
  // Check whether the input or the output is a
  // SpecialCoordinatesImage. If either are, then we cannot use the
  // fast path since index mapping will definitely not be linear.
  using OutputSpecialCoordinatesImageType = SpecialCoordinatesImage<PixelType, OutputImageDimension>;
  using InputSpecialCoordinatesImageType = SpecialCoordinatesImage<InputPixelType, InputImageDimension>;

  if (outputRegionForThread.GetNumberOfPixels() == 0)
  {
    return;
  }

  const bool isSpecialCoordinatesImage =
    ((dynamic_cast<const InputSpecialCoordinatesImageType *>(this->GetInput()) != nullptr) ||
     (dynamic_cast<const OutputSpecialCoordinatesImageType *>(this->GetOutput()) != nullptr));


  // Check whether we can use a fast path for resampling. Fast path
  // can be used if the transformation is linear. Transform respond
  // to the IsLinear() call.
  if (!isSpecialCoordinatesImage &&
      this->GetTransform()->GetTransformCategory() == TransformType::TransformCategoryEnum::Linear)
  {
    this->LinearThreadedGenerateData(outputRegionForThread);
    return;
  }

  // Otherwise, we use the normal method where the transform is called
  // for computing the transformation of every point.
  this->NonlinearThreadedGenerateData(outputRegionForThread);
}


#ifndef ITK_LEGACY_REMOVE
template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
typename ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::PixelType
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  CastPixelWithBoundsChecking(const InterpolatorOutputType value,
                              const ComponentType          minComponent,
                              const ComponentType          maxComponent) const
{
  const unsigned int nComponents = InterpolatorConvertType::GetNumberOfComponents(value);
  PixelType          outputValue;

  NumericTraits<PixelType>::SetLength(outputValue, nComponents);

  for (unsigned int n = 0; n < nComponents; n++)
  {
    ComponentType component = InterpolatorConvertType::GetNthComponent(n, value);

    if (component < minComponent)
    {
      PixelConvertType::SetNthComponent(n, outputValue, static_cast<PixelComponentType>(minComponent));
    }
    else if (component > maxComponent)
    {
      PixelConvertType::SetNthComponent(n, outputValue, static_cast<PixelComponentType>(maxComponent));
    }
    else
    {
      PixelConvertType::SetNthComponent(n, outputValue, static_cast<PixelComponentType>(component));
    }
  }

  return outputValue;
}
#endif // ITK_LEGACY_REMOVE


template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
auto
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  CastComponentWithBoundsChecking(const PixelComponentType value) -> PixelComponentType
{
  // Just return the argument. In this case, there is no need to cast or clamp its value.
  return value;
}


template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
template <typename TComponent>
auto
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  CastComponentWithBoundsChecking(const TComponent value) -> PixelComponentType
{
  static_assert(std::is_same<TComponent, ComponentType>::value,
                "TComponent should just be the same as the ComponentType!");
  static_assert(!std::is_same<TComponent, PixelComponentType>::value,
                "For PixelComponentType there is a more appropriate overload, that should be called instead!");

  // Retrieve minimum and maximum values at compile-time:
  constexpr auto minPixelComponent = NumericTraits<PixelComponentType>::NonpositiveMin();
  constexpr auto maxPixelComponent = NumericTraits<PixelComponentType>::max();
  constexpr auto minComponent = static_cast<ComponentType>(minPixelComponent);
  constexpr auto maxComponent = static_cast<ComponentType>(maxPixelComponent);

  // Clamp the value between minPixelComponent and maxPixelComponent:
  return (value <= minComponent) ? minPixelComponent
                                 : (value >= maxComponent) ? maxPixelComponent : static_cast<PixelComponentType>(value);
}


template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
auto
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  CastPixelWithBoundsChecking(const ComponentType value) -> PixelType
{
  return CastComponentWithBoundsChecking(value);
}


template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
template <typename TPixel>
auto
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  CastPixelWithBoundsChecking(const TPixel value) -> PixelType
{
  static_assert(std::is_same<TPixel, InterpolatorOutputType>::value,
                "TPixel should just be the same as the InterpolatorOutputType!");
  static_assert(!std::is_same<TPixel, ComponentType>::value,
                "For ComponentType there is a more efficient overload, that should be called instead!");

  const unsigned int nComponents = InterpolatorConvertType::GetNumberOfComponents(value);
  PixelType          outputValue;

  NumericTraits<PixelType>::SetLength(outputValue, nComponents);

  for (unsigned int n = 0; n < nComponents; ++n)
  {
    const ComponentType component = InterpolatorConvertType::GetNthComponent(n, value);
    PixelConvertType::SetNthComponent(n, outputValue, Self::CastComponentWithBoundsChecking(component));
  }

  return outputValue;
}


template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  NonlinearThreadedGenerateData(const OutputImageRegionType & outputRegionForThread)
{
  OutputImageType *      outputPtr = this->GetOutput();
  const InputImageType * inputPtr = this->GetInput();
  const TransformType *  transformPtr = this->GetTransform();

  TotalProgressReporter progress(this, outputPtr->GetRequestedRegion().GetNumberOfPixels());

  // Honor the SpecialCoordinatesImage isInside value returned
  // by TransformPhysicalPointToContinuousIndex
  using InputSpecialCoordinatesImageType = SpecialCoordinatesImage<InputPixelType, InputImageDimension>;
  const bool isSpecialCoordinatesImage = (dynamic_cast<const InputSpecialCoordinatesImageType *>(inputPtr) != nullptr);


  // Create an iterator that will walk the output region for this thread.
  using OutputIterator = ImageRegionIteratorWithIndex<TOutputImage>;
  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  OutputPointType outputPoint; // Coordinates of current output pixel
  InputPointType  inputPoint;  // Coordinates of current input pixel

  ContinuousInputIndexType inputIndex;

  using OutputType = typename InterpolatorType::OutputType;

  // Walk the output region
  outIt.GoToBegin();

  while (!outIt.IsAtEnd())
  {
    // Determine the index of the current output pixel
    outputPtr->TransformIndexToPhysicalPoint(outIt.GetIndex(), outputPoint);

    // Compute corresponding input pixel position
    inputPoint = transformPtr->TransformPoint(outputPoint);
    const bool isInsideInput = inputPtr->TransformPhysicalPointToContinuousIndex(inputPoint, inputIndex);

    OutputType value;
    // Evaluate input at right position and copy to the output
    if (m_Interpolator->IsInsideBuffer(inputIndex) && (!isSpecialCoordinatesImage || isInsideInput))
    {
      value = m_Interpolator->EvaluateAtContinuousIndex(inputIndex);
      outIt.Set(Self::CastPixelWithBoundsChecking(value));
    }
    else
    {
      if (m_Extrapolator.IsNull())
      {
        outIt.Set(m_DefaultPixelValue); // default background value
      }
      else
      {
        value = m_Extrapolator->EvaluateAtContinuousIndex(inputIndex);
        outIt.Set(Self::CastPixelWithBoundsChecking(value));
      }
    }
    progress.CompletedPixel();
    ++outIt;
  }
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  LinearThreadedGenerateData(const OutputImageRegionType & outputRegionForThread)
{
  OutputImageType *      outputPtr = this->GetOutput();
  const InputImageType * inputPtr = this->GetInput();
  const TransformType *  transformPtr = this->GetTransform();

  // Create an iterator that will walk the output region for this thread.
  using OutputIterator = ImageScanlineIterator<TOutputImage>;
  OutputIterator outIt(outputPtr, outputRegionForThread);

  TotalProgressReporter progress(this, outputPtr->GetRequestedRegion().GetNumberOfPixels());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  OutputPointType outputPoint; // Coordinates of current output pixel
  InputPointType  inputPoint;  // Coordinates of current input pixel

  const OutputImageRegionType & largestPossibleRegion = outputPtr->GetLargestPossibleRegion();

  using OutputType = typename InterpolatorType::OutputType;

  // Cache information from the superclass
  PixelType defaultValue = this->GetDefaultPixelValue();

  using OutputType = typename InterpolatorType::OutputType;

  // As we walk across a scan line in the output image, we trace
  // an oriented/scaled/translated line in the input image. Each scan
  // line has a starting and ending point. Since all transforms
  // are linear, the path between the points is linear and can be
  // defined by interpolating between the two points. By using
  // interpolation we avoid accumulation errors, and by using the
  // whole scan line from the largest possible region we make the
  // computation independent for each point and independent of the
  // region we are processing which makes the method independent of
  // how the whole image is split for processing ( threading,
  // streaming, etc ).
  //

  while (!outIt.IsAtEnd())
  {
    // Determine the continuous index of the first and end pixel of output
    // scan line when mapped to the input coordinate frame.

    IndexType index = outIt.GetIndex();
    index[0] = largestPossibleRegion.GetIndex(0);

    ContinuousInputIndexType startIndex;
    outputPtr->TransformIndexToPhysicalPoint(index, outputPoint);
    inputPoint = transformPtr->TransformPoint(outputPoint);
    inputPtr->TransformPhysicalPointToContinuousIndex(inputPoint, startIndex);

    ContinuousInputIndexType endIndex;
    index[0] += largestPossibleRegion.GetSize(0);
    outputPtr->TransformIndexToPhysicalPoint(index, outputPoint);
    inputPoint = transformPtr->TransformPoint(outputPoint);
    inputPtr->TransformPhysicalPointToContinuousIndex(inputPoint, endIndex);

    IndexValueType scanlineIndex = outIt.GetIndex()[0];


    while (!outIt.IsAtEndOfLine())
    {

      // Perform linear interpolation between startIndex and endIndex
      const double alpha =
        (scanlineIndex - largestPossibleRegion.GetIndex(0)) / (double)(largestPossibleRegion.GetSize(0));

      ContinuousInputIndexType inputIndex(startIndex);
      for (unsigned int i = 0; i < InputImageDimension; ++i)
      {
        inputIndex[i] += alpha * (endIndex[i] - startIndex[i]);
      }

      OutputType value;
      // Evaluate input at right position and copy to the output
      if (m_Interpolator->IsInsideBuffer(inputIndex))
      {
        value = m_Interpolator->EvaluateAtContinuousIndex(inputIndex);
        outIt.Set(Self::CastPixelWithBoundsChecking(value));
      }
      else
      {
        if (m_Extrapolator.IsNull())
        {
          outIt.Set(defaultValue); // default background value
        }
        else
        {
          value = m_Extrapolator->EvaluateAtContinuousIndex(inputIndex);
          outIt.Set(Self::CastPixelWithBoundsChecking(value));
        }
      }

      ++outIt;
      ++scanlineIndex;
    }
    outIt.NextLine();
    progress.Completed(outputRegionForThread.GetSize()[0]);
  }
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  GenerateInputRequestedRegion()
{
  if (!m_Interpolator)
  {
    itkExceptionMacro(<< "Interpolator not set");
  }

  // Get pointers to the input and output
  auto * input = const_cast<InputImageType *>(this->GetInput());

  // Some interpolators need to look at their images in GetRadius()
  m_Interpolator->SetInputImage(input);

  // Check whether the input or the output is a
  // SpecialCoordinatesImage. If either are, then we cannot use the
  // fast path since index mapping will definitely not be linear.
  using OutputSpecialCoordinatesImageType = SpecialCoordinatesImage<PixelType, OutputImageDimension>;
  using InputSpecialCoordinatesImageType = SpecialCoordinatesImage<InputPixelType, InputImageDimension>;

  const bool isSpecialCoordinatesImage =
    ((dynamic_cast<const InputSpecialCoordinatesImageType *>(this->GetInput()) != nullptr) ||
     (dynamic_cast<const OutputSpecialCoordinatesImageType *>(this->GetOutput()) != nullptr));

  const OutputImageType * output = this->GetOutput();
  // Get the input transform
  const TransformType * transform = this->GetTransform();

  // Check whether we can use upstream streaming for resampling. Upstream streaming
  // can be used if the transformation is linear. Transform respond
  // to the IsLinear() call.
  if (!isSpecialCoordinatesImage && transform->GetTransformCategory() == TransformType::TransformCategoryEnum::Linear)
  {
    typename TInputImage::RegionType inputRequestedRegion;
    inputRequestedRegion = ImageAlgorithm::EnlargeRegionOverBox(output->GetRequestedRegion(), output, input, transform);

    const typename TInputImage::RegionType inputLargestRegion(input->GetLargestPossibleRegion());
    if (inputLargestRegion.IsInside(inputRequestedRegion.GetIndex()) ||
        inputLargestRegion.IsInside(inputRequestedRegion.GetUpperIndex()))
    {
      // Input requested region is partially outside the largest possible region.
      //   or
      // Input requested region is completely inside the largest possible region.
      inputRequestedRegion.PadByRadius(m_Interpolator->GetRadius());
      inputRequestedRegion.Crop(inputLargestRegion);
      input->SetRequestedRegion(inputRequestedRegion);
    }
    else if (inputRequestedRegion.IsInside(inputLargestRegion))
    {
      // Input requested region completely surrounds the largest possible region.
      input->SetRequestedRegion(inputLargestRegion);
    }
    else
    {
      // Input requested region is completely outside the largest possible region. Do not set the requested region in
      // this case.
    }
    return;
  }

  // Otherwise, determining the actual input region is non-trivial, especially
  // when we cannot assume anything about the transform being used.
  // So we do the easy thing and request the entire input image.
  //
  input->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::
  GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  if (InputImageDimension == OutputImageDimension)
  {
    Superclass::GenerateOutputInformation();
  }
  // Get pointers to the input and output
  OutputImageType * outputPtr = this->GetOutput();

  const ReferenceImageBaseType * const referenceImage = this->GetReferenceImage();

  // Set the size of the output region
  if (m_UseReferenceImage && referenceImage)
  {
    outputPtr->SetLargestPossibleRegion(referenceImage->GetLargestPossibleRegion());
  }
  else
  {
    typename TOutputImage::RegionType outputLargestPossibleRegion;
    outputLargestPossibleRegion.SetSize(m_Size);
    outputLargestPossibleRegion.SetIndex(m_OutputStartIndex);
    outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
  }

  // Set spacing and origin
  if (m_UseReferenceImage && referenceImage)
  {
    outputPtr->SetSpacing(referenceImage->GetSpacing());
    outputPtr->SetOrigin(referenceImage->GetOrigin());
    outputPtr->SetDirection(referenceImage->GetDirection());
  }
  else
  {
    outputPtr->SetSpacing(m_OutputSpacing);
    outputPtr->SetOrigin(m_OutputOrigin);
    outputPtr->SetDirection(m_OutputDirection);
  }
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
ModifiedTimeType
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::GetMTime() const
{
  ModifiedTimeType latestTime = Object::GetMTime();

  if (m_Interpolator)
  {
    if (latestTime < m_Interpolator->GetMTime())
    {
      latestTime = m_Interpolator->GetMTime();
    }
  }

  return latestTime;
}

template <typename TInputImage,
          typename TOutputImage,
          typename TInterpolatorPrecisionType,
          typename TTransformPrecisionType>
void
ResampleImageFilter<TInputImage, TOutputImage, TInterpolatorPrecisionType, TTransformPrecisionType>::PrintSelf(
  std::ostream & os,
  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent
     << "DefaultPixelValue: " << static_cast<typename NumericTraits<PixelType>::PrintType>(m_DefaultPixelValue)
     << std::endl;
  os << indent << "Size: " << m_Size << std::endl;
  os << indent << "OutputStartIndex: " << m_OutputStartIndex << std::endl;
  os << indent << "OutputSpacing: " << m_OutputSpacing << std::endl;
  os << indent << "OutputOrigin: " << m_OutputOrigin << std::endl;
  os << indent << "OutputDirection: " << m_OutputDirection << std::endl;
  os << indent << "Transform: " << this->GetTransform() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Extrapolator: " << m_Extrapolator.GetPointer() << std::endl;
  os << indent << "UseReferenceImage: " << (m_UseReferenceImage ? "On" : "Off") << std::endl;
}
} // end namespace itk

#endif
