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
#ifndef itkGradientImageFilter_hxx
#define itkGradientImageFilter_hxx

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkDerivativeOperator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkOffset.h"
#include "itkTotalProgressReporter.h"
#include "itkMath.h"

namespace itk
{

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType, typename TOutputImageType>
GradientImageFilter<TInputImage, TOperatorValueType, TOutputValueType, TOutputImageType>::GradientImageFilter()
{
  this->DynamicMultiThreadingOn();
  this->ThreaderUpdateProgressOff();
}

template <typename TInputImage, typename TOperatorValueType, typename TOutputValue, typename TOutputImage>
void
GradientImageFilter<TInputImage, TOperatorValueType, TOutputValue, TOutputImage>::OverrideBoundaryCondition(
  ImageBoundaryCondition<TInputImage> * boundaryCondition)
{
  m_BoundaryCondition.reset(boundaryCondition);
}

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType, typename TOutputImageType>
void
GradientImageFilter<TInputImage, TOperatorValueType, TOutputValueType, TOutputImageType>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer  inputPtr = const_cast<InputImageType *>(this->GetInput());
  OutputImagePointer outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by one, which is the value of the first
  // coordinate of the operator radius.
  inputRequestedRegion.PadByRadius(1);

  // crop the input requested region at the input's largest possible region
  if (inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()))
  {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
  }
  else
  {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
  }
}

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType, typename TOutputImageType>
void
GradientImageFilter<TInputImage, TOperatorValueType, TOutputValueType, TOutputImageType>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{

  NeighborhoodInnerProduct<InputImageType, OperatorValueType, OutputValueType> SIP;

  OutputImageType *      outputImage = this->GetOutput();
  const InputImageType * inputImage = this->GetInput();

  // Set up operators
  DerivativeOperator<OperatorValueType, InputImageDimension> op[InputImageDimension];

  for (unsigned int i = 0; i < InputImageDimension; ++i)
  {
    // The operator has default values for its direction (0) and its order (1).
    op[i].CreateDirectional();

    // Reverse order of coefficients for the convolution with the image to
    // follow.
    op[i].FlipAxes();

    // Take into account the pixel spacing if necessary
    if (m_UseImageSpacing)
    {
      if (this->GetInput()->GetSpacing()[i] == 0.0)
      {
        itkExceptionMacro("Image spacing cannot be zero.");
      }
      else
      {
        op[i].ScaleCoefficients(1.0 / this->GetInput()->GetSpacing()[i]);
      }
    }
  }

  // Set the iterator radius to one, which is the value of the first
  // coordinate of the operator radius.
  static constexpr auto radius = Size<InputImageDimension>::Filled(1);

  // Find the data-set boundary "faces"
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>                        bC;
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList =
    bC(inputImage, outputRegionForThread, radius);

  TotalProgressReporter progress(this, this->GetOutput()->GetRequestedRegion().GetNumberOfPixels());

  // Initialize the x_slice array
  ConstNeighborhoodIterator<InputImageType> nit(radius, inputImage, faceList.front());

  std::slice x_slice[InputImageDimension];
  for (unsigned int i = 0; i < InputImageDimension; ++i)
  {
    static constexpr SizeValueType neighborhoodSize = Math::UnsignedPower(3, InputImageDimension);
    static constexpr SizeValueType center = neighborhoodSize / 2;

    x_slice[i] = std::slice(center - nit.GetStride(i), op[i].GetSize()[0], nit.GetStride(i));
  }

  CovariantVectorType gradient;
  // Process non-boundary face and then each of the boundary faces.
  // These are N-d regions which border the edge of the buffer.
  for (const auto & face : faceList)
  {
    nit = ConstNeighborhoodIterator<InputImageType>(radius, inputImage, face);
    ImageRegionIterator<OutputImageType> it(outputImage, face);
    nit.OverrideBoundaryCondition(m_BoundaryCondition.get());
    nit.GoToBegin();

    while (!nit.IsAtEnd())
    {
      for (unsigned int i = 0; i < InputImageDimension; ++i)
      {
        gradient[i] = SIP(x_slice[i], nit, op[i]);
      }

      // This method optionally performs a transform for Physical
      // coordinates and potential conversion to a different output
      // pixel type.
      this->SetOutputPixel(it, gradient);

      ++nit;
      ++it;
      progress.CompletedPixel();
    }
  }
}

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType, typename TOutputImageType>
void
GradientImageFilter<TInputImage, TOperatorValueType, TOutputValueType, TOutputImageType>::GenerateOutputInformation()
{
  // this methods is overloaded so that if the output image is a
  // VectorImage then the correct number of components are set.

  Superclass::GenerateOutputInformation();
  OutputImageType * output = this->GetOutput();

  if (!output)
  {
    return;
  }
  if (output->GetNumberOfComponentsPerPixel() != InputImageDimension)
  {
    output->SetNumberOfComponentsPerPixel(InputImageDimension);
  }
}

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType, typename TOutputImageType>
void
GradientImageFilter<TInputImage, TOperatorValueType, TOutputValueType, TOutputImageType>::PrintSelf(std::ostream & os,
                                                                                                    Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfBooleanMacro(UseImageSpacing);
  itkPrintSelfBooleanMacro(UseImageDirection);

  os << indent << "BoundaryCondition: ";
  if (m_BoundaryCondition != nullptr)
  {
    os << m_BoundaryCondition.get() << std::endl;
  }
  else
  {
    os << "(null)" << std::endl;
  }
}
} // end namespace itk

#endif
