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
#ifndef itkStrainImageFilter_hxx
#define itkStrainImageFilter_hxx


#include "itkGradientImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType>
StrainImageFilter<TInputImage, TOperatorValueType, TOutputValueType>::StrainImageFilter()
  : m_InputComponentsFilter(InputComponentsImageFilterType::New())
  , m_StrainForm(INFINITESIMAL)
{
  // The first output is only of interest to the user.  The rest of the outputs
  // are GradientImageFilter outputs used internally, but put on the output so
  // memory management capabilities of the pipeline can be taken advantage of.
  this->SetNumberOfIndexedOutputs(1 + ImageDimension);
  for (unsigned int i = 1; i < ImageDimension + 1; i++)
  {
    this->SetNthOutput(i, GradientOutputImageType::New().GetPointer());
  }

  using GradientImageFilterType = GradientImageFilter<OperatorImageType, TOperatorValueType, TOperatorValueType>;
  this->m_GradientFilter = GradientImageFilterType::New().GetPointer();

  this->DynamicMultiThreadingOn();
}

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType>
void
StrainImageFilter<TInputImage, TOperatorValueType, TOutputValueType>::BeforeThreadedGenerateData()
{
  typename InputImageType::ConstPointer input = this->GetInput();

  if (this->m_VectorGradientFilter.GetPointer() != nullptr)
  {
    this->m_VectorGradientFilter->SetInput(input);
    for (unsigned int i = 1; i < ImageDimension + 1; ++i)
    {
      this->m_VectorGradientFilter->GraftNthOutput(
        i - 1, dynamic_cast<GradientOutputImageType *>(this->ProcessObject::GetOutput(i)));
    }
    this->m_VectorGradientFilter->Update();
    for (unsigned int i = 1; i < ImageDimension + 1; ++i)
    {
      this->GraftNthOutput(i, this->m_VectorGradientFilter->GetOutput(i - 1));
    }
  }
  else
  {
    this->m_InputComponentsFilter->SetInput(input);

    for (unsigned int i = 1; i < ImageDimension + 1; ++i)
    {
      this->m_GradientFilter->SetInput(this->m_InputComponentsFilter->GetOutput(i - 1));
      this->m_GradientFilter->GraftOutput(dynamic_cast<GradientOutputImageType *>(this->ProcessObject::GetOutput(i)));
      this->m_GradientFilter->Update();
      this->GraftNthOutput(i, this->m_GradientFilter->GetOutput());
    }
  }

  const StrainFormType strainForm = this->GetStrainForm();
  if (strainForm != INFINITESIMAL && strainForm != GREENLAGRANGIAN && strainForm != EULERIANALMANSI)
  {
    itkExceptionMacro("Invalid StrainForm!");
  }

  OutputImageType * output = this->GetOutput();
  output->FillBuffer(NumericTraits<OutputPixelType>::ZeroValue());
}

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType>
void
StrainImageFilter<TInputImage, TOperatorValueType, TOutputValueType>::DynamicThreadedGenerateData(
  const OutputRegionType & region)
{
  typename InputImageType::ConstPointer input = this->GetInput();

  OutputImageType * output = this->GetOutput();

  ImageRegionIterator<OutputImageType> outputIt(output, region);

  // e_ij += 1/2( du_i/dx_j + du_j/dx_i )
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    ImageRegionConstIterator<GradientOutputImageType> gradientIt(
      reinterpret_cast<GradientOutputImageType *>(
        dynamic_cast<GradientOutputImageType *>(this->ProcessObject::GetOutput(i + 1))),
      region);
    for (outputIt.GoToBegin(), gradientIt.GoToBegin(); !gradientIt.IsAtEnd(); ++outputIt, ++gradientIt)
    {
      typename OutputImageType::PixelType outputPixel = outputIt.Get();
      const GradientOutputPixelType       gradientPixel = gradientIt.Get();
      for (unsigned int j = 0; j < i; ++j)
      {
        outputPixel(i, j) += gradientPixel[j] / static_cast<TOutputValueType>(2);
      }
      // j == i
      outputPixel(i, i) += gradientPixel[i];
      for (unsigned int j = i + 1; j < ImageDimension; ++j)
      {
        outputPixel(i, j) += gradientPixel[j] / static_cast<TOutputValueType>(2);
      }
      outputIt.Set(outputPixel);
    }
  }
  switch (m_StrainForm)
  {
    case INFINITESIMAL:
      break;
    // e_ij += 1/2 du_m/du_i du_m/du_j
    case GREENLAGRANGIAN:
      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        ImageRegionConstIterator<GradientOutputImageType> gradientIt(
          reinterpret_cast<GradientOutputImageType *>(
            dynamic_cast<GradientOutputImageType *>(this->ProcessObject::GetOutput(i + 1))),
          region);
        for (outputIt.GoToBegin(), gradientIt.GoToBegin(); !gradientIt.IsAtEnd(); ++outputIt, ++gradientIt)
        {
          typename OutputImageType::PixelType outputPixel = outputIt.Get();
          const GradientOutputPixelType       gradientPixel = gradientIt.Get();
          for (unsigned int j = 0; j < ImageDimension; ++j)
          {
            for (unsigned int k = 0; k <= j; ++k)
            {
              outputPixel(j, k) += gradientPixel[j] * gradientPixel[k] / static_cast<TOutputValueType>(2);
            }
          }
          outputIt.Set(outputPixel);
        }
      }
      break;
    // e_ij -= 1/2 du_m/du_i du_m/du_j
    case EULERIANALMANSI:
      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        ImageRegionConstIterator<GradientOutputImageType> gradientIt(
          reinterpret_cast<GradientOutputImageType *>(
            dynamic_cast<GradientOutputImageType *>(this->ProcessObject::GetOutput(i + 1))),
          region);
        for (outputIt.GoToBegin(), gradientIt.GoToBegin(); !gradientIt.IsAtEnd(); ++outputIt, ++gradientIt)
        {
          typename OutputImageType::PixelType outputPixel = outputIt.Get();
          const GradientOutputPixelType       gradientPixel = gradientIt.Get();
          for (unsigned int j = 0; j < ImageDimension; ++j)
          {
            for (unsigned int k = 0; k <= j; ++k)
            {
              outputPixel(j, k) -= gradientPixel[j] * gradientPixel[k] / static_cast<TOutputValueType>(2);
            }
          }
          outputIt.Set(outputPixel);
        }
      }
      break;
    default:
      itkExceptionMacro(<< "Unknown strain form.");
  }
}

template <typename TInputImage, typename TOperatorValueType, typename TOutputValueType>
void
StrainImageFilter<TInputImage, TOperatorValueType, TOutputValueType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(InputComponentsFilter);

  itkPrintSelfObjectMacro(GradientFilter);

  itkPrintSelfObjectMacro(VectorGradientFilter);

  os << indent << "StrainForm: " << static_cast<typename NumericTraits<StrainFormType>::PrintType>(m_StrainForm)
     << std::endl;
}
} // end namespace itk

#endif
