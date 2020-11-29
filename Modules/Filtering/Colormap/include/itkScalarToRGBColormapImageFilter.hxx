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
#ifndef itkScalarToRGBColormapImageFilter_hxx
#define itkScalarToRGBColormapImageFilter_hxx

#include "itkScalarToRGBColormapImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkTotalProgressReporter.h"

#include "itkRedColormapFunction.h"
#include "itkGreenColormapFunction.h"
#include "itkBlueColormapFunction.h"
#include "itkGreyColormapFunction.h"
#include "itkHotColormapFunction.h"
#include "itkCoolColormapFunction.h"
#include "itkSpringColormapFunction.h"
#include "itkSummerColormapFunction.h"
#include "itkAutumnColormapFunction.h"
#include "itkWinterColormapFunction.h"
#include "itkCopperColormapFunction.h"
#include "itkHSVColormapFunction.h"
#include "itkJetColormapFunction.h"
#include "itkOverUnderColormapFunction.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Meeting Andy Warhol Somewhere Over the Rainbow: RGB Colormapping and ITK"
 * by Tustison N., Zhang H., Lehmann G., Yushkevich P., Gee J.
 * https://www.insight-journal.org/browse/publication/285
 *
 */

namespace itk
{

template <typename TInputImage, typename TOutputImage>
ScalarToRGBColormapImageFilter<TInputImage, TOutputImage>::ScalarToRGBColormapImageFilter()
{
  this->SetNumberOfRequiredInputs(1);

  this->m_UseInputImageExtremaForScaling = true;
  this->DynamicMultiThreadingOn();
  this->ThreaderUpdateProgressOff();

  using DefaultColormapType = Function::GreyColormapFunction<InputImagePixelType, OutputImagePixelType>;

  typename DefaultColormapType::Pointer greyColormap = DefaultColormapType::New();
  this->SetColormap(greyColormap);
}

template <typename TInputImage, typename TOutputImage>
void
ScalarToRGBColormapImageFilter<TInputImage, TOutputImage>::BeforeThreadedGenerateData()
{
  if (this->m_UseInputImageExtremaForScaling == true)
  {
    ImageRegionConstIterator<InputImageType> It(this->GetInput(), this->GetInput()->GetRequestedRegion());

    InputImagePixelType minimumValue = NumericTraits<InputImagePixelType>::max();
    InputImagePixelType maximumValue = NumericTraits<InputImagePixelType>::min();

    for (It.GoToBegin(); !It.IsAtEnd(); ++It)
    {
      InputImagePixelType value = It.Get();
      if (value < minimumValue)
      {
        minimumValue = value;
      }
      if (value > maximumValue)
      {
        maximumValue = value;
      }
    }

    this->m_Colormap->SetMinimumInputValue(minimumValue);
    this->m_Colormap->SetMaximumInputValue(maximumValue);
  }
}

template <typename TInputImage, typename TOutputImage>
void
ScalarToRGBColormapImageFilter<TInputImage, TOutputImage>::DynamicThreadedGenerateData(
  const OutputImageRegionType & outputRegionForThread)
{
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  TotalProgressReporter progressReporter(this, this->GetOutput()->GetRequestedRegion().GetNumberOfPixels());

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  ImageRegionConstIterator<TInputImage> inputIt(inputPtr, inputRegionForThread);
  ImageRegionIterator<TOutputImage>     outputIt(outputPtr, outputRegionForThread);

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while (!inputIt.IsAtEnd())
  {
    outputIt.Set(this->m_Colormap->operator()(inputIt.Get()));
    ++inputIt;
    ++outputIt;
    progressReporter.CompletedPixel();
  }
}

template <typename TInputImage, typename TOutputImage>
void
ScalarToRGBColormapImageFilter<TInputImage, TOutputImage>::SetColormap(RGBColormapFilterEnum map)
{
  switch (map)
  {
    case RGBColormapFilterEnum::Red:
    {
      using SpecificColormapType = Function::RedColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Green:
    {
      using SpecificColormapType = Function::GreenColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Blue:
    {
      using SpecificColormapType = Function::BlueColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Grey:
    default:
    {
      using SpecificColormapType = Function::GreyColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Hot:
    {
      using SpecificColormapType = Function::HotColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Cool:
    {
      using SpecificColormapType = Function::CoolColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Spring:
    {
      using SpecificColormapType = Function::SpringColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Summer:
    {
      using SpecificColormapType = Function::SummerColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Autumn:
    {
      using SpecificColormapType = Function::AutumnColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Winter:
    {
      using SpecificColormapType = Function::WinterColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Copper:
    {
      using SpecificColormapType = Function::CopperColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::Jet:
    {
      using SpecificColormapType = Function::JetColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::HSV:
    {
      using SpecificColormapType = Function::HSVColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
    case RGBColormapFilterEnum::OverUnder:
    {
      using SpecificColormapType = Function::OverUnderColormapFunction<InputImagePixelType, OutputImagePixelType>;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
ScalarToRGBColormapImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Class Name: " << this->GetNameOfClass() << std::endl;
  if (this->m_Colormap.IsNotNull())
  {
    os << indent << "Colormap " << this->m_Colormap << std::endl;
  }
  else
  {
    os << indent << "Colormap is nullptr " << std::endl;
  }
  os << indent << "Use Input Image Extrema for Scaling " << this->m_UseInputImageExtremaForScaling << std::endl;
}
} // end namespace itk

#endif
