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
#ifndef itkScalarToRGBColormapImageFilter_hxx
#define itkScalarToRGBColormapImageFilter_hxx

#include "itkScalarToRGBColormapImageFilter.h"

#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

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
 * https://hdl.handle.net/1926/1452
 * http://www.insight-journal.org/browse/publication/285
 *
 */

namespace itk
{

template< typename TInputImage, typename TOutputImage >
ScalarToRGBColormapImageFilter< TInputImage, TOutputImage >
::ScalarToRGBColormapImageFilter()
{
  this->SetNumberOfRequiredInputs(1);

  this->m_UseInputImageExtremaForScaling = true;

  typedef Function::GreyColormapFunction<
    InputImagePixelType, OutputImagePixelType > DefaultColormapType;

  typename DefaultColormapType::Pointer greyColormap = DefaultColormapType::New();
  this->SetColormap(greyColormap);
}

template< typename TInputImage, typename TOutputImage >
void
ScalarToRGBColormapImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  if ( this->m_UseInputImageExtremaForScaling == true )
    {
    ImageRegionConstIterator< InputImageType > It( this->GetInput(),
                                                   this->GetInput()->GetRequestedRegion() );

    InputImagePixelType minimumValue = NumericTraits< InputImagePixelType >::max();
    InputImagePixelType maximumValue = NumericTraits< InputImagePixelType >::min();

    for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      InputImagePixelType value = It.Get();
      if ( value < minimumValue )
        {
        minimumValue = value;
        }
      if ( value > maximumValue )
        {
        maximumValue = value;
        }
      }

    this->m_Colormap->SetMinimumInputValue(minimumValue);
    this->m_Colormap->SetMaximumInputValue(maximumValue);
    }
}

template< typename TInputImage, typename TOutputImage >
void
ScalarToRGBColormapImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // Define the iterators
  ImageRegionConstIterator< TInputImage > inputIt(inputPtr, inputRegionForThread);
  ImageRegionIterator< TOutputImage >     outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while ( !inputIt.IsAtEnd() )
    {
    outputIt.Set( this->m_Colormap->operator()( inputIt.Get() ) );
    ++inputIt;
    ++outputIt;
    progress.CompletedPixel();  // potential exception thrown here
    }
}

template< typename TInputImage, typename TOutputImage >
void
ScalarToRGBColormapImageFilter< TInputImage, TOutputImage >
::SetColormap(ColormapEnumType map)
{
  switch ( map )
    {
    case Red:
      {
      typedef Function::RedColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Green:
      {
      typedef Function::GreenColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Blue:
      {
      typedef Function::BlueColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Grey:
    default:
      {
      typedef Function::GreyColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Hot:
      {
      typedef Function::HotColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Cool:
      {
      typedef Function::CoolColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Spring:
      {
      typedef Function::SpringColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Summer:
      {
      typedef Function::SummerColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Autumn:
      {
      typedef Function::AutumnColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Winter:
      {
      typedef Function::WinterColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Copper:
      {
      typedef Function::CopperColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case Jet:
      {
      typedef Function::JetColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case HSV:
      {
      typedef Function::HSVColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    case OverUnder:
      {
      typedef Function::OverUnderColormapFunction<
        InputImagePixelType, OutputImagePixelType > SpecificColormapType;
      typename SpecificColormapType::Pointer colormap = SpecificColormapType::New();
      this->SetColormap(colormap);
      break;
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
ScalarToRGBColormapImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Class Name: " << this->GetNameOfClass() << std::endl;
  if ( this->m_Colormap.IsNotNull() )
    {
    os << indent << "Colormap " << this->m_Colormap << std::endl;
    }
  else
    {
    os << indent << "Colormap is ITK_NULLPTR " << std::endl;
    }
  os << indent << "Use Input Image Extrema for Scaling " << this->m_UseInputImageExtremaForScaling << std::endl;
}
} // end namespace itk

#endif
