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
#ifndef itkReinitializeLevelSetImageFilter_hxx
#define itkReinitializeLevelSetImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkIndex.h"

namespace itk
{

template <typename TLevelSet>
ReinitializeLevelSetImageFilter<TLevelSet>::ReinitializeLevelSetImageFilter()
{
  m_LevelSetValue = 0.0;

  m_Locator = LocatorType::New();
  m_Marcher = FastMarchingImageFilterType::New();

  m_NarrowBanding = false;
  m_InputNarrowBandwidth = 12.0;
  m_OutputNarrowBandwidth = 12.0;
  m_InputNarrowBand = nullptr;
  m_OutputNarrowBand = nullptr;
}

template <typename TLevelSet>
void
ReinitializeLevelSetImageFilter<TLevelSet>::SetInputNarrowBand(NodeContainer * ptr)
{
  if (m_InputNarrowBand != ptr)
  {
    m_InputNarrowBand = ptr;
    this->Modified();
  }
}

template <typename TLevelSet>
void
ReinitializeLevelSetImageFilter<TLevelSet>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "LevelSetValue: " << m_LevelSetValue << std::endl;

  itkPrintSelfObjectMacro(Locator);
  itkPrintSelfObjectMacro(Marcher);

  os << indent << "Narrowbanding: " << (m_NarrowBanding ? "On" : "Off") << std::endl;
  os << indent << "InputNarrowBandwidth: " << m_InputNarrowBandwidth << std::endl;
  os << indent << "OutputNarrowBandwidth: " << m_OutputNarrowBandwidth << std::endl;

  itkPrintSelfObjectMacro(InputNarrowBand);
  itkPrintSelfObjectMacro(OutputNarrowBand);
}

template <typename TLevelSet>
void
ReinitializeLevelSetImageFilter<TLevelSet>::GenerateInputRequestedRegion()
{
  // use the default implementation.
  this->Superclass::GenerateInputRequestedRegion();
}

template <typename TLevelSet>
void
ReinitializeLevelSetImageFilter<TLevelSet>::EnlargeOutputRequestedRegion(DataObject * output)
{
  // this filter requires the all of the output image to be in
  // the buffer
  TLevelSet * imgData;

  imgData = dynamic_cast<TLevelSet *>(output);
  if (imgData)
  {
    imgData->SetRequestedRegionToLargestPossibleRegion();
  }
  else
  {
    // pointer could not be cast to TLevelSet *
    itkWarningMacro("itk::ReinitializeLevelSetImageFilter"
                    << "::EnlargeOutputRequestedRegion cannot cast " << typeid(output).name() << " to "
                    << typeid(TLevelSet *).name());
  }
}

template <typename TLevelSet>
void
ReinitializeLevelSetImageFilter<TLevelSet>::AllocateOutput()
{
  LevelSetPointer outputPtr = this->GetOutput();

  // allocate the output buffer memory
  outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
  outputPtr->Allocate();

  // set the marcher output size
  this->m_Marcher->SetOutputRegion(outputPtr->GetRequestedRegion());
  this->m_Marcher->SetOutputOrigin(this->GetInput()->GetOrigin());
  this->m_Marcher->SetOutputSpacing(this->GetInput()->GetSpacing());
  this->m_Marcher->SetOutputDirection(this->GetInput()->GetDirection());
}

template <typename TLevelSet>
void
ReinitializeLevelSetImageFilter<TLevelSet>::GenerateData()
{
  this->AllocateOutput();

  if (m_NarrowBanding)
  {
    this->GenerateDataNarrowBand();
  }
  else
  {
    this->GenerateDataFull();
  }
}

template <typename TLevelSet>
void
ReinitializeLevelSetImageFilter<TLevelSet>::GenerateDataFull()
{
  LevelSetConstPointer inputPtr = this->GetInput();
  LevelSetPointer      outputPtr = this->GetOutput();
  LevelSetPointer      tempLevelSet = m_Marcher->GetOutput();

  // define iterators
  using IteratorType = ImageRegionIterator<LevelSetImageType>;
  using ConstIteratorType = ImageRegionConstIterator<LevelSetImageType>;

  ConstIteratorType inputIt(inputPtr, inputPtr->GetBufferedRegion());
  IteratorType      outputIt(outputPtr, outputPtr->GetBufferedRegion());

  IteratorType tempIt;

  this->UpdateProgress(0.0);

  // locate the level set
  m_Locator->SetInputLevelSet(inputPtr);
  m_Locator->SetLevelSetValue(m_LevelSetValue);
  m_Locator->Locate();

  this->UpdateProgress(0.33);

  // march outward
  m_Marcher->SetTrialPoints(m_Locator->GetOutsidePoints());
  m_Marcher->Update();

  tempIt = IteratorType(tempLevelSet, tempLevelSet->GetBufferedRegion());

  double value;

  while (!inputIt.IsAtEnd())
  {
    value = static_cast<double>(inputIt.Get());
    if (value - m_LevelSetValue > 0)
    {
      outputIt.Set(tempIt.Get());
    }

    ++inputIt;
    ++outputIt;
    ++tempIt;
  }

  this->UpdateProgress(0.66);

  // march inward
  m_Marcher->SetTrialPoints(m_Locator->GetInsidePoints());
  m_Marcher->Update();

  inputIt.GoToBegin();
  outputIt.GoToBegin();
  tempIt.GoToBegin();

  while (!inputIt.IsAtEnd())
  {
    value = static_cast<double>(inputIt.Get());
    if (value - m_LevelSetValue <= 0)
    {
      value = static_cast<double>(tempIt.Get());
      outputIt.Set(-1.0 * value);
    }

    ++inputIt;
    ++outputIt;
    ++tempIt;
  }
}

template <typename TLevelSet>
void
ReinitializeLevelSetImageFilter<TLevelSet>::GenerateDataNarrowBand()
{
  LevelSetConstPointer inputPtr = this->GetInput();
  LevelSetPointer      outputPtr = this->GetOutput();
  LevelSetPointer      tempLevelSet = m_Marcher->GetOutput();

  // define iterators
  using IteratorType = ImageRegionIterator<LevelSetImageType>;
  using ConstIteratorType = ImageRegionConstIterator<LevelSetImageType>;

  ConstIteratorType inputIt(inputPtr, inputPtr->GetBufferedRegion());

  IteratorType outputIt(outputPtr, outputPtr->GetBufferedRegion());

  PixelType posInfinity;
  PixelType negInfinity;

  posInfinity = NumericTraits<PixelType>::max();
  negInfinity = NumericTraits<PixelType>::NonpositiveMin();

  // set all internal pixels to minus infinity and
  // all external pixels to positive infinity
  double value;

  while (!inputIt.IsAtEnd())
  {
    value = static_cast<double>(inputIt.Get());
    if (value - m_LevelSetValue <= 0)
    {
      outputIt.Set(negInfinity);
    }
    else
    {
      outputIt.Set(posInfinity);
    }

    ++inputIt;
    ++outputIt;
  }

  // create a new output narrowband container
  m_OutputNarrowBand = NodeContainer::New();

  this->UpdateProgress(0.0);

  // locate the level set
  m_Locator->SetInputLevelSet(inputPtr);
  m_Locator->SetLevelSetValue(m_LevelSetValue);

  if (m_NarrowBanding && m_InputNarrowBand)
  {
    m_Locator->NarrowBandingOn();
    m_Locator->SetNarrowBandwidth(m_InputNarrowBandwidth);
    m_Locator->SetInputNarrowBand(m_InputNarrowBand);
  }
  else
  {
    m_Locator->NarrowBandingOff();
  }

  m_Locator->Locate();

  this->UpdateProgress(0.33);

  // march outward
  double stoppingValue = (m_OutputNarrowBandwidth / 2.0) + 2.0;
  m_Marcher->SetStoppingValue(stoppingValue);
  m_Marcher->CollectPointsOn();
  m_Marcher->SetTrialPoints(m_Locator->GetOutsidePoints());
  m_Marcher->Update();

  NodeContainerPointer procPoints = m_Marcher->GetProcessedPoints();

  typename NodeContainer::ConstIterator pointsIt;
  typename NodeContainer::ConstIterator pointsEnd;

  pointsIt = procPoints->Begin();
  pointsEnd = procPoints->End();

  NodeType  node;
  PixelType inPixel;

  for (; pointsIt != pointsEnd; ++pointsIt)
  {
    node = pointsIt.Value();
    inPixel = inputPtr->GetPixel(node.GetIndex());

    value = static_cast<double>(inPixel);
    if (value - m_LevelSetValue > 0)
    {
      inPixel = tempLevelSet->GetPixel(node.GetIndex());
      outputPtr->SetPixel(node.GetIndex(), inPixel);
      m_OutputNarrowBand->InsertElement(m_OutputNarrowBand->Size(), node);
    }
  } // end for loop

  this->UpdateProgress(0.66);

  // march inward
  m_Marcher->SetTrialPoints(m_Locator->GetInsidePoints());
  m_Marcher->Update();

  procPoints = m_Marcher->GetProcessedPoints();
  pointsIt = procPoints->Begin();
  pointsEnd = procPoints->End();

  for (; pointsIt != pointsEnd; ++pointsIt)
  {
    node = pointsIt.Value();
    inPixel = inputPtr->GetPixel(node.GetIndex());

    value = static_cast<double>(inPixel);
    if (value - m_LevelSetValue <= 0)
    {
      inPixel = tempLevelSet->GetPixel(node.GetIndex());
      value = static_cast<double>(inPixel);
      inPixel = -1.0 * value;
      outputPtr->SetPixel(node.GetIndex(), inPixel);
      node.SetValue(node.GetValue() * -1.0);
      m_OutputNarrowBand->InsertElement(m_OutputNarrowBand->Size(), node);
    }
  } // end for loop
}
} // namespace itk

#endif
