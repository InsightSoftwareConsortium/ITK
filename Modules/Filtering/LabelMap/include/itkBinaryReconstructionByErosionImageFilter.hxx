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
#ifndef itkBinaryReconstructionByErosionImageFilter_hxx
#define itkBinaryReconstructionByErosionImageFilter_hxx

#include "itkProgressAccumulator.h"


namespace itk
{

template <typename TInputImage>
BinaryReconstructionByErosionImageFilter<TInputImage>::BinaryReconstructionByErosionImageFilter()
{
  m_BackgroundValue = NumericTraits<OutputImagePixelType>::NonpositiveMin();
  m_ForegroundValue = NumericTraits<OutputImagePixelType>::max();
  m_FullyConnected = false;
  this->SetPrimaryInputName("MarkerImage");
  this->AddRequiredInputName("MaskImage", 1);
}

template <typename TInputImage>
void
BinaryReconstructionByErosionImageFilter<TInputImage>::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast<InputImageType *>(this->GetMarkerImage());
  if (input)
  {
    input->SetRequestedRegion(input->GetLargestPossibleRegion());
  }

  input = const_cast<InputImageType *>(this->GetMaskImage());
  if (input)
  {
    input->SetRequestedRegion(input->GetLargestPossibleRegion());
  }
}


template <typename TInputImage>
void
BinaryReconstructionByErosionImageFilter<TInputImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion(this->GetOutput()->GetLargestPossibleRegion());
}


template <typename TInputImage>
void
BinaryReconstructionByErosionImageFilter<TInputImage>::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  auto progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  auto notMask = NotType::New();
  notMask->SetInput(this->GetMaskImage());
  notMask->SetForegroundValue(m_ForegroundValue);
  notMask->SetBackgroundValue(m_BackgroundValue);
  notMask->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  progress->RegisterInternalFilter(notMask, .1f);

  auto notMarker = NotType::New();
  notMarker->SetInput(this->GetMarkerImage());
  notMarker->SetForegroundValue(m_ForegroundValue);
  notMarker->SetBackgroundValue(m_BackgroundValue);
  notMarker->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  progress->RegisterInternalFilter(notMarker, .1f);

  auto labelizer = LabelizerType::New();
  labelizer->SetInput(notMask->GetOutput());
  labelizer->SetInputForegroundValue(m_ForegroundValue);
  labelizer->SetOutputBackgroundValue(m_BackgroundValue);
  labelizer->SetFullyConnected(m_FullyConnected);
  labelizer->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  progress->RegisterInternalFilter(labelizer, .2f);

  auto reconstruction = ReconstructionType::New();
  reconstruction->SetInput(labelizer->GetOutput());
  reconstruction->SetMarkerImage(notMarker->GetOutput());
  reconstruction->SetForegroundValue(m_ForegroundValue);
  reconstruction->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  progress->RegisterInternalFilter(reconstruction, .2f);

  auto opening = OpeningType::New();
  opening->SetInput(reconstruction->GetOutput());
  opening->SetLambda(true);
  opening->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  progress->RegisterInternalFilter(opening, .2f);

  // invert the image during the binarization
  auto binarizer = BinarizerType::New();
  binarizer->SetInput(opening->GetOutput());
  binarizer->SetLabel(m_BackgroundValue);
  binarizer->SetNegated(true);
  binarizer->SetBackgroundValue(m_ForegroundValue);
  binarizer->SetFeatureImage(this->GetMaskImage());
  binarizer->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
  progress->RegisterInternalFilter(binarizer, .2f);

  binarizer->GraftOutput(this->GetOutput());
  binarizer->Update();
  this->GraftOutput(binarizer->GetOutput());
}


template <typename TInputImage>
void
BinaryReconstructionByErosionImageFilter<TInputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  itkPrintSelfBooleanMacro(FullyConnected);
  os << indent
     << "BackgroundValue: " << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_BackgroundValue)
     << std::endl;
  os << indent
     << "ForegroundValue: " << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ForegroundValue)
     << std::endl;
}

} // end namespace itk
#endif
