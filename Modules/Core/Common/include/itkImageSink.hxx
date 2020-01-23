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
#ifndef itkImageSink_hxx
#define itkImageSink_hxx

#include "itkImageSink.h"
#include "itkProgressTransformer.h"
#include "itkInputDataObjectConstIterator.h"
#include "itkMultiThreaderBase.h"

namespace itk
{

template <typename TInputImage>
ImageSink<TInputImage>::ImageSink()
{
  // create default region splitter
  m_RegionSplitter = ImageRegionSplitterSlowDimension::New();

  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);
}


template <typename TInputImage>
void
ImageSink<TInputImage>::SetInput(const InputImageType * input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, const_cast<InputImageType *>(input));
}


template <typename TInputImage>
const typename ImageSink<TInputImage>::InputImageType *
ImageSink<TInputImage>::GetInput() const
{
  return itkDynamicCastInDebugMode<const TInputImage *>(this->ProcessObject::GetPrimaryInput());
}


template <typename TInputImage>
const typename ImageSink<TInputImage>::InputImageType *
ImageSink<TInputImage>::GetInput(unsigned int idx) const
{
  const auto * in = dynamic_cast<const TInputImage *>(this->ProcessObject::GetInput(idx));

  if (in == nullptr && this->ProcessObject::GetInput(idx) != nullptr)
  {
    itkWarningMacro(<< "Unable to convert input number " << idx << " to type " << typeid(InputImageType).name());
  }
  return in;
}


template <typename TInputImage>
const typename ImageSink<TInputImage>::InputImageType *
ImageSink<TInputImage>::GetInput(const DataObjectIdentifierType & key) const
{
  const auto * in = dynamic_cast<const TInputImage *>(this->ProcessObject::GetInput(key));

  if (in == nullptr && this->ProcessObject::GetInput(key) != nullptr)
  {
    itkWarningMacro(<< "Unable to convert input \"" << key << "\" to type " << typeid(InputImageType).name());
  }
  return in;
}


template <typename TInputImage>
void
ImageSink<TInputImage>::UpdateLargestPossibleRegion()
{
  this->Update();
}


template <typename TInputImage>
void
ImageSink<TInputImage>::Update()
{
  this->UpdateOutputInformation();
  // if output 1, the just call it
  // this->PropagateRequestedRegion( nullptr );
  this->UpdateOutputData(nullptr);
}


template <typename TInputImage>
void
ImageSink<TInputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "NumberOfStreamDivisions: " << this->m_NumberOfStreamDivisions << std::endl;
  os << indent << "RegionSplitter: " << this->m_RegionSplitter << std::endl;
  os << indent << "CoordinateTolerance: " << this->m_CoordinateTolerance << std::endl;
  os << indent << "DirectionTolerance: " << this->m_DirectionTolerance << std::endl;
}


template <typename TInputImage>
unsigned int
ImageSink<TInputImage>::GetNumberOfInputRequestedRegions()
{
  const InputImageType * inputPtr = const_cast<InputImageType *>(this->GetInput());
  InputImageRegionType   inputImageRegion = inputPtr->GetLargestPossibleRegion();

  return this->GetRegionSplitter()->GetNumberOfSplits(inputImageRegion, this->m_NumberOfStreamDivisions);
}


template <typename TInputImage>
void
ImageSink<TInputImage>::GenerateNthInputRequestedRegion(unsigned int inputRequestedRegionNumber)
{
  Superclass::GenerateInputRequestedRegion();

  auto *               inputPtr = const_cast<InputImageType *>(this->GetInput());
  InputImageRegionType inputImageRegion = inputPtr->GetLargestPossibleRegion();


  this->GetRegionSplitter()->GetSplit(
    inputRequestedRegionNumber, this->GetNumberOfInputRequestedRegions(), inputImageRegion);
  m_CurrentInputRegion = inputImageRegion;

  itkDebugMacro("Generating " << inputRequestedRegionNumber << " chunk as " << m_CurrentInputRegion);


  for (auto & inputName : this->GetInputNames())
  {
    if (this->ProcessObject::GetInput(inputName))
    {
      // Check whether the input is an image of the appropriate
      // dimension (use ProcessObject's version of the GetInput()
      // method since it returns the input as a pointer to a
      // DataObject as opposed to the subclass version which
      // static_casts the input to an TInputImage).
      using ImageBaseType = ImageBase<InputImageDimension>;
      auto * input = dynamic_cast<ImageBaseType *>(this->ProcessObject::GetInput(inputName));

      // If not an image, skip it. A subclass can override this method
      // for particular input types.
      if (input == nullptr)
      {
        continue;
      }
      // copy the requested region of the first input to the others
      InputImageRegionType inputRegion;
      input->SetRequestedRegion(m_CurrentInputRegion);
    }
  }
}


template <typename TInputImage>
void
ImageSink<TInputImage>::VerifyInputInformation() ITKv5_CONST
{
  using ImageBaseType = const ImageBase<InputImageDimension>;

  ImageBaseType *              inputPtr1 = nullptr;
  InputDataObjectConstIterator it(this);

  for (; !it.IsAtEnd(); ++it)
  {
    // Check whether the output is an image of the appropriate
    // dimension (use ProcessObject's version of the GetInput()
    // method since it returns the input as a pointer to a
    // DataObject as opposed to the subclass version which
    // static_casts the input to an TInputImage).
    inputPtr1 = dynamic_cast<ImageBaseType *>(it.GetInput());

    if (inputPtr1)
    {
      break;
    }
  }

  for (; !it.IsAtEnd(); ++it)
  {
    auto * inputPtrN = dynamic_cast<ImageBaseType *>(it.GetInput());

    // Physical space computation only matters if we're using two
    // images, and not an image and a constant.
    if (inputPtrN)
    {
      // check that the image occupy the same physical space, and that
      // each index is at the same physical location

      // tolerance for origin and spacing depends on the size of pixel
      // tolerance for directions a fraction of the unit cube.
      const SpacePrecisionType coordinateTol =
        std::abs(this->m_CoordinateTolerance * inputPtr1->GetSpacing()[0]); // use first dimension spacing

      if (!inputPtr1->GetOrigin().GetVnlVector().is_equal(inputPtrN->GetOrigin().GetVnlVector(), coordinateTol) ||
          !inputPtr1->GetSpacing().GetVnlVector().is_equal(inputPtrN->GetSpacing().GetVnlVector(), coordinateTol) ||
          !inputPtr1->GetDirection().GetVnlMatrix().as_ref().is_equal(inputPtrN->GetDirection().GetVnlMatrix().as_ref(),
                                                                      this->m_DirectionTolerance))
      {
        std::ostringstream originString, spacingString, directionString;
        if (!inputPtr1->GetOrigin().GetVnlVector().is_equal(inputPtrN->GetOrigin().GetVnlVector(), coordinateTol))
        {
          originString.setf(std::ios::scientific);
          originString.precision(7);
          originString << "InputImage Origin: " << inputPtr1->GetOrigin() << ", InputImage" << it.GetName()
                       << " Origin: " << inputPtrN->GetOrigin() << std::endl;
          originString << "\tTolerance: " << coordinateTol << std::endl;
        }
        if (!inputPtr1->GetSpacing().GetVnlVector().is_equal(inputPtrN->GetSpacing().GetVnlVector(), coordinateTol))
        {
          spacingString.setf(std::ios::scientific);
          spacingString.precision(7);
          spacingString << "InputImage Spacing: " << inputPtr1->GetSpacing() << ", InputImage" << it.GetName()
                        << " Spacing: " << inputPtrN->GetSpacing() << std::endl;
          spacingString << "\tTolerance: " << coordinateTol << std::endl;
        }
        if (!inputPtr1->GetDirection().GetVnlMatrix().as_ref().is_equal(
              inputPtrN->GetDirection().GetVnlMatrix().as_ref(), this->m_DirectionTolerance))
        {
          directionString.setf(std::ios::scientific);
          directionString.precision(7);
          directionString << "InputImage Direction: " << inputPtr1->GetDirection() << ", InputImage" << it.GetName()
                          << " Direction: " << inputPtrN->GetDirection() << std::endl;
          directionString << "\tTolerance: " << this->m_DirectionTolerance << std::endl;
        }
        itkExceptionMacro(<< "Inputs do not occupy the same physical space! " << std::endl
                          << originString.str() << spacingString.str() << directionString.str());
      }
    }
  }
}

template <typename TInputImage>
void
ImageSink<TInputImage>::StreamedGenerateData(unsigned int inputRequestedRegionNumber)
{

  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());

  // calculate the progress range for this streamed chunk
  const ThreadIdType  total = this->GetNumberOfInputRequestedRegions();
  const float         oldProgress = float(inputRequestedRegionNumber) / (total);
  const float         newProgress = float(inputRequestedRegionNumber + 1) / (total);
  ProgressTransformer pt(oldProgress, newProgress, this);


  this->GetMultiThreader()->template ParallelizeImageRegion<InputImageDimension>(
    this->m_CurrentInputRegion,
    [this](const InputImageRegionType & inputRegionForThread) {
      this->ThreadedStreamedGenerateData(inputRegionForThread);
    },
    pt.GetProcessObject());
}

} // namespace itk

#endif // itkImageSink_hxx
