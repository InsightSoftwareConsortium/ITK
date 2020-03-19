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
#ifndef itkDICOMOrientImageFilter_hxx
#define itkDICOMOrientImageFilter_hxx

#include "itkDICOMOrientImageFilter.h"
#include "itkPermuteAxesImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{

template <typename TInputImage>
DICOMOrientImageFilter<TInputImage>::DICOMOrientImageFilter()
{

  for (unsigned int dimension = 0; dimension < ImageDimension; ++dimension)
  {
    this->m_PermuteOrder[dimension] = dimension;
  }
}

template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  typename ImageType::Pointer inputPtr = const_cast<ImageType *>(this->GetInput());
  if (inputPtr)
  {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
  }
}


template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::DeterminePermutationsAndFlips(DICOMOrientation desired, DICOMOrientation given)
{
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    m_PermuteOrder[j] = j;
  }

  m_FlipAxes.Fill(false);

  constexpr unsigned int CodeAxisDir = 0x1;

  const DICOMOrientation::CoordinateEnum desired_codes[] = { desired.GetPrimaryTerm(),
                                                             desired.GetSecondaryTerm(),
                                                             desired.GetTertiaryTerm() };
  const DICOMOrientation::CoordinateEnum given_codes[] = { given.GetPrimaryTerm(),
                                                           given.GetSecondaryTerm(),
                                                           given.GetTertiaryTerm() };

  // i, j, k will be the indexes in the Majorness code of the axes to flip;
  // they encode the axes as the reader will find them, 0 is the lowest order
  // axis of whatever spatial interpretation, and 2 is the highest order axis.
  //  Perhaps rename them moving_image_reader_axis_i, etc.

  for (unsigned int i = 0; i < ImageDimension - 1; i++)
  {
    if (!DICOMOrientation::SameOrientationAxes(given_codes[i], desired_codes[i]))
    {
      for (unsigned int j = 0; j < ImageDimension; j++)
      {
        if (DICOMOrientation::SameOrientationAxes(given_codes[i], desired_codes[j]))
        {
          if (i == j)
          {
            // Axis i is already in place
            continue;
          }
          if (DICOMOrientation::SameOrientationAxes(given_codes[j], desired_codes[i]))
          {
            // The cyclic permutation (i j) applies. Therefore the remainder
            // is (k), i.e., stationary
            m_PermuteOrder[i] = j;
            m_PermuteOrder[j] = i;
          }
          else
          {
            // work out an (i j k) cyclic permutation
            for (unsigned int k = 0; k < ImageDimension; k++)
            {

              if (DICOMOrientation::SameOrientationAxes(given_codes[j], desired_codes[k]))
              {
                // At this point, we can pick off (i j k)
                m_PermuteOrder[i] = k;
                m_PermuteOrder[j] = i;
                m_PermuteOrder[k] = j;
                break;
              }
            }
            // Effectively, if (k==3) continue
          }
          break;
        }
      }
      // Effectively, if (j==3) continue
    }
  }

  for (unsigned int i = 0; i < ImageDimension; i++)
  {
    const unsigned int j = m_PermuteOrder[i];
    if ((static_cast<uint8_t>(given_codes[j]) & CodeAxisDir) != (static_cast<uint8_t>(desired_codes[i]) & CodeAxisDir))
    {
      m_FlipAxes[i] = true;
    }
  }
}

template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::SetInputCoordinateOrientation(OrientationEnum newCode)
{
  m_InputCoordinateOrientation = newCode;

  this->DeterminePermutationsAndFlips(m_DesiredCoordinateOrientation, m_InputCoordinateOrientation);
}

template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::SetDesiredCoordinateOrientation(OrientationEnum newCode)
{
  if (m_DesiredCoordinateOrientation != newCode)
  {
    m_DesiredCoordinateOrientation = newCode;
    this->DeterminePermutationsAndFlips(m_DesiredCoordinateOrientation, m_InputCoordinateOrientation);
    this->Modified();
  }
}


template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::SetDesiredCoordinateOrientation(const std::string & desired)
{
  DICOMOrientation o(desired);

  if ( OrientationEnum(o) == OrientationEnum::INVALID)
  {
    itkWarningMacro("Invalid desired coordinate direction string: \"" << desired << "\"!");
  }
  this->SetDesiredCoordinateOrientation(o);
}

template <typename TInputImage>
bool
DICOMOrientImageFilter<TInputImage>::NeedToPermute()
{
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    if (m_PermuteOrder[j] != j)
    {
      return true;
    }
  }
  return false;
}

template <typename TInputImage>
bool
DICOMOrientImageFilter<TInputImage>::NeedToFlip()
{
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    if (m_FlipAxes[j])
    {
      return true;
    }
  }
  return false;
}

template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  typename ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // No need to allocate the output since the minipipeline does it
  // this->AllocateOutputs();

  using PermuteFilterType = PermuteAxesImageFilter<ImageType>;
  using FlipFilterType = FlipImageFilter<ImageType>;
  using CastToOutputFilterType = CastImageFilter<ImageType, ImageType>;

  typename PermuteFilterType::Pointer      permute = PermuteFilterType::New();
  typename FlipFilterType::Pointer         flip = FlipFilterType::New();
  typename CastToOutputFilterType::Pointer cast = CastToOutputFilterType::New();

  progress->RegisterInternalFilter(permute, .45f);
  progress->RegisterInternalFilter(flip, .45);
  progress->RegisterInternalFilter(cast, .1f);

  typename ImageType::Pointer inputPtr = ImageType::New();
  inputPtr->Graft(const_cast<ImageType *>(this->GetInput()));

  typename ImageType::Pointer nextInput = inputPtr;

  // Only run those filters that will do something
  if (NeedToPermute())
  {
    permute->SetInput(nextInput);
    permute->SetOrder(m_PermuteOrder);
    permute->ReleaseDataFlagOn();
    nextInput = permute->GetOutput();
  }
  else
  {
    itkDebugMacro("No need to permute");
  }
  if (NeedToFlip())
  {
    flip->SetInput(nextInput);
    flip->SetFlipAxes(m_FlipAxes);
    flip->FlipAboutOriginOff();
    nextInput = flip->GetOutput();
  }
  else
  {
    itkDebugMacro(<< "No need to flip");
  }

  //
  // Cast might not be necessary, but CastImagefilter is optimized for
  // the case where the ImageType == OutputImageType
  cast->SetInput(nextInput);
  cast->GraftOutput(this->GetOutput());
  cast->Update();

  this->GraftOutput(cast->GetOutput());
}

template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output

  typename ImageType::Pointer inputPtr = ImageType::New();
  inputPtr->Graft(const_cast<ImageType *>(this->GetInput()));
  ImageType * outputPtr = this->GetOutput();


  // Compute the GivenOrientation from the image's direction cosines
  const DICOMOrientation inputOrientation(inputPtr->GetDirection());
  this->SetInputCoordinateOrientation(inputOrientation.GetAsOrientation());

  using PermuteFilterType = PermuteAxesImageFilter<ImageType>;
  using FlipFilterType = FlipImageFilter<ImageType>;

  typename PermuteFilterType::Pointer permute = PermuteFilterType::New();
  typename FlipFilterType::Pointer    flip = FlipFilterType::New();
  permute->SetInput(inputPtr);
  permute->SetOrder(m_PermuteOrder);

  flip->SetInput(permute->GetOutput());
  flip->SetFlipAxes(m_FlipAxes);
  flip->FlipAboutOriginOff();

  flip->GraftOutput(this->GetOutput());
  flip->UpdateOutputInformation();

  outputPtr->CopyInformation(flip->GetOutput());
}


template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::VerifyPreconditions() ITKv5_CONST
{
  Superclass::VerifyPreconditions();

  if (this->m_DesiredCoordinateOrientation == OrientationEnum::INVALID)
  {
    itkExceptionMacro(<<"DesiredCoordinateOrientation is 'INVALID'.")
  }
}

template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Desired Coordinate Orientation: " << static_cast<long>(this->GetDesiredCoordinateOrientation())
     << " (" << this->GetDesiredCoordinateOrientation() << ")" << std::endl;
  os << indent << "Input Coordinate Orientation: " << static_cast<long>(this->GetInputCoordinateOrientation()) << " ("
     << this->GetInputCoordinateOrientation() << ")" << std::endl;
  os << indent << "Permute Axes: " << m_PermuteOrder << std::endl;
  os << indent << "Flip Axes: " << m_FlipAxes << std::endl;
}
} // end namespace itk
#endif
