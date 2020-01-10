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
#include "itkSpatialOrientationAdapter.h"

namespace itk
{
template <typename TInputImage>
DICOMOrientImageFilter<TInputImage>::DICOMOrientImageFilter()
  : m_FlipAxes(false)
{

  // Map between axis string labels and SpatialOrientation
  helperAddCode("RIP", OrientationEnum::RIP);
  helperAddCode("LIP", OrientationEnum::LIP);
  helperAddCode("RSP", OrientationEnum::RSP);
  helperAddCode("LSP", OrientationEnum::LSP);
  helperAddCode("RIA", OrientationEnum::RIA);
  helperAddCode("LIA", OrientationEnum::LIA);
  helperAddCode("RSA", OrientationEnum::RSA);
  helperAddCode("LSA", OrientationEnum::LSA);
  helperAddCode("IRP", OrientationEnum::IRP);
  helperAddCode("ILP", OrientationEnum::ILP);
  helperAddCode("SRP", OrientationEnum::SRP);
  helperAddCode("SLP", OrientationEnum::SLP);
  helperAddCode("IRA", OrientationEnum::IRA);
  helperAddCode("ILA", OrientationEnum::ILA);
  helperAddCode("SRA", OrientationEnum::SRA);
  helperAddCode("SLA", OrientationEnum::SLA);
  helperAddCode("RPI", OrientationEnum::RPI);
  helperAddCode("LPI", OrientationEnum::LPI);
  helperAddCode("RAI", OrientationEnum::RAI);
  helperAddCode("LAI", OrientationEnum::LAI);
  helperAddCode("RPS", OrientationEnum::RPS);
  helperAddCode("LPS", OrientationEnum::LPS);
  helperAddCode("RAS", OrientationEnum::RAS);
  helperAddCode("LAS", OrientationEnum::LAS);
  helperAddCode("PRI", OrientationEnum::PRI);
  helperAddCode("PLI", OrientationEnum::PLI);
  helperAddCode("ARI", OrientationEnum::ARI);
  helperAddCode("ALI", OrientationEnum::ALI);
  helperAddCode("PRS", OrientationEnum::PRS);
  helperAddCode("PLS", OrientationEnum::PLS);
  helperAddCode("ARS", OrientationEnum::ARS);
  helperAddCode("ALS", OrientationEnum::ALS);
  helperAddCode("IPR", OrientationEnum::IPR);
  helperAddCode("SPR", OrientationEnum::SPR);
  helperAddCode("IAR", OrientationEnum::IAR);
  helperAddCode("SAR", OrientationEnum::SAR);
  helperAddCode("IPL", OrientationEnum::IPL);
  helperAddCode("SPL", OrientationEnum::SPL);
  helperAddCode("IAL", OrientationEnum::IAL);
  helperAddCode("SAL", OrientationEnum::SAL);
  helperAddCode("PIR", OrientationEnum::PIR);
  helperAddCode("PSR", OrientationEnum::PSR);
  helperAddCode("AIR", OrientationEnum::AIR);
  helperAddCode("ASR", OrientationEnum::ASR);
  helperAddCode("PIL", OrientationEnum::PIL);
  helperAddCode("PSL", OrientationEnum::PSL);
  helperAddCode("AIL", OrientationEnum::AIL);
  helperAddCode("ASL", OrientationEnum::ASL);


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
typename DICOMOrientImageFilter<TInputImage>::OrientationEnum
DICOMOrientImageFilter<TInputImage>::DirectionCosinesToOrientation(const DirectionType & dir)
{
  // NOTE: This method was based off of itk::SpatialObjectAdaptor::FromDirectionCosines
  // but it is DIFFERENT in the meaning of direction in terms of sign-ness.
  CoordinateEnum terms[3] = { CoordinateEnum::UNKNOWN, CoordinateEnum::UNKNOWN, CoordinateEnum::UNKNOWN };

  for (unsigned i = 0; i < 3; i++)
  {

    const unsigned dominant_axis = Function::Max3(dir[0][i], dir[1][i], dir[2][i]);

    const unsigned dominate_sgn = Math::sgn(dir[dominant_axis][i]);

    switch (dominant_axis)
    {
      case 0: {
        // When the dominate axis sign is positive, assign the coordinate for the direction we are increasing towards.
        // ITK is in LPS, so that is the positive direction
        terms[i] = (dominate_sgn == 1) ? CoordinateEnum::Left : CoordinateEnum::Right;
        break;
      }
      case 1: {
        terms[i] = (dominate_sgn == 1) ? CoordinateEnum::Posterior : CoordinateEnum::Anterior;
        break;
      }
      case 2: {
        terms[i] = (dominate_sgn == 1) ? CoordinateEnum::Superior : CoordinateEnum::Inferior;
        break;
      }
      default:
        itkGenericExceptionMacro("Unexpected Axis")
    }
  }

  return static_cast<OrientationEnum>(toOrientation(terms[0], terms[1], terms[2]));
}

template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::DeterminePermutationsAndFlips(const OrientationEnum desired,
                                                                   const OrientationEnum given)
{
  for (unsigned int j = 0; j < ImageDimension; j++)
  {
    m_PermuteOrder[j] = j;
  }

  m_FlipAxes.Fill(false);


  // 3-dimensional version of code system only. The 3-axis testing is unrolled.
  constexpr unsigned int NumDims = 3; // InputImageDimension is
                                      // regarded as 3.

  constexpr unsigned CodeField = 0xF;

  constexpr unsigned int CodeAxisField = 14; // 3 bits wide, above the
                                             // 0-place bit.
  constexpr unsigned int CodeAxisIncreasingField = 1;

  const unsigned int desired_codes[] = {
    (static_cast<unsigned int>(desired) >> static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) &
      CodeField,
    (static_cast<unsigned int>(desired) >> static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) &
      CodeField,
    (static_cast<unsigned int>(desired) >> static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor)) &
      CodeField
  };
  const unsigned int given_codes[] = {
    (static_cast<unsigned int>(given) >> static_cast<uint8_t>(CoordinateMajornessTermsEnum::PrimaryMinor)) & CodeField,
    (static_cast<unsigned int>(given) >> static_cast<uint8_t>(CoordinateMajornessTermsEnum::SecondaryMinor)) &
      CodeField,
    (static_cast<unsigned int>(given) >> static_cast<uint8_t>(CoordinateMajornessTermsEnum::TertiaryMinor)) & CodeField
  };

  // i, j, k will be the indexes in the Majorness code of the axes to flip;
  // they encode the axes as the reader will find them, 0 is the lowest order
  // axis of whatever spatial interpretation, and 2 is the highest order axis.
  //  Perhaps rename them moving_image_reader_axis_i, etc.

  for (unsigned int i = 0; i < NumDims - 1; i++)
  {
    if ((desired_codes[i] & CodeAxisField) != (given_codes[i] & CodeAxisField))
    {
      for (unsigned int j = 0; j < NumDims; j++)
      {
        if ((given_codes[i] & CodeAxisField) == (desired_codes[j] & CodeAxisField))
        {
          if (i == j)
          {
            // Axis i is already in place
            continue;
          }
          else if ((given_codes[j] & CodeAxisField) == (desired_codes[i] & CodeAxisField))
          {
            // The cyclic permutation (i j) applies. Therefore the remainder
            // is (k), i.e., stationary
            m_PermuteOrder[i] = j;
            m_PermuteOrder[j] = i;
          }
          else
          {
            // work out an (i j k) cyclic permutation
            for (unsigned int k = 0; k < NumDims; k++)
            {
              if ((given_codes[j] & CodeAxisField) == (desired_codes[k] & CodeAxisField))
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

  for (unsigned int i = 0; i < NumDims; i++)
  {
    const unsigned int j = m_PermuteOrder[i];
    if ((given_codes[j] & CodeAxisIncreasingField) != (desired_codes[i] & CodeAxisIncreasingField))
    {
      m_FlipAxes[i] = true;
    }
  }
}

template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::SetGivenCoordinateOrientation(OrientationEnum newCode)
{
  m_GivenCoordinateOrientation = newCode;

  this->DeterminePermutationsAndFlips(m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
}

template <typename TInputImage>
void
DICOMOrientImageFilter<TInputImage>::SetDesiredCoordinateOrientation(OrientationEnum newCode)
{
  if (m_DesiredCoordinateOrientation != newCode)
  {
    m_DesiredCoordinateOrientation = newCode;
    this->DeterminePermutationsAndFlips(m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
    this->Modified();
  }
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

//#define ITK_DEBUG_ORIENT
#if defined(ITK_DEBUG_ORIENT)
#  define DEBUG_EXECUTE(X) X

template <typename ImageType>
void
DumpDirections(const std::string & prompt, const typename ImageType::Pointer & image)
{
  const typename ImageType::DirectionType & dir = image->GetDirection();
  std::cerr << prompt << " " << SO_OrientationToString(itk::SpatialOrientationAdapter().FromDirectionCosines(dir))
            << std::endl;
  for (unsigned i = 0; i < 3; i++)
  {
    for (unsigned j = 0; j < 3; j++)
    {
      std::cerr << dir[i][j] << " ";
    }
    std::cerr << std::endl;
  }
}

#else // ITK_DEBUG_ORIENT
#  define DEBUG_EXECUTE(X)
#endif

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
    DEBUG_EXECUTE(DumpDirections<TInputImage>("before permute", permuteInput);)
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


  // Either use the direction cosines of the image or the user-specified
  // orientation
  if (m_UseImageDirection)
  {
    // Compute the GivenOrientation from the image's direction cosines
    this->SetGivenCoordinateOrientation(Self::DirectionCosinesToOrientation(inputPtr->GetDirection()));
  }

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
DICOMOrientImageFilter<TInputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  auto axes = m_CodeToString.find(m_DesiredCoordinateOrientation);
  os << indent << "Desired Coordinate Orientation: " << static_cast<long>(this->GetDesiredCoordinateOrientation())
     << " (" << (*axes).second << ")" << std::endl;
  axes = m_CodeToString.find(m_GivenCoordinateOrientation);
  os << indent << "Given Coordinate Orientation: " << static_cast<long>(this->GetGivenCoordinateOrientation()) << " ("
     << (*axes).second << ")" << std::endl;
  os << indent << "Use Image Direction: " << m_UseImageDirection << std::endl;
  os << indent << "Permute Axes: " << m_PermuteOrder << std::endl;
  os << indent << "Flip Axes: " << m_FlipAxes << std::endl;
}
} // end namespace itk
#endif
