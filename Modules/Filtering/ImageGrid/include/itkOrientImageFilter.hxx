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
#ifndef itkOrientImageFilter_hxx
#define itkOrientImageFilter_hxx

#include "itkImageRegionIterator.h"
#include "itkCastImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkMetaDataObject.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template <typename TInputImage, typename TOutputImage>
OrientImageFilter<TInputImage, TOutputImage>::OrientImageFilter()
  : m_FlipAxes(false)
{
  for (unsigned int dimension = 0; dimension < InputImageDimension; ++dimension)
  {
    this->m_PermuteOrder[dimension] = dimension;
  }
}

template <typename TInputImage, typename TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  const InputImagePointer  inputPtr = const_cast<TInputImage *>(this->GetInput());
  const OutputImagePointer outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  using PermuteFilterType = PermuteAxesImageFilter<InputImageType>;
  using FlipFilterType = FlipImageFilter<InputImageType>;
  using CastToOutputFilterType = CastImageFilter<InputImageType, OutputImageType>;

  auto permute = PermuteFilterType::New();
  auto flip = FlipFilterType::New();
  auto cast = CastToOutputFilterType::New();
  permute->SetInput(inputPtr);
  permute->SetOrder(m_PermuteOrder);

  flip->SetInput(permute->GetOutput());
  flip->SetFlipAxes(m_FlipAxes);
  flip->FlipAboutOriginOff();

  cast->SetInput(flip->GetOutput());
  cast->GetOutput()->SetRequestedRegion(outputPtr->GetRequestedRegion());

  // The input to the minipipeline is the input to this filter
  // minipipeline. Therefore, the requested region of the minipipeline
  // is the one needed by this filter.
  cast->GetOutput()->UpdateOutputInformation();
  cast->GetOutput()->PropagateRequestedRegion();
}

template <typename TInputImage, typename TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()->SetRequestedRegion(this->GetOutput()->GetLargestPossibleRegion());
}


template <typename TInputImage, typename TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>::VerifyPreconditions() const
{
  Superclass::VerifyPreconditions();

  if (this->m_DesiredCoordinateOrientation == AnatomicalOrientation::PositiveEnum::INVALID)
  {
    itkExceptionMacro(<< "DesiredCoordinateOrientation is 'INVALID'.");
  }
}


template <typename TInputImage, typename TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>::DeterminePermutationsAndFlips(
  const CoordinateOrientationCode fixed_orient,
  const CoordinateOrientationCode moving_orient)
{
  // 3-dimensional version of code system only. The 3-axis testing is unrolled.
  constexpr unsigned int NumDims = 3; // InputImageDimension is
                                      // regarded as 3.
  constexpr unsigned int CodeAxisIncreasingField = 1;
  auto                   fixed_codes = fixed_orient.GetTerms();
  auto                   moving_codes = moving_orient.GetTerms();


  // i, j, k will be the indexes in the Majorness code of the axes to flip;
  // they encode the axes as the reader will find them, 0 is the lowest order
  // axis of whatever spatial interpretation, and 2 is the highest order axis.
  //  Perhaps rename them moving_image_reader_axis_i, etc.

  for (unsigned int i = 0; i < NumDims - 1; ++i)
  {
    if (!AnatomicalOrientation::SameOrientationAxes(fixed_codes[i], moving_codes[i]))
    {
      for (unsigned int j = 0; j < NumDims; ++j)
      {
        if (AnatomicalOrientation::SameOrientationAxes(fixed_codes[j], moving_codes[i]))
        {
          if (i == j)
          {
            // Axis i is already in place
            continue;
          }
          if (AnatomicalOrientation::SameOrientationAxes(fixed_codes[i], moving_codes[j]))
          {
            // The cyclic permutation (i j) applies. Therefore, the remainder
            // is (k), i.e., stationary
            m_PermuteOrder[i] = j;
            m_PermuteOrder[j] = i;
          }
          else
          {
            // Need to work out an (i j k) cyclic permutation
            for (unsigned int k = 0; k < NumDims; ++k)
            {
              if (AnatomicalOrientation::SameOrientationAxes(moving_codes[j], fixed_codes[k]))
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

  for (unsigned int i = 0; i < NumDims; ++i)
  {
    const unsigned int j = m_PermuteOrder[i];
    if ((static_cast<uint8_t>(moving_codes[j]) & CodeAxisIncreasingField) !=
        (static_cast<uint8_t>(fixed_codes[i]) & CodeAxisIncreasingField))
    {
      m_FlipAxes[i] = true;
    }
  }
}

template <typename TInputImage, typename TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>::SetGivenCoordinateOrientation(CoordinateOrientationCode newCode)
{
  m_GivenCoordinateOrientation = newCode;

  for (unsigned int j = 0; j < InputImageDimension; ++j)
  {
    m_PermuteOrder[j] = j;
  }

  m_FlipAxes.Fill(false);

  this->DeterminePermutationsAndFlips(m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
}

template <typename TInputImage, typename TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>::SetDesiredCoordinateOrientation(CoordinateOrientationCode newCode)
{
  if (m_DesiredCoordinateOrientation != newCode)
  {
    m_DesiredCoordinateOrientation = newCode;

    for (unsigned int j = 0; j < InputImageDimension; ++j)
    {
      m_PermuteOrder[j] = j;
    }

    m_FlipAxes.Fill(false);

    this->DeterminePermutationsAndFlips(m_DesiredCoordinateOrientation, m_GivenCoordinateOrientation);
    this->Modified();
  }
}

template <typename TInputImage, typename TOutputImage>
bool
OrientImageFilter<TInputImage, TOutputImage>::NeedToPermute()
{
  for (unsigned int j = 0; j < InputImageDimension; ++j)
  {
    if (m_PermuteOrder[j] != j)
    {
      return true;
    }
  }
  return false;
}

template <typename TInputImage, typename TOutputImage>
bool
OrientImageFilter<TInputImage, TOutputImage>::NeedToFlip()
{
  for (unsigned int j = 0; j < InputImageDimension; ++j)
  {
    if (m_FlipAxes[j])
    {
      return true;
    }
  }
  return false;
}

// #define DefinedDebugOrient
#if defined(DefinedDebugOrient)
#  define DEBUG_EXECUTE(X) X

using SO_OrientationType = itk::SpatialOrientationEnums::ValidCoordinateOrientations;

std::string
SO_OrientationToString(SO_OrientationType in)
{
  switch (in)
  {
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_RIP:
      return std::string("RIP");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_LIP:
      return std::string("LIP");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_RSP:
      return std::string("RSP");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_LSP:
      return std::string("LSP");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_RIA:
      return std::string("RIA");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_LIA:
      return std::string("LIA");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_RSA:
      return std::string("RSA");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_LSA:
      return std::string("LSA");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_IRP:
      return std::string("IRP");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_ILP:
      return std::string("ILP");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_SRP:
      return std::string("SRP");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_SLP:
      return std::string("SLP");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_IRA:
      return std::string("IRA");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_ILA:
      return std::string("ILA");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_SRA:
      return std::string("SRA");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_SLA:
      return std::string("SLA");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_RPI:
      return std::string("RPI");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_LPI:
      return std::string("LPI");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_RAI:
      return std::string("RAI");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_LAI:
      return std::string("LAI");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_RPS:
      return std::string("RPS");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_LPS:
      return std::string("LPS");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_RAS:
      return std::string("RAS");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_LAS:
      return std::string("LAS");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_PRI:
      return std::string("PRI");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_PLI:
      return std::string("PLI");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_ARI:
      return std::string("ARI");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_ALI:
      return std::string("ALI");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_PRS:
      return std::string("PRS");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_PLS:
      return std::string("PLS");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_ARS:
      return std::string("ARS");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_ALS:
      return std::string("ALS");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_IPR:
      return std::string("IPR");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_SPR:
      return std::string("SPR");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_IAR:
      return std::string("IAR");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_SAR:
      return std::string("SAR");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_IPL:
      return std::string("IPL");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_SPL:
      return std::string("SPL");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_IAL:
      return std::string("IAL");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_SAL:
      return std::string("SAL");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_PIR:
      return std::string("PIR");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_PSR:
      return std::string("PSR");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_AIR:
      return std::string("AIR");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_ASR:
      return std::string("ASR");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_PIL:
      return std::string("PIL");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_PSL:
      return std::string("PSL");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_AIL:
      return std::string("AIL");
    case SO_OrientationType::ITK_COORDINATE_ORIENTATION_ASL:
      return "ASL";
    default:
    {
      std::stringstream x;
      x << (in & 0xff) << ", " << ((in >> 8) & 0xff) << ", " << ((in >> 16) && 0xff);
      return x.str();
    }
  }
}

template <typename ImageType>
void
DumpDirections(const std::string & prompt, const typename ImageType::Pointer & image)
{
  const typename ImageType::DirectionType & dir = image->GetDirection();
  std::cerr << prompt << ' ' << SO_OrientationToString(itk::SpatialOrientationAdapter().FromDirectionCosines(dir))
            << std::endl;
  for (unsigned int i = 0; i < 3; ++i)
  {
    for (unsigned int j = 0; j < 3; ++j)
    {
      std::cerr << dir[i][j] << ' ';
    }
    std::cerr << std::endl;
  }
}

#else // DefinedDebugOrient
#  define DEBUG_EXECUTE(X)
#endif

template <typename TInputImage, typename TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  auto progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);

  // No need to allocate the output since the minipipeline does it
  // this->AllocateOutputs();

  using PermuteFilterType = PermuteAxesImageFilter<InputImageType>;
  using FlipFilterType = FlipImageFilter<InputImageType>;
  using CastToOutputFilterType = CastImageFilter<InputImageType, OutputImageType>;

  auto permute = PermuteFilterType::New();
  auto flip = FlipFilterType::New();
  auto cast = CastToOutputFilterType::New();

  progress->RegisterInternalFilter(permute, .3333333f);
  progress->RegisterInternalFilter(flip, .3333333f);
  progress->RegisterInternalFilter(cast, .3333333f);

  const InputImagePointer permuteInput = const_cast<TInputImage *>(this->GetInput());
  InputImagePointer       flipInput = permuteInput;
  InputImagePointer       castInput = permuteInput;

  // Only run those filters that will do something
  if (NeedToPermute())
  {
    DEBUG_EXECUTE(DumpDirections<TInputImage>("before permute", permuteInput);)
    permute->SetInput(permuteInput);
    permute->SetOrder(m_PermuteOrder);
    permute->ReleaseDataFlagOn();
    DEBUG_EXECUTE(std::cerr << "Permute Axes: "; for (unsigned int i = 0; i < 3; ++i) {
      std::cerr << m_PermuteOrder[i] << ' ';
    } std::cerr << std::endl;
                  permute->Update();
                  DumpDirections<TInputImage>("after permute", permute->GetOutput());)
    flipInput = permute->GetOutput();
    castInput = permute->GetOutput();
  }
  else
  {
    itkDebugMacro("No need to permute");
  }
  if (NeedToFlip())
  {
    flip->SetInput(flipInput);
    flip->SetFlipAxes(m_FlipAxes);
    flip->FlipAboutOriginOff();
    flip->ReleaseDataFlagOn();
    DEBUG_EXECUTE(std::cerr << "Flip Axes: ";
                  for (unsigned int i = 0; i < 3; ++i) { std::cerr << m_FlipAxes[i] << ' '; } std::cerr << std::endl;
                  flip->Update();
                  DumpDirections<TInputImage>("after flip", flip->GetOutput());)
    castInput = flip->GetOutput();
  }
  else
  {
    itkDebugMacro("No need to flip");
  }

  //
  // Cast might not be necessary, but CastImageFilter is optimized for
  // the case where the InputImageType == OutputImageType
  cast->SetInput(castInput);
  cast->GetOutput()->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
  cast->Update();
  this->GraftOutput(cast->GetOutput());

  this->GetOutput()->SetMetaDataDictionary(this->GetInput()->GetMetaDataDictionary());
}

template <typename TInputImage, typename TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  const InputImageConstPointer inputPtr = this->GetInput();
  const OutputImagePointer     outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  // Either use the direction cosines of the image or the user-specified
  // orientation
  if (m_UseImageDirection)
  {
    // Compute the GivenOrientation from the image's direction cosines
    this->SetGivenCoordinateOrientation(AnatomicalOrientation(inputPtr->GetDirection()));
  }

  using PermuteFilterType = PermuteAxesImageFilter<InputImageType>;
  using FlipFilterType = FlipImageFilter<InputImageType>;
  using CastToOutputFilterType = CastImageFilter<InputImageType, OutputImageType>;

  auto permute = PermuteFilterType::New();
  auto flip = FlipFilterType::New();
  auto cast = CastToOutputFilterType::New();
  permute->SetInput(inputPtr);
  permute->SetOrder(m_PermuteOrder);

  flip->SetInput(permute->GetOutput());
  flip->SetFlipAxes(m_FlipAxes);
  flip->FlipAboutOriginOff();

  cast->SetInput(flip->GetOutput());
  cast->UpdateOutputInformation();

  outputPtr->CopyInformation(cast->GetOutput());
}

template <typename TInputImage, typename TOutputImage>
void
OrientImageFilter<TInputImage, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Desired Coordinate Orientation: " << this->GetDesiredCoordinateOrientation() << std::endl;
  os << indent << "Given Coordinate Orientation: " << this->GetGivenCoordinateOrientation() << std::endl;
  os << indent << "Use Image Direction: " << m_UseImageDirection << std::endl;
  os << indent << "Permute Axes: " << m_PermuteOrder << std::endl;
  os << indent << "Flip Axes: " << m_FlipAxes << std::endl;
}
} // end namespace itk
#endif
