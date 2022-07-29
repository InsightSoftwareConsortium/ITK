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
  // Map between axis string labels and spatial orientation
  m_StringToCode["RIP"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP;
  m_StringToCode["LIP"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIP;
  m_StringToCode["RSP"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSP;
  m_StringToCode["LSP"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSP;
  m_StringToCode["RIA"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIA;
  m_StringToCode["LIA"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIA;
  m_StringToCode["RSA"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSA;
  m_StringToCode["LSA"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSA;
  m_StringToCode["IRP"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRP;
  m_StringToCode["ILP"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILP;
  m_StringToCode["SRP"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRP;
  m_StringToCode["SLP"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLP;
  m_StringToCode["IRA"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRA;
  m_StringToCode["ILA"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILA;
  m_StringToCode["SRA"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRA;
  m_StringToCode["SLA"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLA;
  m_StringToCode["RPI"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPI;
  m_StringToCode["LPI"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPI;
  m_StringToCode["RAI"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI;
  m_StringToCode["LAI"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAI;
  m_StringToCode["RPS"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPS;
  m_StringToCode["LPS"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPS;
  m_StringToCode["RAS"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAS;
  m_StringToCode["LAS"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAS;
  m_StringToCode["PRI"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRI;
  m_StringToCode["PLI"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLI;
  m_StringToCode["ARI"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARI;
  m_StringToCode["ALI"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALI;
  m_StringToCode["PRS"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRS;
  m_StringToCode["PLS"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLS;
  m_StringToCode["ARS"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARS;
  m_StringToCode["ALS"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALS;
  m_StringToCode["IPR"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPR;
  m_StringToCode["SPR"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPR;
  m_StringToCode["IAR"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAR;
  m_StringToCode["SAR"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAR;
  m_StringToCode["IPL"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPL;
  m_StringToCode["SPL"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPL;
  m_StringToCode["IAL"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAL;
  m_StringToCode["SAL"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAL;
  m_StringToCode["PIR"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIR;
  m_StringToCode["PSR"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSR;
  m_StringToCode["AIR"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIR;
  m_StringToCode["ASR"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASR;
  m_StringToCode["PIL"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIL;
  m_StringToCode["PSL"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSL;
  m_StringToCode["AIL"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIL;
  m_StringToCode["ASL"] = SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASL;

  // Map between spatial orientation and axis string labels
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP] = "RIP";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIP] = "LIP";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSP] = "RSP";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSP] = "LSP";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIA] = "RIA";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIA] = "LIA";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSA] = "RSA";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSA] = "LSA";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRP] = "IRP";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILP] = "ILP";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRP] = "SRP";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLP] = "SLP";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRA] = "IRA";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILA] = "ILA";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRA] = "SRA";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLA] = "SLA";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPI] = "RPI";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPI] = "LPI";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI] = "RAI";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAI] = "LAI";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPS] = "RPS";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPS] = "LPS";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAS] = "RAS";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAS] = "LAS";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRI] = "PRI";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLI] = "PLI";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARI] = "ARI";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALI] = "ALI";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRS] = "PRS";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLS] = "PLS";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARS] = "ARS";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALS] = "ALS";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPR] = "IPR";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPR] = "SPR";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAR] = "IAR";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAR] = "SAR";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPL] = "IPL";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPL] = "SPL";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAL] = "IAL";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAL] = "SAL";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIR] = "PIR";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSR] = "PSR";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIR] = "AIR";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASR] = "ASR";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIL] = "PIL";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSL] = "PSL";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIL] = "AIL";
  m_CodeToString[SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASL] = "ASL";

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
  InputImagePointer  inputPtr = const_cast<TInputImage *>(this->GetInput());
  OutputImagePointer outputPtr = this->GetOutput();

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
OrientImageFilter<TInputImage, TOutputImage>::DeterminePermutationsAndFlips(
  const SpatialOrientationEnums::ValidCoordinateOrientations fixed_orient,
  const SpatialOrientationEnums::ValidCoordinateOrientations moving_orient)
{
  // 3-dimensional version of code system only. The 3-axis testing is unrolled.
  constexpr unsigned int NumDims = 3;        // InputImageDimension is
                                             // regarded as 3.
  constexpr unsigned int CodeField = 15;     // 4 bits wide
  constexpr unsigned int CodeAxisField = 14; // 3 bits wide, above the
                                             // 0-place bit.
  constexpr unsigned int CodeAxisIncreasingField = 1;
  uint32_t               fixed_codes[NumDims];
  uint32_t               moving_codes[NumDims];

  fixed_codes[0] =
    (static_cast<uint32_t>(fixed_orient) >>
     static_cast<uint32_t>(SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) &
    CodeField;
  fixed_codes[1] =
    (static_cast<uint32_t>(fixed_orient) >>
     static_cast<uint32_t>(SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) &
    CodeField;
  fixed_codes[2] =
    (static_cast<uint32_t>(fixed_orient) >>
     static_cast<uint32_t>(SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)) &
    CodeField;
  moving_codes[0] =
    (static_cast<uint32_t>(moving_orient) >>
     static_cast<uint32_t>(SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor)) &
    CodeField;
  moving_codes[1] =
    (static_cast<uint32_t>(moving_orient) >>
     static_cast<uint32_t>(SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor)) &
    CodeField;
  moving_codes[2] =
    (static_cast<uint32_t>(moving_orient) >>
     static_cast<uint32_t>(SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor)) &
    CodeField;

  // i, j, k will be the indexes in the Majorness code of the axes to flip;
  // they encode the axes as the reader will find them, 0 is the lowest order
  // axis of whatever spatial interpretation, and 2 is the highest order axis.
  //  Perhaps rename them moving_image_reader_axis_i, etc.

  for (unsigned int i = 0; i < NumDims - 1; ++i)
  {
    if ((fixed_codes[i] & CodeAxisField) != (moving_codes[i] & CodeAxisField))
    {
      for (unsigned int j = 0; j < NumDims; ++j)
      {
        if ((moving_codes[i] & CodeAxisField) == (fixed_codes[j] & CodeAxisField))
        {
          if (i == j)
          {
            // Axis i is already in place
            continue;
          }
          else if ((moving_codes[j] & CodeAxisField) == (fixed_codes[i] & CodeAxisField))
          {
            // The cyclic permutation (i j) applies. Therefore the remainder
            // is (k), i.e., stationary
            m_PermuteOrder[i] = j;
            m_PermuteOrder[j] = i;
          }
          else
          {
            // Need to work out an (i j k) cyclic permutation
            for (unsigned int k = 0; k < NumDims; ++k)
            {
              if ((moving_codes[j] & CodeAxisField) == (fixed_codes[k] & CodeAxisField))
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
    if ((moving_codes[j] & CodeAxisIncreasingField) != (fixed_codes[i] & CodeAxisIncreasingField))
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

//#define DefinedDebugOrient
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
  std::cerr << prompt << " " << SO_OrientationToString(itk::SpatialOrientationAdapter().FromDirectionCosines(dir))
            << std::endl;
  for (unsigned int i = 0; i < 3; ++i)
  {
    for (unsigned int j = 0; j < 3; ++j)
    {
      std::cerr << dir[i][j] << " ";
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

  InputImagePointer permuteInput = const_cast<TInputImage *>(this->GetInput());
  InputImagePointer flipInput = permuteInput;
  InputImagePointer castInput = permuteInput;

  // Only run those filters that will do something
  if (NeedToPermute())
  {
    DEBUG_EXECUTE(DumpDirections<TInputImage>("before permute", permuteInput);)
    permute->SetInput(permuteInput);
    permute->SetOrder(m_PermuteOrder);
    permute->ReleaseDataFlagOn();
    DEBUG_EXECUTE(std::cerr << "Permute Axes: "; for (unsigned int i = 0; i < 3; ++i) {
      std::cerr << m_PermuteOrder[i] << " ";
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
                  for (unsigned int i = 0; i < 3; ++i) { std::cerr << m_FlipAxes[i] << " "; } std::cerr << std::endl;
                  flip->Update();
                  DumpDirections<TInputImage>("after flip", flip->GetOutput());)
    castInput = flip->GetOutput();
  }
  else
  {
    itkDebugMacro(<< "No need to flip");
  }

  //
  // Cast might not be necessary, but CastImagefilter is optimized for
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
  InputImageConstPointer inputPtr = this->GetInput();
  OutputImagePointer     outputPtr = this->GetOutput();

  if (!inputPtr || !outputPtr)
  {
    return;
  }

  // Either use the direction cosines of the image or the user-specified
  // orientation
  if (m_UseImageDirection)
  {
    // Compute the GivenOrientation from the image's direction cosines
    this->SetGivenCoordinateOrientation(SpatialOrientationAdapter().FromDirectionCosines(inputPtr->GetDirection()));
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
  std::map<CoordinateOrientationCode, std::string>::const_iterator axes;

  axes = m_CodeToString.find(m_DesiredCoordinateOrientation);
  os << indent << "Desired Coordinate Orientation: " << static_cast<long>(this->GetDesiredCoordinateOrientation())
     << " (" << axes->second << ")" << std::endl;
  axes = m_CodeToString.find(m_GivenCoordinateOrientation);
  os << indent << "Given Coordinate Orientation: " << static_cast<long>(this->GetGivenCoordinateOrientation()) << " ("
     << axes->second << ")" << std::endl;
  os << indent << "Use Image Direction: " << m_UseImageDirection << std::endl;
  os << indent << "Permute Axes: " << m_PermuteOrder << std::endl;
  os << indent << "Flip Axes: " << m_FlipAxes << std::endl;
}
} // end namespace itk
#endif
