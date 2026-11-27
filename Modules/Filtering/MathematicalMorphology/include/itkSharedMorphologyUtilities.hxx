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
#ifndef itkSharedMorphologyUtilities_hxx
#define itkSharedMorphologyUtilities_hxx

#include "itkImageRegionConstIterator.h"
#include "itkNeighborhoodAlgorithm.h"

namespace itk
{
/**
 * \class SharedMorphUtilities
 * \brief functionality in common for anchor and VanHerkGilWerman openings/closings and
 * erosions/dilation
 *
 */

template <typename TRegion, typename TLine>
bool
NeedToDoFace(const TRegion AllImage, const TRegion face, const TLine line)
{
  // can't use the continuous IsInside (even if I could get it to
  // work) because on the edge doesn't count as inside for this test

  // If the component of the vector orthogonal to the face doesn't go
  // inside the image then we can ignore the face

  // find the small dimension of the face - should only be one
  typename TRegion::IndexType ISt = AllImage.GetIndex();

  typename TRegion::SizeType  FSz = face.GetSize();
  typename TRegion::IndexType FSt = face.GetIndex();

  unsigned int smallDim = 0;
  for (unsigned int i = 0; i < AllImage.GetImageDimension(); ++i)
  {
    if (FSz[i] == 1)
    {
      smallDim = i;
      break;
    }
  }
  const IndexValueType startI = ISt[smallDim];
  const IndexValueType facePos = FSt[smallDim] + FSz[smallDim] - 1;
  if (facePos == startI)
  {
    // at the start of dimension - vector must be positive
    if (line[smallDim] > 0.000001)
    {
      return true;
    }
    // some small angle that we consider to be zero - should be more rigorous
  }
  else
  {
    // at the end of dimension - vector must be positive
    if (line[smallDim] < -0.000001)
    {
      return true;
    }
  }
  return false;
}

template <typename TImage, typename TBres, typename TLine>
int
ComputeStartEnd(const typename TImage::IndexType  StartIndex,
                const TLine                       line,
                const float                       tol,
                const typename TBres::OffsetArray LineOffsets,
                const typename TImage::RegionType AllImage,
                unsigned int &                    start,
                unsigned int &                    end)
{
  // compute intersection between ray and box
  typename TImage::IndexType ImStart = AllImage.GetIndex();
  typename TImage::SizeType  ImSize = AllImage.GetSize();
  float                      Tfar = NumericTraits<float>::max();
  float                      Tnear = NumericTraits<float>::NonpositiveMin();
  float                      domdir = NumericTraits<float>::NonpositiveMin();

  unsigned int perpdir = 0;
  for (unsigned int i = 0; i < TImage::RegionType::ImageDimension; ++i)
  {
    const auto abs_line_elmt_tmp = itk::Math::Absolute(line[i]);
    if (abs_line_elmt_tmp > domdir)
    {
      domdir = abs_line_elmt_tmp;
      perpdir = i;
    }
    if (abs_line_elmt_tmp > tol)
    {
      const float P1{ static_cast<float>(ImStart[i] - StartIndex[i]) };
      const float P2{ P1 + static_cast<float>(ImSize[i]) - 1.0F };
      float       T1 = P1 / line[i];
      float       T2 = P2 / line[i];

      if (T1 > T2)
      {
        // T1 is meant to be the near face
        std::swap(T1, T2);
      }
      // want the farthest Tnear and nearest TFar
      if (T1 > Tnear)
      {
        Tnear = T1;
      }
      if (T2 < Tfar)
      {
        Tfar = T2;
      }
    }
    else
    {
      // parallel to an axis - check for intersection at all
      if ((StartIndex[i] < ImStart[i]) || (StartIndex[i] > ImStart[i] + static_cast<int>(ImSize[i]) - 1))
      {
        // no intersection
        start = end = 0;
        return 0;
      }
    }
  }
  int sPos = static_cast<int>(Tnear * itk::Math::Absolute(line[perpdir]) + 0.5);
  int ePos = static_cast<int>(Tfar * itk::Math::Absolute(line[perpdir]) + 0.5);

  // std::cout << Tnear << ' ' << Tfar << std::endl;
  if (Tfar < Tnear) // seems to need some margin
  {
    // in theory, no intersection, but search between them
    bool         intersection = false;
    unsigned int inside = 0; // initialize to avoid warning
    if (Tnear - Tfar < 10)
    {
      //      std::cout << "Searching " << Tnear << ' ' << Tfar << std::endl;
      itkAssertInDebugAndIgnoreInReleaseMacro(ePos >= 0);
      itkAssertInDebugAndIgnoreInReleaseMacro(sPos < static_cast<int>(LineOffsets.size()));
      for (int i = ePos; i <= sPos; ++i)
      {
        if (AllImage.IsInside(StartIndex + LineOffsets[i]))
        {
          inside = i;
          intersection = true;
          break;
        }
      }
    }
    if (intersection)
    {
      //      std::cout << "Found intersection after all :: " << inside << std::endl;
      sPos = ePos = inside;
      itkAssertInDebugAndIgnoreInReleaseMacro(ePos + 1 >= 0);
      itkAssertInDebugAndIgnoreInReleaseMacro(ePos + 1 < static_cast<int>(LineOffsets.size()));
      while (AllImage.IsInside(StartIndex + LineOffsets[ePos + 1]))
      {
        ++ePos;
        itkAssertInDebugAndIgnoreInReleaseMacro(ePos + 1 >= 0);
        itkAssertInDebugAndIgnoreInReleaseMacro(ePos + 1 < static_cast<int>(LineOffsets.size()));
      }
      itkAssertInDebugAndIgnoreInReleaseMacro(sPos - 1 >= 0);
      itkAssertInDebugAndIgnoreInReleaseMacro(sPos - 1 < static_cast<int>(LineOffsets.size()));
      while (AllImage.IsInside(StartIndex + LineOffsets[sPos - 1]))
      {
        --sPos;
        itkAssertInDebugAndIgnoreInReleaseMacro(sPos - 1 >= 0);
        itkAssertInDebugAndIgnoreInReleaseMacro(sPos - 1 < static_cast<int>(LineOffsets.size()));
      }
      start = sPos;
      end = ePos;
    }
    else
    {
      //      std::cout << StartIndex << "No intersection" << std::endl;
      start = end = 0;
      return 0;
    }
  }
  else
  {
    itkAssertInDebugAndIgnoreInReleaseMacro(sPos >= 0);
    itkAssertInDebugAndIgnoreInReleaseMacro(sPos < static_cast<int>(LineOffsets.size()));
    if (AllImage.IsInside(StartIndex + LineOffsets[sPos]))
    {
      for (; sPos > 0;)
      {
        itkAssertInDebugAndIgnoreInReleaseMacro(sPos - 1 >= 0);
        itkAssertInDebugAndIgnoreInReleaseMacro(sPos - 1 < static_cast<int>(LineOffsets.size()));
        if (!AllImage.IsInside(StartIndex + LineOffsets[sPos - 1]))
        {
          break;
        }

        --sPos;
      }
    }
    else
    {
      for (; sPos < static_cast<int>(LineOffsets.size());)
      {
        itkAssertInDebugAndIgnoreInReleaseMacro(sPos >= 0);
        itkAssertInDebugAndIgnoreInReleaseMacro(sPos < static_cast<int>(LineOffsets.size()));
        ++sPos;
        if (!AllImage.IsInside(StartIndex + LineOffsets[sPos]))
        {
          ++sPos;
        }
        else
        {
          break;
        }
      }
    }
    if (AllImage.IsInside(StartIndex + LineOffsets[ePos]))
    {
      for (; ePos < static_cast<int>(LineOffsets.size());)
      {
        itkAssertInDebugAndIgnoreInReleaseMacro(ePos + 1 >= 0);
        itkAssertInDebugAndIgnoreInReleaseMacro(ePos + 1 < static_cast<int>(LineOffsets.size()));
        if (!AllImage.IsInside(StartIndex + LineOffsets[ePos + 1]))
        {
          break;
        }

        ++ePos;
      }
    }
    else
    {
      for (; ePos > 0;)
      {
        --ePos;
        itkAssertInDebugAndIgnoreInReleaseMacro(ePos >= 0);
        itkAssertInDebugAndIgnoreInReleaseMacro(ePos < static_cast<int>(LineOffsets.size()));
        if (!AllImage.IsInside(StartIndex + LineOffsets[ePos]))
        {
          --ePos;
        }
        else
        {
          break;
        }
      }
    }
  }
  start = sPos;
  end = ePos;
  return 1;
}

template <typename TImage, typename TBres>
void
CopyLineToImage(const typename TImage::Pointer            output,
                const typename TImage::IndexType          StartIndex,
                const typename TBres::OffsetArray         LineOffsets,
                std::vector<typename TImage::PixelType> & outbuffer,
                const unsigned int                        start,
                const unsigned int                        end)
{
  const unsigned int size = end - start + 1;

  for (unsigned int i = 0; i < size; ++i)
  {
    // itkAssertInDebugAndIgnoreInReleaseMacro(start + i >= 0);
    itkAssertInDebugAndIgnoreInReleaseMacro(start + i < LineOffsets.size());
    output->SetPixel(StartIndex + LineOffsets[start + i], outbuffer[i + 1]); // compat
  }
}

template <typename TInputImage, typename TLine>
typename TInputImage::RegionType
MakeEnlargedFace(const typename TInputImage::ConstPointer itkNotUsed(input),
                 const typename TInputImage::RegionType   AllImage,
                 const TLine                              line)
{
  // figure out the dominant direction of the line
  float        MaxComp = NumericTraits<float>::NonpositiveMin();
  unsigned int DomDir = 0;
  for (unsigned int i = 0; i < TInputImage::RegionType::ImageDimension; ++i)
  {
    const auto abs_line_elmt_tmp = itk::Math::Absolute(line[i]);
    if (abs_line_elmt_tmp > MaxComp)
    {
      MaxComp = abs_line_elmt_tmp;
      DomDir = i;
    }
  }

  // Select the face by DomDir, not by a size of 1: AllImage itself may have size 1 along other dimensions.
  typename TInputImage::RegionType RelevantRegion = AllImage;
  RelevantRegion.SetSize(DomDir, 1);
  bool foundFace = true;
  if (line[DomDir] < -0.000001)
  {
    RelevantRegion.SetIndex(DomDir,
                            AllImage.GetIndex(DomDir) + static_cast<IndexValueType>(AllImage.GetSize(DomDir)) - 1);
  }
  else if (line[DomDir] <= 0.000001)
  {
    foundFace = false;
  }
  if (foundFace)
  {
    // enlarge the region so that sweeping the line across it will
    // cause all pixels to be visited.
    const unsigned int NonFaceDim = DomDir;

    // figure out how much extra each other dimension needs to be extended
    typename TInputImage::SizeType  NewSize = RelevantRegion.GetSize();
    typename TInputImage::IndexType NewStart = RelevantRegion.GetIndex();
    const unsigned int              NonFaceLen = AllImage.GetSize()[NonFaceDim];
    for (unsigned int i = 0; i < TInputImage::RegionType::ImageDimension; ++i)
    {
      if (i != NonFaceDim)
      {
        auto Pad = Math::Ceil<int>(static_cast<float>(NonFaceLen) * line[i] / itk::Math::Absolute(line[NonFaceDim]));
        if (Pad < 0)
        {
          // just increase the size - no need to change the start
          NewSize[i] += itk::Math::Absolute(Pad) + 1;
        }
        else
        {
          // change the size and index
          NewSize[i] += Pad + 1;
          NewStart[i] -= Pad + 1;
        }
      }
    }
    RelevantRegion = { NewStart, NewSize };
  }
  else
  {
    std::cout << "Line " << line << " doesn't correspond to a face" << std::endl;
  }

  return RelevantRegion;
}

template <typename TImage, typename TBres, typename TLine>
int
FillLineBuffer(typename TImage::ConstPointer             input,
               const typename TImage::IndexType          StartIndex,
               const TLine                               line, // unit vector
               const float                               tol,
               const typename TBres::OffsetArray         LineOffsets,
               const typename TImage::RegionType         AllImage,
               std::vector<typename TImage::PixelType> & inbuffer,
               unsigned int &                            start,
               unsigned int &                            end)
{
  const int status = ComputeStartEnd<TImage, TBres, TLine>(StartIndex, line, tol, LineOffsets, AllImage, start, end);
  if (!status)
  {
    return status;
  }
  const unsigned int size = end - start + 1;
  // compat
  for (unsigned int i = 0; i < size; ++i)
  {
    itkAssertInDebugAndIgnoreInReleaseMacro(start + i < LineOffsets.size());
    inbuffer[i + 1] = input->GetPixel(StartIndex + LineOffsets[start + i]);
  }
  return 1;
}

template <typename TLine>
unsigned int
GetLinePixels(const TLine line)
{
  float N = line.GetNorm();
  float correction = 0.0;

  for (unsigned int i = 0; i < TLine::Dimension; ++i)
  {
    const float tt = itk::Math::Absolute(line[i] / N);
    if (tt > correction)
    {
      correction = tt;
    }
  }

  N *= correction;
  return static_cast<int>(N + 0.5);
}
} // namespace itk

#endif
