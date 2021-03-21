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
#ifndef itkContourExtractor2DImageFilter_hxx
#define itkContourExtractor2DImageFilter_hxx

#include <algorithm>

#include "itkConstantBoundaryImageNeighborhoodPixelAccessPolicy.h"
#include "itkContourExtractor2DImageFilter.h"
#include "itkMultiThreaderBase.h"
#include "itkShapedImageNeighborhoodRange.h"
#include "itkTotalProgressReporter.h"

namespace itk
{


template <typename TInputImage>
ContourExtractor2DImageFilter<TInputImage>::ContourExtractor2DImageFilter()
{
  this->m_ContourValue = NumericTraits<InputRealType>::ZeroValue();
  this->m_ReverseContourOrientation = false;
  this->m_VertexConnectHighPixels = false;
  this->m_LabelContours = false;
  this->m_UseCustomRegion = false;
  // We do not need to initialize this->m_RequestedRegion because m_UseCustomRegion == false.
  this->m_UnusedLabel = NumericTraits<InputPixelType>::min();
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::GenerateData()
{
  if (m_LabelContours) // each label has one or more contours
  {
    this->GenerateDataForLabels();
  }
  else // simple case of a single iso-value
  {
    const InputImageType * const input{ this->GetInput() };
    const InputRegionType        region{ input->GetRequestedRegion() };

    // Compute a shrunkRegion: we don't want the 3-by-3 SquareIterator
    // to be centered in the bottom row or right column because then
    // its lower-right 2-by-2 sub-square won't be wholly within the
    // region.
    const InputSizeType   shrunkSize{ { region.GetSize()[0] - 1, region.GetSize()[1] - 1 } };
    const InputRegionType shrunkRegion{ region.GetIndex(), shrunkSize };
    // Since we are comparing to m_ContourValue, we don't care what label is supplied
    // as the first argument of CreateSingleContour.
    const InputPixelType label{ NumericTraits<InputPixelType>::Zero };
    LabelsContainer      allLabels;
    allLabels.push_back(label);
    std::unordered_map<InputPixelType, ContourContainerType> labelsContoursOutput;
    labelsContoursOutput[label] = ContourContainerType{};
    this->CreateSingleContour(
      label, input, shrunkRegion, shrunkRegion.GetNumberOfPixels(), labelsContoursOutput[label]);
    FillOutputs(allLabels, labelsContoursOutput);
  }
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::CreateSingleContour(InputPixelType         label,
                                                                const InputImageType * input,
                                                                const InputRegionType  usableRegion,
                                                                SizeValueType          totalNumberOfPixels,
                                                                ContourContainerType & contoursOutput)
{
  TotalProgressReporter progress(this, totalNumberOfPixels);
  // ContourData is the working space of CreateSingleContour
  ContourData contourData;

  // Set up an iterator to "march the squares" across the image.
  // We associate each 2px-by-2px square with the pixel in the top left of
  // that square. We then iterate across the image, examining these 2x2 squares
  // and building the contour. By iterating the top-left pixel of our
  // "current square" across every pixel in the image except those on the
  // bottom row and rightmost column, we have visited every valid square in the
  // image.

  // A 1-pixel radius sets up a neighborhood with the following indices:
  // 0 1 2
  // 3 4 5
  // 6 7 8
  // We are interested only in the square of 4,5,7,8 which is the 2x2 square
  // with the center pixel at the top-left. So we only activate the
  // coresponding offsets, and only query pixels 4, 5, 7, and 8 with the
  // iterator's GetPixel method.

  const InputOffsetType none{ { 0, 0 } };
  const InputOffsetType right{ { 1, 0 } };
  const InputOffsetType down{ { 0, 1 } };
  const InputOffsetType diag{ { 1, 1 } };

  const std::array<InputOffsetType, 4> offsets{ { none, right, down, diag } };
  using Policy = itk::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<InputImageType>;
  using NeighborhoodRange = itk::ShapedImageNeighborhoodRange<const InputImageType, Policy>;
  NeighborhoodRange neighborhoodRange{ *input, InputIndexType(), offsets, m_UnusedLabel };

  for (const InputIndexType index : RegionIndexRange{ usableRegion })
  {
    neighborhoodRange.SetLocation(index);
    // There are sixteen different possible square types, diagramed below.
    // A + indicates that the vertex is above the contour value, and a -
    // indicates that the vertex is below or equal to the contour value.
    // The vertices of each square are here numbered:
    // 01
    // 23
    // and treated as a binary value with the bits in that order. Thus each
    // square can be so numbered:
    //  0--   1+-   2-+   3++   4--   5+-   6-+   7++
    //   --    --    --    --    +-    +-    +-    +-
    //
    //  8--   9+-  10-+  11++  12--  13+-  14-+  15++
    //   -+    -+    -+    -+    ++    ++    ++    ++
    //
    // The position of the line segment that cuts through (or doesn't, in case
    // 0 and 15) each square is clear, except in cases  6 and 9. In this case,
    // where the segments are placed is determined by
    // m_VertexConnectHighPixels. If m_VertexConnectHighPixels is false, then
    // lines like are drawn through square 6, and lines like are drawn through
    // square 9. Otherwise, the situation is reversed.
    // Finally, recall that we draw the lines so that (moving from tail to
    // head) the lower-valued pixels are on the left of the line. So, for
    // example, case 1 entails a line slanting from the middle of the top of
    // the square to the middle of the left side of the square.

    // (1) Determine what number square we are currently inspecting. Remember
    // that as far as the neighborhood iterator is concerned, our square
    // 01    is numbered as    45
    // 23                      78

    InputPixelType v0, v1, v2, v3;
    unsigned char  squareCase{ 0 };
    if (m_LabelContours)
    {
      v0 = (neighborhoodRange[0] == label);
      v1 = (neighborhoodRange[1] == label);
      v2 = (neighborhoodRange[2] == label);
      v3 = (neighborhoodRange[3] == label);
      squareCase = v0 + 2 * v1 + 4 * v2 + 8 * v3;
    }
    else
    {
      v0 = neighborhoodRange[0];
      v1 = neighborhoodRange[1];
      v2 = neighborhoodRange[2];
      v3 = neighborhoodRange[3];
      squareCase =
        (v0 > m_ContourValue) + 2 * (v1 > m_ContourValue) + 4 * (v2 > m_ContourValue) + 8 * (v3 > m_ContourValue);
    }

// Set up macros to find the ContinuousIndex where the contour intersects
// one of the sides of the square.  Normally macros should, of course, be
// eschewed, but since this is an inner loop not calling the function four
// times when two would do is probably worthwhile. Plus, copy-pasting
// these into the switch below is even worse.  InterpolateContourPosition
// takes the values at two vertices, the index of the first vertex, and the
// offset between the two vertices.
#define TOP_ this->InterpolateContourPosition(v0, v1, index, right)
#define BOTTOM_ this->InterpolateContourPosition(v2, v3, index + down, right)
#define LEFT_ this->InterpolateContourPosition(v0, v2, index, down)
#define RIGHT_ this->InterpolateContourPosition(v1, v3, index + right, down)

    // (2) Add line segments to the growing contours as defined by the cases.
    // AddSegment takes a "from" vertex and a "to" vertex, and adds it to the
    // a growing contour, creates a new contour, or merges two together.
    switch (squareCase)
    {
      case 0: // no line
        break;
      case 1: // top to left
        this->AddSegment(TOP_, LEFT_, contourData);
        break;
      case 2: // right to top
        this->AddSegment(RIGHT_, TOP_, contourData);
        break;
      case 3: // right to left
        this->AddSegment(RIGHT_, LEFT_, contourData);
        break;
      case 4: // left to bottom
        this->AddSegment(LEFT_, BOTTOM_, contourData);
        break;
      case 5: // top to bottom
        this->AddSegment(TOP_, BOTTOM_, contourData);
        break;
      case 6:
        if (m_VertexConnectHighPixels)
        {
          // left to top
          this->AddSegment(LEFT_, TOP_, contourData);
          // right to bottom
          this->AddSegment(RIGHT_, BOTTOM_, contourData);
        }
        else
        {
          // right to top
          this->AddSegment(RIGHT_, TOP_, contourData);
          // left to bottom
          this->AddSegment(LEFT_, BOTTOM_, contourData);
        }
        break;
      case 7: // right to bottom
        this->AddSegment(RIGHT_, BOTTOM_, contourData);
        break;
      case 8: // bottom to right
        this->AddSegment(BOTTOM_, RIGHT_, contourData);
        break;
      case 9:
        if (m_VertexConnectHighPixels)
        {
          // top to right
          this->AddSegment(TOP_, RIGHT_, contourData);
          // bottom to left
          this->AddSegment(BOTTOM_, LEFT_, contourData);
        }
        else
        {
          // top to left
          this->AddSegment(TOP_, LEFT_, contourData);
          // bottom to right
          this->AddSegment(BOTTOM_, RIGHT_, contourData);
        }
        break;
      case 10: // bottom to top
        this->AddSegment(BOTTOM_, TOP_, contourData);
        break;
      case 11: // bottom to left
        this->AddSegment(BOTTOM_, LEFT_, contourData);
        break;
      case 12: // left to right
        this->AddSegment(LEFT_, RIGHT_, contourData);
        break;
      case 13: // top to right
        this->AddSegment(TOP_, RIGHT_, contourData);
        break;
      case 14: // left to top
        this->AddSegment(LEFT_, TOP_, contourData);
        break;
      case 15: // no line
        break;
    } // switch squareCase
    progress.CompletedPixel();
  } // pixel square iteration

  // We have been working with our local contourData.m_Contours to minimize the
  // chance of false sharing among threads.  Now we risk the write to memory
  // locations that are likely near the corresponding memory locations for other
  // threads, because we have to collect this output somehow.
  contoursOutput = contourData.m_Contours;
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::GenerateDataForLabels()
{
  // We want to make use of ConstantBoundaryCondition, which works
  // with *images*, but use it with the input *region*.  So, we copy
  // the region to be its own image if necessary.
  InputImagePointer      inputGC{ nullptr };
  const InputImageType * input{ this->GetInput() };
  const InputRegionType  inputRegion{ input->GetRequestedRegion() };
  // We can remove this if statement and its contents once a Policy (e.g.,
  // itk::ConstantBoundaryImageNeighborhoodPixelAccessPolicy<InputImageType>) can support constant values outside of the
  // region (rather than merely outside of the image).
  if (input->GetLargestPossibleRegion() != inputRegion)
  {
    inputGC = InputImageType::New();
    inputGC->SetRegions(inputRegion);
    inputGC->Allocate();
    const RegionRange      inputRange{ *inputGC, inputRegion };
    const RegionConstRange originalRange{ *this->GetInput(), inputRegion };
    std::copy(originalRange.cbegin(), originalRange.cend(), inputRange.begin());
    input = static_cast<InputImageType *>(inputGC);
  }

  // Find all the distinct labels in the input region.
  const RegionConstRange inputRange{ *input, inputRegion };
  LabelsContainer        allLabels;
  {
    allLabels.assign(inputRange.cbegin(), inputRange.cend());
    std::sort(allLabels.begin(), allLabels.end());
    const LabelsIterator last{ std::unique(allLabels.begin(), allLabels.end()) };
    allLabels.erase(last, allLabels.end());
  }

  // Find an unused label
  m_UnusedLabel = NumericTraits<InputPixelType>::min();
  for (LabelsConstIterator checkedLabel{ allLabels.cbegin() };
       checkedLabel != allLabels.cend() && m_UnusedLabel == *checkedLabel;
       ++checkedLabel)
  {
    if /* constexpr */ (std::is_integral<InputPixelType>::value)
    {
      ++m_UnusedLabel;
    }
    else if /* constexpr */ (std::is_floating_point<InputPixelType>::value)
    {
      m_UnusedLabel = std::nextafter(m_UnusedLabel, NumericTraits<InputPixelType>::max());
    }
    else
    {
      itkAssertOrThrowMacro(false,
                            "Unsupported InputPixelType in ContourExtractor2DImageFilter(with LabelContours=true)");
    }
  }
  // The only failure case is that we wrapped around back to allLabels.front().
  itkAssertOrThrowMacro(m_UnusedLabel != allLabels.front(), "Need at least one unused value in the space of labels");

  // Compute bounding box for each label.  These will be [inclusive, inclusive]
  // ranges in each coordinate, not [inclusive, exclusive).  Then we will convert
  // them to regions with the usual conventions.  We will also create space for the
  // threads to return their contours.
  std::unordered_map<InputPixelType, InputRegionType>      labelsRegions;
  std::unordered_map<InputPixelType, ContourContainerType> labelsContoursOutput;
  SizeValueType                                            totalPixelCount{ 0 };
  {
    struct BoundingBoxType
    {
      InputIndexType min;
      InputIndexType max;
    };
    const InputIndexType left_top = inputRegion.GetIndex();
    const InputIndexType right_bot{
      { inputRegion.GetIndex()[0] + static_cast<IndexValueType>(inputRegion.GetSize()[0]) - 1,
        inputRegion.GetIndex()[1] + static_cast<IndexValueType>(inputRegion.GetSize()[1]) - 1 }
    };
    std::unordered_map<InputPixelType, BoundingBoxType> labelBoundingBoxes;
    for (InputPixelType label : allLabels)
    {
      labelBoundingBoxes[label] = BoundingBoxType{ right_bot, left_top };
    }
    // We use RegionConstIterator here instead of RegionRange because we want access to the GetIndex() method.
    RegionConstIterator inputIt{ input, inputRegion };
    for (inputIt.GoToBegin(); !inputIt.IsAtEnd(); ++inputIt)
    {
      BoundingBoxType & bbox = labelBoundingBoxes[inputIt.Get()];
      bbox.min[0] = std::min(bbox.min[0], inputIt.GetIndex()[0]);
      bbox.min[1] = std::min(bbox.min[1], inputIt.GetIndex()[1]);
      bbox.max[0] = std::max(bbox.max[0], inputIt.GetIndex()[0]);
      bbox.max[1] = std::max(bbox.max[1], inputIt.GetIndex()[1]);
    }
    // Build the extended regions from the bounding boxes
    for (InputPixelType label : allLabels)
    {
      const BoundingBoxType & bbox = labelBoundingBoxes[label];
      // Compute a extendedRegion that includes one-pixel border on all
      // sides. However, we don't want the 3-by-3 SquareIterator to be centered
      // in the bottom row or right column of this one-pixel-extended region
      // because then its lower-right 2-by-2 sub-square won't be wholly within
      // the region; for example, a label that exists at only one pixel location
      // will need its 3-by-3 SquareIterator to traverse 4 pixels rather than
      // all 9 pixels of the extended region.
      const InputIndexType  extendedIndex{ { bbox.min[0] - 1, bbox.min[1] - 1 } };
      const InputSizeType   extendedSize{ { static_cast<SizeValueType>(bbox.max[0] - bbox.min[0]) + 2,
                                          static_cast<SizeValueType>(bbox.max[1] - bbox.min[1]) + 2 } };
      const InputRegionType extendedRegion{ extendedIndex, extendedSize };
      totalPixelCount += extendedRegion.GetNumberOfPixels();
      labelsRegions[label] = extendedRegion;
      labelsContoursOutput[label] = ContourContainerType{};
    }
  }

  itk::MultiThreaderBase::Pointer mt = this->GetMultiThreader();
  mt->ParallelizeArray(
    0,
    allLabels.size(),
    [this, &allLabels, &input, &labelsRegions, totalPixelCount, &labelsContoursOutput](SizeValueType i) -> void {
      const InputPixelType label{ allLabels[i] };
      this->CreateSingleContour(label, input, labelsRegions[label], totalPixelCount, labelsContoursOutput[label]);
    },
    nullptr);

  FillOutputs(allLabels, labelsContoursOutput);
}


template <typename TInputImage>
inline typename ContourExtractor2DImageFilter<TInputImage>::VertexType
ContourExtractor2DImageFilter<TInputImage>::InterpolateContourPosition(InputPixelType  fromValue,
                                                                       InputPixelType  toValue,
                                                                       InputIndexType  fromIndex,
                                                                       InputOffsetType toOffset)
{
  // Now calculate the fraction of the way from 'from' to 'to' that the contour
  // crosses. Interpolate linearly: y = v0 + (v1 - v0) * x, and solve for the
  // x that gives y = m_ContourValue: x = (m_ContourValue - v0) / (v1 - v0).
  // This assumes that v0 and v1 are separated by exactly ONE unit. So the to
  // Offset. value must have exactly one component 1 and the other component 0.
  // Also this assumes that fromValue and toValue are different. Otherwise we
  // can't interpolate anything!
  itkAssertOrThrowMacro((fromValue != toValue), "source and destination are the same");

  itkAssertOrThrowMacro(((toOffset[0] == 0 && toOffset[1] == 1) || (toOffset[0] == 1 && toOffset[1] == 0)),
                        "toOffset has unexpected values");
  const double x{ m_LabelContours ? 0.5
                                  : (m_ContourValue - static_cast<InputRealType>(fromValue)) /
                                      (toValue - static_cast<InputRealType>(fromValue)) };
  VertexType   output;
  output[0] = fromIndex[0] + x * toOffset[0];
  output[1] = fromIndex[1] + x * toOffset[1];
  return output;
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::AddSegment(VertexType from, VertexType to, ContourData & contourData)
{
  if (from == to)
  {
    // Arc is degenerate: ignore, and the from/two point will be connected
    // later by other squares. Degeneracy happens when (and only when) a square
    // has exactly one vertex that is the contour value, and the rest are above
    // that value.
    return;
  }

  // Try to find an existing contour that starts where the new segment ends.
  const VertexToContourContainerIteratorMapIterator newTail(contourData.m_ContourStarts.find(to));
  // Try to find an existing contour that ends where the new segment starts.
  const VertexToContourContainerIteratorMapIterator newHead(contourData.m_ContourEnds.find(from));

  if (newTail != contourData.m_ContourStarts.end() && newHead != contourData.m_ContourEnds.end())
  {
    // We need to connect these two contours with the current arc. The act of
    // connecting the two contours will add the needed arc.
    const ContourContainerIterator tail(newTail->second);
    itkAssertOrThrowMacro((tail->front() == to), "End doesn't match Beginning");
    const ContourContainerIterator head(newHead->second);
    itkAssertOrThrowMacro((head->back() == from), "Beginning doesn't match End");
    if (head == tail)
    {
      // We've closed a contour. Add the end point, and remove from the maps
      head->push_back(to);
      contourData.m_ContourStarts.erase(newTail);
      // erase the front of tail. Because head and tail are the same contour,
      // don't worry about erasing the front of head!
      contourData.m_ContourEnds.erase(newHead); // erase the end of head/tail.
    }
    else
    {
      // We have found two distinct contours that need to be joined.  Careful
      // here: we want to keep the first segment in the list when merging so
      // that contours are always returned in top-to-bottom, right-to-left
      // order (with regard to the image pixel found to be inside the contour).
      if (tail->m_ContourNumber > head->m_ContourNumber)
      {
        // if tail was created later than head...
        // Copy tail to the end of head and remove
        // tail from everything.
        head->insert(head->end(), tail->begin(), tail->end());

        // Now remove 'tail' from the list and the maps because it has been
        // subsumed.
        contourData.m_ContourStarts.erase(newTail);
        const typename VertexToContourContainerIteratorMap::size_type erased{ contourData.m_ContourEnds.erase(
          tail->back()) };
        // There should be exactly one entry in the hash for that endpoint
        if (erased != 1)
        {
          itkWarningMacro(<< "There should be exactly one entry in the hash for that endpoint, but there are "
                          << erased);
        }
        contourData.m_Contours.erase(tail); // remove from the master list

        // Now remove the old end of 'head' from the ends map and add
        // the new end.
        contourData.m_ContourEnds.erase(newHead);
        contourData.m_ContourEnds.emplace(head->back(), head);
      }
      else
      {
        // Copy head to the beginning of tail and remove
        // head from everything.
        tail->insert(tail->begin(), head->begin(), head->end());

        // Now remove 'head' from the list and the maps because
        // it has been subsumed.
        contourData.m_ContourEnds.erase(newHead);
        const typename VertexToContourContainerIteratorMap::size_type erased{ contourData.m_ContourStarts.erase(
          head->front()) };
        if (erased != 1)
        {
          itkWarningMacro(<< "There should be exactly one entry in the hash for that endpoint, but there are "
                          << erased);
        }
        contourData.m_Contours.erase(head); // remove from the master list

        // Now remove the old start of 'tail' from the starts map and
        // add the new start.
        contourData.m_ContourStarts.erase(newTail);
        contourData.m_ContourStarts.emplace(tail->front(), tail);
      }
    }
  }
  else if (newTail == contourData.m_ContourStarts.end() && newHead == contourData.m_ContourEnds.end())
  {
    // No contours found: add a new one.
    // Make it on the heap. It will be copied into contours.
    ContourType contour;

    // Add the endpoints
    contour.push_front(from);
    contour.push_back(to);
    contour.m_ContourNumber = contourData.m_NumberOfContoursCreated++;
    // Add the contour to the end of the list and get a reference to it.
    contourData.m_Contours.push_back(contour);

    // recall that end() is an iterator to one past the back!
    const ContourContainerIterator newContour(--contourData.m_Contours.end());
    // add the endpoints and an iterator pointing to the contour
    // in the list to the maps.
    contourData.m_ContourStarts.emplace(from, newContour);
    contourData.m_ContourEnds.emplace(to, newContour);
  }
  else if (newTail != contourData.m_ContourStarts.end() && newHead == contourData.m_ContourEnds.end())
  {
    // Found a single contour to which the new arc should be prepended.
    const ContourContainerIterator tail(newTail->second);
    itkAssertOrThrowMacro((tail->front() == to), "End doesn't match Beginning");
    tail->push_front(from);
    // erase the old start of this contour
    contourData.m_ContourStarts.erase(newTail);
    // Now add the new start of this contour.
    contourData.m_ContourStarts.emplace(from, tail);
  }
  else if (newTail == contourData.m_ContourStarts.end() && newHead != contourData.m_ContourEnds.end())
  {
    // Found a single contour to which the new arc should be appended.
    const ContourContainerIterator head(newHead->second);
    itkAssertOrThrowMacro((head->back() == from), "Beginning doesn't match End");
    head->push_back(to);
    // erase the old end of this contour
    contourData.m_ContourEnds.erase(newHead);
    // Now add the new start of this contour.
    contourData.m_ContourEnds.emplace(to, head);
  }
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::FillOutputs(
  const LabelsContainer &                                    allLabels,
  std::unordered_map<InputPixelType, ContourContainerType> & labelsContoursOutput)
{
  ContourContainerType allContours;
  for (InputPixelType label : allLabels)
  {
    allContours.splice(allContours.end(), labelsContoursOutput[label]);
  }
  this->SetNumberOfIndexedOutputs(allContours.size());
  std::size_t NumberOutputsWritten{ 0 };

  for (ContourContainerConstIterator it{ allContours.cbegin() }; it != allContours.cend(); ++it, ++NumberOutputsWritten)
  {
    OutputPathPointer output{ this->GetOutput(NumberOutputsWritten) };
    if (output.IsNull())
    {
      output = dynamic_cast<OutputPathType *>(this->MakeOutput(NumberOutputsWritten).GetPointer());
      this->SetNthOutput(NumberOutputsWritten, output.GetPointer());
    }
    typename VertexListType::Pointer path{ const_cast<VertexListType *>(output->GetVertexList()) };
    path->Initialize();
    // use std::vector version of 'reserve()' instead of
    // VectorContainer::Reserve() to work around the fact that the latter is
    // essentially std::vector::resize(), which is not what we want.
    path->reserve(it->size());

    // Now put all the points from the contour deque into the path and
    // mark output as modified
    if (m_ReverseContourOrientation)
    {
      ContourConstIterator itC{ (*it).cend() };
      do
      {
        itC--;
        path->push_back(*itC);
      } while (itC != (*it).cbegin());
    }
    else
    {
      ContourConstIterator itC{ (*it).cbegin() };
      while (itC != (*it).cend())
      {
        path->push_back(*itC);
        ++itC;
      }
    }
    output->Modified();
  }
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::SetRequestedRegion(const InputRegionType region)
{
  itkDebugMacro("setting RequestedRegion to " << region);
  if ((!m_UseCustomRegion) | (this->m_RequestedRegion != region))
  {
    m_UseCustomRegion = true;
    this->m_RequestedRegion = region;
    this->Modified();
  }
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::ClearRequestedRegion()
{
  itkDebugMacro("Clearing RequestedRegion.");
  if (this->m_UseCustomRegion == true)
  {
    this->m_UseCustomRegion = false;
    this->Modified();
  }
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::GenerateInputRequestedRegion()
{
  InputImageType * input = const_cast<InputImageType *>(this->GetInput());

  if (!input)
  {
    return;
  }

  if (m_UseCustomRegion)
  {
    InputRegionType requestedRegion{ m_RequestedRegion };
    if (requestedRegion.Crop(input->GetLargestPossibleRegion()))
    {
      input->SetRequestedRegion(requestedRegion);
      return;
    }
    else
    {
      // Couldn't crop the region (requested region is outside the largest
      // possible region).  Throw an exception.

      // store what we tried to request (prior to trying to crop)
      input->SetRequestedRegion(requestedRegion);

      // build an exception
      InvalidRequestedRegionError e(__FILE__, __LINE__);
      e.SetLocation(ITK_LOCATION);
      e.SetDescription("Requested region is outside the largest possible region.");
      e.SetDataObject(input);
      throw e;
    }
  }
  else
  {
    input->SetRequestedRegion(input->GetLargestPossibleRegion());
  }
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  using InputRealPrintType = typename NumericTraits<InputRealType>::PrintType;
  using InputPixelPrintType = typename NumericTraits<InputPixelType>::PrintType;
  os << indent << "ContourValue: " << static_cast<InputRealPrintType>(m_ContourValue) << std::endl;
  os << indent << "ReverseContourOrientation: " << m_ReverseContourOrientation << std::endl;
  os << indent << "VertexConnectHighPixels: " << m_VertexConnectHighPixels << std::endl;
  os << indent << "LabelContours: " << m_LabelContours << std::endl;
  os << indent << "UseCustomRegion: " << m_UseCustomRegion << std::endl;
  if (m_UseCustomRegion)
  {
    os << indent << "RequestedRegion: " << m_RequestedRegion << std::endl;
  }
  os << indent << "UnusedLabel: " << static_cast<InputPixelPrintType>(m_UnusedLabel) << std::endl;
}
} // end namespace itk

#endif
