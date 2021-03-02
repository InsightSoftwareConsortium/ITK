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

#include "itkConstantBoundaryCondition.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkTotalProgressReporter.h"
#include "itkContourExtractor2DImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionRange.h"

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
}

template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::GenerateData()
{
  m_NumberOutputsAllocated = 0;
  m_NumberOutputsWritten = 0;

  if (m_LabelContours) // each label has one or more contours
  {
    this->GenerateDataForLabels();
  }
  else // simple case of a single iso-value
  {
    m_NumberLabelsRemaining = 1;
    m_Interpolate = true;
    const InputRegionType region{ this->GetInput()->GetRequestedRegion() };

    typename InputRegionType::SizeType shrunkSize{ region.GetSize() };
    shrunkSize[0] -= 1;
    shrunkSize[1] -= 1;
    InputRegionType shrunkRegion(region.GetIndex(), shrunkSize);

    this->CreateSingleContour(this->GetInput(),
                              shrunkRegion,
                              m_ContourValue,
                              NumericTraits<InputPixelType>::max(),
                              shrunkRegion.GetNumberOfPixels());
  }

  if (m_NumberOutputsWritten != m_NumberOutputsAllocated)
  {
    this->SetNumberOfIndexedOutputs(m_NumberOutputsWritten);
  }
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::CreateSingleContour(const InputImageType * image,
                                                                InputRegionType        shrunkRegion,
                                                                InputRealType          lowerIsovalue,
                                                                InputRealType          upperIsovalue,
                                                                SizeValueType          totalNumberOfPixels)
{
  ContourData contourData;

  TotalProgressReporter progress(this, totalNumberOfPixels);

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

  using BoundaryConditionType = ConstantBoundaryCondition<InputImageType, InputImageType>;
  BoundaryConditionType boundaryCondition;
  boundaryCondition.SetConstant(lowerIsovalue); // a value distinct from the current label

  using SquareIterator = ConstShapedNeighborhoodIterator<InputImageType, BoundaryConditionType>;
  const typename SquareIterator::RadiusType radius{ { 1, 1 } };
  SquareIterator                            it(radius, image, shrunkRegion);

  // it.SetBoundaryCondition(boundaryCondition); // doesn't compile
  auto nit = static_cast<NeighborhoodIterator<InputImageType, BoundaryConditionType> *>((void *)&it);
  nit->SetBoundaryCondition(boundaryCondition);

  const InputOffsetType none{ { 0, 0 } };
  const InputOffsetType right{ { 1, 0 } };
  const InputOffsetType down{ { 0, 1 } };
  const InputOffsetType diag{ { 1, 1 } };
  it.ActivateOffset(none);
  it.ActivateOffset(right);
  it.ActivateOffset(down);
  it.ActivateOffset(diag);

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
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

    const InputPixelType v0{ it.GetPixel(4) };
    const InputPixelType v1{ it.GetPixel(5) };
    const InputPixelType v2{ it.GetPixel(7) };
    const InputPixelType v3{ it.GetPixel(8) };
    const InputIndexType index{ it.GetIndex() };
    unsigned char        squareCase{ 0 };
    if (v0 > lowerIsovalue && v0 <= upperIsovalue)
    {
      squareCase += 1;
    }
    if (v1 > lowerIsovalue && v1 <= upperIsovalue)
    {
      squareCase += 2;
    }
    if (v2 > lowerIsovalue && v2 <= upperIsovalue)
    {
      squareCase += 4;
    }
    if (v3 > lowerIsovalue && v3 <= upperIsovalue)
    {
      squareCase += 8;
    }

// Set up macros to find the ContinuousIndex where the contour intersects
// one of the sides of the square.  Normally macros should, of course, be
// eschewed, but since this is an inner loop not calling the function four
// times when two would do is probably worth while. Plus, copy-pasting
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

  // Now create the outputs paths from the deques we've been using.
  this->FillOutputs(contourData);
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::GenerateDataForLabels()
{
  using IndexType = typename InputRegionType::IndexType;
  using RegionConstIterator = ImageRegionConstIterator<InputImageType>;
  using ConstImageRegionRange = ImageRegionRange<const InputImageType>;
  using ImageRegionRange = ImageRegionRange<InputImageType>;
  using SizeType = typename InputRegionType::SizeType;

  // Requested region
  const InputRegionType       inputRegion{ this->GetInput()->GetRequestedRegion() };
  const ConstImageRegionRange inputRange{ *this->GetInput(), inputRegion };

  // Find all the distinct labels in the input region.
  std::vector<InputPixelType> allLabels;
  {
    allLabels.assign(inputRange.cbegin(), inputRange.cend());
    std::sort(allLabels.begin(), allLabels.end());
    const typename std::vector<InputPixelType>::const_iterator last{ std::unique(allLabels.begin(), allLabels.end()) };
    allLabels.erase(last, allLabels.end());
  }
  m_NumberLabelsRemaining = allLabels.size(); // We haven't processed any yet

  // Compute bounding box for each label.  These will be [inclusive, inclusive] ranges in each coordinate, not
  // [inclusive, exclusive).
  const IndexType left_top{ inputRegion.GetIndex() };
  const IndexType right_bot{ inputRegion.GetIndex()[0] + static_cast<IndexValueType>(inputRegion.GetSize()[0]) - 1,
                             inputRegion.GetIndex()[1] + static_cast<IndexValueType>(inputRegion.GetSize()[1]) - 1 };
  using BoundingBox = std::pair<IndexType, IndexType>;
  std::unordered_map<InputPixelType, BoundingBox> bboxes;
  {
    for (InputPixelType label : allLabels)
    {
      bboxes[label] = BoundingBox{ right_bot, left_top };
    }
    // We use RegionConstIterator here instead of ImageRegionRange because we want access to the GetIndex() method.
    RegionConstIterator inputIt{ this->GetInput(), inputRegion };
    for (inputIt.GoToBegin(); !inputIt.IsAtEnd(); ++inputIt)
    {
      BoundingBox & bbox{ bboxes[inputIt.Get()] };
      bbox.first[0] = std::min(bbox.first[0], inputIt.GetIndex()[0]);
      bbox.first[1] = std::min(bbox.first[1], inputIt.GetIndex()[1]);
      bbox.second[0] = std::max(bbox.second[0], inputIt.GetIndex()[0]);
      bbox.second[1] = std::max(bbox.second[1], inputIt.GetIndex()[1]);
    }
  }

  // Make room so that largerImage and its largerRegion can have a one-pixel wide border on all sides of the input
  // region.
  const InputImagePointer largerImage{ InputImageType::New() };
  const IndexType         largerIndex{ inputRegion.GetIndex()[0] - 1, inputRegion.GetIndex()[1] - 1 };
  const SizeType          largerSize{ inputRegion.GetSize()[0] + 2, inputRegion.GetSize()[1] + 2 };
  const InputRegionType   largerRegion{ largerIndex, largerSize };
  largerImage->SetRegions(largerRegion);
  largerImage->Allocate();
  {
    const ImageRegionRange largerRange{ *largerImage, inputRegion };
    std::copy(inputRange.cbegin(), inputRange.cend(), largerRange.begin());
  }

  SizeValueType totalPixelCount{ 0 };

  std::unordered_map<InputPixelType, InputRegionType> labelsRegions;

  for (const InputPixelType label : allLabels) // count total pixels for progress
  {
    const BoundingBox & bbox{ bboxes[label] };
    const IndexType     min{ bbox.first };
    const IndexType     max{ bbox.second };

    const IndexType       shrunkIndex{ min[0] - 1, min[1] - 1 };
    const SizeType        shrunkSize{ static_cast<SizeValueType>(max[0] - min[0]) + 2,
                               static_cast<SizeValueType>(max[1] - min[1]) + 2 };
    const InputRegionType shrunkRegion{ shrunkIndex, shrunkSize };

    totalPixelCount += shrunkRegion.GetNumberOfPixels();
    labelsRegions[label] = shrunkRegion;
  }

  m_Interpolate = false;

  for (SizeValueType i = 0; i < allLabels.size(); i++)
  {
    const InputPixelType label{ allLabels[i] };
    const InputRealType  previousLabel = i > 0 ? allLabels[i - 1] : label - 1;
    const InputRealType  followingLabel = i < allLabels.size() - 1 ? allLabels[i + 1] : label + 1;
    // this does not work if labels are floats such as 0.1, 0.23, 0.31, 0.7, etc.
    // this->CreateSingleContour(largerImage, labelsRegions[label], label - 0.5, label + 0.5, totalPixelCount);

    this->CreateSingleContour(largerImage,
                              labelsRegions[label],
                              0.5 * previousLabel + 0.5 * label,
                              0.5 * label + 0.5 * followingLabel,
                              totalPixelCount);
  }
}


template <typename TInputImage>
inline typename ContourExtractor2DImageFilter<TInputImage>::VertexType
ContourExtractor2DImageFilter<TInputImage>::InterpolateContourPosition(InputPixelType  fromValue,
                                                                       InputPixelType  toValue,
                                                                       InputIndexType  fromIndex,
                                                                       InputOffsetType toOffset)
{
  VertexType output;

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

  if (m_Interpolate) // interpolate position
  {
    const double x{ (m_ContourValue - static_cast<InputRealType>(fromValue)) /
                    (toValue - static_cast<InputRealType>(fromValue)) };

    output[0] = fromIndex[0] + x * toOffset[0];
    output[1] = fromIndex[1] + x * toOffset[1];
  }
  else // always half a pixel offset
  {
    output[0] = fromIndex[0] + 0.5 * toOffset[0];
    output[1] = fromIndex[1] + 0.5 * toOffset[1];
  }

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
  const auto newTail{ contourData.m_ContourStarts.find(to) };
  // Try to find an existing contour that ends where the new segment starts.
  const auto newHead{ contourData.m_ContourEnds.find(from) };

  if (newTail != contourData.m_ContourStarts.end() && newHead != contourData.m_ContourEnds.end())
  {
    // We need to connect these two contours with the current arc. The act of
    // connecting the two contours will add the needed arc.
    const auto tail{ newTail->second };
    itkAssertOrThrowMacro((tail->front() == to), "End doesn't match Beginning");
    const auto head{ newHead->second };
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
        const typename VertexToContourMap::size_type erased{ contourData.m_ContourEnds.erase(tail->back()) };
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
        contourData.m_ContourEnds.insert(VertexContourRefPair(head->back(), head));
      }
      else
      {
        // Copy head to the beginning of tail and remove
        // head from everything.
        tail->insert(tail->begin(), head->begin(), head->end());

        // Now remove 'head' from the list and the maps because
        // it has been subsumed.
        contourData.m_ContourEnds.erase(newHead);
        const typename VertexToContourMap::size_type erased{ contourData.m_ContourStarts.erase(head->front()) };
        if (erased != 1)
        {
          itkWarningMacro(<< "There should be exactly one entry in the hash for that endpoint, but there are "
                          << erased);
        }
        contourData.m_Contours.erase(head); // remove from the master list

        // Now remove the old start of 'tail' from the starts map and
        // add the new start.
        contourData.m_ContourStarts.erase(newTail);
        contourData.m_ContourStarts.insert(VertexContourRefPair(tail->front(), tail));
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
    const auto newContour{ --contourData.m_Contours.end() };
    // add the endpoints and an iterator pointing to the contour
    // in the list to the maps.
    contourData.m_ContourStarts.insert(VertexContourRefPair(from, newContour));
    contourData.m_ContourEnds.insert(VertexContourRefPair(to, newContour));
  }
  else if (newTail != contourData.m_ContourStarts.end() && newHead == contourData.m_ContourEnds.end())
  {
    // Found a single contour to which the new arc should be prepended.
    const auto tail{ newTail->second };
    itkAssertOrThrowMacro((tail->front() == to), "End doesn't match Beginning");
    tail->push_front(from);
    // erase the old start of this contour
    contourData.m_ContourStarts.erase(newTail);
    // Now add the new start of this contour.
    contourData.m_ContourStarts.insert(VertexContourRefPair(from, tail));
  }
  else if (newTail == contourData.m_ContourStarts.end() && newHead != contourData.m_ContourEnds.end())
  {
    // Found a single contour to which the new arc should be appended.
    const auto head{ newHead->second };
    itkAssertOrThrowMacro((head->back() == from), "Beginning doesn't match End");
    head->push_back(to);
    // erase the old end of this contour
    contourData.m_ContourEnds.erase(newHead);
    // Now add the new start of this contour.
    contourData.m_ContourEnds.insert(VertexContourRefPair(to, head));
  }
}


template <typename TInputImage>
void
ContourExtractor2DImageFilter<TInputImage>::FillOutputs(ContourData & contourData)
{
  --m_NumberLabelsRemaining;
  if (m_NumberOutputsWritten + contourData.m_Contours.size() > m_NumberOutputsAllocated)
  {
    // We do not have enough capacity; increase capacity to what we need,
    // plus a guess of one contour for each unprocessed label.
    m_NumberOutputsAllocated = m_NumberOutputsWritten + contourData.m_Contours.size() + m_NumberLabelsRemaining;
    this->SetNumberOfIndexedOutputs(m_NumberOutputsAllocated);
  }

  for (auto it = contourData.m_Contours.begin(); it != contourData.m_Contours.end(); ++it, ++m_NumberOutputsWritten)
  {
    OutputPathPointer output{ this->GetOutput(m_NumberOutputsWritten) };
    if (output.IsNull())
    {
      // Dynamic cast is OK because we know PathSource will make its templated
      // class type
      output = dynamic_cast<OutputPathType *>(this->MakeOutput(m_NumberOutputsWritten).GetPointer());
      this->SetNthOutput(m_NumberOutputsWritten, output.GetPointer());
    }
    typename VertexListType::Pointer path{ const_cast<VertexListType *>(output->GetVertexList()) };
    path->Initialize();
    // use std::vector version of 'reserve()' instead of
    // VectorContainer::Reserve() to work around the fact that the latter is
    // essentially std::vector::resize(), which is not what we want.
    path->reserve(it->size());

    // Now put all the points from the contour deque into the path and
    // mark output as modified
    using ConstIteratorType = typename ContourType::const_iterator;
    if (m_ReverseContourOrientation)
    {
      ConstIteratorType itC{ (*it).end() };
      do
      {
        itC--;
        path->push_back(*itC);
      } while (itC != (*it).begin());
    }
    else
    {
      ConstIteratorType itC{ (*it).begin() };
      while (itC != (*it).end())
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
  m_UseCustomRegion = true;
  if (this->m_RequestedRegion != region)
  {
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
  os << indent << "ReverseContourOrientation: " << m_ReverseContourOrientation << std::endl;
  os << indent << "VertexConnectHighPixels: " << m_VertexConnectHighPixels << std::endl;
  os << indent << "LabelContours: " << m_LabelContours << std::endl;
  os << indent << "UseCustomRegion: " << m_UseCustomRegion << std::endl;
  if (m_UseCustomRegion)
  {
    os << indent << "Custom region: " << m_RequestedRegion << std::endl;
  }

  using InputRealPrintType = typename NumericTraits<InputRealType>::PrintType;

  os << indent << "Contour value: " << static_cast<InputRealPrintType>(m_ContourValue) << std::endl;
}
} // end namespace itk

#endif
