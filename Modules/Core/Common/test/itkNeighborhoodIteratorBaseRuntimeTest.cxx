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

// Runtime test for NeighborhoodIteratorBase<..., VIsConst>.
//
// Exercises:
//   * Construction and iteration on the non-const instantiation.
//   * Voxel-count traversal over a small 3-D region.
//   * SetCenterPixel() mutation round-trips into the image buffer.
//   * Converting ctor non-const -> const yields a read-only iterator that
//     observes the just-written values.

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodIteratorBase.h"

#include <cstdlib>
#include <iostream>

int
itkNeighborhoodIteratorBaseRuntimeTest(int, char *[])
{
  using ImageType = itk::Image<float, 3>;
  using MutIt = itk::NeighborhoodIterator2<ImageType>;
  using ConIt = itk::ConstNeighborhoodIterator2<ImageType>;

  // Build a 5x5x5 ramp image.
  auto                  image = ImageType::New();
  ImageType::SizeType   size = { { 5, 5, 5 } };
  ImageType::IndexType  index = { { 0, 0, 0 } };
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  image->SetRegions(region);
  image->Allocate();

  {
    itk::ImageRegionIterator<ImageType> it(image, region);
    float                               v = 0.0f;
    for (it.GoToBegin(); !it.IsAtEnd(); ++it, v += 1.0f)
    {
      it.Set(v);
    }
  }

  constexpr auto radius = itk::MakeFilled<MutIt::SizeType>(1);

  MutIt nIt(radius, image, region);

  // Count visited voxels via GoToBegin/operator++/IsAtEnd.
  itk::SizeValueType visited = 0;
  for (nIt.GoToBegin(); !nIt.IsAtEnd(); ++nIt)
  {
    ++visited;
  }
  const itk::SizeValueType expected = region.GetNumberOfPixels();
  if (visited != expected)
  {
    std::cerr << "FAIL: visited " << visited << " voxels, expected " << expected << std::endl;
    return EXIT_FAILURE;
  }

  // Write at a known location and read it back from the raw image.
  nIt.GoToBegin();
  ImageType::IndexType target = { { 2, 2, 2 } };
  nIt.SetLocation(target);
  nIt.SetCenterPixel(42.0f);
  if (image->GetPixel(target) != 42.0f)
  {
    std::cerr << "FAIL: SetCenterPixel did not round-trip; got " << image->GetPixel(target) << std::endl;
    return EXIT_FAILURE;
  }

  // Build a const iterator via the converting ctor and read the same location.
  ConIt cIt(nIt);
  cIt.SetLocation(target);
  if (cIt.GetCenterPixel() != 42.0f)
  {
    std::cerr << "FAIL: const-converted iterator center pixel = " << cIt.GetCenterPixel() << " (expected 42)"
              << std::endl;
    return EXIT_FAILURE;
  }

  // Iterate const-side over the same region and verify count matches.
  itk::SizeValueType cVisited = 0;
  for (cIt.GoToBegin(); !cIt.IsAtEnd(); ++cIt)
  {
    ++cVisited;
  }
  if (cVisited != expected)
  {
    std::cerr << "FAIL: const iterator visited " << cVisited << " voxels, expected " << expected << std::endl;
    return EXIT_FAILURE;
  }

  // Verify GetImagePointer return-type constness on both instantiations.
  static_assert(std::is_same_v<decltype(nIt.GetImagePointer()), ImageType *>,
                "Non-const NeighborhoodIteratorBase should return ImageType* (no const_cast).");
  static_assert(std::is_same_v<decltype(cIt.GetImagePointer()), const ImageType *>,
                "Const NeighborhoodIteratorBase should return const ImageType*.");

  // --------------------------------------------------------------------
  // ShapedNeighborhoodIteratorBase exercise.
  // --------------------------------------------------------------------
  using MutShaped = itk::ShapedNeighborhoodIterator2<ImageType>;
  using ConShaped = itk::ConstShapedNeighborhoodIterator2<ImageType>;

  // Reverse-converting ctor must be compile-disabled.
  static_assert(!std::is_constructible_v<MutShaped, const ConShaped &>,
                "Must not construct non-const ShapedNeighborhoodIteratorBase from const one.");
  // Forward converting ctor must be available.
  static_assert(std::is_constructible_v<ConShaped, const MutShaped &>,
                "Const ShapedNeighborhoodIteratorBase must be constructible from non-const.");

  MutShaped sIt(radius, image, region);
  sIt.SetLocation(target);

  // Activate the 6-connected stencil (Manhattan neighbors at distance 1).
  for (unsigned d = 0; d < ImageType::ImageDimension; ++d)
  {
    MutShaped::OffsetType off{};
    off[d] = 1;
    sIt.ActivateOffset(off);
    off[d] = -1;
    sIt.ActivateOffset(off);
  }

  if (sIt.GetActiveIndexListSize() != 6)
  {
    std::cerr << "FAIL: expected 6 active offsets, got " << sIt.GetActiveIndexListSize() << std::endl;
    return EXIT_FAILURE;
  }
  if (sIt.GetCenterIsActive())
  {
    std::cerr << "FAIL: center unexpectedly active." << std::endl;
    return EXIT_FAILURE;
  }

  // Iterate inner Begin()/End() and mutate via Set(); verify one pixel round-trips.
  itk::SizeValueType sVisited = 0;
  bool               wroteOne = false;
  for (auto innerIt = sIt.Begin(); innerIt != sIt.End(); ++innerIt)
  {
    ++sVisited;
    if (!wroteOne)
    {
      innerIt.Set(77.0f);
      wroteOne = true;
    }
  }
  if (sVisited != 6)
  {
    std::cerr << "FAIL: shaped inner iteration visited " << sVisited << " (expected 6)." << std::endl;
    return EXIT_FAILURE;
  }

  // The first active offset in sorted list order was index 0 of the list; find which neighbor
  // it was and verify the image pixel changed to 77.
  const auto           firstActive = *sIt.GetActiveIndexList().begin();
  const auto           firstOffset = sIt.GetOffset(firstActive);
  ImageType::IndexType writtenIdx = target;
  for (unsigned d = 0; d < ImageType::ImageDimension; ++d)
  {
    writtenIdx[d] += firstOffset[d];
  }
  if (image->GetPixel(writtenIdx) != 77.0f)
  {
    std::cerr << "FAIL: shaped Set() did not round-trip at " << writtenIdx << ", got " << image->GetPixel(writtenIdx)
              << std::endl;
    return EXIT_FAILURE;
  }

  // Converting ctor: build a const shaped from the non-const one and read back.
  ConShaped          csIt(sIt);
  itk::SizeValueType cReadCount = 0;
  bool               sawWritten = false;
  for (auto innerIt = csIt.Begin(); innerIt != csIt.End(); ++innerIt)
  {
    ++cReadCount;
    if (innerIt.Get() == 77.0f)
    {
      sawWritten = true;
    }
  }
  if (cReadCount != 6)
  {
    std::cerr << "FAIL: const shaped inner iteration visited " << cReadCount << " (expected 6)." << std::endl;
    return EXIT_FAILURE;
  }
  if (!sawWritten)
  {
    std::cerr << "FAIL: const shaped did not observe written value." << std::endl;
    return EXIT_FAILURE;
  }

  // Deactivate round-trip.
  sIt.ClearActiveList();
  if (sIt.GetActiveIndexListSize() != 0)
  {
    std::cerr << "FAIL: ClearActiveList did not empty list." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "itkNeighborhoodIteratorBaseRuntimeTest: PASS" << std::endl;
  return EXIT_SUCCESS;
}
