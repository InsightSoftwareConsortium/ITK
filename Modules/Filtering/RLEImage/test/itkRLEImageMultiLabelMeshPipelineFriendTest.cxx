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

// This test pins the `friend class ::MultiLabelMeshPipeline;` grant
// declared by `itk::ImageConstIterator<itk::RLEImage<...>>` and
// `itk::ImageRegionConstIterator<itk::RLEImage<...>>` (see
// itkRLEImageConstIterator.h and itkRLEImageRegionConstIterator.h).
//
// `MultiLabelMeshPipeline` is the downstream consumer (originally in
// ITK-SNAP) that needs direct access to the iterators' protected
// scan-line bookkeeping (m_Index0 / m_BeginIndex0 / m_EndIndex0 /
// m_RealIndex / m_SegmentRemainder / m_BI) for the multi-label
// surface-extraction pipeline.  Removing the friendship would silently
// break that downstream build.  This test compiles successfully if and
// only if the friendship is present, so it acts as a build-time gate
// against accidental removal.

#include "itkRLEImage.h"
#include "itkRLEImageRegionIterator.h"
#include "itkImageRegionIterator.h"

// MultiLabelMeshPipeline is declared at global scope (matching the
// `friend class ::MultiLabelMeshPipeline;` grants) and reads several
// protected members of the const-iterator.  The class is intentionally
// defined here in test scope rather than imported from the downstream
// project; it is the simplest in-tree witness to the friend-access
// contract.
class MultiLabelMeshPipeline
{
public:
  template <typename TIterator>
  static bool
  ExerciseProtectedAccess(const TIterator & it)
  {
    // Access several of the protected scan-line bookkeeping members.
    // If the friendship is removed, every line below becomes a
    // protected-access compile error.
    const auto idx0 = it.m_Index0;
    const auto beginIdx0 = it.m_BeginIndex0;
    const auto endIdx0 = it.m_EndIndex0;
    const auto realIdx = it.m_RealIndex;
    const auto segRem = it.m_SegmentRemainder;
    const bool inRange = idx0 >= beginIdx0 && idx0 <= endIdx0;
    return inRange && (realIdx + segRem) >= 0;
  }
};

int
itkRLEImageMultiLabelMeshPipelineFriendTest(int, char *[])
{
  using PixelType = unsigned short;
  constexpr unsigned int Dimension = 3;
  using ImageType = itk::RLEImage<PixelType, Dimension>;
  using ConstIteratorType = itk::ImageConstIterator<ImageType>;
  using RegionConstIteratorType = itk::ImageRegionConstIterator<ImageType>;

  auto image = ImageType::New();

  ImageType::IndexType  idx{};
  ImageType::SizeType   size{ { 4, 4, 4 } };
  ImageType::RegionType region{ idx, size };

  image->SetRegions(region);
  image->Allocate(true);
  image->FillBuffer(PixelType{ 0 });

  // Plain ImageConstIterator<RLEImage<...>> path
  ConstIteratorType cit(image, region);
  cit.GoToBegin();
  if (!MultiLabelMeshPipeline::ExerciseProtectedAccess(cit))
  {
    std::cerr << "Friend access via ImageConstIterator returned false.\n";
    return EXIT_FAILURE;
  }

  // ImageRegionConstIterator<RLEImage<...>> path (separate friend grant)
  RegionConstIteratorType rcit(image, region);
  rcit.GoToBegin();
  if (!MultiLabelMeshPipeline::ExerciseProtectedAccess(rcit))
  {
    std::cerr << "Friend access via ImageRegionConstIterator returned false.\n";
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
