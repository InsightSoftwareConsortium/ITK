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

#include "itkKdTree.h"
#include "itkListSample.h"

#include "itkGTest.h"

TEST(KdTree, ReplaceFarthestNeighborTracksSubnormalDistances)
{
  using MeasurementVectorType = itk::Array<double>;
  using SampleType = itk::Statistics::ListSample<MeasurementVectorType>;
  using TreeType = itk::Statistics::KdTree<SampleType>;

  std::vector<double>        distances;
  TreeType::NearestNeighbors nearestNeighbors(distances);
  nearestNeighbors.resize(3);

  nearestNeighbors.ReplaceFarthestNeighbor(0, 1e-310);
  nearestNeighbors.ReplaceFarthestNeighbor(1, 3e-310);
  nearestNeighbors.ReplaceFarthestNeighbor(2, 2e-310);

  EXPECT_DOUBLE_EQ(nearestNeighbors.GetLargestDistance(), 3e-310);
}
