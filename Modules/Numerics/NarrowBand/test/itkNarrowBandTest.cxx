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

#include "itkNarrowBand.h"
#include "itkTestingMacros.h"


int
itkNarrowBandTest(int, char *[])
{
  using IndexType = unsigned int;
  using DataType = float;
  using BandNodeType = itk::BandNode<IndexType, DataType>;
  using BandType = itk::NarrowBand<BandNodeType>;
  using RegionType = BandType::RegionType;

  auto band = BandType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(band, NarrowBand, LightObject);


  band->Reserve(100);
  // Create nodes
  BandNodeType node;
  band->SetTotalRadius(10);
  band->SetInnerRadius(5);
  for (unsigned int i = 0; i < 20; ++i)
  {
    node.m_Data = i * 5.0;
    node.m_Index = i;
    node.m_NodeState = 0;
    // Fill the band
    band->PushBack(node);
  }

  std::cout << "Band size: " << band->Size() << '\n';
  // Iterate over the band
  auto       it = band->Begin();
  const auto itend = band->End();

  //  BandNodeType *tmp;
  for (unsigned int i = 0; it != itend; ++it)
  {
    std::cout << "Node " << i << '\n' << "Index: " << it->m_Index << " Data: " << it->m_Data << '\n';
    i++;
  }

  // Split the band
  std::vector<RegionType> regions = band->SplitBand(10);
  auto                    regionsit = regions.begin();
  const auto              regionsitend = regions.end();
  std::cout << "Number of regions: " << regions.size() << '\n';
  for (unsigned int i = 0; regionsit != regionsitend; ++regionsit)
  {
    std::cout << "Region " << i << '\n';
    for (; regions[i].Begin != regions[i].End; regions[i].Begin++)
    {
      std::cout << "Index: " << regions[i].Begin->m_Index << " Data: " << regions[i].Begin->m_Data << '\n';
    }
    i++;
  }
  std::cout << "Test Passed. " << '\n';
  return EXIT_SUCCESS;
}
