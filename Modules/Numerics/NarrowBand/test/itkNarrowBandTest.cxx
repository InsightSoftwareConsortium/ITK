/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkNarrowBand.h"


int itkNarrowBandTest (int, char*[])
{
  unsigned int i;
  typedef unsigned int                      IndexType;
  typedef float                             DataType;
  typedef itk::BandNode<IndexType,DataType> BandNodeType;
  typedef itk::NarrowBand<BandNodeType>     BandType;
  typedef BandType::RegionType              RegionType;

  BandType::Pointer band = BandType::New();
  band->Reserve(100);
  //Create nodes
  BandNodeType node;
  band->SetTotalRadius(10);
  band->SetInnerRadius(5);
  for(i=0; i < 20; i++)
    {
    node.m_Data = i*5.0;
    node.m_Index = i;
    node.m_NodeState = 0;
    //Fill the band
    band->PushBack(node);
    }

  std::cout<<"Band size: "<<band->Size()<<std::endl;
  //Iterate over the band
  typedef BandType::ConstIterator itType;

  itType it = band->Begin();
  itType itend = band->End();

  i= 0;
//  BandNodeType *tmp;
  for(; it != itend; it++)
  {
  std::cout <<"Node "<<i<<std::endl<<"Index: "<<it->m_Index<<" Data: "<<it->m_Data<<std::endl;
  i++;
  }

  //Split the band
  std::vector<RegionType> regions;
  regions = band->SplitBand(10);
//  RegionType region;
  typedef std::vector<RegionType>::const_iterator regionitType;
  regionitType regionsit = regions.begin();
  regionitType regionsitend = regions.end();
  std::cout<<"Number of regions: "<<regions.size()<<std::endl;
  i = 0;
  for(; regionsit != regionsitend; ++regionsit)
  {
    std::cout<<"Region "<<i<<std::endl;
    for(; regions[i].Begin != regions[i].End; regions[i].Begin++)
       std::cout<<"Index: "<<regions[i].Begin->m_Index<<" Data: "<<regions[i].Begin->m_Data<<std::endl;
    i++;
  }
  std::cout << "Test Passed. " << std::endl;
  return EXIT_SUCCESS;
}
