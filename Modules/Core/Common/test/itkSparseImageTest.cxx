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

#include "itkSparseImage.h"
#include <iostream>

/* This test exercises the itkSparseImage class. */

namespace itk
{

template <typename TImageType>
class NodeClass
{
public:
  using ImageType = TImageType;
  using IndexType = typename ImageType::IndexType;
  int         m_Value;
  IndexType   m_Index;
  NodeClass * Next;
  NodeClass * Previous;
};

} // namespace itk

int
itkSparseImageTest(int, char *[])
{
  using DummyImageType = itk::Image<int, 2>;
  using NodeType = itk::NodeClass<DummyImageType>;
  using SparseImageType = itk::SparseImage<NodeType, 2>;
  using ImageType = SparseImageType::Superclass;

  SparseImageType::Pointer im = SparseImageType::New();
  ImageType::RegionType    r;
  ImageType::SizeType      sz = { { 24, 24 } };
  ImageType::IndexType     idx = { { 0, 0 } };
  r.SetSize(sz);
  r.SetIndex(idx);

  im->SetLargestPossibleRegion(r);
  im->SetBufferedRegion(r);
  im->SetRequestedRegion(r);
  im->Allocate();

  ImageType::IndexType index;
  NodeType *           node;
  int                  cnt = 0;

  for (index[0] = 0; index[0] < 24; index[0]++)
    for (index[1] = 0; index[1] < 24; index[1]++)
    {
      if ((index[0] >= 6) && (index[0] <= 12) && (index[1] >= 6) && (index[1] <= 12))
      {
        node = im->AddNode(index);
        node->m_Value = cnt++;
      }
    }

  using NodeListType = SparseImageType::NodeListType;
  NodeListType::Pointer nodelist = im->GetNodeList();
  nodelist->Print(std::cout);
  im->Print(std::cout);
  im->Initialize();
  nodelist = im->GetNodeList();
  nodelist->Print(std::cout);
  im->Print(std::cout);

  return EXIT_SUCCESS;
}
