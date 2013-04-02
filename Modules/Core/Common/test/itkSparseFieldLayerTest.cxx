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

#include "itkSparseFieldLayer.h"
#include <iostream>


struct node_type
{
  unsigned int value;
  node_type *Next;
  node_type *Previous;
};


int itkSparseFieldLayerTest(int , char *[] )
{
  unsigned int i, j;
  node_type *store = new node_type[4000];
  itk::SparseFieldLayer<node_type>::RegionListType rlist;

  itk::SparseFieldLayer<node_type>::Pointer layer
    = itk::SparseFieldLayer<node_type>::New();

  for (j = 0; j < 2; j++)
    {
      std::cout << "---------------" << std::endl;
      for (i = 0; i < 4000; i++)
        {
          (store+i)->value = i;
        }

      layer->Print(std::cout);
      std::cout << layer->Size() << std::endl;

      for (i = 0; i < 4000; i++)
        {
          layer->PushFront(store +i);
        }

      layer->Print(std::cout);
      std::cout << layer->Size() << std::endl;

      rlist=layer->SplitRegions(5);
      for (int k=0;k<5;k++)
        {
          std::cout<<"Region begin:"<<(rlist[k].first)->value<<std::endl;
        }


      itk::SparseFieldLayer<node_type>::ConstIterator cit
        = layer->Begin();
      i = 3999;
      while (cit != layer->End() )
        {
          if ( (*cit).value != i || cit->value != i) return EXIT_FAILURE;
          ++cit;
          --i;
        }

      itk::SparseFieldLayer<node_type>::Iterator it
        = layer->Begin();
      i = 3999;
      while (it != layer->End())
        {
          if ( (*it).value != i || it->value != i) return EXIT_FAILURE;
          (*it).value = 32567;
          if ( (*it).value != 32567 || it->value != 32567) return EXIT_FAILURE;
          ++it;
          --i;
        }

      for (i = 0; i < 5000; i++)
        {
          layer->PopFront();
        }
      layer->Print(std::cout);
      std::cout << layer->Size() << std::endl;

      for (i = 0; i < 4000; i++)
        {
          layer->PushFront(store +i);
        }
      for (i = 0; i < 5000; i++)
        {
          layer->Unlink(layer->Front());
        }
      layer->Print(std::cout);
      std::cout << layer->Size() << std::endl;
    }

  delete[] store;

  return EXIT_SUCCESS;
}
