/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFieldLayerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkSparseFieldLayer.h"
#include <iostream>


struct node_type
{
  unsigned int value;
  node_type *Next;
  node_type *Previous;
};


int itkSparseFieldLayerTest(int argc, char *argv[] )
{
  unsigned int i, j;
  node_type *store = new node_type[4000];
  itk::SparseFieldLayer<node_type>::RegionListType rlist;
  
  itk::SparseFieldLayer<node_type>::Pointer layer
    = itk::SparseFieldLayer<node_type>::New();

  for (j = 0 ; j < 2 ; j++)
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
          itk::SparseFieldLayer<node_type>::ConstIterator ptr=rlist[k].last;
          std::cout<<"Region begin:"<<(rlist[k].first)->value<<std::endl;
        }
      

      itk::SparseFieldLayer<node_type>::ConstIterator cit
        = layer->Begin();
      i = 3999;
      while (cit != layer->End() )
        {
          if ( (*cit).value != i || cit->value != i) return 1;
          ++cit;
          --i;
        }
      
      itk::SparseFieldLayer<node_type>::Iterator it
        = layer->Begin();
      i = 3999;
      while (it != layer->End())
        {
          if ( (*it).value != i || it->value != i) return 1;
          (*it).value = 32567;
          if ( (*it).value != 32567 || it->value != 32567) return 1;
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
