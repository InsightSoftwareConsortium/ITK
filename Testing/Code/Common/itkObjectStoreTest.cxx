/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObjectStoreTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkObjectStore.h"
#include <iostream>
#include <list>

struct TestObject
{
  float vector[3];
  int counter; 
};

int itkObjectStoreTest( int , char * [] )
{
  unsigned i, j;
  itk::ObjectStore<TestObject>::Pointer store
    = itk::ObjectStore<TestObject>::New();

  std::list<TestObject *> borrowed_list;
  store->SetGrowthStrategyToExponential();
  store->Reserve(40000);

  // for increased code coverage
  store->SetLinearGrowthSize( store->GetLinearGrowthSize() );
  store->SetGrowthStrategy( store->GetGrowthStrategy() );
  
  
  for (j = 0; j< 2; j++)
    {
      std::cout << "_______________________" << std::endl;
      store->Print(std::cout);
      

      // Borrow some items
      for (i = 0; i < 30000; i++)
        {
          borrowed_list.push_back(store->Borrow());
        }
      store->Print(std::cout);
      
      // Force allocation of a more memory
      for (i = 0; i < 1000000; i++)
        {
          borrowed_list.push_back(store->Borrow());
        }
      store->Print(std::cout);
      
      // Return all items
      unsigned int sz = static_cast< unsigned int>( borrowed_list.size() );
      for (i = 0; i < sz; ++i)
        {
          store->Return(borrowed_list.back());
          borrowed_list.pop_back();
        }
      store->Print(std::cout);

      store->Clear();
      store->Squeeze();
    }

  return EXIT_SUCCESS;
}

