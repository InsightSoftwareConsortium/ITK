/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstShapedNeighborhoodIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkNeighborhoodIteratorTestCommon.txx"
#include "itkConstShapedNeighborhoodIterator.h"

void PrintShapedNeighborhood(const itk::ConstShapedNeighborhoodIterator<TestImageType> &n)
{
  itk::ConstShapedNeighborhoodIterator<TestImageType>::ConstIterator it;
  std::cout << n.GetIndex() <<  "->[";
  for (it = n.Begin(); ! it.IsAtEnd(); ++it)
    {      std::cout << it.Get();    }
  std::cout << "]" << std::endl;
}

int itkConstShapedNeighborhoodIteratorTest(int, char* [] )
{
  TestImageType::Pointer img = GetTestImage(10, 10, 5, 3);
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexType loc;
  loc[0] = 4; loc[1] = 4; loc[2] = 2; loc[3] = 1;
  
  // radius of the iterator
  itk::ConstShapedNeighborhoodIterator<TestImageType>::RadiusType radius;
  radius[0] = radius[1] = radius[2] = radius[3] = 1;

  // region over which the iterator is defined
  itk::ConstShapedNeighborhoodIterator<TestImageType>::RegionType reg;
  itk::ConstShapedNeighborhoodIterator<TestImageType>::SizeType sz;
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexType idx;
  idx[0] = idx[1] = idx[2] = 0;  idx[3] = 1;
  sz[0] = sz[1] = 10; sz[2] = 5; sz[3] = 1;
  reg.SetIndex(idx); reg.SetSize(sz);

  // initialize an iterator
  println("Creating ConstShapedNeighborhoodIterator");
  itk::ConstShapedNeighborhoodIterator<TestImageType>
    it(radius, img, reg);
  it.Print(std::cout);
  
  println("Moving iterator using SetLocation()");
  it.SetLocation(loc);
  it.Print(std::cout);
  
  println("Testing GoToBegin()");
  it.GoToBegin();
  it.Print(std::cout);
  
  println("Testing IsAtBegin()");
  std::cout << it.IsAtBegin() << std::endl;
  
  println("Testing GoToEnd()");
  it.GoToEnd();
  it.Print(std::cout);
  
  println("Testing IsAtEnd()");
  std::cout << it.IsAtEnd() << std::endl;
  
  println("Testing forward iteration");
  it.GoToBegin();
  itk::ConstShapedNeighborhoodIterator<TestImageType>::OffsetType off;
  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  while (! it.IsAtEnd())
    {
      PrintShapedNeighborhood(it);
      ++it;
    }
  
  println("Testing reverse iteration");
  it.GoToEnd();
  while (! it.IsAtBegin())
    {
      PrintShapedNeighborhood(it);
      --it;
    }

  println ("Moving iterator: it.GoToBegin(); it += (1, 1, 1, 1)");
  it.GoToBegin();
  off[0] = 1; off[1] = 1; off[2] = 1; off[3] = 1;
  it += off;
  PrintShapedNeighborhood(it);

  println ("Moving iterator: it -= (1, 1, 1, 1)");
  it -= off;
  PrintShapedNeighborhood(it);
  
  println("Moving iterator using SetLocation()");
  it.SetLocation(loc);
  it.Print(std::cout);  
  
  println("Initializing ConstShapedNeighborhoodIterator");
  println("...turn on [0,0,0,0], the center pixel");
  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);

  for (unsigned int r = 0; r < 1; r++)
    {
      println("...turn on [1,0,0,0]");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);
      
      println("...turn on [1,0,0,0] again");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);
      
      println("...turn on [-1,0,0,0]");
      off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);
      
      println("...turn on [0,-1,0,0]");
      off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);
      
      println("...turn on [0,1,0,0]");
      off[0] = 0; off[1] = 1; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);
      
      println("...turn off [-1,0,0,0]");
      off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);
      
      println("...turn off [1,0,0,0]");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);
      
      println("...turn off [0,1,0,0]");
      off[0] = 0; off[1] = 1; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);
      
      println("...turn off [0,-1,0,0]");
      off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);
      
      println("...turn off [0,-1,0,0] again");
      off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);
      
      println("...turn off [0,0 ,0,0]");
      off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
      it.DeactivateOffset(off);
      it.Print(std::cout);

      println("...turn on [1,0,0,0]");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);
      
      println("...turn on [1,0,0,0] again");
      off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);
      
      println("...turn on [-1,0,0,0]");
      off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);
      
      println("...turn on [0,-1,0,0]");
      off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
      it.ActivateOffset(off);
      it.Print(std::cout);

      println(" Testing it.ClearActiveList() ");
      it.ClearActiveList();
      it.Print(std::cout);
      
      println(" NOW REPEAT " );
    }

  println("...turn on [1,0,0,0]");
  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);
  
  println("...turn on [1,0,0,0] again");
  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);
  
  println("...turn on [-1,0,0,0]");
  off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);
  
  println("...turn on [0,-1,0,0]");
  off[0] = 0; off[1] = -1; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);
  
  println("...turn on [0,1,0,0]");
  off[0] = 0; off[1] = 1; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  it.Print(std::cout);
  
  std::cout << "it.GetActiveIndexListSize()="
            << it.GetActiveIndexListSize();

  println("Testing GetActiveIndexList()");
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexListType l
    = it.GetActiveIndexList();
  itk::ConstShapedNeighborhoodIterator<TestImageType>::IndexListType
    ::const_iterator ali = l.begin();
  while (ali != l.end())
    {
      std::cout << *ali << " ";
      ++ali;
    }
  std::cout << std::endl;

  println("Testing const iteration through the neighborhood.");
  itk::ConstShapedNeighborhoodIterator<TestImageType>::ConstIterator
    ci = it.Begin();

  println("Testing using IsAtEnd()");
  while (! ci.IsAtEnd())
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
      ci++;
    }


  println("Testing using != it.End()");
  for (ci = it.Begin(); ci != it.End(); ++ci)
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
    }
  
  println("Testing reverse iteration using != it.Begin()");
  ci = it.End();
  --ci;
  while (ci != it.Begin())
    {
      std::cout << ci.GetNeighborhoodIndex() << " -> "
                << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
      ci--;
    }
  std::cout << ci.GetNeighborhoodIndex() << " -> "
            << ci.GetNeighborhoodOffset() << " = " << ci.Get() << std::endl;
  
  std::cout << std::endl;
  std::cout << "------------------------------" << std::endl;
  std::cout << std::endl;
  println("Testing activating and deactivating pixels on-the-fly");
  println("it.GoToBegin(); it.ClearActiveList();  Activate 1 0 0 0 and -1 0 0 0 and 0 0 0 0");
  it.GoToBegin();
  it.ClearActiveList();
  off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
    
  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
    
  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
    
  PrintShapedNeighborhood(it);

  println("Move the neighborhood two pixels using operator ++");
  ++it;
  ++it;
  PrintShapedNeighborhood(it);

  println("Clear the active list");
  it.ClearActiveList();
  PrintShapedNeighborhood(it);

  println("Move the neighborhood one pixel using operator ++");
  ++it;

  println("Reactivate the same indicies");
  off[0] = -1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  off[0] = 1; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  off[0] = 0; off[1] = 0; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  
  PrintShapedNeighborhood(it);

  println("Activate 0 1 0 0");
  off[0] = 0; off[1] = 1; off[2] = 0; off[3] = 0;
  it.ActivateOffset(off);
  PrintShapedNeighborhood(it);
  
      
  
  return 0;
};
