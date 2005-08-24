/*=========================================================================

  Program:   itkUNC
  Module:    itkNeuralNetworkTests.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 CADDLab @ UNC. All rights reserved.
  See itkUNCCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// some compilers have trouble with the size of this test
#define ITK_LEAN_AND_MEAN

#include <iostream>
#include "itkTestMain.h" 


void
RegisterTests()
{
  REGISTER_TEST(XORTest1);
  REGISTER_TEST(XORTest2);
  REGISTER_TEST(QPropXORTest1);
  REGISTER_TEST(NNetClassifierTest1);
  REGISTER_TEST(NNetClassifierTest2);
  REGISTER_TEST(NNetClassifierTest3);
  REGISTER_TEST(NNetClassifierTest4);
}


