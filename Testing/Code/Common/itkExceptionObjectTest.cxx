/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkExceptionObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

//
//  TO COMPILE:
//	$(CC) -I ../../../Code/Common -LANG:std -o itkExceptionObjectTest
//	   itkExceptionObjectTest.cxx $../../../Code/Common/itkExceptionObject.cxx 
//

#include "itkExceptionObject.h"
#include <iostream>

class mammal
 { public:  virtual int GetType()=0;  virtual bool operator== (mammal &); };
class human : public mammal {  public: int GetType() { return 32; }};
class naked_mole_rat : public mammal{ public:  int GetType() { return 2; }};

bool mammal::operator== (mammal &o)
{
  if ( this->GetType() != o.GetType() )
    {
    itkIncompatibleOperandsError e;
    e.SetLocation("bool mammal::operator==(mammal&, mammal&)");
    e.SetDescription("Cannot compare mammals of unequal type");
    throw e;
    }
  else
    {
    if ( /* blah blah blah */ 1) 
      {
      return true;
      }
    else 
      {
      return false;
      }
    }
}

int lookup(const int& i)
{
  static int table[5] = { 23,42,42,32,12 };
  if ( ! ( 0 <= i && i < 5 ) )
    {
    itkRangeError e;
    e.SetLocation("int lookup(const int& )");
    e.SetDescription("Attempted to access out-of-bounds array element");
    throw e;
    }
  return table[i];
}


main()
{
  // SOME BASIC TESTS OF THE itkExceptionObject 's
  
  itkRangeError E;
  E.SetLocation("main()");
  E.SetDescription("E");
  std::cout << E << std::endl;
  itkRangeError F(E);
  std::cout << F << std::endl;
  itkRangeError G;
  G.SetLocation("main()");
  G.SetDescription("G");
  std::cout << "F==G? " << (F==G) << std::endl;
  E = F = G;
  std::cout << F << std::endl;
  std::cout << "F==G? " << (F==G) << std::endl;

  itkRangeError *Ep  = new itkRangeError;
  Ep->SetLocation("main()");
  Ep->SetDescription("Ep");
  itkRangeError *Fp = new itkRangeError;
  *Fp = *Ep;
  delete Ep;
  std::cout << *Fp << std::endl;
  
  // ** BE SURE TO CATCH BY REFERENCE TO AVOID SLICING **
  try {
    lookup(4);  // OK
    lookup(12); // ERROR
    }
  catch (itkExceptionObject &e) 
    { 
    std::cout << e << std::endl; 
    }

  try
    {
    human john, jane;
    naked_mole_rat hal;
    john == john;  // OK
    jane == john;  // OK
    hal == john;   // ERROR
    }
  catch (itkIncompatibleOperandsError &e) 
    { 
    std::cout << e << std::endl; 
    }

  /*
  // SAMPLE ERROR STUFF
  itkSampleError Se;
  Se.SetLocation("SE LOCATION");
  Se.SetDescription("SE DESCRIPTION");
  itkSampleError Sf(Se);
  itkSampleError Sg;
  Sg = Sf;
  std::cout << Sg << std::endl;
  

  try
	{
	  itkSampleError E;
	  E.SetLocation("djibouti");
	  E.SetDescription("sample error");
	  throw E;
	}
  catch (itkExceptionObject &e) { std::cout << e << std::endl; }
  */
  
}
