/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkExceptionObjectTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
    itk::IncompatibleOperandsError e;
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
    itk::RangeError e;
    e.SetLocation("int lookup(const int& )");
    e.SetDescription("Attempted to access out-of-bounds array element");
    throw e;
    }
  return table[i];
}


int main()
{
  // SOME BASIC TESTS OF THE itk::ExceptionObject 's
  
  itk::RangeError E;
  E.SetLocation("main()");
  E.SetDescription("E");
  std::cout << E << std::endl;
  itk::RangeError F(E);
  std::cout << F << std::endl;
  itk::RangeError G;
  G.SetLocation("main()");
  G.SetDescription("G");
  std::cout << "F==G? " << (F==G) << std::endl;
  E = F = G;
  std::cout << F << std::endl;
  std::cout << "F==G? " << (F==G) << std::endl;

  itk::RangeError *Ep  = new itk::RangeError;
  Ep->SetLocation("main()");
  Ep->SetDescription("Ep");
  itk::RangeError *Fp = new itk::RangeError;
  *Fp = *Ep;
  delete Ep;
  std::cout << *Fp << std::endl;
  
  // ** BE SURE TO CATCH BY REFERENCE TO AVOID SLICING **
  try {
    lookup(4);  // OK
    lookup(12); // ERROR
    }
  catch (itk::ExceptionObject &e) 
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
  catch (itk::IncompatibleOperandsError &e) 
    { 
    std::cout << e << std::endl; 
    }

  /*
  // SAMPLE ERROR STUFF
  itk::SampleError Se;
  Se.SetLocation("SE LOCATION");
  Se.SetDescription("SE DESCRIPTION");
  itk::SampleError Sf(Se);
  itk::SampleError Sg;
  Sg = Sf;
  std::cout << Sg << std::endl;
  

  try
	{
	  itk::SampleError E;
	  E.SetLocation("djibouti");
	  E.SetDescription("sample error");
	  throw E;
	}
  catch (itk::ExceptionObject &e) { std::cout << e << std::endl; }
  */

  return 0;
  
}
