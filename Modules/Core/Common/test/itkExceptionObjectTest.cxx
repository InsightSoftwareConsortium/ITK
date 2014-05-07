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

#include "itkMacro.h"
#include <iostream>

class mammal
 {
 public:
   virtual int GetType()=0;
   virtual bool operator== (mammal &);
   mammal() {};
   virtual ~mammal() {};
 };

class human : public mammal
{
  public:
    virtual int GetType() ITK_OVERRIDE
    {
      return 32;
    }
};

class naked_mole_rat : public mammal
{
  public:
    virtual int GetType() ITK_OVERRIDE
    {
      return 2;
    }
};

bool mammal::operator== (mammal &o)
{
  if ( this->GetType() != o.GetType() )
    {
    itk::IncompatibleOperandsError e(__FILE__, __LINE__);
    e.SetLocation("bool mammal::operator==(mammal&, mammal&)");
    e.SetDescription("Cannot compare mammals of unequal type");
    throw e;
    }
  return true;
}

int lookup(const int& i)
{
  static int table[5] = { 23,42,42,32,12 };
  if ( ! ( 0 <= i && i < 5 ) )
    {
    itk::RangeError e(__FILE__, __LINE__);
    e.SetLocation("int lookup(const int& )");
    e.SetDescription("Attempted to access out-of-bounds array element");
    throw e;
    }
  return table[i];
}


int itkExceptionObjectTest(int, char* [] )
{
  // SOME BASIC TESTS OF THE itk::ExceptionObject 's

  itk::RangeError E;
  E.SetLocation("itkExceptionObjectTest(int, char**)");
  E.SetDescription("E");
  std::cout << E << std::endl;
  itk::RangeError F(E);
  std::cout << F << std::endl;
  itk::RangeError G;
  G.SetLocation("itkExceptionObjectTest(int, char**)");
  G.SetDescription("G");
  std::cout << "F==G? " << (F==G) << std::endl;
  E = F = G;
  std::cout << F << std::endl;
  std::cout << "F==G? " << (F==G) << std::endl;

  itk::RangeError *Ep  = new itk::RangeError;
  Ep->SetLocation("itkExceptionObjectTest(int, char**)");
  Ep->SetDescription("Ep");
  itk::RangeError *Fp = new itk::RangeError;
  *Fp = *Ep;
  delete Ep;
  std::cout << *Fp << std::endl;

  // ** BE SURE TO CATCH BY REFERENCE TO AVOID SLICING **
  bool raised = false;
  try {
    lookup(4);  // OK
    lookup(12); // ERROR
    }
  catch (itk::ExceptionObject &e)
    {
    std::cout << e << std::endl;
    raised = true;
    }
  if( !raised )
    {
    return EXIT_FAILURE;
    }

  raised = false;
  bool OneShouldFail=true;
  try
    {
    human john, jane;
    naked_mole_rat hal;
    OneShouldFail &= (john == john);  // OK
    OneShouldFail &= (jane == john);  // OK
    //NOTE:  (hal == john) throws an exception, and does not actually return false!
    //       This means that the &= operator below is never executed, and
    //       the OneShouldFail variable is never actually set to false!
    OneShouldFail &= (hal == john);   // ERROR
    }
  catch (itk::IncompatibleOperandsError &e)
    {
    std::cout << e << std::endl;
    raised = true;
    }
  if( !raised || OneShouldFail==false )
    {
    return EXIT_FAILURE;
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

  delete Fp;

  return EXIT_SUCCESS;

}
