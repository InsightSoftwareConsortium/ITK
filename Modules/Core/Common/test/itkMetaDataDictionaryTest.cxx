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

#include "itkMetaDataObject.h"
#include <iostream>

int itkMetaDataDictionaryTest(int , char * [])
{
  //This is a demo program to show how to put data into a dictionary.
  itk::MetaDataDictionary MyDictionary;

  //------------------------Testing of native types
  //-------Floats
  itk::EncapsulateMetaData<float>(MyDictionary,"ASimpleFloatInitalized",static_cast<float>(1.234560F));
  {
    float tempfloat = 0.0;
    const bool IsValidReturn=itk::ExposeMetaData<float>(MyDictionary,"ASimpleFloatInitalized",tempfloat);
    if(IsValidReturn == true)
    {
      std::cout << tempfloat << std::endl;
    }
    else
    {
     std::cout << "Invalid key, or invalid type specified." << std::endl;
    }
  }

  itk::EncapsulateMetaData<float>(MyDictionary,"ASimpleFloatChanged",static_cast<float>(-1000.234560F));
  itk::EncapsulateMetaData<double>(MyDictionary,"ASimpleFloatChanged",static_cast<float>(-0.000000001F));

  //-------Char pointers --  These can be tricky, so be careful!
  itk::EncapsulateMetaData<const char *>(MyDictionary,"charconst*","Value String");
  const char * value="Value String";
  itk::EncapsulateMetaData<const char *>(MyDictionary,"charconst*2",value);
  itk::EncapsulateMetaData<std::string>(MyDictionary,"srtringfromcharconst*",std::string("Value Never Seen"));

  //Other gotchas with the Dictionary
  char * StrandedMemory=new char[2345];
  strcpy(StrandedMemory,"XXXXXXXXXXXXThis is stranded memory that will not be released when the Dictionary is cleaned up");
  //NOTE: Only the pointer is copied, not the data within the pointer!
  itk::EncapsulateMetaData<char *>(MyDictionary,"MemoryChangedOutsideOfDictionary",StrandedMemory);
  {
    char * temp = ITK_NULLPTR;
    itk::ExposeMetaData<char *>(MyDictionary,"MemoryChangedOutsideOfDictionary",temp);
    std::cout << "Memory Before Change: "<<temp <<std::endl;
  }
  strcpy(StrandedMemory,"------------This this was changed outside the class, and may cause all types of errors.");
  {
    char * temp = ITK_NULLPTR;
    itk::ExposeMetaData<char *>(MyDictionary,"MemoryChangedOutsideOfDictionary",temp);
    std::cout << "Memory After Change: "<<temp <<std::endl;
  }

  //Print functionality Test
  std::cout << "===========================================================" << std::endl;
  std::cout << "Printing Dictionary" << std::endl;
  MyDictionary.Print(std::cout);


  // Iterator are broken on VS6
#if !(defined(_MSC_VER) && _MSC_VER < 1300)
  std::cout << "Exercise the Iterator access" << std::endl;
  try
    {
    itk::MetaDataDictionary::Iterator itr = MyDictionary.Begin();
    itk::MetaDataDictionary::Iterator end = MyDictionary.End();

    while( itr != end )
      {
      std::cout << "Key   = " << itr->first << std::endl;
      std::cout << "Value = ";
      itr->second->Print( std::cout );
      std::cout << std::endl;
      ++itr;
      }
    }
  catch( itk::ExceptionObject  & excp )
    {
    std::cerr << "Exception Thrown." << std::endl;
    std::cerr << excp << std::endl;
    delete[] StrandedMemory;
    return EXIT_FAILURE;
    }

  std::cout << "Exercise the const Iterator access" << std::endl;
  try
    {
    const itk::MetaDataDictionary & MyConstDictionary = MyDictionary;
    itk::MetaDataDictionary::ConstIterator itr = MyConstDictionary.Begin();
    itk::MetaDataDictionary::ConstIterator end = MyConstDictionary.End();

    while( itr != end )
      {
      std::cout << "Key   = " << itr->first << std::endl;
      std::cout << "Value = ";
      itr->second->Print( std::cout );
      std::cout << std::endl;
      ++itr;
      }
    }
  catch( itk::ExceptionObject  & excp )
    {
    std::cerr << "Exception Thrown." << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
  // Getter/Setter test
  std::cout << "Exercise the Getter/Setter test" <<std::endl;
  try
   {
    itk::MetaDataDictionary::Iterator itr = MyDictionary.Begin();
    itk::MetaDataDictionary::Iterator end = MyDictionary.End();
    while( itr != end )
    {
        std::cout << "Key   = " << itr->first << std::endl;
        std::cout << "Value = ";
        MyDictionary.Get(itr->first)->Print( std::cout );
        MyDictionary.Set(itr->first, itr->second);
        std::cout << std::endl;
        ++itr;
    }
   }
  catch( itk::ExceptionObject  & excp )
  {
   std::cerr << "Exception Thrown." << std::endl;
   std::cerr << excp << std::endl;
   return EXIT_FAILURE;
  }
  try
   {
       MyDictionary.Get("InvalidKeyString")->Print( std::cout );
       std::cerr << "Failed to throw expected exception" << std::endl;
       return EXIT_FAILURE;
   }
   catch( itk::ExceptionObject & excp )
   {
       std::cout << excp << std::endl;
       std::cout << "catched EXPECTED exception for invalid key string to MetaDataDictionary" << std::endl;
   }

  if( MyDictionary.Erase( "ASimpleFloatChanged" ) == false )
    {
    std::cerr << "Failed to erase ASimpleFloatChanged" << std::endl;
    return EXIT_FAILURE;
    }
  if( MyDictionary.Erase( "itk" ) == true )
    {
    std::cerr << "Failed erase itk" << std::endl;
    return EXIT_FAILURE;
    }


#endif

  //NOTE: Must clean up memory allocated with char * StrandedMemory=new char[2345];
  delete[] StrandedMemory;

  return EXIT_SUCCESS;

}
