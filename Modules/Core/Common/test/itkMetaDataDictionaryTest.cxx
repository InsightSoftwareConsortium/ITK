/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

int
itkMetaDataDictionaryTest(int, char *[])
{
  // This is a demo program to show how to put data into a dictionary.
  itk::MetaDataDictionary MyDictionary;

  //------------------------Testing of native types
  //-------Floats
  itk::EncapsulateMetaData<float>(MyDictionary, "ASimpleFloatInitalized", static_cast<float>(1.234560F));
  {
    float      tempfloat = 0.0;
    const bool IsValidReturn = itk::ExposeMetaData<float>(MyDictionary, "ASimpleFloatInitalized", tempfloat);
    if (IsValidReturn)
    {
      std::cout << tempfloat << '\n';
    }
    else
    {
      std::cout << "Invalid key, or invalid type specified." << '\n';
    }
  }

  itk::EncapsulateMetaData<float>(MyDictionary, "ASimpleFloatChanged", static_cast<float>(-1000.234560F));
  itk::EncapsulateMetaData<double>(MyDictionary, "ASimpleFloatChanged", static_cast<float>(-0.000000001F));

  //-------Char pointers --  These can be tricky, so be careful!
  itk::EncapsulateMetaData<const char *>(MyDictionary, "charconst*", "Value String");
  const char * value = "Value String";
  itk::EncapsulateMetaData<const char *>(MyDictionary, "charconst*2", value);
  itk::EncapsulateMetaData<std::string>(MyDictionary, "srtringfromcharconst*", std::string("Value Never Seen"));

  // Other gotchas with the Dictionary
  auto * StrandedMemory = new char[2345];
  strcpy(StrandedMemory,
         "XXXXXXXXXXXXThis is stranded memory that will not be released when the Dictionary is cleaned up");
  // NOTE: Only the pointer is copied, not the data within the pointer!
  itk::EncapsulateMetaData<char *>(MyDictionary, "MemoryChangedOutsideOfDictionary", StrandedMemory);
  {
    char * temp = nullptr;
    itk::ExposeMetaData<char *>(MyDictionary, "MemoryChangedOutsideOfDictionary", temp);
    std::cout << "Memory Before Change: " << temp << '\n';
  }
  strcpy(StrandedMemory, "------------This this was changed outside the class, and may cause all types of errors.");
  {
    char * temp = nullptr;
    itk::ExposeMetaData<char *>(MyDictionary, "MemoryChangedOutsideOfDictionary", temp);
    std::cout << "Memory After Change: " << temp << '\n';
  }

  // Print functionality Test
  std::cout << "===========================================================" << '\n';
  std::cout << "Printing Dictionary" << '\n';
  MyDictionary.Print(std::cout);


  std::cout << "Exercise the Iterator access" << '\n';
  try
  {
    auto itr = MyDictionary.Begin();
    auto end = MyDictionary.End();

    while (itr != end)
    {
      std::cout << "Key   = " << itr->first << '\n';
      std::cout << "Value = ";
      itr->second->Print(std::cout);
      std::cout << '\n';
      ++itr;
    }
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception Thrown." << '\n';
    std::cerr << excp << '\n';
    delete[] StrandedMemory;
    return EXIT_FAILURE;
  }

  std::cout << "Exercise the const Iterator access" << '\n';
  try
  {
    const itk::MetaDataDictionary & MyConstDictionary = MyDictionary;
    auto                            itr = MyConstDictionary.Begin();
    auto                            end = MyConstDictionary.End();

    while (itr != end)
    {
      std::cout << "Key   = " << itr->first << '\n';
      std::cout << "Value = ";
      itr->second->Print(std::cout);
      std::cout << '\n';
      ++itr;
    }
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception Thrown." << '\n';
    std::cerr << excp << '\n';
    return EXIT_FAILURE;
  }
  // Getter/Setter test
  std::cout << "Exercise the Getter/Setter test" << '\n';
  try
  {
    auto itr = MyDictionary.Begin();
    auto end = MyDictionary.End();
    while (itr != end)
    {
      std::cout << "Key   = " << itr->first << '\n';
      std::cout << "Value = ";
      MyDictionary.Get(itr->first)->Print(std::cout);
      MyDictionary.Set(itr->first, itr->second);
      std::cout << '\n';
      ++itr;
    }
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception Thrown." << '\n';
    std::cerr << excp << '\n';
    return EXIT_FAILURE;
  }
  try
  {
    MyDictionary.Get("InvalidKeyString")->Print(std::cout);
    std::cerr << "Failed to throw expected exception" << '\n';
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cout << excp << '\n';
    std::cout << "caught EXPECTED exception for invalid key string to MetaDataDictionary" << '\n';
  }

  if (MyDictionary.Erase("ASimpleFloatChanged") == false)
  {
    std::cerr << "Failed to erase ASimpleFloatChanged" << '\n';
    return EXIT_FAILURE;
  }
  if (MyDictionary.Erase("itk"))
  {
    std::cerr << "Failed erase itk" << '\n';
    return EXIT_FAILURE;
  }


  // NOTE: Must clean up memory allocated with char * StrandedMemory=new char[2345];
  delete[] StrandedMemory;

  return EXIT_SUCCESS;
}
