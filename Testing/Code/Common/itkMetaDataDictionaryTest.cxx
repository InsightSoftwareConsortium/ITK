/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDataDictionaryTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
#include "itkImage.h"
#include "itkObject.h"
#include <iostream>
#include <complex>
#if 0
//================================================================================
//================================================================================
//================================================================================
//================================================================================
// The behavior of the MetaDataObject<Type>::Print() function has many plausible
// application dependant implementations.  The default implementation prints the
// a string "UNKNOWN PRINT CHARACTERISTICS]" that is applicable to all possible
// MetaDataObject types.
//
// The application developer may overload the default implementation to provide
// a specialization that produces results desirable for their applicaiton.
//
// Below is one possible implementation that may be used.

/**
 * \macro NATIVE_TYPE_METADATAPRINT
 * An ungly macro to facilitate creating a simple implementation of
 * the MetaDataObject<Type>::Print() function for types that
 * have operator<< defined.
 * \param TYPE_NAME the native type parameter type
 */
#define NATIVE_TYPE_METADATAPRINT(TYPE_NAME) \
void \
itk::MetaDataObject<TYPE_NAME> \
::Print(std::ostream& os) const \
{ \
  os << this->m_MetaDataObjectValue << std::endl; \
} \
void \
itk::MetaDataObject<const TYPE_NAME> \
::Print(std::ostream& os) const \
{ \
  os << this->m_MetaDataObjectValue << std::endl; \
}

/**
 * \macro ITK_OBJECT_TYPE_METADATAPRINT_1COMMA
 * An ungly macro to facilitate creating a simple implementation of
 * the MetaDataObject<Type>::Print() function for
 * itk::Objects that have 1 comma in their type definition
 * \param TYPE_NAME_PART1
 * \param TYPE_NAME_PART2
 */
#define ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(TYPE_NAME_PART1,TYPE_NAME_PART2) \
void \
itk::MetaDataObject<TYPE_NAME_PART1,TYPE_NAME_PART2> \
::Print(std::ostream& os) const \
{ \
     this->m_MetaDataObjectValue->Print(os); \
} \
void \
itk::MetaDataObject<const TYPE_NAME_PART1,TYPE_NAME_PART2> \
::Print(std::ostream& os) const \
{ \
     this->m_MetaDataObjectValue->Print(os); \
}

/**
 * \macro ITK_IMAGE_TYPE_METADATAPRINT
 * An ungly macro to facilitate creating a simple implementation of
 * the MetaDataObject<Type>::Print() function for
 * itk::Objects that have 1 comma in their type definition
 * \param STORAGE_TYPE The storage type of the image type to print.
 */
#define ITK_IMAGE_TYPE_METADATAPRINT(STORAGE_TYPE) \
ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,1>::Pointer) \
ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,2>::Pointer) \
ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,3>::Pointer) \
ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,4>::Pointer) \


NATIVE_TYPE_METADATAPRINT(char)
NATIVE_TYPE_METADATAPRINT(char *)
NATIVE_TYPE_METADATAPRINT(char * const)
NATIVE_TYPE_METADATAPRINT(unsigned char)
NATIVE_TYPE_METADATAPRINT(short int)
NATIVE_TYPE_METADATAPRINT(unsigned short int)
NATIVE_TYPE_METADATAPRINT(int)
NATIVE_TYPE_METADATAPRINT(unsigned int)
NATIVE_TYPE_METADATAPRINT(long int)
NATIVE_TYPE_METADATAPRINT(unsigned long int)
NATIVE_TYPE_METADATAPRINT(float)
NATIVE_TYPE_METADATAPRINT(double)
NATIVE_TYPE_METADATAPRINT(std::string)
NATIVE_TYPE_METADATAPRINT(std::complex<float>)
NATIVE_TYPE_METADATAPRINT(std::complex<double>)

ITK_IMAGE_TYPE_METADATAPRINT(char)
ITK_IMAGE_TYPE_METADATAPRINT(char *)
ITK_IMAGE_TYPE_METADATAPRINT(char * const)
ITK_IMAGE_TYPE_METADATAPRINT(unsigned char)
ITK_IMAGE_TYPE_METADATAPRINT(short int)
ITK_IMAGE_TYPE_METADATAPRINT(unsigned short int)
ITK_IMAGE_TYPE_METADATAPRINT(int)
ITK_IMAGE_TYPE_METADATAPRINT(unsigned int)
ITK_IMAGE_TYPE_METADATAPRINT(long int)
ITK_IMAGE_TYPE_METADATAPRINT(unsigned long int)
ITK_IMAGE_TYPE_METADATAPRINT(float)
ITK_IMAGE_TYPE_METADATAPRINT(double)
ITK_IMAGE_TYPE_METADATAPRINT(std::string)
ITK_IMAGE_TYPE_METADATAPRINT(std::complex<float>)
ITK_IMAGE_TYPE_METADATAPRINT(std::complex<double>)
//================================================================================
//================================================================================
//================================================================================
//================================================================================
#endif

int itkMetaDataDictionaryTest(int , char * [])
{
  //This is a demo program to show how to put data into a dictionary.
  itk::MetaDataDictionary MyDictionary;

  //------------------------Testing of native types
  //-------Floats
  itk::EncapsulateMetaData<float>(MyDictionary,"ASimpleFloatInitalized",static_cast<float>(1.234560F));
  {
    float tempfloat;
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
  //NOTE: Only the pointer is copied, not the data withing the pointer!
  itk::EncapsulateMetaData<char *>(MyDictionary,"MemoryChangedOutsideOfDictionary",StrandedMemory);
  {
    char * temp;
    itk::ExposeMetaData<char *>(MyDictionary,"MemoryChangedOutsideOfDictionary",temp);
    std::cout << "Memory Before Change: "<<temp <<std::endl;
  }
  strcpy(StrandedMemory,"------------This this was changed outside the class, and may cause all types of errors.");
  {
    char * temp;
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
#endif


  //NOTE: Must clean up memory allocated with char * StrandedMemory=new char[2345];
  delete [] StrandedMemory;

  return EXIT_SUCCESS;

}
