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

int itkMetaDataDictionaryTest(int argc, char * argv[])
{
  //This is a demo program to show how to put data into a dictionary.
  itk::MetaDataDictionary MyDictionary;

  //------------------------Testing of native types
  //-------Floats
  MyDictionary["ASimpleFloatInitalized"]=new itk::MetaDataObject<float>(1.234560F);
  std::cout << dynamic_cast<itk::MetaDataObject<float> *>(MyDictionary["ASimpleFloatInitalized"].GetPointer())->GetMetaDataObjectValue() << std::endl;

  MyDictionary["ASimpleConstFloatInitalized"]=new itk::MetaDataObject<const float>(-1000.234560F);
  std::cout << dynamic_cast<itk::MetaDataObject<const float> *>(MyDictionary["ASimpleConstFloatInitalized"].GetPointer())->GetMetaDataObjectValue() << std::endl;

  MyDictionary["ASimpleFloatUnitialized"]=new itk::MetaDataObject<float>;
  dynamic_cast<itk::MetaDataObject<float> *>(MyDictionary["ASimpleFloatUnitialized"].GetPointer())->SetMetaDataObjectValue(2.2);
  std::cout << dynamic_cast<itk::MetaDataObject<float> *>(MyDictionary["ASimpleFloatUnitialized"].GetPointer())->GetMetaDataObjectValue() << std::endl;

  MyDictionary["ASimpleFloatInitializedAndChanged"]=new itk::MetaDataObject<float>(1.234560F);
  dynamic_cast<itk::MetaDataObject<float> *>(MyDictionary["ASimpleFloatInitializedAndChanged"].GetPointer())->SetMetaDataObjectValue(3.3);
  std::cout << dynamic_cast<itk::MetaDataObject<float> *>(MyDictionary["ASimpleFloatInitializedAndChanged"].GetPointer())->GetMetaDataObjectValue() << std::endl;


  //-------Char pointers --  These can be tricky, so be careful!
  MyDictionary["char const *"]=new itk::MetaDataObject<char const *>("Value Never Seen");
  dynamic_cast<itk::MetaDataObject<char const * > *>(MyDictionary["char const *"].GetPointer())->SetMetaDataObjectValue("Value That Is Seen");
  //This is OK because the pointer can be changed, but the value of the pointer can not. NOTE: no the char array is not copied, just the pointer to the array.
  std::cout << (dynamic_cast<itk::MetaDataObject<char const *> *>(MyDictionary["char const *"].GetPointer())->GetMetaDataObjectValue()) << std::endl;

  MyDictionary["char const * const =\"Initial Value\""]=new itk::MetaDataObject<char const * const>("Initial Value");
  std::cout << (dynamic_cast<itk::MetaDataObject<char const * const> *>(MyDictionary["char const * const =\"Initial Value\""].GetPointer())->GetMetaDataObjectValue()) << std::endl;
  //The data can not be changed, and the pointer can not be changed.
  //Compiler Error: assignment of read-only data-member
  //dynamic_cast<itk::MetaDataObject<char const * const> *>(MyDictionary["char const * const =\"Initial Value\""])->SetMetaDataObjectValue("Compiler Error");

  const char tempvar[60]="Setting value from const variable";
  MyDictionary["char const * const = tempvar"]=new itk::MetaDataObject<char const * const>(tempvar);
  std::cout << (dynamic_cast<itk::MetaDataObject<char const * const> *>(MyDictionary["char const * const = tempvar"].GetPointer())->GetMetaDataObjectValue()) << std::endl;
  //Compiler Error: assignment of read-only data-member
  //dynamic_cast<itk::MetaDataObject<char const * const> *>(MyDictionary["char const * const = tempvar"])->SetMetaDataObjectValue("Compiler Error");

  //NOTE: Default value will be a pointer to NULL, or even worse to some unknown place.
  //gcc 2.96 will create this useless entry, but the SGI MipsPro compiler recognizes
  //That this is uninitialized, and causes a compiler error.
  //MyDictionary["char const * const = tempvar2"]=new itk::MetaDataObject<char const * const>;
  //
  //const char tempvar2[60]="Setting value from const variable";
  //Compiler Error: assignment of read-only data-member
  //dynamic_cast<itk::MetaDataObject<char const * const> *>(MyDictionary["char const * const = tempvar2"])->SetMetaDataObjectValue(tempvar2);
  //Runtime Error:  Trying to print a character string that points to NULL.
  //std::cout << (dynamic_cast<itk::MetaDataObject<char const * const> *>(MyDictionary["char const * const = tempvar2"])->GetMetaDataObjectValue()) << std::endl;

  //Other gotchas with the Dictionary
  char * StrandedMemeory=new char[2345];
  strcpy(StrandedMemeory,"This is stranded memory that will not be released when the Dictionary is cleaned up");
  //NOTE: Only the pointer is copied, not the data withing the pointer!
  MyDictionary["StrandedMemoryExample"]=new itk::MetaDataObject<char *>(StrandedMemeory);
  std::cout << (dynamic_cast<itk::MetaDataObject<char *> *>(MyDictionary["StrandedMemoryExample"].GetPointer())->GetMetaDataObjectValue()) << std::endl;
  strcpy(StrandedMemeory,"This this was changed outside the class, and may cause all types of errors.");
  std::cout << (dynamic_cast<itk::MetaDataObject<char *> *>(MyDictionary["StrandedMemoryExample"].GetPointer())->GetMetaDataObjectValue()) << std::endl;

  //-------Classes that can be initialized.
  MyDictionary["AnSTLString"]=new itk::MetaDataObject<std::string>("This is a std::string That is never Seen");
  dynamic_cast<itk::MetaDataObject<std::string> *>(MyDictionary["AnSTLString"].GetPointer())->SetMetaDataObjectValue("This is a std::string");
  std::cout << dynamic_cast<itk::MetaDataObject<std::string> *>(MyDictionary["AnSTLString"].GetPointer())->GetMetaDataObjectValue() << std::endl;

  //------- An ITK  Image
  MyDictionary["AnITKImage"]=new itk::MetaDataObject<itk::Image<float,3> *>(itk::Image<float,3>::New());

  std::map<std::string, itk::MetaDataObjectBase::Pointer>::iterator it;
  for(it=MyDictionary.begin();
      it != MyDictionary.end();
      it++)
  {
    std::cout << "Type name for "<<it->first <<" is " << it->second->GetMetaDataObjectTypeName() << std::endl;
  }

  //------Test copying of Dictionary
  //itk::MetaDataDictionary NewDictionary(MyDictionary);
  itk::MetaDataDictionary NewDictionary;
  NewDictionary=MyDictionary;

  std::cout << dynamic_cast<itk::MetaDataObject<float> *>(NewDictionary["ASimpleFloatInitalized"].GetPointer())->GetMetaDataObjectValue() << std::endl;
  std::cout << dynamic_cast<itk::MetaDataObject<const float> *>(NewDictionary["ASimpleConstFloatInitalized"].GetPointer())->GetMetaDataObjectValue() << std::endl;
  std::cout << dynamic_cast<itk::MetaDataObject<float> *>(NewDictionary["ASimpleFloatUnitialized"].GetPointer())->GetMetaDataObjectValue() << std::endl;
  std::cout << dynamic_cast<itk::MetaDataObject<float> *>(NewDictionary["ASimpleFloatInitializedAndChanged"].GetPointer())->GetMetaDataObjectValue() << std::endl;
  std::cout << (dynamic_cast<itk::MetaDataObject<char const *> *>(NewDictionary["char const *"].GetPointer())->GetMetaDataObjectValue()) << std::endl;
  std::cout << (dynamic_cast<itk::MetaDataObject<char const * const> *>(NewDictionary["char const * const =\"Initial Value\""].GetPointer())->GetMetaDataObjectValue()) << std::endl;
  std::cout << (dynamic_cast<itk::MetaDataObject<char const * const> *>(NewDictionary["char const * const = tempvar"].GetPointer())->GetMetaDataObjectValue()) << std::endl;
  std::cout << dynamic_cast<itk::MetaDataObject<std::string> *>(NewDictionary["AnSTLString"].GetPointer())->GetMetaDataObjectValue() << std::endl;

  for(it=NewDictionary.begin();
      it != NewDictionary.end();
      it++)
  {
    std::cout << "Type name for "<<it->first <<" is " << it->second->GetMetaDataObjectTypeInfo().name() << std::endl;
  }

  //Print functionality Test
  std::cout << "===========================================================" << std::endl;
  std::cout << "Printing Dictionary" << std::endl;
  NewDictionary.Print(std::cout);
  return 0;
}
