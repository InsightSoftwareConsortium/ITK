#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
#include <iostream>

int itkMetaDataDictionaryTest(int argc, char * argv[])
{
  //This is a demo program to show how to put data into a dictionary.
  itk::MetaDataDictionary MyDictionary;

  //------------------------Testing of native types
  //-------Floats
  MyDictionary["ASimpleFloatInitalized"]=new itk::MetaDataObject<float>(1.234560F);
  std::cout << dynamic_cast<itk::MetaDataObject<float>::Pointer>(MyDictionary["ASimpleFloatInitalized"])->GetMetaDataObjectValue() << std::endl;

  MyDictionary["ASimpleConstFloatInitalized"]=new itk::MetaDataObject<const float>(-1000.234560F);
  std::cout << dynamic_cast<itk::MetaDataObject<const float>::Pointer>(MyDictionary["ASimpleConstFloatInitalized"])->GetMetaDataObjectValue() << std::endl;

  MyDictionary["ASimpleFloatUnitialized"]=new itk::MetaDataObject<float>;
  dynamic_cast<itk::MetaDataObject<float>::Pointer>(MyDictionary["ASimpleFloatUnitialized"])->SetMetaDataObjectValue(2.2);
  std::cout << dynamic_cast<itk::MetaDataObject<float>::Pointer>(MyDictionary["ASimpleFloatUnitialized"])->GetMetaDataObjectValue() << std::endl;

  MyDictionary["ASimpleFloatInitializedAndChanged"]=new itk::MetaDataObject<float>(1.234560F);
  dynamic_cast<itk::MetaDataObject<float>::Pointer>(MyDictionary["ASimpleFloatInitializedAndChanged"])->SetMetaDataObjectValue(3.3);
  std::cout << dynamic_cast<itk::MetaDataObject<float>::Pointer>(MyDictionary["ASimpleFloatInitializedAndChanged"])->GetMetaDataObjectValue() << std::endl;


  //-------Char pointers --  These can be tricky, so be careful!
  MyDictionary["char const *"]=new itk::MetaDataObject<char const *>("Value Never Seen");
  dynamic_cast<itk::MetaDataObject<char const * >::Pointer>(MyDictionary["char const *"])->SetMetaDataObjectValue("Value That Is Seen");
  //This is OK because the pointer can be changed, but the value of the pointer can not. NOTE: no the char array is not copied, just the pointer to the array.
  std::cout << (dynamic_cast<itk::MetaDataObject<char const *>::Pointer>(MyDictionary["char const *"])->GetMetaDataObjectValue()) << std::endl;

  MyDictionary["char const * const =\"Initial Value\""]=new itk::MetaDataObject<char const * const>("Initial Value");
  std::cout << (dynamic_cast<itk::MetaDataObject<char const * const>::Pointer>(MyDictionary["char const * const =\"Initial Value\""])->GetMetaDataObjectValue()) << std::endl;
  //The data can not be changed, and the pointer can not be changed.
  //Compiler Error: assignment of read-only data-member
  //dynamic_cast<itk::MetaDataObject<char const * const>::Pointer>(MyDictionary["char const * const =\"Initial Value\""])->SetMetaDataObjectValue("Compiler Error");

  const char tempvar[60]="Setting value from const variable";
  MyDictionary["char const * const = tempvar"]=new itk::MetaDataObject<char const * const>(tempvar);
  std::cout << (dynamic_cast<itk::MetaDataObject<char const * const>::Pointer>(MyDictionary["char const * const = tempvar"])->GetMetaDataObjectValue()) << std::endl;
  //Compiler Error: assignment of read-only data-member
  //dynamic_cast<itk::MetaDataObject<char const * const>::Pointer>(MyDictionary["char const * const = tempvar"])->SetMetaDataObjectValue("Compiler Error");

  //NOTE: Default value will be a pointer to NULL, or even worse to some unknown place.
  //gcc 2.96 will create this useless entry, but the SGI MipsPro compiler recognizes
  //That this is uninitialized, and causes a compiler error.
  //MyDictionary["char const * const = tempvar2"]=new itk::MetaDataObject<char const * const>;
  //
  //const char tempvar2[60]="Setting value from const variable";
  //Compiler Error: assignment of read-only data-member
  //dynamic_cast<itk::MetaDataObject<char const * const>::Pointer>(MyDictionary["char const * const = tempvar2"])->SetMetaDataObjectValue(tempvar2);
  //Runtime Error:  Trying to print a character string that points to NULL.
  //std::cout << (dynamic_cast<itk::MetaDataObject<char const * const>::Pointer>(MyDictionary["char const * const = tempvar2"])->GetMetaDataObjectValue()) << std::endl;

  //Other gotchas with the Dictionary
  char * StrandedMemeory=new char[2345];
  strcpy(StrandedMemeory,"This is stranded memory that will not be released when the Dictionary is cleaned up");
  //NOTE: Only the pointer is copied, not the data withing the pointer!
  MyDictionary["StrandedMemoryExample"]=new itk::MetaDataObject<char *>(StrandedMemeory);
  std::cout << (dynamic_cast<itk::MetaDataObject<char *>::Pointer>(MyDictionary["StrandedMemoryExample"])->GetMetaDataObjectValue()) << std::endl;
  strcpy(StrandedMemeory,"This this was changed outside the class, and may cause all types of errors.");
  std::cout << (dynamic_cast<itk::MetaDataObject<char *>::Pointer>(MyDictionary["StrandedMemoryExample"])->GetMetaDataObjectValue()) << std::endl;

  //-------Classes that can be initialized.
  MyDictionary["AnSTLString"]=new itk::MetaDataObject<std::string>("This is a std::string That is never Seen");
  dynamic_cast<itk::MetaDataObject<std::string>::Pointer>(MyDictionary["AnSTLString"])->SetMetaDataObjectValue("This is a std::string");
  std::cout << dynamic_cast<itk::MetaDataObject<std::string>::Pointer>(MyDictionary["AnSTLString"])->GetMetaDataObjectValue() << std::endl;

  for(std::map<std::string, itk::MetaDataObjectBase::Pointer>::iterator it=MyDictionary.begin();
      it != MyDictionary.end();
      it++)
  {
    std::cout << "Type name for "<<it->first <<" is " << it->second->GetMetaDataObjectTypeName() << std::endl;
  }

  //------Test copying of Dictionary
  //itk::MetaDataDictionary NewDictionary(MyDictionary);
  itk::MetaDataDictionary NewDictionary;
  NewDictionary=MyDictionary;

  std::cout << dynamic_cast<itk::MetaDataObject<float>::Pointer>(NewDictionary["ASimpleFloatInitalized"])->GetMetaDataObjectValue() << std::endl;
  std::cout << dynamic_cast<itk::MetaDataObject<const float>::Pointer>(NewDictionary["ASimpleConstFloatInitalized"])->GetMetaDataObjectValue() << std::endl;
  std::cout << dynamic_cast<itk::MetaDataObject<float>::Pointer>(NewDictionary["ASimpleFloatUnitialized"])->GetMetaDataObjectValue() << std::endl;
  std::cout << dynamic_cast<itk::MetaDataObject<float>::Pointer>(NewDictionary["ASimpleFloatInitializedAndChanged"])->GetMetaDataObjectValue() << std::endl;
  std::cout << (dynamic_cast<itk::MetaDataObject<char const *>::Pointer>(NewDictionary["char const *"])->GetMetaDataObjectValue()) << std::endl;
  std::cout << (dynamic_cast<itk::MetaDataObject<char const * const>::Pointer>(NewDictionary["char const * const =\"Initial Value\""])->GetMetaDataObjectValue()) << std::endl;
  std::cout << (dynamic_cast<itk::MetaDataObject<char const * const>::Pointer>(NewDictionary["char const * const = tempvar"])->GetMetaDataObjectValue()) << std::endl;
  std::cout << dynamic_cast<itk::MetaDataObject<std::string>::Pointer>(NewDictionary["AnSTLString"])->GetMetaDataObjectValue() << std::endl;

  for(std::map<std::string, itk::MetaDataObjectBase::Pointer>::iterator it=NewDictionary.begin();
      it != NewDictionary.end();
      it++)
  {
    std::cout << "Type name for "<<it->first <<" is " << it->second->GetMetaDataObjectTypeInfo().name() << std::endl;
  }

  //PrintSelf functionality Test
  std::cout << "===========================================================" << std::endl;
  std::cout << "Printing Dictionary" << std::endl;
  NewDictionary.PrintSelf(std::cout, 10);
  return 0;
}
