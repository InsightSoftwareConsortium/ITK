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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef __itkMetaDataObject_h
#define __itkMetaDataObject_h

#include "itkMetaDataDictionary.h"
#include "itkMacro.h"
#include "itkCommand.h"
#include "itkFastMutexLock.h"

#include <string.h>
#include <cstring>

namespace itk
{
/**
 * \class MetaDataObject
 * \author Hans J. Johnson
 * The MetaDataObject class is a templated class that
 * is a specialization of the MetaDataObjectBase type.
 * This class allows arbitrary data types to be
 * stored as MetaDataObjectBase types, and to be stored in
 * a MetaDataDictionary.
 *
 * Any class or built in type that has valid copy constructor and operator=
 * can be wrapped directly with this simple template type.
 *
 * Classes or built in types that do not have valid copy constructors or operator=
 * implemented will have to implement those functions by deriving from MetaDataObject<MetaDataObjectType>
 * and redefining the copy constructor and initializing constructor and the Get/Set functions
 * to work around those deficiencies.
 *
 * The behavior of the MetaDataObject<Type>::Print() function has many plausible
 * application dependant implementations.  The default implementation prints the
 * string "[UNKNOWN PRINT CHARACTERISTICS]" that works for all possible
 * MetaDataObject types.
 *
 * The application developer may overload the default implementation to provide
 * a specialized Print() characteristics to produce results desirable for their application.
 * A set of very crude Macros {NATIVE_TYPE_METADATAPRINT, ITK_OBJECT_TYPE_METADATAPRINT_1COMMA, ITK_IMAGE_TYPE_METADATAPRINT  }
 * are provided to facilitate a very simple implementation, and as an example.
 * \ingroup ITK-Common
 */
template< class MetaDataObjectType >
class ITK_EXPORT MetaDataObject:public MetaDataObjectBase
{
public:
  /** Smart pointer typedef support. */
  typedef MetaDataObject             Self;
  typedef MetaDataObjectBase         Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaDataObject, MetaDataObjectBase);

  /**
   * \author Hans J. Johnson
   * Default constructor with no initialization.
   */
  MetaDataObject(void);
  /** \author Hans J. Johnson
   * Default virtual Destructor
   */
  virtual ~MetaDataObject(void);
  /**
   * \author Hans J. Johnson
   * Initializer constructor that sets m_MetaDataObjectValue to InitializerValue
   */
  MetaDataObject(const MetaDataObjectType InitializerValue);
  /**
   * \author Hans J. Johnson
   * Copy constructor that sets m_MetaDataObjectValue to TemplateObject.m_MetaDataObjectValue
   */
  MetaDataObject(const MetaDataObject< MetaDataObjectType > & TemplateObject);
  /**
   * \author Hans J. Johnson
   *
   * The definition of this function is necessary to fulfill
   * the interface of the MetaDataObjectBase
   * \return A pointer to a const char array containing the unique type name.
   */
  virtual const char * GetMetaDataObjectTypeName(void) const;

  /**
   * \author Hans J. Johnson
   *
   * The definition of this function is necessary to fulfill
   * the interface of the MetaDataObjectBase
   * \return A constant reference to a std::type_info object
   */
  virtual const std::type_info & GetMetaDataObjectTypeInfo(void) const;

  /**
   * \author Hans J. Johnson
   * Function to return the stored value of type MetaDataObjectType.
   * \return a constant reference to a MetaDataObjectType
   */
  const MetaDataObjectType & GetMetaDataObjectValue(void) const;

  /**
   * \author Hans J. Johnson
   * Function to set the stored value of type MetaDataObjectType.
   * \param NewValue A constant reference to at MetaDataObjectType.
   */
  void SetMetaDataObjectValue(const MetaDataObjectType & NewValue);

  /**
   * Defines the default behavior for printing out this element
   * \param os An output stream
   */
  virtual void Print(std::ostream & os) const;

private:
  //This is made private to force the use of the
  // MetaDataObject<MetaDataObjectType>::New() operator!
  //void * operator new(SizeValueType nothing) {};//purposefully not implemented
  /**
   * \author Hans J. Johnson
   * A variable to store this derived type.
   */
  MetaDataObjectType m_MetaDataObjectValue;
};

/**
 * EncapsulateMetaData is a convenience function that encapsulates raw MetaData into a
 * MetaDataObject that can be put into the MetaDataDictionary.
 * \param Dictionary TODO
 * \param key TODO
 * \param invalue the value of type T that is to be encapsulated.
 * \return A smartpointer ot a MetaDataObject that is suitable for
 * insertion into a MetaDataDictionary.
 */
template< class T >
inline void EncapsulateMetaData(MetaDataDictionary & Dictionary, const std::string & key, const T & invalue)
{
  typename MetaDataObject< T >::Pointer temp = MetaDataObject< T >::New();
  temp->SetMetaDataObjectValue(invalue);
  Dictionary[key] = temp;
}

template< class T >
inline void EncapsulateMetaData(MetaDataDictionary & Dictionary, const char *key, const T & invalue)
{
  EncapsulateMetaData(Dictionary, std::string(key), invalue);
}

/**
 * FindValInDictionary provides a shortcut for pulling a value of type
 * T out of a MetaDataDictionary.
 * If Dictionary[key] isn't set, return false, otherwise copy into
 * outval reference and return true.
 * \param Dictionary -- reference to a dictionary
 * \param key -- string identifier for this object
 * \param outval -- where to store value found in table.
 */
template< class T >
inline bool ExposeMetaData(MetaDataDictionary & Dictionary, const std::string key, T & outval)
{
  if ( !Dictionary.HasKey(key) )
    {
    return false;
    }

  MetaDataObjectBase::Pointer baseObjectSmartPointer = Dictionary[key];

  if ( strcmp( typeid( T ).name(), baseObjectSmartPointer->GetMetaDataObjectTypeName() ) != 0 )
    {
    return false;
    }
    {
    if ( MetaDataObject< T > *TempMetaDataObject = dynamic_cast< MetaDataObject< T > * >( Dictionary[key].GetPointer() ) )
      {
      outval = TempMetaDataObject->GetMetaDataObjectValue();
      }
    else
      {
      return false;
      }
    }
  return true;
}

// This should not change the behavior, it just adds an extra level of complexity
// to using the ExposeMetaData with const char * keys.
template< class T >
inline bool ExposeMetaData(MetaDataDictionary & Dictionary, const char *const key, T & outval)
{
  return ExposeMetaData(Dictionary, std::string(key), outval);
}

// const versions of ExposeMetaData just to make life easier for enduser
// programmers, and to maintain backwards compatibility.
// The other option is to cast away constness in the main function.
template< class T >
inline bool ExposeMetaData(const MetaDataDictionary & Dictionary, const std::string key, T & outval)
{
  MetaDataDictionary NonConstVersion = Dictionary;

  return ExposeMetaData(NonConstVersion, key, outval);
}

template< class T >
inline bool ExposeMetaData(const MetaDataDictionary & Dictionary, const char *const key, T & outval)
{
  MetaDataDictionary NonConstVersion = Dictionary;

  return ExposeMetaData(Dictionary, std::string(key), outval);
}
} // end namespace itk

/**
 * NATIVE_TYPE_METADATAPRINT
 * An ugly macro to facilitate creating a simple implementation of
 * the MetaDataObject<Type>::Print() function for types that
 * have operator<< defined.
 * \param TYPE_NAME the native type parameter type
 */
#define NATIVE_TYPE_METADATAPRINT(TYPE_NAME)        \
  template< >                                       \
  void                                              \
  itk::MetaDataObject< TYPE_NAME >                  \
  ::Print(std::ostream & os) const                  \
    {                                               \
    os << this->m_MetaDataObjectValue << std::endl; \
    }                                               \
  template< >                                       \
  void                                              \
  itk::MetaDataObject< const TYPE_NAME >            \
  ::Print(std::ostream & os) const                  \
    {                                               \
    os << this->m_MetaDataObjectValue << std::endl; \
    }

/**
 * ITK_OBJECT_TYPE_METADATAPRINT_1COMMA
 * An ugly macro to facilitate creating a simple implementation of
 * the MetaDataObject< Type >::Print() function for
 * itk::Objects that have 1 comma in their type definition
 * \param TYPE_NAME_PART1
 * \param TYPE_NAME_PART2
 */
#define ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(TYPE_NAME_PART1, TYPE_NAME_PART2) \
  template< >                                                                  \
  void                                                                         \
  itk::MetaDataObject< TYPE_NAME_PART1, TYPE_NAME_PART2 >                      \
  ::Print(std::ostream & os) const                                             \
    {                                                                          \
    this->m_MetaDataObjectValue->Print(os);                                    \
    }                                                                          \
  template< >                                                                  \
  void                                                                         \
  itk::MetaDataObject< const TYPE_NAME_PART1, TYPE_NAME_PART2 >                \
  ::Print(std::ostream & os) const                                             \
    {                                                                          \
    this->m_MetaDataObjectValue->Print(os);                                    \
    }

/**
 * ITK_IMAGE_TYPE_METADATAPRINT
 * An ugly macro to facilitate creating a simple implementation of
 * the MetaDataObject<Type>::Print() function for
 * itk::Image<STORAGE_TYPE,[1-8]>::Pointer
 * \param STORAGE_TYPE The storage type of the image type to print.
 */
#define ITK_IMAGE_TYPE_METADATAPRINT(STORAGE_TYPE)                             \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image< STORAGE_TYPE, 1 >::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image< STORAGE_TYPE, 2 >::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image< STORAGE_TYPE, 3 >::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image< STORAGE_TYPE, 4 >::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image< STORAGE_TYPE, 5 >::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image< STORAGE_TYPE, 6 >::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image< STORAGE_TYPE, 7 >::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image< STORAGE_TYPE, 8 >::Pointer) \

#if 0
// Define a specialization of the Print function
// for some basic types. We don't use the initial
// NATIVE_TYPE_METADATAPRINT macro because it lacks
// the inline keyword and it tries to specialize
// for const types which does not compile on MSVC
#define NATIVE_TYPE_METADATAPRINT_NOCONST(TYPE_NAME) \
  template< >                                        \
  inline void itk::MetaDataObject< TYPE_NAME >       \
  ::Print(std::ostream & os) const                   \
    {                                                \
    os << this->m_MetaDataObjectValue << std::endl;  \
    }

NATIVE_TYPE_METADATAPRINT_NOCONST(unsigned char)
NATIVE_TYPE_METADATAPRINT_NOCONST(short)
NATIVE_TYPE_METADATAPRINT_NOCONST(unsigned short)
NATIVE_TYPE_METADATAPRINT_NOCONST(int)
NATIVE_TYPE_METADATAPRINT_NOCONST(unsigned int)
NATIVE_TYPE_METADATAPRINT_NOCONST(long)
NATIVE_TYPE_METADATAPRINT_NOCONST(unsigned long)
NATIVE_TYPE_METADATAPRINT_NOCONST(float)
NATIVE_TYPE_METADATAPRINT_NOCONST(double)
NATIVE_TYPE_METADATAPRINT_NOCONST(std::string)

// undef the macro to clean up things
#undef NATIVE_TYPE_METADATAPRINT_NOCONST
#endif

// Define instantiation macro for this template.
#define ITK_TEMPLATE_MetaDataObject(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                  \
  {                                                              \
  _( 1 ( class EXPORT MetaDataObject< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                            \
  {                                                              \
  typedef MetaDataObject< ITK_TEMPLATE_1 TypeX >                 \
  MetaDataObject##TypeY;                                       \
  }                                                              \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkMetaDataObject+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkMetaDataObject.txx"
#endif

#endif //itkMetaDataObject_h
