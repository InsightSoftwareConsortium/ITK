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

#include <cstring>

namespace itk
{
/**
 * \class MetaDataObject
 * \brief Allows arbitrary data types to be stored as MetaDataObjectBase types,
 *        and to be stored in a MetaDataDictionary.
 *
 * \author Hans J. Johnson
 *
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
 * application dependent implementations.  The default implementation prints the
 * string "[UNKNOWN PRINT CHARACTERISTICS]" that works for all possible
 * MetaDataObject types.
 *
 * The application developer may overload the default implementation to provide
 * a specialized Print() characteristics to produce results desirable for their application.
 * A set of very crude Macros {NATIVE_TYPE_METADATAPRINT, ITK_OBJECT_TYPE_METADATAPRINT_1COMMA, ITK_IMAGE_TYPE_METADATAPRINT  }
 * are provided to facilitate a very simple implementation, and as an example.
 * \ingroup ITKCommon
 */
template< typename MetaDataObjectType >
class MetaDataObject:public MetaDataObjectBase
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
   * The definition of this function is necessary to fulfill
   * the interface of the MetaDataObjectBase
   * \author Hans J. Johnson
   * \return A pointer to a const char array containing the unique type name.
   */
  virtual const char * GetMetaDataObjectTypeName() const;

  /**
   * The definition of this function is necessary to fulfill
   * the interface of the MetaDataObjectBase
   * \author Hans J. Johnson
   * \return A constant reference to a std::type_info object
   */
  virtual const std::type_info & GetMetaDataObjectTypeInfo() const;

  /**
   * Function to return the stored value of type MetaDataObjectType.
   * \author Hans J. Johnson
   * \return a constant reference to a MetaDataObjectType
   */
  const MetaDataObjectType & GetMetaDataObjectValue() const;

  /**
   * Function to set the stored value of type MetaDataObjectType.
   * \author Hans J. Johnson
   * \param NewValue A constant reference to at MetaDataObjectType.
   */
  void SetMetaDataObjectValue(const MetaDataObjectType & newValue);

  /**
   * Defines the default behavior for printing out this element
   * \param os An output stream
   */
  virtual void Print(std::ostream & os) const;

protected:
  MetaDataObject();
  virtual ~MetaDataObject();

private:
  MetaDataObject(const Self &); // purposely not implemented
  void operator=(const Self &); // purposely not implemented

  /**
   * A variable to store this derived type.
   * \author Hans J. Johnson
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
template< typename T >
inline void EncapsulateMetaData(MetaDataDictionary & Dictionary, const std::string & key, const T & invalue)
{
  typename MetaDataObject< T >::Pointer temp = MetaDataObject< T >::New();
  temp->SetMetaDataObjectValue(invalue);
  Dictionary[key] = temp;
}

template< typename T >
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
template< typename T >
inline bool ExposeMetaData(const MetaDataDictionary & Dictionary, const std::string key, T & outval)
{
  if ( !Dictionary.HasKey(key) )
    {
    return false;
    }

  const MetaDataObjectBase::ConstPointer baseObjectSmartPointer = Dictionary[key];

  if ( baseObjectSmartPointer.IsNull() || strcmp( typeid( T ).name(), baseObjectSmartPointer->GetMetaDataObjectTypeName() ) != 0 )
    {
    return false;
    }
    {
    MetaDataObject< T > const * const TempMetaDataObject = dynamic_cast< MetaDataObject< T > const * >( baseObjectSmartPointer.GetPointer() );
    if ( TempMetaDataObject != NULL )
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

// Specializations
template <> ITKCommon_EXPORT void MetaDataObject< unsigned char >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< char >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< signed char >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< unsigned short >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< short >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< unsigned int >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< int >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< unsigned long >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< long >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< float >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< double >::Print( std::ostream & os ) const;
template <> ITKCommon_EXPORT void MetaDataObject< std::string >::Print( std::ostream & os ) const;

} // end namespace itk

/**
 * \def NATIVE_TYPE_METADATAPRINT( TYPE_NAME )
 * \brief An ugly macro to facilitate creating a simple implementation of
 * the MetaDataObject<Type>::Print() function for types that
 * have operator<< defined.
 * \param TYPE_NAME the native type parameter type
 */
#define ITK_NATIVE_TYPE_METADATAPRINT(TYPE_NAME)        \
  template< >                                       \
  void                                              \
  ::itk::MetaDataObject< TYPE_NAME >                  \
  ::Print(std::ostream & os) const                  \
    {                                               \
    os << this->m_MetaDataObjectValue << std::endl; \
    }                                               \

/**
 * \def ITK_OBJECT_TYPE_METADATAPRINT_1COMMA( TYPE_NAME_PART1, TYPE_NAME_PART2 )
 * \brief An ugly macro to facilitate creating a simple implementation of
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

/**
 * \def ITK_IMAGE_TYPE_METADATAPRINT( STORAGE_TYPE )
 * An ugly macro to facilitate creating a simple implementation of
 * the MetaDataObject<Type>::Print() function for
 * itk::Image\<STORAGE_TYPE,[1-8]\>\::Pointer
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

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMetaDataObject.hxx"
#endif

#endif //itkMetaDataObject_h
