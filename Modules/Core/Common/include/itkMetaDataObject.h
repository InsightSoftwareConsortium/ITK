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
#ifndef itkMetaDataObject_h
#define itkMetaDataObject_h

#include "itkMetaDataDictionary.h"
#include "itkMacro.h"
#include "itkArray.h"
#include "itkMatrix.h"

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
 * The default implementation prints uses the MetaDataObjectType's Print or operator<< if available. Otherwise, it
 * prints string "[UNKNOWN PRINT CHARACTERISTICS]".
 *
 * \ingroup ITKCommon
 *
 */
template <typename MetaDataObjectType>
class ITK_TEMPLATE_EXPORT MetaDataObject : public MetaDataObjectBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MetaDataObject);

  /** Smart pointer type alias support */
  using Self = MetaDataObject;
  using Superclass = MetaDataObjectBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkFactorylessNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MetaDataObject);

  /**
   * The definition of this function is necessary to fulfill
   * the interface of the MetaDataObjectBase
   * \author Hans J. Johnson
   * \return A pointer to a const char array containing the unique type name.
   */
  const char *
  GetMetaDataObjectTypeName() const override;

  /**
   * The definition of this function is necessary to fulfill
   * the interface of the MetaDataObjectBase
   * \author Hans J. Johnson
   * \return A constant reference to a std::type_info object
   */
  const std::type_info &
  GetMetaDataObjectTypeInfo() const override;

  /**
   * Function to return the stored value of type MetaDataObjectType.
   * \author Hans J. Johnson
   * \return a constant reference to a MetaDataObjectType
   */
  const MetaDataObjectType &
  GetMetaDataObjectValue() const;

  /**
   * Function to set the stored value of type MetaDataObjectType.
   * \author Hans J. Johnson
   * \param newValue A constant reference to at MetaDataObjectType.
   */
  void
  SetMetaDataObjectValue(const MetaDataObjectType & newValue);

  /**
   * Defines the default behavior for printing out this element
   * \param os An output stream
   */
  void
  Print(std::ostream & os) const override;

  /** Returns (metaDataObject1 == metaDataObject2). */
  friend bool
  operator==(const Self & lhs, const Self & rhs)
  {
    return Self::EqualValues(lhs.m_MetaDataObjectValue, rhs.m_MetaDataObjectValue);
  }

  /** Returns (metaDataObject1 != metaDataObject2). */
  friend bool
  operator!=(const Self & lhs, const Self & rhs)
  {
    return !(lhs == rhs);
  }

protected:
  MetaDataObject() = default;
  ~MetaDataObject() override = default;

private:
  /** Assigns the value of `source` to `target`.
   * \note The trailing return type is there, just to enable SFINAE.*/
  template <typename TValue>
  static auto
  Assign(TValue & target, const TValue & source) -> decltype(target = source)
  {
    return target = source;
  }

  /** `Assign` overload for C-style arrays (as well as arrays of arrays). */
  template <typename TValue, size_t VNumberOfElements>
  static void
  Assign(TValue (&target)[VNumberOfElements], const TValue (&source)[VNumberOfElements])
  {
    for (size_t i = 0; i < VNumberOfElements; ++i)
    {
      Self::Assign(target[i], source[i]);
    }
  }


  /** Tells whether the specified arguments compare equal.
   * \note The trailing return type is there, just to enable SFINAE.*/
  template <typename TValue>
  static auto
  EqualValues(const TValue & lhs, const TValue & rhs) -> decltype(lhs == rhs)
  {
    return lhs == rhs;
  }

  /** `EqualValues` overload for C-style arrays (as well as arrays of arrays). */
  template <typename TValue, size_t VNumberOfElements>
  static bool
  EqualValues(const TValue (&lhs)[VNumberOfElements], const TValue (&rhs)[VNumberOfElements])
  {
    for (size_t i = 0; i < VNumberOfElements; ++i)
    {
      if (!Self::EqualValues(lhs[i], rhs[i]))
      {
        return false;
      }
    }
    return true;
  }


  /** Internal helper function used to implement operator== for MetaDataObjectBase. */
  bool
  Equal(const MetaDataObjectBase & metaDataObjectBase) const override
  {
    const auto metaDataObject = dynamic_cast<const Self *>(&metaDataObjectBase);
    return (metaDataObject != nullptr) && (*this == *metaDataObject);
  }

  /**
   * A variable to store this derived type.
   * \author Hans J. Johnson
   */
  MetaDataObjectType m_MetaDataObjectValue{};
};

/**
 * EncapsulateMetaData is a convenience function that encapsulates raw MetaData into a
 * MetaDataObject that can be put into the MetaDataDictionary.
 * \param Dictionary reference to a dictionary
 * \param key string identifier for this object
 * \param invalue the value of type T that is to be encapsulated.
 */
template <typename T>
inline void
EncapsulateMetaData(MetaDataDictionary & Dictionary, const std::string & key, const T & invalue)
{
  auto temp = MetaDataObject<T>::New();
  temp->SetMetaDataObjectValue(invalue);
  Dictionary[key] = temp;
}

template <typename T>
inline void
EncapsulateMetaData(MetaDataDictionary & Dictionary, const char * key, const T & invalue)
{
  EncapsulateMetaData(Dictionary, std::string(key), invalue);
}

/**
 * ExposeMetaData provides a shortcut for pulling a value of type
 * T out of a MetaDataDictionary.
 * If Dictionary[key] isn't set, return false, otherwise copy into
 * outval reference and return true.
 * \param Dictionary -- reference to a dictionary
 * \param key -- string identifier for this object
 * \param outval -- where to store value found in table.
 */
template <typename T>
inline bool
ExposeMetaData(const MetaDataDictionary & Dictionary, const std::string key, T & outval)
{
  auto keyIter = Dictionary.Find(key);
  if (keyIter == Dictionary.End())
  {
    return false;
  }

  const auto * const TempMetaDataObject = dynamic_cast<const MetaDataObject<T> *>(keyIter->second.GetPointer());
  if (TempMetaDataObject == nullptr)
  {
    return false;
  }

  outval = TempMetaDataObject->GetMetaDataObjectValue();
  return true;
}

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMetaDataObject.hxx"
#endif

#endif // itkMetaDataObject_h

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_MetaDataObject
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#if defined(ITKCommon_EXPORTS)
//   We are building this library
#  define ITKCommon_EXPORT_EXPLICIT ITK_TEMPLATE_EXPORT
#else
//   We are using this library
#  define ITKCommon_EXPORT_EXPLICIT ITKCommon_EXPORT
#endif
namespace itk
{

ITK_GCC_PRAGMA_DIAG_PUSH()
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<bool>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<unsigned char>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<char>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<signed char>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<unsigned short>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<short>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<unsigned int>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<int>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<unsigned long>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<long>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<unsigned long long>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<long long>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<float>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<double>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<std::string>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<std::vector<float>>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<std::vector<double>>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<std::vector<std::vector<float>>>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<std::vector<std::vector<double>>>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<Array<char>>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<Array<int>>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<Array<float>>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<Array<double>>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<Matrix<float, 4, 4>>;
extern template class ITKCommon_EXPORT_EXPLICIT MetaDataObject<Matrix<double>>;

ITK_GCC_PRAGMA_DIAG_POP()

} // end namespace itk
#undef ITKCommon_EXPORT_EXPLICIT
#endif
