/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkMetaDataObject.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

Portions of this code are covered under the VTK copyright.
See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef MetaDataObject_h_h
#define MetaDataObject_h_h

//#include "itkMetaDataObjectBase.h"
#include "itkMetaDataDictionary.h"
#include "itkMacro.h"
#include "itkObjectFactory.h"
#include "itkCommand.h"
#include "itkFastMutexLock.h"
#include <string>


namespace itk
{
  /**
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
   * a string "[UNKNOWN PRINT CHARACTERISTICS]" that is works for all possible
   * MetaDataObject types.
   *
   * The application developer may overload the default implementation to provide
   * a specialized Print() characteristics to produce results desirable for their applicaiton.
   * A set of very crude Macros {NATIVE_TYPE_METADATAPRINT, ITK_OBJECT_TYPE_METADATAPRINT_1COMMA, ITK_IMAGE_TYPE_METADATAPRINT  }
   * are provided to facilitate a very simple implementation, and as an example.

   */
  template <class MetaDataObjectType>
    class ITK_EXPORT MetaDataObject: public itk::MetaDataObjectBase
    {
      public:
        /** Smart pointer typedef support. */
        typedef MetaDataObject  Self;
        typedef MetaDataObjectBase  Superclass;
        typedef SmartPointer<Self>  Pointer;
        typedef SmartPointer<const Self>  ConstPointer;

        /** Method for creation through the object factory. */
        itkNewMacro(Self);

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
        MetaDataObject(const MetaDataObject<MetaDataObjectType> &TemplateObject);
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
         * \param A constant reference to at MetaDataObjectType.
         */
        void SetMetaDataObjectValue(const MetaDataObjectType & NewValue );
        /**
         * Defines the default behavior for printing out this element
         * \param os An output stream
         */
        virtual void Print(std::ostream& os) const;
      private:
        //This is made private to force the use of the MetaDataObject<MetaDataObjectType>::New() operator!
        //void * operator new(size_t nothing) {};//purposefully not implemented
        /**
         * \author Hans J. Johnson
         * A variable to store this derived type.
         */
        MetaDataObjectType m_MetaDataObjectValue;
    };


  /**
   * EncapsulateMetaData is a convenience function that encapsulates raw MetaData into a
   * MetaDataObject that can be put into the MetaDataDictionary.
   * \param value the value of type T that is to be encapsulated.
   * \return A smartpointer ot a MetaDataObject that is suitable for
   * insertion into a MetaDataDictionary.
   */
  template <class T>
    inline void EncapsulateMetaData(MetaDataDictionary &Dictionary, const std::string & key, const T &invalue)
    {
      MetaDataObject<T>::Pointer temp=MetaDataObject<T>::New();
      temp->SetMetaDataObjectValue(invalue);
      Dictionary[key]=temp;
    }

  template <class T>
    inline void EncapsulateMetaData(MetaDataDictionary &Dictionary, const char *key, const T &invalue)
    {
      EncapsulateMetaData(Dictionary, std::string(key), invalue);
    }

  /**
   * FindValInDictionary provides a shortcut for pulling a value of type
   * T out of a MetaDataDictionary.
   * If Dictionary[key] isn't set, return false, otherwise copy into
   * outval reference and return true.
   * \param Dictionary -- reference to a dictionary
   * \parm key -- string identifier for this object
   * \param outval -- where to store value found in table.
   */
  template <class T>
    inline bool ExposeMetaData(MetaDataDictionary &Dictionary, const std::string key, T &outval)
    {
      if(Dictionary.find(key) == Dictionary.end())
      {
        return false;
      }

      MetaDataObjectBase::Pointer baseObjectSmartPointer = Dictionary[key];

      if(strcmp(typeid(T).name(),baseObjectSmartPointer->GetMetaDataObjectTypeName()) != 0)
      {
        return false;
      }
#if (defined(__sgi) && !defined(__GNUC__))
      outval =
        reinterpret_cast<MetaDataObject <T> *>(Dictionary[key].GetPointer())->GetMetaDataObjectValue();
#else
      outval =
        dynamic_cast<MetaDataObject <T> *>(Dictionary[key].GetPointer())->GetMetaDataObjectValue();

#endif
      //                                 --------------- ^^^^^^^^^^^^
      //                                 SmartPointer    MetaDataObject<T>*
      return true;
    }

  template <class T>
    inline bool ExposeMetaData(MetaDataDictionary &Dictionary, const char *key, T &outval)
    {
      return ExposeMetaData(Dictionary, std::string(key), outval);
    }

} // end namespace itk

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
 * itk::Image<STORAGE_TYPE,[1-8]>::Pointer
 * \param STORAGE_TYPE The storage type of the image type to print.
 */
#define ITK_IMAGE_TYPE_METADATAPRINT(STORAGE_TYPE) \
ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,1>::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,2>::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,3>::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,4>::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,5>::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,6>::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,7>::Pointer) \
  ITK_OBJECT_TYPE_METADATAPRINT_1COMMA(itk::Image<STORAGE_TYPE,8>::Pointer) \

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMetaDataObject.txx"
#endif

#endif //MetaDataObject_h_h

