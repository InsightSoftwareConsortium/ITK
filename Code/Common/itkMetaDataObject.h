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

#include "itkMetaDataObjectBase.h"

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
   */
  template <class MetaDataObjectType>
    class MetaDataObject: public itk::MetaDataObjectBase
    {
      public:
        typedef MetaDataObject Self;
        typedef itk::MetaDataObjectBase Superclass;
        typedef Self * Pointer;
        typedef const Self * ConstPointer;
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
         * \param indent an indent value.
         */
        virtual void PrintSelf(std::ostream& os, Indent indent) const;
      private:
        /**
         * \author Hans J. Johnson
         * A variable to store this derived type.
         */
        MetaDataObjectType m_MetaDataObjectValue;
    };
} // end namespace itk

//Define the PrintSelf for known types.  These are defined in itkMetaDataObject.cxx;
extern void itk::MetaDataObject<float>::PrintSelf(std::ostream& os, Indent indent) const;
extern void itk::MetaDataObject<double>::PrintSelf(std::ostream& os, Indent indent) const;
extern void itk::MetaDataObject<char>::PrintSelf(std::ostream& os, Indent indent) const;
extern void itk::MetaDataObject<unsigned char>::PrintSelf(std::ostream& os, Indent indent) const;
extern void itk::MetaDataObject<int>::PrintSelf(std::ostream& os, Indent indent) const;
extern void itk::MetaDataObject<unsigned int>::PrintSelf(std::ostream& os, Indent indent) const;
extern void itk::MetaDataObject<short int>::PrintSelf(std::ostream& os, Indent indent) const;
extern void itk::MetaDataObject<unsigned short int>::PrintSelf(std::ostream& os, Indent indent) const;

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMetaDataObject.txx"
#endif

#endif //MetaDataObject_h_h

