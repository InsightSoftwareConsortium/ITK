/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkMetaDataDictionary.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

Portions of this code are covered under the VTK copyright.
See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef MetaDataDictionary_h_h
#define MetaDataDictionary_h_h

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <string>
#include "itkMetaDataObjectBase.h"

namespace itk
{
  // Forward declare the datastructure that will be used to hold the
  // dictionary. This is a private implementation.
  class MetaDataDictionaryMapType;

  /**
   * \author Hans J. Johnson
   * The MetaDataDictionary, along with the MetaDataObject derived template
   * classes, is designed to provide a mechanism for storing a collection of
   * arbitrary data types. The main motivation for such a collection is to
   * associate arbitrary data elements with itk DataObjects.
   */
  class ITKCommon_EXPORT MetaDataDictionary
    {
      public:
        typedef MetaDataDictionary Self;
        /**
         * Defines the default behavior for printing out this element
         * \param os An output stream
         */
        virtual void Print(std::ostream& os) const;

        // Constructor
        MetaDataDictionary();
        // Copy Constructor
        MetaDataDictionary(const MetaDataDictionary&);
        // operator =
        void operator=(const MetaDataDictionary&);

        // Destructor
        virtual ~MetaDataDictionary();

        /** Returns a const pointer to the internal key/value map structure. */
        const MetaDataDictionaryMapType *GetMap() const
        {
          return m_Dictionary;
        }
        // Implement map's api. On some Micorsoft compilers, stl containers
        // cannot be exported. This causes problems when building DLL's.
        // Here we inherit privately from std::map and provide a simple
        // API. The implementation will be in the DLL.
        MetaDataObjectBase::Pointer &operator [](const std::string &);
        bool HasKey (const std::string &);
    private:
        MetaDataDictionaryMapType *m_Dictionary;
    };
}
#endif// MetaDataDictionary_h_h

