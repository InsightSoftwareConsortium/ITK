/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDataObjectBase.h
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
#ifndef MetaDataObjectBase_h_h
#define MetaDataObjectBase_h_h

#include <typeinfo>
#include <iostream>
#include "itkLightObject.h"
#include "itkWin32Header.h"
//#include "itkObjectFactory.h"


namespace itk {
  /**
   * \author Hans J. Johnson
   * The MetaDataObjectBase class is designed as the
   * common interface for MetaDataObject's.
   * This class is intended as the value part
   * of the (key,value) pair to be stored in
   * a MetaDataDictionary
   */
  class ITK_EXPORT MetaDataObjectBase : public LightObject
  {
    public:
      /** Smart pointer typedef support. */
      typedef MetaDataObjectBase  Self;
      typedef LightObject  Superclass;
      typedef SmartPointer<Self>  Pointer;
      typedef SmartPointer<const Self>  ConstPointer;

      /** Method for creation through the object factory.   */
      //itkNewMacro(Self);
      //static  Pointer New(void);

      /** Run-time type information (and related methods). */
      itkTypeMacro(MetaDataObjectBase, LightObject);

      /**
       * \author Hans J. Johnson
       * \return A pointer to a const char array containing the unique type name.
       */
      virtual const char * GetMetaDataObjectTypeName(void) const;
      /**
       * \author Hans J. Johnson
       * \return A constant reference to a std::type_info object
       */
      virtual const std::type_info & GetMetaDataObjectTypeInfo(void) const;
      /**
       * Defines the default behavior for printing out this element
       * \param os An output stream
       */
      virtual void Print(std::ostream& os) const;
    protected:
      /**
       * Default destructor
       */
      virtual ~MetaDataObjectBase();
      MetaDataObjectBase();
    private:
      MetaDataObjectBase(const Self &);//purposely not implemented
      void operator=(const Self&); //purposely not implemented
  };
}

#endif //MetaDataObjectBase_h_h

