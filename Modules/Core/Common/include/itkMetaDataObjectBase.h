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
#ifndef itkMetaDataObjectBase_h
#define itkMetaDataObjectBase_h

#include "itkLightObject.h"
#include <typeinfo>
#include <iostream>

namespace itk
{
/** \class MetaDataObjectBase
 * \brief The common interface for MetaDataObject's.
 *
 * This class is intended as the value part
 * of the (key,value) pair to be stored in
 * a MetaDataDictionary
 *
 * \author Hans J. Johnson
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT MetaDataObjectBase: public LightObject
{
public:
  /** Smart pointer typedef support. */
  typedef MetaDataObjectBase         Self;
  typedef LightObject                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MetaDataObjectBase, LightObject);

  /**
   * \author Hans J. Johnson
   * \return A pointer to a const char array containing the unique type name.
   */
  virtual const char * GetMetaDataObjectTypeName() const;

  /**
   * \author Hans J. Johnson
   * \return A constant reference to a std::type_info object
   */
  virtual const std::type_info & GetMetaDataObjectTypeInfo() const;

  /**
   * Defines the default behavior for printing out this element
   * \param os An output stream
   */
  virtual void Print( std::ostream & os ) const;

protected:
  MetaDataObjectBase();
  virtual ~MetaDataObjectBase() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MetaDataObjectBase);
};
}

#endif //itkMetaDataObjectBase_h
