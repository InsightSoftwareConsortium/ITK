/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFactoryBase.h
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
#ifndef __itkTransformFactoryBase_h
#define __itkTransformFactoryBase_h

#include "itkObjectFactoryBase.h"

namespace itk
{
/** \class TransformFactoryBase
 * \brief Create instances of Transforms
 */

class ITKCommon_EXPORT TransformFactoryBase : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef TransformFactoryBase   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer< Self >   Pointer;
  typedef SmartPointer< const Self >  ConstPointer;
  
  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformFactoryBase, ObjectFactoryBase);

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Register all builtin transforms */
  static void RegisterDefaultTransforms();

  /** Register this transform */
  static TransformFactoryBase::Pointer GetFactory () 
    {
    if ( m_Factory == 0 )
      {
      // Make and register the factory
      TransformFactoryBase::Pointer p = TransformFactoryBase::New();
      m_Factory = p.GetPointer();
      ObjectFactoryBase::RegisterFactory ( p );
      p->RegisterDefaultTransforms ();
      }
    TransformFactoryBase::Pointer ret = m_Factory;
    return ret;
    }

  void RegisterTransform(const char* classOverride,
      const char* overrideClassName,
      const char* description,
      bool enableFlag,
      CreateObjectFunctionBase* createFunction)
  {
    this->RegisterOverride ( classOverride, overrideClassName, description, enableFlag, createFunction );
  }
  
protected:
  TransformFactoryBase();
  virtual ~TransformFactoryBase();

private:
  TransformFactoryBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  static TransformFactoryBase* m_Factory;
};
} // end namespace itk

#endif
