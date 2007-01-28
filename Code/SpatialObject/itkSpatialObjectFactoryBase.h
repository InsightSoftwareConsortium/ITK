/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectFactoryBase.h
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
#ifndef __itkSpatialObjectFactoryBase_h
#define __itkSpatialObjectFactoryBase_h

#include "itkObjectFactoryBase.h"

namespace itk
{
/** \class SpatialObjectFactoryBase
 * \brief Create instances of SpatialObjects
 */

class SpatialObjectFactoryBase : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef SpatialObjectFactoryBase    Self;
  typedef ObjectFactoryBase           Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;
  
  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectFactoryBase, ObjectFactoryBase);

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Register all builtin SpatialObjects */
  static void RegisterDefaultSpatialObjects();

  /** Register this SpatialObject */
  static SpatialObjectFactoryBase* GetFactory () 
    {
    if ( m_Factory == 0 )
      {
      // Make and register the factory
      SpatialObjectFactoryBase::Pointer p = SpatialObjectFactoryBase::New();
      m_Factory = p.GetPointer();
      ObjectFactoryBase::RegisterFactory ( p );
      p->RegisterDefaultSpatialObjects ();
      }
    return m_Factory;
    }

  void RegisterSpatialObject(const char* classOverride,
      const char* overrideClassName,
      const char* description,
      bool enableFlag,
      CreateObjectFunctionBase* createFunction)
    {
    this->RegisterOverride ( classOverride, overrideClassName, 
                           description, enableFlag, createFunction );
    }
  
protected:
  SpatialObjectFactoryBase();
  virtual ~SpatialObjectFactoryBase();

private:
  SpatialObjectFactoryBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  static SpatialObjectFactoryBase* m_Factory;
};

} // end namespace itk
#endif
