/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFactory.h
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

class ITKCommon_EXPORT TransformFactory : public ObjectFactoryBase
{
public:  
  /** Standard class typedefs. */
  typedef TransformFactory   Self;
  typedef ObjectFactoryBase  Superclass;
  typedef SmartPointer< Self >   Pointer;
  typedef SmartPointer< const Self >  ConstPointer;
  
  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion(void) const;
  virtual const char* GetDescription(void) const;

  /** Run-time type information (and related methods). */
  itkTypeMacro(AnalyzeImageIOFactory, ObjectFactoryBase);

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Register all builtin transforms */
  static void RegisterDefaultTransforms();

  /** Register this transform */
  template <class T>
    static void RegisterTransform (T* notused = 0 )
    {
    // Search for singleton, create if not existing
    if ( m_Factory == 0 )
      {
      // Make and register the factory
      TransformFactory::Pointer p = TransformFactory::New();
      m_Factory = p.GetPointer();
      ObjectFactoryBase::RegisterFactory ( p );
      p->RegisterDefaultTransforms ();
      }
    typename T::Pointer t = T::New();
    std::cout << "Registering: " << t->GetTransformTypeAsString() << std::endl;
    m_Factory->RegisterOverride ( t->GetTransformTypeAsString().c_str(),
                                  t->GetTransformTypeAsString().c_str(),
                                  t->GetTransformTypeAsString().c_str(),
                                  1,
                                  CreateObjectFunction<T>::New() );
    };

protected:
  TransformFactory();
  virtual ~TransformFactory();

private:
  TransformFactory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  static TransformFactory* m_Factory;
};
} // end namespace itk

#endif
