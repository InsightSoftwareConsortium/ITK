/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDecoratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkVector.h"
#include "itkAffineTransform.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkDataObjectDecorator.h"
#include "itkAutoPointerDataObjectDecorator.h"

namespace {
template <class charT, class traits, class T, class A>
std::basic_ostream<charT, traits>&
operator<<(std::basic_ostream<charT, traits>&os, const std::vector<T,A> &p) 
{
  os << "vector<" << typeid(T).name() << "> with " << p.size() << " elements " << std::endl;
  return os;
}
}

int itkDecoratorTest(int, char* [] )
{
  int status = 0;

  std::cout << "----------------------------------------------------"
            << std::endl;
  
  typedef itk::SimpleDataObjectDecorator<float> FloatObjectType;

  FloatObjectType::Pointer f = FloatObjectType::New();
  f->Set(5.0);

  std::cout << "Value of f: " << f->Get() << std::endl;
  std::cout << "FloatDataObject: " << f << std::endl;

  std::cout << "----------------------------------------------------"
            << std::endl;

  typedef itk::AffineTransform<double, 3> TransformType;
  typedef itk::DataObjectDecorator<TransformType> TransformObjectType;

  TransformObjectType::Pointer transform = TransformObjectType::New();
  transform->Set( TransformType::New() );
  
  transform->Get()->Scale(5.0);

  std::cout << "Value of transform: ";
  transform->Get()->Print(std::cout);
  std::cout << "TransformDataObject: " << transform;

  std::cout << "----------------------------------------------------"
            << std::endl;

  typedef std::vector<float> VectorType;
  typedef VectorType* VectorPointer;
  typedef itk::SimpleDataObjectDecorator<VectorType> VectorObjectType;
  typedef itk::AutoPointerDataObjectDecorator<VectorType> VectorPointerObjectType;

  VectorType v;
  v.resize(5);
  std::cout << v << std::endl;
  
  VectorObjectType::Pointer vo = VectorObjectType::New();
  vo->Set(v);
  std::cout << vo;

  std::cout << "----------------------------------------------------"
            << std::endl;

  // The following code block will NOT cause a memory leak because the
  // ownership of the dynamically allocated memory is passed to the
  // AutoPointerDataObjectDecorator 
  {
  VectorPointer vp;
  vp = new VectorType;
  vp->resize(3);
  std::cout << *vp << std::endl;

  VectorPointerObjectType::Pointer vop = VectorPointerObjectType::New();
  vop->Set(vp);

  std::cout << vop;
  }

  std::cout << "----------------------------------------------------"
            << std::endl;

  // The following code block will cause a memory leak because the
  // decorator does not deallocate the memory that was passed in on a
  // pointer. The AutoPointerDataObjectDecorator does delete the memory.
  //typedef itk::SimpleDataObjectDecorator<VectorPointer> VectorPointerObjectType2;
  //{
  //VectorPointer vp2;
  //vp2 = new VectorType;
  //vp2->resize(4);
  //std::cout << *vp2 << std::endl;
  
  //VectorPointerObjectType2::Pointer vop2 = VectorPointerObjectType2::New();
  //vop2->Set(vp2);

  //std::cout << vop2;
  //}

  return status;
}
