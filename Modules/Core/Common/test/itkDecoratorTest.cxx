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

#include <iostream>
#include "itkAffineTransform.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itkDataObjectDecorator.h"
#include "itkAutoPointerDataObjectDecorator.h"
#include "itkTestingMacros.h"

namespace {
template <typename CharType, typename TraitsType, typename MemberType, typename AllocatorType>
std::basic_ostream<CharType, TraitsType>&
operator<<(std::basic_ostream<CharType, TraitsType>&os, const std::vector<MemberType,AllocatorType> &p)
{
  os << "vector<" << typeid(MemberType).name() << "> with " << p.size() << " elements " << std::endl;
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

  typedef itk::AffineTransform<double, 3>         TransformType;
  typedef itk::DataObjectDecorator<TransformType> TransformObjectType;

  TransformObjectType::Pointer decoratedTransform = TransformObjectType::New();
  TransformType::Pointer transformObject = TransformType::New();
  const TransformType * constTransformObject = transformObject;

  transformObject->Scale( 5.0 );

  decoratedTransform->Set( constTransformObject );

  const itk::ModifiedTimeType t1 = decoratedTransform->GetMTime();
  transformObject->Modified();
  TEST_EXPECT_TRUE(t1 < decoratedTransform->GetMTime());

  TransformObjectType::Pointer decoratedTransform2 = TransformObjectType::New();
  decoratedTransform2->Graft( decoratedTransform );

  TEST_EXPECT_EQUAL( decoratedTransform2->Get(), decoratedTransform->Get() );

  const itk::ModifiedTimeType t2 = decoratedTransform->GetMTime();
  decoratedTransform2->GetModifiable()->Modified();
  TEST_EXPECT_TRUE(t2 < decoratedTransform->GetMTime());


  std::cout << "Value of decoratedTransform: ";
  decoratedTransform->Get()->Print(std::cout);
  std::cout << "TransformDataObject: " << decoratedTransform;

  typedef itk::Transform<double, 3>                   TransformBaseType;
  typedef itk::DataObjectDecorator<TransformBaseType> TransformBaseObjectType;

  TransformBaseObjectType::Pointer decoratedBaseTransform = TransformBaseObjectType::New();
  decoratedBaseTransform->Graft( decoratedTransform.GetPointer() );
  TEST_EXPECT_TRUE( decoratedBaseTransform->Get() != ITK_NULLPTR );

  decoratedBaseTransform->ReleaseData();
  TEST_EXPECT_TRUE( decoratedBaseTransform->Get() == ITK_NULLPTR );
  decoratedBaseTransform->Graft( f.GetPointer() );
  TEST_EXPECT_TRUE( decoratedBaseTransform->Get() == ITK_NULLPTR );

  decoratedBaseTransform->Graft( static_cast<itk::DataObject *>(ITK_NULLPTR) );
  decoratedBaseTransform->Graft( decoratedTransform.GetPointer() );
  TEST_EXPECT_TRUE( decoratedBaseTransform->Get() != ITK_NULLPTR );

  decoratedBaseTransform->Graft( static_cast<itk::DataObject *>(ITK_NULLPTR) );
  TEST_EXPECT_TRUE( decoratedBaseTransform->Get() != ITK_NULLPTR );

  decoratedTransform->ReleaseData();
  decoratedTransform->Graft( decoratedBaseTransform );
  TEST_EXPECT_TRUE( decoratedTransform->Get() == ITK_NULLPTR );

  std::cout << "----------------------------------------------------"
            << std::endl;

  typedef std::vector<float>                              VectorType;
  typedef VectorType*                                     VectorPointer;
  typedef itk::SimpleDataObjectDecorator<VectorType>      VectorObjectType;
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
