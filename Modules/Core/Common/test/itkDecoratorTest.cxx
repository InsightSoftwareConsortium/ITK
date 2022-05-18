/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

namespace
{
template <typename CharType, typename TraitsType, typename MemberType, typename AllocatorType>
std::basic_ostream<CharType, TraitsType> &
operator<<(std::basic_ostream<CharType, TraitsType> & os, const std::vector<MemberType, AllocatorType> & p)
{
  os << "vector<" << typeid(MemberType).name() << "> with " << p.size() << " elements " << std::endl;
  return os;
}
} // namespace

int
itkDecoratorTest(int, char *[])
{
  int status = 0;

  std::cout << "----------------------------------------------------" << std::endl;

  using FloatObjectType = itk::SimpleDataObjectDecorator<float>;

  auto f = FloatObjectType::New();
  f->Set(5.0);

  std::cout << "Value of f: " << f->Get() << std::endl;
  std::cout << "FloatDataObject: " << f << std::endl;

  std::cout << "----------------------------------------------------" << std::endl;

  using TransformType = itk::AffineTransform<double, 3>;
  using TransformObjectType = itk::DataObjectDecorator<TransformType>;

  auto                  decoratedTransform = TransformObjectType::New();
  auto                  transformObject = TransformType::New();
  const TransformType * constTransformObject = transformObject;

  transformObject->Scale(5.0);

  decoratedTransform->Set(constTransformObject);

  const itk::ModifiedTimeType t1 = decoratedTransform->GetMTime();
  transformObject->Modified();
  ITK_TEST_EXPECT_TRUE(t1 < decoratedTransform->GetMTime());

  auto decoratedTransform2 = TransformObjectType::New();
  decoratedTransform2->Graft(decoratedTransform);

  ITK_TEST_EXPECT_EQUAL(decoratedTransform2->Get(), decoratedTransform->Get());

  const itk::ModifiedTimeType t2 = decoratedTransform->GetMTime();
  decoratedTransform2->GetModifiable()->Modified();
  ITK_TEST_EXPECT_TRUE(t2 < decoratedTransform->GetMTime());


  std::cout << "Value of decoratedTransform: ";
  decoratedTransform->Get()->Print(std::cout);
  std::cout << "TransformDataObject: " << decoratedTransform;

  using TransformBaseType = itk::Transform<double, 3>;
  using TransformBaseObjectType = itk::DataObjectDecorator<TransformBaseType>;

  auto decoratedBaseTransform = TransformBaseObjectType::New();
  // NOTE: GetPointer is needed to force selection of the correct overloaded function signature.
  decoratedBaseTransform->Graft(decoratedTransform.GetPointer());
  ITK_TEST_EXPECT_TRUE(decoratedBaseTransform->Get() != nullptr);

  decoratedBaseTransform->ReleaseData();
  ITK_TEST_EXPECT_TRUE(decoratedBaseTransform->Get() == nullptr);
  // NOTE: GetPointer is needed to force selection of the correct overloaded function signature.
  decoratedBaseTransform->Graft(f.GetPointer());
  ITK_TEST_EXPECT_TRUE(decoratedBaseTransform->Get() == nullptr);

  decoratedBaseTransform->Graft(static_cast<itk::DataObject *>(nullptr));
  // NOTE: GetPointer is needed to force selection of the correct overloaded function signature.
  decoratedBaseTransform->Graft(decoratedTransform.GetPointer());
  ITK_TEST_EXPECT_TRUE(decoratedBaseTransform->Get() != nullptr);

  decoratedBaseTransform->Graft(static_cast<itk::DataObject *>(nullptr));
  ITK_TEST_EXPECT_TRUE(decoratedBaseTransform->Get() != nullptr);

  decoratedTransform->ReleaseData();
  decoratedTransform->Graft(decoratedBaseTransform);
  ITK_TEST_EXPECT_TRUE(decoratedTransform->Get() == nullptr);

  std::cout << "----------------------------------------------------" << std::endl;

  using VectorType = std::vector<float>;
  using VectorPointer = VectorType *;
  using VectorObjectType = itk::SimpleDataObjectDecorator<VectorType>;
  using VectorPointerObjectType = itk::AutoPointerDataObjectDecorator<VectorType>;

  VectorType v;
  v.resize(5);
  std::cout << v << std::endl;
  auto vo = VectorObjectType::New();
  vo->Set(v);
  std::cout << vo;
  std::cout << "----------------------------------------------------" << std::endl;

  // The following code block will NOT cause a memory leak because the
  // ownership of the dynamically allocated memory is passed to the
  // AutoPointerDataObjectDecorator
  {
    VectorPointer vp;
    vp = new VectorType;
    vp->resize(3);
    std::cout << *vp << std::endl;

    auto vop = VectorPointerObjectType::New();
    vop->Set(vp);

    std::cout << vop;
  }

  std::cout << "----------------------------------------------------" << std::endl;

  // The following code block will cause a memory leak because the
  // decorator does not deallocate the memory that was passed in on a
  // pointer. The AutoPointerDataObjectDecorator does delete the memory.
  // using VectorPointerObjectType2 = itk::SimpleDataObjectDecorator<VectorPointer>;
  //{
  // VectorPointer vp2;
  // vp2 = new VectorType;
  // vp2->resize(4);
  // std::cout << *vp2 << std::endl;

  // auto vop2 = VectorPointerObjectType2::New();
  // vop2->Set(vp2);

  // std::cout << vop2;
  //}

  return status;
}
