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
#ifndef __itkFEMLoadImplementationTest_h
#define __itkFEMLoadImplementationTest_h

#include "itkFEMElement2DC0LinearLineStress.h"
#include "itkFEMLoadTest.h"

namespace itk {
namespace fem {

/**
 * \class LoadImplementationTest
 * \brief Example implementation of templated LoadTest class.
 *
 * This is an example of how to define the implementation of a templated
 * Load class. Since the Load class is templated, its implementation must
 * also be templated. Due to limitations of MS compiler, we define this
 * implementation function as a static function inside a templated class.
 *
 * To make things easier to use, we template the class over the whole
 * templated load class and not only over the template parameters required
 * to define the templated Load class.
 *
 * You must manually instantiate this class to register the load
 * implementation function with the VisitorDispatcher. The
 * instantiation is normally done like this:
 *
 *     typedef LoadTest<...> MyLoadTestClass;
 *     template class LoadImplementationTest<MyLoadTestClass>;
 * \ingroup ITK-FEM
 */
template<class TLoadClass>
class LoadImplementationTest
{
public:
  static void impl(Element2DC0LinearLineStress::ConstPointer element, Element::LoadPointer load, Element::VectorType& Fe)
  {
    // We must dynamically cast the given load pointer to the
    // correct templated load class, which is given as
    // template parameter.
    typename TLoadClass::Pointer l0=dynamic_cast<TLoadClass*>(&*load);
    if ( !l0 ) throw FEMException(__FILE__, __LINE__, "FEM error");

    std::cout<<"Load object's data:"<<l0->data<<"\n";
  }
private:
  static const bool m_Registered;
};

// When the templated load implementation function is instantiated,
// it will automatically be registered with the VisitorDispatcher so
// that it is called as required.
// Instantiating the implementation function will also instantiate the
// corresponding Load class.
template<class TLoadClass>
const bool LoadImplementationTest<TLoadClass>::m_Registered=
  VisitorDispatcher<Element2DC0LinearLineStress,Element::LoadType,Element2DC0LinearLineStress::LoadImplementationFunctionPointer>
  ::RegisterVisitor((TLoadClass*)0, &LoadImplementationTest<TLoadClass>::impl);

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadImplementationTest_h
