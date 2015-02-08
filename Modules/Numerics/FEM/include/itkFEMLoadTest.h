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

#ifndef itkFEMLoadTest_h
#define itkFEMLoadTest_h

#include "itkFEMLoadElementBase.h"

namespace itk
{
namespace fem
{
/**
 * \class LoadTest
 * \brief Example to show how to define templated load classes.
 *
 * \note The class must be instantiated, before the object factory can
 *       produce more objects of this class. Instantiate the specific
 *       derived classes with: "template class LoadTest<...>;" where required.
 * \ingroup ITKFEM
 */
template <typename TClass>
class LoadTest : public LoadElement
{
public:
  /** Standard class typedefs. */
  typedef LoadTest                 Self;
  typedef LoadElement              Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadTest, LoadElement);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  virtual::itk::LightObject::Pointer CreateAnother(void) const
  {
    ::itk::LightObject::Pointer smartPtr;
    Pointer copyPtr = Self::New();
    for( unsigned int i = 0; i < this->m_Element.size(); i++ )
      {
      copyPtr->AddNextElement( this->m_Element[i] );
      }
    copyPtr->SetGlobalNumber( this->GetGlobalNumber() );

    smartPtr = static_cast<Pointer>(copyPtr);

    return smartPtr;
  }

  /**
   * Default constructor
   */
  LoadTest()
  {
  }

  /**
   * Some data that this load defines.
   */
  TClass data;

private:

};

}
}  // end namespace itk::fem

#endif // #ifndef itkFEMLoadTest_h
