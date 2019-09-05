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

#ifndef itkFEMLoadElementBase_h
#define itkFEMLoadElementBase_h

#include "itkFEMLoadBase.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class LoadElement
 * \brief Virtual element load base class.
 *
 * This load class defines an external load that acts on elements in a system.
 * The vector with pointers to elements defines, on which elements
 * the load acts. The derived load classes should provide members, that allow the
 * Element's class Fe() member function to uniquely transform the load into nodal loads.
 * No special requirements are enforced on those members.
 *
 * Ultimately, when assembling the right hand side of the master equation (master force vector)
 * the Element's Fe() member function is called with the pointer to the LoadElement class that is
 * prescribed on that element. Fe() function shuld dynamically cast this pointer to specific
 * load class, which it can handle and return the element's force vector.
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LoadElement : public Load
{
public:
  /** Standard class type aliases. */
  using Self = LoadElement;
  using Superclass = Load;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadElement, Load);

  /** CreateAnother method will clone the existing instance of this type,
   * including its internal member variables. */
  ::itk::LightObject::Pointer
  CreateAnother() const override;

  /**
   * Float type used in Element and derived classes
   */
  using Float = Element::Float;

  /**
   * Type of array of pointers to element objects
   */
  using ElementPointersVectorType = std::vector<const Element *>;

  // FIXME: should clear vector, not zero it
  LoadElement()
    : m_Element(0)
  {}
  void
  AddNextElement(Element::ConstPointer e)
  {
    this->AddNextElementInternal(e);
  }
  void
  AddNextElement(Element::Pointer e)
  {
    this->AddNextElementInternal(e);
  }

  Element::ConstPointer
  GetElement(int i);

  unsigned int
  GetNumberOfElements();

  ElementPointersVectorType &
  GetElementArray()
  {
    return this->m_Element;
  }

  const ElementPointersVectorType &
  GetElementArray() const
  {
    return this->m_Element;
  }

  /** Apply the load to the specified element */
  virtual void
  ApplyLoad(Element::ConstPointer, Element::VectorType &)
  { /* HACK:  This should probably throw an exception if it is not intended to be used. */
  }

protected:
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
  void
                            AddNextElementInternal(const Element * e);
  ElementPointersVectorType m_Element; /** pointers to element objects on which the
                                  load acts */
};
} // end namespace fem
} // end namespace itk

#endif // itkFEMLoadElementBase_h
