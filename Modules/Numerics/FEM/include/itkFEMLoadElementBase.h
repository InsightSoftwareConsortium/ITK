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
#ifndef __itkFEMLoadElementBase_h
#define __itkFEMLoadElementBase_h

#include "itkFEMLoadBase.h"

namespace itk {
namespace fem {

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
 * the Element's Fe() member funtion is called with the pointer to the LoadElement class that is
 * prescribed on that element. Fe() function shuld dynamically cast this pointer to specific
 * load class, which it can handle and return the element's force vector.
 * \ingroup ITK-FEM
 */
class LoadElement : public Load
{
  FEM_CLASS(LoadElement,Load)
public:
  /**
   * Float type used in Element and derived classes
   */
  typedef Element::Float Float;

  /**
   * Type of array of pointers to element objects
   */
  typedef std::vector<Element::ConstPointer> ElementPointersVectorType;
  ElementPointersVectorType el;  /** pointers to element objects on which the load acts */

  virtual void Read( std::istream& f, void* info );
  void Write( std::ostream& f ) const;

  // FIXME: should clear vector, not zero it
  LoadElement() : el(0) {}

};

FEM_CLASS_INIT(LoadElement)

}} // end namespace itk::fem

#endif // #ifndef __itkFEMLoadElementBase_h
