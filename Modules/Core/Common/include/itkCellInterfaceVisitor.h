/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkCellInterfaceVisitor_h
#define itkCellInterfaceVisitor_h

#include "itkLightObject.h"
#include "itkObjectFactory.h"
#include "itkCommonEnums.h"

namespace itk
{
// Forward reference of CellInterface because of circular #include dependencies
template <typename TPixelType, typename TCellTraits>
class ITK_TEMPLATE_EXPORT CellInterface;

/** \class CellInterfaceVisitor
 *  \brief Abstract interface for a visitor class that can visit the
 *         cells in a Mesh.
 *
 * Define the abstract interface for a visitor class that can visit the
 * cells in a Mesh.  This follows the Visitor Design Pattern.   To make
 * this class easier to use, the CellInterfaceVisitorImplementation is
 * provided as a templated class to implement the pure virtual functions
 * of CellInterfaceVisitor.
 *
 * \ingroup MeshAccess
 * \ingroup ITKCommon
 */
template <typename TPixelType, typename TCellTraits>
class ITK_TEMPLATE_EXPORT CellInterfaceVisitor : public LightObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CellInterfaceVisitor);

  /** Standard class type aliases. */
  using Self = CellInterfaceVisitor;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using CellIdentifier = typename TCellTraits::CellIdentifier;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CellInterfaceVisitor, LightObject);

  /** This method is called by each cell as it visits this visitor. */
  virtual void
  VisitFromCell(CellIdentifier cellId, CellInterface<TPixelType, TCellTraits> *) = 0;

  /**  Return the index of the CellTopology. */
  virtual CellGeometryEnum
  GetCellTopologyId() = 0;

protected:
  CellInterfaceVisitor() = default;
  ~CellInterfaceVisitor() override = default;
};

/** \class CellInterfaceVisitorImplementation
 *  \brief A template class used to implement a visitor object.
 *
 * A template class used to implement a visitor object.
 *
 * The Visitor implementation does the down cast to
 * the specific cell type that is being visited.  After the
 * cast, a member of the UserVisitor type called Visit is
 * passed the exact cell type being visited.  To use this
 * class, write a class that implements a function
 * Visit(int id, CellTopology*).   Then, use that as the UserVisitor
 * template parameter.
 *
 * Template parameters for CellInterfaceVisitorImplementation:
 * TPixelType = see CellInterface
 *
 * TCellTraits = see CellInterface
 *
 * CellTopology = The specific type of cell that needs to be visited.
 *
 * UserVisitor = A user supplied class that implements the function
 *               Visit(int id, CellTopology*)
 *
 * \ingroup MeshAccess
 * \ingroup ITKCommon
 */
template <typename TPixelType, typename TCellTraits, typename CellTopology, typename UserVisitor>
class CellInterfaceVisitorImplementation
  : public CellInterfaceVisitor<TPixelType, TCellTraits>
  , public UserVisitor
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(CellInterfaceVisitorImplementation);

  /** Standard class type aliases. */
  using Self = CellInterfaceVisitorImplementation;
  using Pointer = SmartPointer<Self>;
  using CellIdentifier = typename TCellTraits::CellIdentifier;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CellInterfaceVisitorImplementation, LightObject);

  /** Call the static method GetTopologyId for the CellTopology type that
   * we are templated over. */
  CellGeometryEnum
  GetCellTopologyId() override
  {
    return CellTopology::GetTopologyId();
  }

  /** Call the method Visit from the UserVisitor template parameter that
   * this class inherits from.  I am my own gradpa... */
  void
  VisitFromCell(CellIdentifier cellId, CellInterface<TPixelType, TCellTraits> * c) override
  {
    this->UserVisitor::Visit(cellId, (CellTopology *)c);
  }

protected:
  CellInterfaceVisitorImplementation() = default;
  ~CellInterfaceVisitorImplementation() override = default;
};
} // end namespace itk

#endif
