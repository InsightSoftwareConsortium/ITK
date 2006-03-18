/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellInterfaceVisitor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCellInterfaceVisitor_h
#define __itkCellInterfaceVisitor_h

#include "itkLightObject.h"
#include "itkObjectFactory.h"

namespace itk
{
// forward reference CellInterface
template <
  typename TPixelType,
  typename TCellTraits
> 
class CellInterface;
  
/** \class CellInterfaceVisitor
 * Define the abstract interface for a visitor class that can visit the
 * cells in a Mesh.  This follows the Visitor Design Pattern.   To make
 * this class easier to use, the CellInterfaceVisitorImplementation is 
 * provided as a templated class to implement the pure virtual functions
 * of CellInterfaceVisitor.
 *
 * \ingroup MeshAccess 
 */
template <
  typename TPixelType,
  typename TCellTraits
>  
class CellInterfaceVisitor : public LightObject
{
public: 
  /** Standard class typedefs. */
  typedef CellInterfaceVisitor      Self;
  typedef LightObject               Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Run-time type information (and related methods). */
  itkTypeMacro(CellInterfaceVisitor,LightObject);

  /** This method is called by each cell as it visits this visitor. */
  virtual void VisitFromCell(unsigned long cellId, CellInterface<TPixelType, 
                                                           TCellTraits>*) = 0;

  /**  Return the index of the CellTopology. */
  virtual int GetCellTopologyId() = 0;
  
protected:
  CellInterfaceVisitor() {};
  ~CellInterfaceVisitor() {};

private:
  CellInterfaceVisitor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

  
/** \class CellInterfaceVisitorImplementation
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
 */
template<
typename TPixelType,
  typename TCellTraits,
  class CellTopology,
  class UserVisitor 
>
class CellInterfaceVisitorImplementation : 
    public CellInterfaceVisitor<TPixelType, TCellTraits>, public UserVisitor
{
public:
  /** Standard class typedefs. */
  typedef CellInterfaceVisitorImplementation   Self;
  typedef SmartPointer<Self>                   Pointer;
    
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(CellInterfaceVisitorImplementation,LightObject);
  
  /** Call the static method GetTopologyId for the CellTopology type that
   * we are templated over. */
  virtual int GetCellTopologyId() { return CellTopology::GetTopologyId();}

  /** Call the method Visit from the UserVisitor template parameter that
   * this class inherits from.  I am my own gradpa... */
  void VisitFromCell(unsigned long cellId, CellInterface<TPixelType, 
                                                         TCellTraits>* c)
    {
    this->UserVisitor::Visit(cellId, (CellTopology*)c);
    }

protected:
  CellInterfaceVisitorImplementation() {};
  ~CellInterfaceVisitorImplementation() {};

private:
  CellInterfaceVisitorImplementation(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

} // end namespace itk

#endif
