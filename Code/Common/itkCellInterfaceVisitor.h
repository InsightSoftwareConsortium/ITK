/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellInterfaceVisitor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkCellInterfaceVisitor_h
#define __itkCellInterfaceVisitor_h

#include "itkLightObject.h"

namespace itk
{
// forward reference CellInterface
template <
  typename TPixelType,
  typename TCellType
> 
class CellInterface;
  
/** \class CellInterfaceVisitor
 * Define the abstract interface for a visitor class that can visit the
 * cells in a Mesh.  This follows the Visitor Design Pattern.   To make
 * this class easier to use, the CellInterfaceVisitorImplementation is 
 * provided as a templated class to implement the pure virtual functions
 * of CellInterfaceVisitor.
 */
template <
  typename TPixelType,
  typename TCellType
>  
class CellInterfaceVisitor : public LightObject
{
public: 
  /**
   * Standard "Self" typedef.
   */
  typedef CellInterfaceVisitor       Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef LightObject  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(CellInterfaceVisitor,LightObject);
  /** 
   * This method is called by each cell as it visits this visitor.
   */
  virtual void VisitFromCell(unsigned long cellId, CellInterface<TPixelType, TCellType>*) = 0;
  /**
   *  Return the index of the CellTopology.
   */
  virtual int GetCellTopologyId() = 0;
  
protected:
  CellInterfaceVisitor() {};
  ~CellInterfaceVisitor() {};
  CellInterfaceVisitor(const Self&) {}
  void operator=(const Self&) {}
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
 * TCellType = see CellInterface
 *
 * CellTopology = The specific type of cell that needs to be visited.
 *
 * UserVisitor = A user supplied class that implements the function
 *               Visit(int id, CellTopology*)
 */
template<
typename TPixelType,
  typename TCellType,
  class CellTopology,
  class UserVisitor 
>
class CellInterfaceVisitorImplementation : 
    public CellInterfaceVisitor<TPixelType, TCellType>, public UserVisitor
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CellInterfaceVisitorImplementation       Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(CellInterfaceVisitorImplementation,LightObject);
  
  /**
   * Call the static method GetTopologyId for the CellTopology type that
   * we are templated over.
   */
  virtual int GetCellTopologyId() { return CellTopology::GetTopologyId();}
  /**
   * Call the method Visit from the UserVisitor template parameter that
   * this class inherits from.  I am my own gradpa...
   */
  void VisitFromCell(unsigned long cellId, CellInterface<TPixelType, TCellType>* c)
    {
      this->UserVisitor::Visit(cellId, (CellTopology*)c);
    }
protected:
  CellInterfaceVisitorImplementation() {};
  ~CellInterfaceVisitorImplementation() {};
  CellInterfaceVisitorImplementation(const Self&) {}
  void operator=(const Self&) {}
};

} // end namespace itk

#endif
