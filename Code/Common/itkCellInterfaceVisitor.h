/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellInterfaceVisitor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkCellInterfaceVisitor_h
#define __itkCellInterfaceVisitor_h

#include "itkLightObject.h"

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
  typedef SmartPointer<const Self>  ConstPointer;
  
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
  virtual void VisitFromCell(unsigned long cellId, CellInterface<TPixelType, TCellTraits>*) = 0;
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
  void VisitFromCell(unsigned long cellId, CellInterface<TPixelType, TCellTraits>* c)
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
