/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellBoundary.h
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
#ifndef __itkCellBoundary_h
#define __itkCellBoundary_h

#include "itkCellInterface.h"

namespace itk
{
  
/** \class TCell
 * \brief Template parameter used to define superclass for CellBoundary.
 */

/** \class CellBoundary
 * \brief Wrap an ITK cell so that it behaves like a boundary cell.
 *
 * CellBoundary wraps any ITK Cell type with it's corresponding boundary
 * interface definitions.  It re-implements the boundary interface methods
 * that were defined in CellInterface to actually do something.
 *
 * Template parameters for CellBoundary:
 *
 * TCell =  The type of cell we want to wrap.
 */

template <
  typename TCell
  >
class CellBoundary: public TCell
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CellBoundary  Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef TCell  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * The type of the cell that is wrapped with the additional boundary
   * functionality.
   */
  typedef TCell  Cell;
  
  /**
   * The type of container to store the cells using this boundary.
   */
  typedef typename Cell::UsingCellsContainer      UsingCellsContainer;
  
  /**
   * An iterator through the UsingCellsContainer.
   */
  typedef typename UsingCellsContainer::iterator  UsingCellsContainerIterator;
  
  /**
   * The type stored in the UsingCellsContainer.  This should always be
   * the Cell's CellIdentifier type.
   */
  typedef typename Cell::CellIdentifier           CellIdentifier;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Define the interface to the boundary information.
   */
  virtual bool IsBoundary(void);
  virtual void AddUsingCell(CellIdentifier cellId);
  virtual void RemoveUsingCell(CellIdentifier cellId);
  virtual bool IsUsingCell(CellIdentifier cellId);
  virtual int GetNumUsingCells(void);
  virtual UsingCellsContainerIterator UsingCellsBegin(void);
  virtual UsingCellsContainerIterator UsingCellsEnd(void);
  
  /**
   * Standard part of Object class.  Used for debugging output.
   */
  itkTypeMacro(CellBoundary, Cell);
  
protected:
  /**
   * Store the set of cells using this boundary.
   */
  UsingCellsContainer m_UsingCells;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCellBoundary.txx"
#endif

#endif
