/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellBoundary.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * CellBoundary wraps any ITK Cell type with its corresponding boundary
 * interface definitions.  It re-implements the boundary interface methods
 * that were defined in CellInterface to actually do something.
 *
 * Template parameters for CellBoundary:
 *
 * TCell =  The type of cell we want to wrap.
 *
 * \ingroup MeshObjects
 */

template <
  typename TCell
  >
class ITK_EXPORT CellBoundary: public TCell
{
public:
  /** Standard class typedefs. */
  typedef CellBoundary  Self;
  typedef TCell  Superclass;
    
  /** Standard part of Object class.  Used for debugging output. */
  itkTypeMacro(CellBoundary, Cell);
  
  /** The type of the cell that is wrapped with the additional boundary
   * functionality. */
  typedef TCell  CellType;
  
  /** The type of container to store the cells using this boundary. */
  typedef typename CellType::UsingCellsContainer      UsingCellsContainer;
  
  /** An iterator through the UsingCellsContainer. */
  typedef typename UsingCellsContainer::iterator  UsingCellsContainerIterator;
  
  /** The type stored in the UsingCellsContainer.  This should always be
   * the Cell's CellIdentifier type. */
  typedef typename CellType::CellIdentifier           CellIdentifier;
  
  /** Define the interface to the boundary information. */
  virtual bool IsBoundary(void);
  virtual void AddUsingCell(CellIdentifier cellId);
  virtual void RemoveUsingCell(CellIdentifier cellId);
  virtual bool IsUsingCell(CellIdentifier cellId);
  virtual unsigned int GetNumberOfUsingCells(void);
  virtual UsingCellsContainerIterator UsingCellsBegin(void);
  virtual UsingCellsContainerIterator UsingCellsEnd(void);
    
  /** Constructor and destructor */
  CellBoundary() {};
  ~CellBoundary() {};


protected:
  /** Store the set of cells using this boundary. */
  UsingCellsContainer m_UsingCells;

 
private:
  CellBoundary(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCellBoundary.txx"
#endif

#endif
