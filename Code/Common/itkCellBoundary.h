/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellBoundary.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>   Pointer;
  
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

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCellBoundary.txx"
#endif

#endif
