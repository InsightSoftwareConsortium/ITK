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

#include "itkCell.h"

/**
 * itkCellBoundary wraps any ITK Cell type with it's corresponding boundary
 * interface definitions.
 */

template <
  /**
   * The type of cell we want to wrap.
   */
  typename TCell
  >
class itkCellBoundary: public TCell
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkCellBoundary         Self;
  typedef itkSmartPointer<Self>   Pointer;
  
  /**
   * Save template parameter information.
   */
  typedef TCell  Cell;
  typedef typename Cell::UsingCellsContainer      UsingCellsContainer;
  typedef typename Cell::CellIdentifier           CellIdentifier;
  typedef typename UsingCellsContainer::iterator  UsingCellsContainerIterator;
  
  /**
   * Define the interface to the boundary information.
   */
  static Pointer New(void);
  virtual bool IsBoundary(void);
  virtual void AddUsingCell(CellIdentifier cellId);
  virtual void RemoveUsingCell(CellIdentifier cellId);
  virtual bool IsUsingCell(CellIdentifier cellId);
  virtual int GetNumUsingCells(void);
  virtual UsingCellsContainerIterator UsingCellsBegin(void);
  virtual UsingCellsContainerIterator UsingCellsEnd(void);
  
  /**
   * Standard part of itkObject class.  Used for debugging output.
   */
  itkTypeMacro(itkCellBoundary, itkCell);
  
protected:
  /**
   * Store the set of cells using this boundary.
   */
  UsingCellsContainer m_UsingCells;
};


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCellBoundary.cxx"
#endif

#endif
