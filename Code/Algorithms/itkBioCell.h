/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioCell.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBioCell_h
#define __itkBioCell_h

#include "itkBioCellBase.h"
#include "itkBioCellularAggregateBase.h"


namespace itk {

namespace bio {




/** \class Cell
 * \brief This class implement the minimal behavior 
 * of a biological cell.
 * The basic behavior of a cell is related with the
 * cell cycle. Geometrical concepts like size and shape
 * are also managed by this abstract cell.
 */

template<unsigned int NSpaceDimension=3>
class Cell : public CellBase 
{
public:
  typedef   CellBase                     Superclass;

  typedef   itk::Vector<double,NSpaceDimension>  VectorType;
  typedef   itk::Point<double,NSpaceDimension>   PointType;

  friend class CellularAggregateBase; // need to give access to the constructor.

public:
  virtual ~Cell();
  virtual void ClearForce(void);
  virtual void AddForce(const VectorType & force);
  virtual void AdvanceTimeStep(void);

  virtual void Mitosis(void);
  virtual void Apoptosis(void);
  virtual void ReceptorsReading(void);
 
  virtual void SetCellularAggregate( CellularAggregateBase * );

  virtual       CellularAggregateBase * GetCellularAggregate( void );
  virtual const CellularAggregateBase * GetCellularAggregate( void ) const;
  static  const char * GetSpeciesName(void) 
                              { return "Primitive Cell"; }
  static  Cell * CreateEgg(void);

  static  unsigned int GetDimension() 
                              { return NSpaceDimension; }

protected:
  Cell(); // Users should create cell with the CreateEgg() method

public:

  virtual const VectorType & GetForce(void) const;

 
protected:
   VectorType               m_Force;

   CellularAggregateBase  * m_Aggregate;

};


} // end namespace bio

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBioCell.txx"
#endif


#endif
