/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioCellularAggregateBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBioCellularAggregateBase_h
#define __itkBioCellularAggregateBase_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{
namespace bio
{
class CellBase;

/** \class CellularAggregateBase
 * \brief Base class for the CellularAggregates. This base class is not
 * templated over the space dimension.
 */
class ITK_EXPORT CellularAggregateBase:public Object
{
public:
  /** Standard class typedefs. */
  typedef CellularAggregateBase      Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /*** Run-time type information (and related methods). */
  itkTypeMacro(BioCellularAggregateBase, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Convenient typedefs. */
  typedef float          ImagePixelType;
  typedef ImagePixelType SubstrateValueType;
public:
  virtual void Add(CellBase *cellA, CellBase *cellB, double perturbationLength);

  virtual void Remove(CellBase *cell);

  virtual SubstrateValueType GetSubstrateValue(unsigned long int cellId,
                                               unsigned int substrateId) const;

protected:
  CellularAggregateBase();
  virtual ~CellularAggregateBase();
  CellularAggregateBase(const Self &);
  void PrintSelf(std::ostream & os, Indent indent) const;
};
} // end namespace bio
} // end namespace itk

#endif
