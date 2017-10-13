/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkBioCellularAggregateBase_h
#define itkBioCellularAggregateBase_h

#include "itkIntTypes.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "ITKBioCellExport.h"

namespace itk
{
namespace bio
{
// Forward reference because of circular dependencies
class ITK_FORWARD_EXPORT CellBase;

/** \class CellularAggregateBase
 * \brief Base class for the CellularAggregates.
 *
 * This base class is not templated over the space dimension.
 *
 * \ingroup ITKBioCell
 */
class ITKBioCell_EXPORT CellularAggregateBase:public Object
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

  virtual SubstrateValueType GetSubstrateValue(IdentifierType cellId,
                                               unsigned int substrateId) const;

protected:
  CellularAggregateBase();
  virtual ~CellularAggregateBase() ITK_OVERRIDE;
  CellularAggregateBase(const Self &);
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;
};
} // end namespace bio
} // end namespace itk

#endif
