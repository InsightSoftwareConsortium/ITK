/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPathToChainCodePathFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkPathToChainCodePathFilter_h
#define _itkPathToChainCodePathFilter_h

#include "itkPathToPathFilter.h"
#include "itkOffset.h"
//Templates require interfaces conforming to itkPath.h and itkChainCodePath.h

namespace itk
{
  
/** \class PathToChainCodePathFilter
 * \brief Filter that produces a chain code version of a path
 *
 * PathToChainCodePathFilter produces a chain code representation of a path.
 * If MaximallyConnectedOn() is called, then the resulting chain code will be
 * maximally connected (for example, 4-connected instead of 8-connected in 2D).
 * 
 * \ingroup PathFilters
 */
template <class TInputPath, class TOutputChainCodePath>
class ITK_EXPORT PathToChainCodePathFilter : public
  PathToPathFilter< TInputPath, TOutputChainCodePath >
{
public:
  /** Standard class typedefs. */
  typedef PathToChainCodePathFilter  Self;
  typedef PathToPathFilter< TInputPath, TOutputChainCodePath >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(PathToChainCodePathFilter, PathToPathFilter);

  /** Some convenient typedefs. */
  typedef TInputPath                          InputPathType;
  typedef typename InputPathType::Pointer     InputPathPointer;
  typedef typename InputPathType::InputType   InputPathInputType;
  typedef TOutputChainCodePath                OutputPathType;
  typedef typename OutputPathType::Pointer    OutputPathPointer;
  typedef typename OutputPathType::InputType  OutputPathInputType;
  typedef typename InputPathType::IndexType   IndexType;
  typedef typename InputPathType::OffsetType  OffsetType;

  /** Set the direction in which to reflect the data. */
  itkSetMacro( MaximallyConnected, bool )
  itkBooleanMacro( MaximallyConnected )
  
protected:
  PathToChainCodePathFilter();
  virtual ~PathToChainCodePathFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData(void);

private:
  PathToChainCodePathFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  bool m_MaximallyConnected;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPathToChainCodePathFilter.txx"
#endif

#endif
