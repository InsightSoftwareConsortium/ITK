/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSwathChainCodePathFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSwathChainCodePathFilter_h
#define __itkSwathChainCodePathFilter_h

#include "itkPathAndImageToPathFilter.h"

namespace itk
{
  
/** \class SwathChainCodePathFilter
 * \brief Filter that optimizes a path relative to an image in ND.
 *
 * SwathChainCodePathFilter produces a chain code representation of a path
 * that is optimal with respect to an image and an original chain code path
 * (sometimes referred to as an "initial contour").  A hypercube "fovea" traces
 * along the initial contour, sweeping out a "swath" through the image.
 * Dynamic programming is used to find the "optimal" path through the image,
 * where "optimal" is defined by a user-specified "index metrit" function (not
 * to be confused with registration metrics):
 *
 * merit of including an index in a path at step_i =
 * f( direction(step_i), the index)
 *
 * \ingroup PathFilters
 */
template <class TPath, class TImage>
class ITK_EXPORT SwathChainCodePathFilter : public
PathAndImageToPathFilter< TPath, TImage, TPath >
{
public:
  /** Standard class typedefs. */
  typedef SwathChainCodePathFilter                            Self;
  typedef PathAndImageToPathFilter< TPath, TImage, TPath >    Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SwathChainCodePathFilter, PathAndImageToPathFilter);

  /** Some convenient typedefs. */
  typedef          TPath                   PathType;
  typedef typename PathType::Pointer       PathPointer;
  typedef typename PathType::ConstPointer  PathConstPointer;
  typedef typename PathType::InputType     PathInputType;
  typedef typename PathType::IndexType     IndexType;
  typedef typename PathType::OffsetType    OffsetType;
  
protected:
  SwathChainCodePathFilter();
  virtual ~SwathChainCodePathFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData(void);

private:
  SwathChainCodePathFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSwathChainCodePathFilter.txx"
#endif

#endif
