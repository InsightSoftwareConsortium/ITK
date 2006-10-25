/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFlatStructuringElement.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFlatStructuringElement_h
#define __itkFlatStructuringElement_h

#include "itkNeighborhood.h"
#include "itkSize.h"
#include "itkOffset.h"
#include <vector>
#include "itkVector.h"
#include "itkImage.h"

namespace itk {

/** \class FlatStructuringElement
* \brief A class to support a variety of flat structuring elements, 
* including versions created by decomposition of lines.
**/

template<unsigned int NDimension>
class ITK_EXPORT FlatStructuringElement : 
  public Neighborhood <bool,NDimension>
{
public:

  /** External support for dimensionality. */
  itkStaticConstMacro(NeighborhoodDimension, unsigned int, NDimension);

  /** External support for dimensionality. */
  itkStaticConstMacro(Dimension, unsigned int, NDimension);

  /** Standard class typedefs. */
  typedef FlatStructuringElement< NDimension >  Self;
  typedef Neighborhood<bool, NDimension >       Superclass;

  /** External support for pixel type. */
  typedef typename Superclass::PixelType PixelType;

  /** Iterator typedef support. Note the naming is intentional, i.e.,
  * ::iterator and ::const_iterator, because the allocator may be a
  * vnl object or other type, which uses this form. */
  typedef typename Superclass::Iterator       Iterator;
  typedef typename Superclass::ConstIterator  ConstIterator;
  
  /** Size and value typedef support. */
  typedef typename Superclass::SizeType       SizeType;
  typedef typename Superclass::SizeValueType  SizeValueType;
  
  /** Radius typedef support. */
  typedef ::itk::Size< NDimension >           RadiusType;

  /** External slice iterator type typedef support. */
  typedef typename Superclass::SliceIteratorType SliceIteratorType;
  
  /** Default destructor. */
  virtual ~FlatStructuringElement() {}

  /** Default consructor. */
  FlatStructuringElement() {}

  /** Various constructors */
  static Self Box( RadiusType radius );
  
  static Self Ball( RadiusType radius );
  
  
protected:


private:


};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFlatStructuringElement.txx"
#endif



#endif
