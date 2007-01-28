/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectTreeContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectTreeContainer_h
#define __itkSpatialObjectTreeContainer_h

#include <itkTreeContainer.h>
#include <itkSpatialObject.h>
#include <itkSpatialObjectTreeNode.h>

namespace itk
{

template< unsigned int TDimension>  class SpatialObject;

/** \class SpatialObjectTreeContainer
 *  \brief Array class with size defined at construction time.
 * 
 * This class derives from the vnl_vector<> class. 
 * Its size is assigned at construction time (run time) and can 
 * not be changed afterwards except by using assignment to another
 * Array.
 *
 * The class is templated over the type of the elements.
 *
 * Template parameters for class Array:
 *
 * - TValueType = Element type stored at each location in the array.
 *
 * \ingroup DataRepresentation 
 */
template <unsigned int TDimension>
class SpatialObjectTreeContainer : 
             public TreeContainer<itk::SpatialObject<TDimension>*>
{

public:

  /** Standard typedefs */
  typedef SpatialObject<TDimension>              SpatialObjectType;
  typedef SpatialObjectType*                     SpatialObjectPointer;
  typedef TreeContainer<SpatialObjectPointer>    Superclass;
  typedef SpatialObjectTreeContainer<TDimension> Self;
  typedef SmartPointer<Self>                     Pointer;
  typedef SmartPointer<const Self>               ConstPointer;
  typedef SpatialObjectTreeNode<TDimension>      TreeNodeType;

  /** Iterators typedef */
  typedef typename Superclass::IteratorType IteratorType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialObjectTreeContainer, TreeContainer);

  /** Set the root */
  bool SetRoot(SpatialObjectPointer element);
  bool SetRoot(typename Superclass::TreeNodeType* node)
    {return Superclass::SetRoot(node);}

protected:
  
  SpatialObjectTreeContainer(); 
  virtual ~SpatialObjectTreeContainer();
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectTreeContainer.txx"
#endif


#endif
