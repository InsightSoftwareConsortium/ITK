/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorNeighborhoodInnerProduct.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorNeighborhoodInnerProduct_h
#define __itkVectorNeighborhoodInnerProduct_h

#include "itkNeighborhoodIterator.h"
#include "itkSmartNeighborhoodIterator.h"
#include "itkVector.h"
#include "itkNeighborhood.h"
#include "itkConstSliceIterator.h"
#include "itkImageBoundaryCondition.h"

namespace itk {
  
/** \class VectorNeighborhoodInnerProduct
 *
 * This is an explicit implementation of what should really be a partial
 * template specialization of NeighborhoodInnerProduct for itkVector.
 * 
 * This class defines the inner product operation between an itk::Neighborhood
 * and and itk::NeighborhoodOperator.  The operator() method is overloaded
 * to support various types of neighborhoods as well as inner products with
 * slices of neighborhoods.
 *
 * \ingroup Operators
 * 
 */
template<class TImage>
class ITK_EXPORT VectorNeighborhoodInnerProduct
{
public:
  /** Standard typedefs */
  typedef VectorNeighborhoodInnerProduct Self;

  /** Extract the pixel type and scalar type from the image template parameter. */
  typedef typename TImage::PixelType PixelType;
  typedef typename PixelType::ValueType ScalarValueType;
  
  /** Extract the image and vector dimension from the image template parameter. */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      PixelType::Dimension);
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);
  
  /** Operator typedef */
  typedef Neighborhood<ScalarValueType,
                      itkGetStaticConstMacro(ImageDimension)> OperatorType;

  /** Conversion operator. */
  PixelType operator()(const std::slice &s,
                       const ConstNeighborhoodIterator<TImage> &it,
                       const OperatorType &op) const;

  /** Conversion operator. */
  PixelType operator()(const ConstNeighborhoodIterator<TImage> &it,
                       const OperatorType &op) const
    {
      return this->operator()(std::slice(0, it.Size(), 1), it, op);
    }
};
  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorNeighborhoodInnerProduct.txx"
#endif

#endif
