/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialFunction_h
#define __itkSpatialFunction_h

#include "itkFunctionBase.h"
#include "itkPoint.h"

namespace itk
{

/** \class SpatialFunction
 * \brief N-dimensional spatial function class
 *
 * itk::SpatialFunction provides the ability to define functions that can
 * be evaluated at an arbitrary point in space (physical or otherwise). The return
 * type is specified by the derived class, and the input to the function
 * is an n-dimensional itk::Point.
 *
 * Although itk::ImageFunction and itk::SpatialFunction are quite similar,
 * itk::SpatialFunction derived classes exist without reference to an Image
 * type.
 *
 * SpatialFunction is templated over output type (the data type
 * returned by an evaluate() call) and dimensionality.
 *
 * \ingroup SpatialFunctions
 */
template <typename TOutput, 
          unsigned int VImageDimension=3,
          typename TInput=Point<double, VImageDimension> >
class ITK_EXPORT SpatialFunction : public FunctionBase<TInput, TOutput>
{
public:
  /** Standard class typedefs. */
  typedef SpatialFunction Self;
  typedef FunctionBase< TInput, TOutput> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(SpatialFunction, FunctionBase);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Evaluate the function at a given position. Remember, position is
  * represented by an n-d itk::Point object with data type double. */
  virtual OutputType Evaluate( const InputType& input ) const = 0;

protected:
  SpatialFunction();
  virtual ~SpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialFunction.txx"
#endif

#endif
