/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineInterpolationWeightFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBSplineInterpolationWeightFunction_h
#define __itkBSplineInterpolationWeightFunction_h

#include "itkFunctionBase.h"
#include "itkContinuousIndex.h"
#include "itkBSplineKernelFunction.h"
#include "itkArray.h"
#include "itkArray2D.h"

namespace itk
{

/** \class BSplineInterpolationWeightFunction
 * \brief Returns the weights over the support region used for B-spline
 * interpolation/reconstruction.
 *
 * Computes/evaluate the B-spline interpolation weights over the
 * support region of the B-spline.
 *
 * This class is templated over the coordinate representation type,
 * the space dimension and the spline order.
 *
 * \sa Point
 * \sa Index
 * \sa ContinuousIndex
 *
 * \ingroup Functions ImageInterpolators
 */
template <
class TCoordRep = float, 
unsigned int VSpaceDimension = 2,
unsigned int VSplineOrder = 3 
>
class ITK_EXPORT BSplineInterpolationWeightFunction : 
  public FunctionBase< ContinuousIndex<TCoordRep,VSpaceDimension>, 
                       Array<double> > 
{
public:
  /** Standard class typedefs. */
  typedef BSplineInterpolationWeightFunction Self;
  typedef FunctionBase< ContinuousIndex<TCoordRep,VSpaceDimension>,
            Array<double> >                  Superclass;
  typedef SmartPointer<Self>                 Pointer;
  typedef SmartPointer<const Self>           ConstPointer;

  /** New macro for creation of through the object factory.*/
  itkNewMacro( Self );
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineInterpolationWeightFunction, FunctionBase);

  /** Space dimension. */
  itkStaticConstMacro(SpaceDimension, unsigned int, VSpaceDimension);

  /** Spline order. */
  itkStaticConstMacro(SplineOrder, unsigned int, VSplineOrder);

  /** OutputType typedef support. */
  typedef Array<double> WeightsType;

  /** Index and size typedef support. */
  typedef Index<VSpaceDimension> IndexType;
  typedef Size<VSpaceDimension>  SizeType;

  /** ContinuousIndex typedef support. */
  typedef ContinuousIndex<TCoordRep,VSpaceDimension> ContinuousIndexType;

  /** Evaluate the weights at specified ContinousIndex position.
   * Subclasses must provide this method. */
  virtual WeightsType Evaluate( const ContinuousIndexType & index ) const;

  /** Evaluate the weights at specified ContinousIndex position.
   * The weights are returned in the user specified container.
   * This function assume that weights can hold 
   * (SplineOrder + 1)^(SpaceDimension) elements. For efficiency,
   * no size checking is done. 
   * On return, startIndex contains the start index of the
   * support region over which the weights are defined.
   */
  virtual void Evaluate( const ContinuousIndexType & index,
    WeightsType & weights, IndexType & startIndex ) const;

  /** Get support region size. */
  itkGetMacro( SupportSize, SizeType );

  /** Get number of weights. */
  itkGetMacro( NumberOfWeights, unsigned long );
  
protected:
  BSplineInterpolationWeightFunction();
  ~BSplineInterpolationWeightFunction() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  BSplineInterpolationWeightFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Number of weights. */
  unsigned long  m_NumberOfWeights; 

  /** Size of support region. */
  SizeType m_SupportSize;

  /** Lookup table type. */
  typedef Array2D<unsigned long> TableType;

  /** Table mapping linear offset to indices. */
  TableType m_OffsetToIndexTable;

  /** Interpolation kernel type. */
  typedef BSplineKernelFunction<itkGetStaticConstMacro(SplineOrder)> KernelType;
  
  /** Interpolation kernel. */
  typename KernelType::Pointer m_Kernel;
  
};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BSplineInterpolationWeightFunction(_, EXPORT, x, y) namespace itk { \
  _(3(class EXPORT BSplineInterpolationWeightFunction< ITK_TEMPLATE_3 x >)) \
  namespace Templates { typedef BSplineInterpolationWeightFunction< ITK_TEMPLATE_3 x > BSplineInterpolationWeightFunction##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkBSplineInterpolationWeightFunction+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkBSplineInterpolationWeightFunction.txx"
#endif


#endif
