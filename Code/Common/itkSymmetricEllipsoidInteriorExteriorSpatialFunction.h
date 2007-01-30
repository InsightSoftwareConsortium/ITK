/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSymmetricEllipsoidInteriorExteriorSpatialFunction_h
#define __itkSymmetricEllipsoidInteriorExteriorSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{

/** \class EllipsoidSpatialFunction
 * \brief Function implementation of an ellipsoid
 *
 * Similar to itkEllipsoidInteriorExteriorSpatialFunction in that it 
 * implements a function that returns 1 for points inside or on the surface
 * of a ellipsoid and 0 for points outside the ellipsoid. However, this
 * ellipsoid is defined by a single orientation vector and deals
 * only with symmetric ellipsoids. An n-dimensional symmetric ellipsoid 
 * is one which has m axes of equal length and (n - m) unique axes lengths.
 * Specifically, this class deals with the case where (n - m) = 1 and
 * the ellipsoid's major axis is oriented along a singles orientation vector.
 */
template <unsigned int VDimension = 3,
          typename TInput = Point<double, VDimension> >
class ITK_EXPORT SymmetricEllipsoidInteriorExteriorSpatialFunction:
    public InteriorExteriorSpatialFunction<VDimension, TInput>
{
public:
  /** Standard class typedefs. */
  typedef SymmetricEllipsoidInteriorExteriorSpatialFunction Self;
  typedef InteriorExteriorSpatialFunction<VDimension> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  typedef Vector<double,VDimension> VectorType;
      
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time information. */
  itkTypeMacro(SymmetricEllipsoidInteriorExteriorSpatialFunction,InteriorExteriorSpatialFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Evaluates the function at a given position. */
  OutputType Evaluate(const InputType& position) const;

  /** Get and set the center of the ellipsoid. */
  itkGetMacro(Center, InputType);
  itkSetMacro(Center, InputType);
  
  /** Set the orientation vector of the ellipsoid's unique axis and axes lengths.
   * Must be normalized!!!!! */
  void SetOrientation(VectorType orientation, double uniqueAxis, double symmetricAxes);
     
protected:
  SymmetricEllipsoidInteriorExteriorSpatialFunction();
  virtual ~SymmetricEllipsoidInteriorExteriorSpatialFunction();

  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SymmetricEllipsoidInteriorExteriorSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The center of the ellipsoid. */
  InputType m_Center;

  /** The unique axis length of the ellipsoid. */
  double m_UniqueAxis;
  
  /** The symmetric axes lengths of the ellipsoid. */
  double m_SymmetricAxes;

  /** The orientation vector of the ellipsoid's unique axis. */  
  Vector<double, VDimension> m_Orientation;

  /** The vector ratio. */  
  double m_VectorRatio;

};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_SymmetricEllipsoidInteriorExteriorSpatialFunction(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT SymmetricEllipsoidInteriorExteriorSpatialFunction< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef SymmetricEllipsoidInteriorExteriorSpatialFunction< ITK_TEMPLATE_2 x > \
                                           SymmetricEllipsoidInteriorExteriorSpatialFunction##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkSymmetricEllipsoidInteriorExteriorSpatialFunction+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.txx"
#endif

#endif
