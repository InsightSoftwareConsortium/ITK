/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToPointSetMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPointSetToPointSetMetric_h
#define __itkPointSetToPointSetMetric_h

#include "itkImageBase.h"
#include "itkTransform.h"
#include "itkMultipleValuedCostFunction.h"
#include "itkExceptionObject.h"
#include "itkGradientRecursiveGaussianImageFilter.h"

namespace itk
{
/** \class PointSetToPointSetMetric
 * \brief Computes similarity between two point sets.
 *
 * This Class is templated over the type of the two point-sets.  It
 * expects a Transform to be plugged in.  This particular
 * class is the base class for a hierarchy of point-set to point-set metrics.
 *
 * This class computes a value that measures the similarity between the fixed point-set
 * and the transformed moving point-set.
 *
 * \ingroup RegistrationMetrics
 *
 */

template< class TFixedPointSet,  class TMovingPointSet >
class ITK_EXPORT PointSetToPointSetMetric:public MultipleValuedCostFunction
{
public:

  /** Standard class typedefs. */
  typedef PointSetToPointSetMetric   Self;
  typedef MultipleValuedCostFunction Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Type used for representing point components  */
  typedef Superclass::ParametersValueType CoordinateRepresentationType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToPointSetMetric, MultipleValuedCostFunction);

  /**  Type of the moving Pointset. */
  typedef TMovingPointSet                           MovingPointSetType;
  typedef typename TMovingPointSet::PixelType       MovingPointSetPixelType;
  typedef typename MovingPointSetType::ConstPointer MovingPointSetConstPointer;

  /**  Type of the fixed Pointset. */
  typedef TFixedPointSet                           FixedPointSetType;
  typedef typename FixedPointSetType::ConstPointer FixedPointSetConstPointer;

  /** Constants for the pointset dimensions */
  itkStaticConstMacro(MovingPointSetDimension, unsigned int,
                      TMovingPointSet::PointDimension);
  itkStaticConstMacro(FixedPointSetDimension, unsigned int,
                      TFixedPointSet::PointDimension);

  typedef typename FixedPointSetType::PointsContainer::ConstIterator    PointIterator;
  typedef typename FixedPointSetType::PointDataContainer::ConstIterator PointDataIterator;

  /**  Type of the Transform Base class */
  typedef Transform< CoordinateRepresentationType,
                     itkGetStaticConstMacro(MovingPointSetDimension),
                     itkGetStaticConstMacro(FixedPointSetDimension) > TransformType;

  typedef typename TransformType::Pointer         TransformPointer;
  typedef typename TransformType::InputPointType  InputPointType;
  typedef typename TransformType::OutputPointType OutputPointType;
  typedef typename TransformType::ParametersType  TransformParametersType;
  typedef typename TransformType::JacobianType    TransformJacobianType;

  /**  Type of the measure. */
  typedef Superclass::MeasureType MeasureType;

  /**  Type of the derivative. */
  typedef Superclass::DerivativeType DerivativeType;

  /**  Type of the parameters. */
  typedef Superclass::ParametersType ParametersType;

  /** Connect the Fixed Pointset.  */
  itkSetConstObjectMacro(FixedPointSet, FixedPointSetType);

  /** Get the Fixed Pointset. */
  itkGetConstObjectMacro(FixedPointSet, FixedPointSetType);

  /** Connect the Moving Pointset.  */
  itkSetConstObjectMacro(MovingPointSet, MovingPointSetType);

  /** Get the Moving Pointset. */
  itkGetConstObjectMacro(MovingPointSet, MovingPointSetType);

  /** Connect the Transform. */
  itkSetObjectMacro(Transform, TransformType);

  /** Get a pointer to the Transform.  */
  itkGetObjectMacro(Transform, TransformType);

  /** Set the parameters defining the Transform. */
  void SetTransformParameters(const ParametersType & parameters) const;

  /** Return the number of parameters required by the Transform */
  unsigned int GetNumberOfParameters(void) const
  { return m_Transform->GetNumberOfParameters(); }

  /** Initialize the Metric by making sure that all the components
   *  are present and plugged together correctly     */
  virtual void Initialize(void)
  throw ( ExceptionObject );

protected:
  PointSetToPointSetMetric();
  virtual ~PointSetToPointSetMetric() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  FixedPointSetConstPointer m_FixedPointSet;

  MovingPointSetConstPointer m_MovingPointSet;

  mutable TransformPointer m_Transform;
private:
  PointSetToPointSetMetric(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToPointSetMetric.txx"
#endif

#endif
