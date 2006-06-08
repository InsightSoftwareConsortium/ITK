/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToPointSetRegistrationMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPointSetToPointSetRegistrationMethod_h
#define __itkPointSetToPointSetRegistrationMethod_h

#include "itkProcessObject.h"
#include "itkPointSetToPointSetMetric.h"
#include "itkMultipleValuedNonLinearOptimizer.h"
#include "itkDataObjectDecorator.h"

namespace itk
{

/** \class PointSetToPointSetRegistrationMethod
 * \brief Base class for PointSet to PointSet Registration Methods
 *
 * This Class define the generic interface for a registration method.
 *
 * This class is templated over the type of the PointSet and the PointSet to be
 * registered. A generic Transform is used by this class. That allows to select
 * at run time the particular type of transformation that is to be applied for
 * registering the PointSets.
 *
 * This method use a generic Metric in order to compare the PointSet and the
 * PointSet.  The final goal of the registration method is to find the set of
 * parameters of the Transformation that optimizes the metric.
 *
 * The registration method also support a generic optimizer that can be
 * selected at run-time. The only restriction for the optimizer is that it
 * should be able to operate in single-valued cost functions given that the
 * metrics used to compare PointSet with PointSets provide a single value as
 * output.
 *
 * The terms : FixedPointSet and MovingPointSet are used in this class to indicate
 * that the PointSet is being mapped by the transform.
 *
 * This class uses the coordinate system of the Fixed PointSet as a reference
 * and searchs for a Transform that will map points from the space of the Fixed
 * PointSet to the space of the Moving PointSet.
 *
 * For doing so, a Metric will be continously applied to compare the Fixed
 * PointSet with the Transformed Moving PointSet. This process also requires to
 * interpolate values from the Moving PointSet.
 *
 * \ingroup RegistrationFilters
 */
template <typename TFixedPointSet, typename TMovingPointSet>
class ITK_EXPORT PointSetToPointSetRegistrationMethod : public ProcessObject 
{
public:
  /** Standard class typedefs. */
  typedef PointSetToPointSetRegistrationMethod  Self;
  typedef ProcessObject  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToPointSetRegistrationMethod, ProcessObject);

  /**  Type of the Fixed PointSet. */
  typedef          TFixedPointSet                   FixedPointSetType;
  typedef typename FixedPointSetType::ConstPointer  FixedPointSetConstPointer;

  /**  Type of the Moving PointSet. */
  typedef          TMovingPointSet                  MovingPointSetType;
  typedef typename MovingPointSetType::ConstPointer    MovingPointSetConstPointer;

  /**  Type of the metric. */
  typedef PointSetToPointSetMetric< FixedPointSetType,
                                 MovingPointSetType >    MetricType;
  typedef typename MetricType::Pointer                MetricPointer;

  /**  Type of the Transform . */
  typedef  typename MetricType::TransformType      TransformType;
  typedef  typename TransformType::Pointer         TransformPointer;

  /** Type for the output: Using Decorator pattern for enabling
   *  the Transform to be passed in the data pipeline */
  typedef  DataObjectDecorator< TransformType >    TransformOutputType;
  typedef typename TransformOutputType::Pointer    TransformOutputPointer;
  typedef typename TransformOutputType::ConstPointer    TransformOutputConstPointer;
  /**  Type of the optimizer. */
  typedef   MultipleValuedNonLinearOptimizer         OptimizerType;

  /** Type of the Transformation parameters This is the same type used to
   *  represent the search space of the optimization algorithm */
  typedef  typename MetricType::TransformParametersType    ParametersType;

  /** Smart Pointer type to a DataObject. */
  typedef typename DataObject::Pointer DataObjectPointer;

  /** Method that initiates the registration. */
  void StartRegistration(void);

  /** Set/Get the Fixed PointSet. */
  itkSetConstObjectMacro( FixedPointSet, FixedPointSetType );
  itkGetConstObjectMacro( FixedPointSet, FixedPointSetType ); 

  /** Set/Get the Moving PointSet. */
  itkSetConstObjectMacro( MovingPointSet, MovingPointSetType );
  itkGetConstObjectMacro( MovingPointSet, MovingPointSetType );

  /** Set/Get the Optimizer. */
  itkSetObjectMacro( Optimizer,  OptimizerType );
  itkGetObjectMacro( Optimizer,  OptimizerType );

  /** Set/Get the Metric. */
  itkSetObjectMacro( Metric, MetricType );
  itkGetObjectMacro( Metric, MetricType );

  /** Set/Get the Transfrom. */
  itkSetObjectMacro( Transform, TransformType );
  itkGetObjectMacro( Transform, TransformType );

  /** Set/Get the initial transformation parameters. */
  virtual void SetInitialTransformParameters( const ParametersType & param );
  itkGetConstReferenceMacro( InitialTransformParameters, ParametersType );

  /** Get the last transformation parameters visited by 
   * the optimizer. */
  itkGetConstReferenceMacro( LastTransformParameters, ParametersType );

  /** Initialize by setting the interconnects between the components. */
  void Initialize() throw (ExceptionObject);

  /** Returns the transform resulting from the registration process  */
  const TransformOutputType * GetOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  virtual DataObjectPointer MakeOutput(unsigned int idx);

  /** Method to return the latest modified time of this object or
   * any of its cached ivars */
  unsigned long GetMTime() const;  

protected:
  PointSetToPointSetRegistrationMethod();
  virtual ~PointSetToPointSetRegistrationMethod() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Method invoked by the pipeline in order to trigger the computation of 
   * the registration. */
  void  GenerateData ();
  
private:
  PointSetToPointSetRegistrationMethod(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  MetricPointer                    m_Metric;
  OptimizerType::Pointer           m_Optimizer;

  MovingPointSetConstPointer       m_MovingPointSet;
  FixedPointSetConstPointer        m_FixedPointSet;

  TransformPointer                 m_Transform;

  ParametersType                   m_InitialTransformParameters;
  ParametersType                   m_LastTransformParameters;
  
};


} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToPointSetRegistrationMethod.txx"
#endif

#endif



