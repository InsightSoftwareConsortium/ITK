/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureFlowFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCurvatureFlowFunction_h_
#define __itkCurvatureFlowFunction_h_

#include "itkFiniteDifferenceFunction.h"
#include "itkMacro.h"

namespace itk {

/** \class CurvatureFlowFunction
 *  
 * This class encapsulate the finite difference equation which drives a
 * curvature flow denoising algorithm.
 *
 * This class uses a zero flux Neumann boundary condition when computing
 * derivatives near the data boundary.
 *
 * This class operates as part of the finite difference solver hierarchy.
 *
 * \sa CurvatureFlowImageFilter
 * \sa ZeroFluxNeumannBoundaryCondition 
 * \ingroup FiniteDifferenceFunctions
*/
template <class TImage>
class CurvatureFlowFunction :
    public FiniteDifferenceFunction<TImage>
{
public:
  /**  Standard class typedefs. */
  typedef CurvatureFlowFunction Self;
  typedef FiniteDifferenceFunction<TImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( CurvatureFlowFunction,
                FiniteDifferenceFunction );
  
  /** Inherit some parameters from the superclass type. */
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Extract superclass dimension. */
  enum { ImageDimension = Superclass::ImageDimension };  

  /** Returns the time step supplied by the user.  We don't need to use the
   * global data supplied since we are returning a fixed value. */
  virtual TimeStepType ComputeGlobalTimeStep(void *GlobalData) const
    { return this->GetTimeStep(); }

  /** This class does not use this particular parameter
   * so it's safe to return a null value. */
  virtual void *GetGlobalDataPointer() const
    {  return 0; }

  /** Does nothing.  No global data is used in this class of equations. */
  virtual void ReleaseGlobalDataPointer(void *GlobalData) const
    { /* do nothing */ }

  /** Set the time step parameter */
  void SetTimeStep( const TimeStepType & t )
    { m_TimeStep = t; }

  /** Get the time step parameter */
  const TimeStepType &GetTimeStep() const
    { return m_TimeStep; }

  /** This method computes the solution update for each pixel that does not
   * lie on a the data set boundary. */
  virtual PixelType ComputeUpdate(const NeighborhoodType &neighborhood,
                                  void * globalData,
                                  const FloatOffsetType& offset = m_ZeroOffset
                                  ) const;

  /** This method computes the solution update for each pixel that lies
   * on the data set boundary. */
  virtual PixelType ComputeUpdate(const BoundaryNeighborhoodType
                                  &neighborhood, void * globalData,
                                  const FloatOffsetType& offset = m_ZeroOffset
                                  ) const;

protected:
  CurvatureFlowFunction();
  ~CurvatureFlowFunction() {}

private:
  CurvatureFlowFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  TimeStepType       m_TimeStep;
  
  
};

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureFlowFunction.txx"
#endif

#endif
