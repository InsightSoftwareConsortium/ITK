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
  typedef typename Superclass::ImageType  ImageType;
  typedef typename Superclass::PixelType  PixelType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef PixelType  ScalarValueType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;
  typedef typename Superclass::TimeStepType TimeStepType;

  /** Extract superclass dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

 /** Computes the time step for an update given a global data structure.
   * The data used in the computation may take different forms depending on
   * the nature of the equations.  This global data cannot be kept in the
   * instance of the equation object itself since the equation object must
   * remain stateless for thread safety.  The global data is therefore managed
   * for each thread by the finite difference solver filters. 
   *
   * Currently, this function returns the user specified constant time step. 
   * \todo compute timestep based on CFL condition.
   **/
  virtual TimeStepType ComputeGlobalTimeStep(void *GlobalData) const;

  /** Returns a pointer to a global data structure that is passed to this
   * object from the solver at each calculation.  The idea is that the solver
   * holds the state of any global values needed to calculate the time step,
   * while the equation object performs the actual calculations.  The global
   * data should also be initialized in this method. */
  virtual void *GetGlobalDataPointer() const
    {  
      GlobalDataStruct *ans = new GlobalDataStruct();
      ans->m_MaxChange   = NumericTraits<ScalarValueType>::Zero;
      return ans; 
    }

  /** When the finite difference solver filter has finished using a global
   * data pointer, it passes it to this method, which frees the memory.
   * The solver cannot free the memory because it does not know the type
   * to which the pointer points. */
  virtual void ReleaseGlobalDataPointer(void *GlobalData) const
    { delete (GlobalDataStruct *) GlobalData; }

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
                                  const FloatOffsetType& offset = FloatOffsetType(0.0)
                                  ) const;

  /** This method computes the solution update for each pixel that lies
   * on the data set boundary. */
  virtual PixelType ComputeUpdate(const BoundaryNeighborhoodType
                                  &neighborhood, void * globalData,
                                  const FloatOffsetType& offset = FloatOffsetType(0.0)
                                  ) const;

protected:

  /** A global data type for this class of equations.  Used to store
   * values that are needed in calculating the time step. */
  struct GlobalDataStruct
  {
    GlobalDataStruct()
    {
      m_MaxChange = NumericTraits<ScalarValueType>::Zero;
    }
    ~GlobalDataStruct() {}
    
    ScalarValueType m_MaxChange;
  };

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
