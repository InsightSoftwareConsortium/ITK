/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevelSetFunction_h_
#define __itkLevelSetFunction_h_

#include "itkLevelSetFunctionBase.h"

namespace itk {

/** \class LevelSetFunction
  * \brief The LevelSetFunction class is a generic function object which can be
 * used to create a level set method filter when combined with an appropriate
 * finite difference image filter.  (See FiniteDifferenceImageFilter.)
 *
 * LevelSetFunction implements a generic level set function.  This function is
 * an expanded form of the basic equation developed in [1].
 *
 * \f$\phi_{t} + \alpha
 * \stackrel{\rightharpoonup}{A}(\mathbf{x})\cdot\nabla\phi + \beta
 * P(\mathbf{x})\mid\nabla\phi\mid = \gamma Z(\mathbf{x})\kappa\mid\nabla\phi\mid\f$
 *
 * where \f$ \stackrel{\rightharpoonup}{A} \f$ is an advection term, \f$ P \f$
 * is a propagation (growth) term, and \f$ Z \f$ is a spatial modifier term for
 * the mean curvature \f$ \kappa \f$.  \f$ \alpha \f$, \f$ \beta \f$, and \f$
 * \gamma \f$ are all scalar constants.
 *
 * Terms in the equation above are supplied through virtual methods, which must
 * be subclassed to complete an implementation.  Terms can be eliminated from
 * the equation by setting the corresponding constants to zero. A wide variety
 * of level set methods can be implemented by subclassing this basic equation.
 *
 * In ITK, the usual sign convention is that the INSIDE of a surface contains
 * NEGATIVE values and the OUTSIDE of the surface contains POSITIVE values.
 *
 * \warning You MUST call Initialize() in the constructor of subclasses of this
 * object to set it up properly to do level-set Calculations.  The argument
 * that you pass Initialize is the radius of the neighborhood needed to perform
 * the calculations.  If your subclass does not do any additional neighborhood
 * processing, then the default radius should be 1 in each direction.
 *
 * \par REFERENCES
 * \par
 * [1] Sethian, J.A. Level Set Methods. Cambridge University Press. 1996.
 *
 * \ingroup FiniteDifferenceFunctions
 * \ingroup Functions
 */
template <class TImageType>
class ITK_EXPORT LevelSetFunction
  : public LevelSetFunctionBase<TImageType>
{
public:
  /** Standard class typedefs. */
  typedef LevelSetFunction Self;
  typedef LevelSetFunctionBase<TImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( LevelSetFunction, LevelSetFunctionBase );

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType ImageType;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Convenient typedefs. */
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::ScalarValueType  ScalarValueType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::VectorType VectorType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;

  /** Compute the equation value. */
  virtual PixelType ComputeUpdate(const NeighborhoodType &neighborhood,
                                  void *globalData,
                                  const FloatOffsetType& = FloatOffsetType(0.0)) const;

 /** Computes the time step for an update given a global data structure.
   * The data used in the computation may take different forms depending on
   * the nature of the equations.  This global data cannot be kept in the
   * instance of the equation object itself since the equation object must
   * remain stateless for thread safety.  The global data is therefore managed
   * for each thread by the finite difference solver filters. */
  virtual TimeStepType ComputeGlobalTimeStep(void *GlobalData) const;

  /** Returns a pointer to a global data structure that is passed to this
   * object from the solver at each calculation.  The idea is that the solver
   * holds the state of any global values needed to calculate the time step,
   * while the equation object performs the actual calculations.  The global
   * data should also be initialized in this method. */
  virtual void *GetGlobalDataPointer() const
    {
      GlobalDataStruct *ans = new GlobalDataStruct();
      ans->m_MaxAdvectionChange   = NumericTraits<ScalarValueType>::Zero;
      ans->m_MaxPropagationChange = NumericTraits<ScalarValueType>::Zero;
      return ans; 
    }

  /** This method creates the appropriate member variable operators for the
   * level-set calculations.  The argument to this function is a the radius
   * necessary for performing the level-set calculations. */
  virtual void Initialize(const RadiusType &r);
  
  /** When the finite difference solver filter has finished using a global
   * data pointer, it passes it to this method, which frees the memory.
   * The solver cannot free the memory because it does not know the type
   * to which the pointer points. */
  virtual void ReleaseGlobalDataPointer(void *GlobalData) const
    { delete (GlobalDataStruct *) GlobalData; }

protected:
  /** A global data type for this class of equations.  Used to store
   * values that are needed in calculating the time step. */
  struct GlobalDataStruct
  {
    ScalarValueType m_MaxAdvectionChange;
    ScalarValueType m_MaxPropagationChange;
  };
  
  LevelSetFunction() {}
  ~LevelSetFunction() {}
  void PrintSelf(std::ostream &s, Indent indent) const;
  
  /** Constants used in the time step calculation. */
  static double m_WaveDT;
  static double m_DT;

private:
  LevelSetFunction(const Self&); //purposely not implemented
  void operator=(const Self&);   //purposely not implemented

  /** Slices for the ND neighborhood. */
  std::slice  x_slice[ImageDimension];

  /** The offset of the center pixel in the neighborhood. */
  ::size_t m_Center;

  /** Stride length along the y-dimension. */
  ::size_t m_xStride[ImageDimension];
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetFunction.txx"
#endif

#endif

