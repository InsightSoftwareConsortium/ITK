/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSet2DEquation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkLevelSet2DEquation_h_
#define __itkLevelSet2DEquation_h_

#include "itkLevelSetEquation.h"
#include "itkDerivativeOperator.h"

namespace itk {

/**
 *
 * \class LevelSet2DEquation
 *
 * \f$\phi_{t} = \alpha \stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi
 * + \beta G(\mathbf{x})\mid\nabla\phi\mid + \gamma Z(\mathbf{x})\kappa\f$
 *
 * \warning You MUST call Initialize() in the constructor of subclasses of this
 * object to set it up properly to do level-set Calculations.  The argument
 * that you pass Initialize is the radius of the neighborhood needed to perform
 * the calculations.  If your subclass does not do any additional neighborhood
 * processing, then the default radius should be 1 in each direction.
 *
 * \todo Documentation & References
 * \todo Account for variable curvature term in the CFL Condition when
 *       calculating the time step.
 */
template <class TImageType>
class LevelSet2DEquation
  : public LevelSetEquation<TImageType>
{
public:
  /**
   * Standard itk Self & Superclass typedefs
   */
  typedef LevelSet2DEquation Self;
  typedef LevelSetEquation<TImageType> Superclass;

  /**
   * Inherit some parameters from the superclass
   */
  typedef Superclass::ImageType ImageType;
  enum { ImageDimension = Superclass::ImageDimension };

  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType  TimeStepType;
  typedef typename Superclass::ScalarValueType  ScalarValueType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType
    BoundaryNeighborhoodType;
  typedef typename Superclass::VectorType VectorType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;

  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( LevelSet2DEquation, LevelSetEquation );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   *
   */
  virtual PixelType ComputeUpdate(const NeighborhoodType &neighborhood,
                                  void *globalData,
                 const FloatOffsetType& = m_ZeroOffset)  const ;

  /**
   *
   */
  virtual PixelType ComputeUpdate(const BoundaryNeighborhoodType
                                  &neighborhood, void * globalData,
                                    const FloatOffsetType& = m_ZeroOffset) const;

 /**
   * Computes the time step for an update given a global data structure.
   * The data used in the computation may take different forms depending on
   * the nature of the equations.  This global data cannot be kept in the
   * instance of the equation object itself since the equation object must
   * remain stateless for thread safety.  The global data is therefore managed
   * for each thread by the finite difference solver filters.
   */
  virtual TimeStepType ComputeGlobalTimeStep(void *GlobalData) const;

  /**
   * Returns a pointer to a global data structure that is passed to this
   * object from the solver at each calculation.  The idea is that the
   * solver holds the state of any global values needed to calculate the
   * time step, while the equation object performs the actual calculations.
   *
   * The global data should also be initialized in this method.
   */
  virtual void *GetGlobalDataPointer() const
    {
      GlobalDataStruct *ans = new GlobalDataStruct();
      ans->m_MaxAdvectionChange   = NumericTraits<ScalarValueType>::Zero;
      ans->m_MaxPropagationChange = NumericTraits<ScalarValueType>::Zero;
      return ans; 
    }

  /**
   * This method creates the appropriate member variable operators for the
   * level-set calculations.  The argument to this function is a the radius
   * necessary for performing the level-set calculations.
   */
  virtual void Initialize(const RadiusType &r);

  
  /**
   * When the finite difference solver filter has finished using a global
   * data pointer, it passes it to this method, which frees the memory.
   *
   * The solver cannot free the memory because it does not know the type
   * to which the pointer points.
   */
  virtual void ReleaseGlobalDataPointer(void *GlobalData) const
    {      delete (GlobalDataStruct *) GlobalData;    }

protected:
  /**
   * A global data type for this class of equations.  Used to store
   * values that are needed in calculating the time step.
   */
  struct GlobalDataStruct
  {
    ScalarValueType m_MaxAdvectionChange;
    ScalarValueType m_MaxPropagationChange;
  };
  
  LevelSet2DEquation() {}
  ~LevelSet2DEquation() {}
  LevelSet2DEquation(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const
  {
    os << indent << "LevelSet2DEquation";
    Superclass::PrintSelf(os, indent.GetNextIndent() );
  }

  /**
   * Constants used in the time step calculation.
   */
  static double m_WaveDT;
  static double m_DT;

private:
  /**
   * First order derivative operator
   */
  DerivativeOperator<ScalarValueType, 2>  dx_op;

  /**
   * Second order derivative operator
   */
  DerivativeOperator<ScalarValueType, 2>  dxx_op;

  /**
   * Slices for the 2D neighborhood.
   */
  std::slice  x_slice;
  std::slice  y_slice;

  /**
   * The offset of the center pixel in the neighborhood
   */
  std::size_t m_Center;

  /**
   * Stride length along the y-dimension
   */
  std::size_t m_yStride;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSet2DEquation.txx"
#endif

#endif

