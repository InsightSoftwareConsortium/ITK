/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetEquation.h
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
#ifndef __itkLevelSetEquation_h_
#define __itkLevelSetEquation_h_

#include "itkFiniteDifferenceEquation.h"
#include "itkVector.h"
#include "itkNeighborhoodInnerProduct.h"

namespace itk {

/**
 *
 * \class LevelSetEquation
 *
 * \f$\phi_{t} = \alpha \stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi
 * + \beta G(\mathbf{x})\mid\nabla\phi\mid + \gamma Z(\mathbf{x})\kappa\f$
 *
 * references: Sethian Chap 6, Vispack documentation, insight documents
 * level set arch proposal
 */
template <class TImageType>
class LevelSetEquation : public FiniteDifferenceEquation<TImageType>
{
public:
  /**
   * Standard itk Self & Superclass typedefs
   */
  typedef LevelSetEquation Self;
  typedef FiniteDifferenceEquation<TImageType> Superclass;

  /**
   * Extract some parameters from the image type
   */
  typedef typename Superclass::ImageType ImageType;
  enum { ImageDimension = Superclass::ImageDimension };
  typedef typename Superclass::PixelType        PixelType;
  typedef double TimeStepType;
  typedef PixelType  ScalarValueType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType
  BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  
  /**
   * The vector type that will be used in the calculations.
   */
  typedef Vector<ScalarValueType, ImageDimension> VectorType;

  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( LevelSetEquation,  FiniteDifferenceEquation );

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Advection field.  Default implementation returns a vector of zeros.
   */
  virtual VectorType AdvectionField(const NeighborhoodType &neighborhood,
                                    const FloatOffsetType &)  const
    { return m_ZeroVectorConstant; }

  virtual VectorType AdvectionField(const BoundaryNeighborhoodType
                                    &neighborhood, const FloatOffsetType &
                                    ) const
    { return m_ZeroVectorConstant; }
  
  /**
   * Propagation speed.  Default implementation returns zero.
   */
  virtual ScalarValueType PropagationSpeed(
                          const NeighborhoodType& neighborhood,
                            const FloatOffsetType
                          ) const
    { return NumericTraits<ScalarValueType>::Zero; }

  virtual ScalarValueType PropagationSpeed(const BoundaryNeighborhoodType
                               &neighborhood, const FloatOffsetType &
                                           ) const
    { return NumericTraits<ScalarValueType>::Zero; }

  /**
   * Curvature speed.  Default implementation returns one.
   */
  virtual ScalarValueType CurvatureSpeed(const NeighborhoodType
                                         &neighborhood, const FloatOffsetType &
                                         ) const
    { return NumericTraits<ScalarValueType>::One; }

  virtual ScalarValueType CurvatureSpeed(const BoundaryNeighborhoodType
                                         &neighborhood, const FloatOffsetType &
                                         ) const
    { return NumericTraits<ScalarValueType>::One; }

  /**
   * Alpha
   */
  void SetAdvectionWeight(const ScalarValueType a)
    { m_AdvectionWeight = a; }
  ScalarValueType GetAdvectionWeight() const
    { return m_AdvectionWeight; }
  
  /**
   * Beta
   */
  void SetPropagationWeight(const ScalarValueType p)
    { m_PropagationWeight = p; }
  ScalarValueType GetPropagationWeight() const
    { return m_PropagationWeight; }
  
  /**
   * Gamma
   */
  void SetCurvatureWeight(const ScalarValueType c)
    { m_CurvatureWeight = c; }
  ScalarValueType GetCurvatureWeight() const
    { return m_CurvatureWeight; }
  
  /**
   *
   */
  void SetEpsilonMagnitude(const ScalarValueType e)
    { m_EpsilonMagnitude = e; }
  ScalarValueType GetEpsilonMagnitude() const
    { return m_EpsilonMagnitude; }
      
protected:
  LevelSetEquation()
    {
      m_EpsilonMagnitude = 1.0e-5;
      m_AdvectionWeight = m_PropagationWeight = m_CurvatureWeight =
        NumericTraits<ScalarValueType>::Zero;
    }
  ~LevelSetEquation() {}
  LevelSetEquation(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const
  {
    os << indent << "LevelSetEquation";
    Superclass::PrintSelf(os, indent.GetNextIndent() );
  }
  /**
   * Inner product function.
   */
  NeighborhoodInnerProduct<ImageType> m_InnerProduct;

  /**
   * Boundary Inner product function.
   */
  SmartNeighborhoodInnerProduct<ImageType> m_SmartInnerProduct;

  /**
   * This method's only purpose is to initialize the zero vector
   * constant.
   */
  static VectorType InitializeZeroVectorConstant();
  
  /**
   * Zero vector constant.
   */
  static VectorType m_ZeroVectorConstant;

  /**
   * Epsilon magnitude controls the lower limit for gradient magnitude   
   */
  ScalarValueType m_EpsilonMagnitude;
  
  /**
   * alpha
   */
  ScalarValueType m_AdvectionWeight;

  /**
   * beta
   */
  ScalarValueType m_PropagationWeight;

  /**
   * gamma
   */
  ScalarValueType m_CurvatureWeight;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetEquation.txx"
#endif

#endif




