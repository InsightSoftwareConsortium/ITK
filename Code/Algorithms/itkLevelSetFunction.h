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

#include "itkFiniteDifferenceFunction.h"
#include "itkVector.h"
#include "itkNeighborhoodInnerProduct.h"

namespace itk {

/** \class LevelSetFunction
 *
 * \f$\phi_{t} = \alpha \stackrel{\rightharpoonup}{F}(\mathbf{x})\cdot\nabla\phi
 * + \beta G(\mathbf{x})\mid\nabla\phi\mid + \gamma Z(\mathbf{x})\kappa\f$
 *
 * references: Sethian Chap 6, Vispack documentation, insight documents
 * level set arch proposal
 * \ingroup FiniteDifferenceFunctions
 */
template <class TImageType>
class LevelSetFunction : public FiniteDifferenceFunction<TImageType>
{
public:
  /** Standard class typedefs. */
  typedef LevelSetFunction Self;
  typedef FiniteDifferenceFunction<TImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( LevelSetFunction,  FiniteDifferenceFunction );

  /** The image dimension. */
  enum { ImageDimension = Superclass::ImageDimension };

  /** Extract some parameters from the image type. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef double TimeStepType;
  typedef PixelType  ScalarValueType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType
                   BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;
  
  /** The vector type that will be used in the calculations. */
  typedef Vector<ScalarValueType, ImageDimension> VectorType;

  /** Advection field.  Default implementation returns a vector of zeros. */
  virtual VectorType AdvectionField(const NeighborhoodType &neighborhood,
                                    const FloatOffsetType &)  const
    { return m_ZeroVectorConstant; }
  virtual VectorType AdvectionField(const BoundaryNeighborhoodType
                                    &neighborhood, const FloatOffsetType &
                                    ) const
    { return m_ZeroVectorConstant; }
  
  /** Propagation speed.  Default implementation returns zero. */
  virtual ScalarValueType PropagationSpeed(
    const NeighborhoodType& neighborhood,
    const FloatOffsetType ) const
    { return NumericTraits<ScalarValueType>::Zero; }
  virtual ScalarValueType PropagationSpeed(
    const BoundaryNeighborhoodType
    &neighborhood, const FloatOffsetType &) const
    { return NumericTraits<ScalarValueType>::Zero; }

  /** Curvature speed.  Default implementation returns one. */
  virtual ScalarValueType CurvatureSpeed(const NeighborhoodType
                                         &neighborhood, const FloatOffsetType &
                                         ) const
    { return NumericTraits<ScalarValueType>::One; }

  virtual ScalarValueType CurvatureSpeed(const BoundaryNeighborhoodType
                                         &neighborhood, const FloatOffsetType &
                                         ) const
    { return NumericTraits<ScalarValueType>::One; }

  /** Alpha. */
  void SetAdvectionWeight(const ScalarValueType a)
    { m_AdvectionWeight = a; }
  ScalarValueType GetAdvectionWeight() const
    { return m_AdvectionWeight; }
  
  /** Beta. */
  void SetPropagationWeight(const ScalarValueType p)
    { m_PropagationWeight = p; }
  ScalarValueType GetPropagationWeight() const
    { return m_PropagationWeight; }
  
  /** Gamma. */
  void SetCurvatureWeight(const ScalarValueType c)
    { m_CurvatureWeight = c; }
  ScalarValueType GetCurvatureWeight() const
    { return m_CurvatureWeight; }
  
  /** Epsilon. */
  void SetEpsilonMagnitude(const ScalarValueType e)
    { m_EpsilonMagnitude = e; }
  ScalarValueType GetEpsilonMagnitude() const
    { return m_EpsilonMagnitude; }

  virtual void Initialize(const RadiusType &r) =0;

protected:
  LevelSetFunction()
    {
      m_EpsilonMagnitude = 1.0e-5;
      m_AdvectionWeight = m_PropagationWeight = m_CurvatureWeight =
        NumericTraits<ScalarValueType>::Zero;
    }
  ~LevelSetFunction() {}

  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Inner product function. */
  NeighborhoodInnerProduct<ImageType> m_InnerProduct;

  /** Boundary Inner product function. */
  SmartNeighborhoodInnerProduct<ImageType> m_SmartInnerProduct;

  /** This method's only purpose is to initialize the zero vector
   * constant. */
  static VectorType InitializeZeroVectorConstant();
  
  /** Zero vector constant. */
  static VectorType m_ZeroVectorConstant;

  /** Epsilon magnitude controls the lower limit for gradient magnitude. */
  ScalarValueType m_EpsilonMagnitude;
  
  /** Alpha. */
  ScalarValueType m_AdvectionWeight;

  /** Beta. */
  ScalarValueType m_PropagationWeight;

  /** Gamma. */
  ScalarValueType m_CurvatureWeight;

private:
  LevelSetFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetFunction.txx"
#endif

#endif




