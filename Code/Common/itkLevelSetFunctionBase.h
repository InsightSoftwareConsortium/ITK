/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevelSetFunctionBase_h_
#define __itkLevelSetFunctionBase_h_

#include "itkFiniteDifferenceFunction.h"
#include "itkVector.h"

namespace itk {

/** \class LevelSetFunctionBase
  *
 * This class is a base class for the LeveSetFunction object.  See
 * LevelSetFunction object for complete information.
 *
 * \ingroup FiniteDifferenceFunctions
 * \ingroup Functions
 */
template <class TImageType>
class LevelSetFunctionBase : public FiniteDifferenceFunction<TImageType>
{
public:
  /** Standard class typedefs. */
  typedef LevelSetFunctionBase Self;
  typedef FiniteDifferenceFunction<TImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( LevelSetFunctionBase,  FiniteDifferenceFunction );

  /** The image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Extract some parameters from the image type. */
  typedef double TimeStepType;
  typedef typename Superclass::ImageType  ImageType;
  typedef typename Superclass::PixelType  PixelType;
  typedef                      PixelType  ScalarValueType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType
                                                BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;
  
  /** The vector type that will be used in the calculations. */
  typedef
    Vector<ScalarValueType, itkGetStaticConstMacro(ImageDimension)> VectorType;

  /** Advection field.  Default implementation returns a vector of zeros. */
  virtual VectorType AdvectionField(const NeighborhoodType &neighborhood,
                                    const FloatOffsetType &)  const
    { return m_ZeroVectorConstant; }
  virtual VectorType AdvectionField(const BoundaryNeighborhoodType
                                    &neighborhood, const FloatOffsetType &
                                    ) const
    { return m_ZeroVectorConstant; }

  /** Propagation speed.  This term controls surface expansion/contraction.
   *  Default implementation returns zero. */ 
  virtual ScalarValueType PropagationSpeed(
    const NeighborhoodType& neighborhood,
    const FloatOffsetType ) const
    { return NumericTraits<ScalarValueType>::Zero; }
  virtual ScalarValueType PropagationSpeed(
    const BoundaryNeighborhoodType
    &neighborhood, const FloatOffsetType &) const
    { return NumericTraits<ScalarValueType>::Zero; }

  /** Curvature speed.  Can be used to spatially modify the effects of
      curvature . The default implementation returns one. */
  virtual ScalarValueType CurvatureSpeed(const NeighborhoodType
                                         &neighborhood, const FloatOffsetType &
                                         ) const
    { return NumericTraits<ScalarValueType>::One; }

  virtual ScalarValueType CurvatureSpeed(const BoundaryNeighborhoodType
                                         &neighborhood, const FloatOffsetType &
                                         ) const
    { return NumericTraits<ScalarValueType>::One; }

  /** Alpha.  Scales all advection term values.*/ 
  void SetAdvectionWeight(const ScalarValueType a)
    { m_AdvectionWeight = a; }
  ScalarValueType GetAdvectionWeight() const
    { return m_AdvectionWeight; }
  
  /** Beta.  Scales all propagation term values. */
  void SetPropagationWeight(const ScalarValueType p)
    { m_PropagationWeight = p; }
  ScalarValueType GetPropagationWeight() const
    { return m_PropagationWeight; }
  
  /** Gamma. Scales all curvature weight values */
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
  LevelSetFunctionBase()
    {
      m_EpsilonMagnitude = 1.0e-5;
      m_AdvectionWeight = m_PropagationWeight = m_CurvatureWeight =
        NumericTraits<ScalarValueType>::Zero;
    }
  ~LevelSetFunctionBase() {}

  void PrintSelf(std::ostream& os, Indent indent) const;

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
  LevelSetFunctionBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetFunctionBase.txx"
#endif

#endif




