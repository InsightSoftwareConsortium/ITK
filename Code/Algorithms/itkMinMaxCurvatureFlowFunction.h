 /*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinMaxCurvatureFlowFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMinMaxCurvatureFlowFunction_h_
#define __itkMinMaxCurvatureFlowFunction_h_

#include "itkCurvatureFlowFunction.h"
#include "itkMacro.h"
#include "itkNeighborhoodOperator.h"

namespace itk {

/** \class MinMaxCurvatureFlowFunction
 *  
 * This class encapsulate the finite difference equation which drives a
 * min/max curvature flow denoising algorithm.
 *
 * This class uses a zero flux Neumann boundary condition when computing
 * derivatives near the data boundary.
 *
 * This class operates as part of the finite difference solver hierarchy.
 *
 * \sa MinMaxCurvatureFlowImageFilter
 * \sa ZeroFluxNeumannBoundaryCondition 
 * \ingroup FiniteDifferenceFunctions
*/
template <class TImage>
class ITK_EXPORT MinMaxCurvatureFlowFunction :
    public CurvatureFlowFunction<TImage>
{
public:
  /**  Standard class typedefs. */
  typedef MinMaxCurvatureFlowFunction Self;
  typedef CurvatureFlowFunction<TImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( MinMaxCurvatureFlowFunction,
                CurvatureFlowFunction );
  
  /** Inherit some parameters from the superclass type. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;

  /** Extract superclass dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Typedef support for the stencil radius. */
  typedef typename RadiusType::SizeValueType RadiusValueType;

  /** Set/Get the stencil radius. */
  void SetStencilRadius( const RadiusValueType radius );
  const RadiusValueType &GetRadiusValueType() const
    { return m_StencilRadius; }

  /** This method computes the solution update for each pixel that does not
   * lie on a the data set boundary. */
  virtual PixelType ComputeUpdate(const NeighborhoodType &neighborhood,
                                  void * globalData,
                                  const FloatOffsetType& offset = FloatOffsetType(0.0)
                                  ) const;

protected:
  MinMaxCurvatureFlowFunction();
  ~MinMaxCurvatureFlowFunction() {}

  typedef Neighborhood<PixelType,itkGetStaticConstMacro(ImageDimension)> StencilOperatorType;
  StencilOperatorType  m_StencilOperator;

  /** Initialize the stencil opearator to be an N-Dimensional sphere
   * of radius m_StencilRadius. */
  void InitializeStencilOperator();


private:
  MinMaxCurvatureFlowFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  RadiusValueType  m_StencilRadius;

  // To control overloaded versions of ComputeThreshold
  struct DispatchBase {};
  template<signed int VDimension>
  struct Dispatch : DispatchBase {};
  
  /** This method computes the threshold by averaging the intensity
   *  in direction perpendicular to the image gradient. */
  virtual PixelType ComputeThreshold( const Dispatch<2> &,
    const NeighborhoodType & neighborhood ) const;
  virtual PixelType ComputeThreshold( const Dispatch<3> &,
    const NeighborhoodType & neighborhood ) const;
  virtual PixelType ComputeThreshold( const DispatchBase &,
    const NeighborhoodType & neighborhood ) const;
};

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinMaxCurvatureFlowFunction.txx"
#endif

#endif
