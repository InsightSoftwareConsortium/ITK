/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianSegmentationLevelSetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianSegmentationLevelSetFunction_h
#define __itkLaplacianSegmentationLevelSetFunction_h

#include "itkSegmentationLevelSetFunction.h"

namespace itk
{
/** \class LaplacianSegmentationLevelSetFunction
 *
 * \brief This function is used in LaplacianSegmentationImageFilter to
 * segment structures in an image based Laplacian edges.
 *
 * Assumes a strictly POSITIVE feature image
 */
template< class TImageType, class TFeatureImageType = TImageType >
class ITK_EXPORT LaplacianSegmentationLevelSetFunction:
  public SegmentationLevelSetFunction< TImageType, TFeatureImageType >
{
public:
  /** Standard class typedefs. */
  typedef LaplacianSegmentationLevelSetFunction Self;
  typedef SegmentationLevelSetFunction< TImageType, TFeatureImageType >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef TFeatureImageType          FeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(LaplacianSegmentationLevelSetFunction, SegmentationLevelSetFunction);

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType         ImageType;
  typedef typename Superclass::ScalarValueType   ScalarValueType;
  typedef typename Superclass::FeatureScalarType FeatureScalarType;
  typedef typename Superclass::RadiusType        RadiusType;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  virtual void CalculateSpeedImage();

  virtual void Initialize(const RadiusType & r)
  {
    Superclass::Initialize(r);

    this->SetAdvectionWeight(NumericTraits< ScalarValueType >::Zero);
    this->SetPropagationWeight(-1.0 * NumericTraits< ScalarValueType >::One);
    this->SetCurvatureWeight(NumericTraits< ScalarValueType >::One);
  }

  /**
   * The Laplacian level set does not use an advection term. We clamp
   * the value to ZERO here because a superclass may try to set it
   * otherwise. in fact, SegmentationLevelSetImageFilter tries to set
   * it when SetFeatureScaling is called.
   */
  void SetAdvectionWeight(const ScalarValueType value)
  {
    if ( value == NumericTraits< ScalarValueType >::Zero )
      {
      Superclass::SetAdvectionWeight(value);
      }
  }

protected:

  LaplacianSegmentationLevelSetFunction()
  {
    this->SetAdvectionWeight(0.0);
    this->SetPropagationWeight(1.0);
    this->SetCurvatureWeight(1.0);
  }

  virtual ~LaplacianSegmentationLevelSetFunction() {}

  LaplacianSegmentationLevelSetFunction(const Self &); //purposely not
                                                       // implemented
  void operator=(const Self &);                        //purposely not
                                                       // implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLaplacianSegmentationLevelSetFunction.txx"
#endif

#endif
