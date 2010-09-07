/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCannySegmentationLevelSetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCannySegmentationLevelSetFunction_h
#define __itkCannySegmentationLevelSetFunction_h

#include "itkSegmentationLevelSetFunction.h"
#include "itkCastImageFilter.h"
#include "itkCannyEdgeDetectionImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"

namespace itk
{
/** \class CannySegmentationLevelSetFunction
 * \brief A refinement of the standard level-set function which computes a
 * speed term and advection term based on pseudo-Canny edges.  See
 * CannySegmentationLevelSetImageFilter for complete information.
 */
template< class TImageType, class TFeatureImageType = TImageType >
class ITK_EXPORT CannySegmentationLevelSetFunction:
  public SegmentationLevelSetFunction< TImageType, TFeatureImageType >
{
public:
  /** Standard class typedefs. */
  typedef CannySegmentationLevelSetFunction                             Self;
  typedef SegmentationLevelSetFunction< TImageType, TFeatureImageType > Superclass;
  typedef SmartPointer< Self >                                          Pointer;
  typedef SmartPointer< const Self >                                    ConstPointer;
  typedef TFeatureImageType                                             FeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(CannySegmentationLevelSetFunction, SegmentationLevelSetFunction);

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType         ImageType;
  typedef typename Superclass::ScalarValueType   ScalarValueType;
  typedef typename Superclass::VectorImageType   VectorImageType;
  typedef typename Superclass::FeatureScalarType FeatureScalarType;
  typedef typename Superclass::RadiusType        RadiusType;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** */
  void SetThreshold(ScalarValueType v)
  { m_Threshold = v; }
  ScalarValueType GetThreshold() const
  { return m_Threshold; }

  /** */
  void SetVariance(double v)
  { m_Variance = v; }
  double GetVariance() const
  { return m_Variance; }

  /** Compute the Speed Image. The Speed Image is the distance to the
      canny edges. */
  virtual void CalculateSpeedImage();

  /** Compute the advection image. The Advection Image is the gradeint
      image attenuated with the distance to the canny edges. */
  virtual void CalculateAdvectionImage();

  /** Compute the distance image. This is the distance to the canny
   * edges. */
  virtual void CalculateDistanceImage();

  virtual void Initialize(const RadiusType & r)
  {
    Superclass::Initialize(r);

    this->SetAdvectionWeight(-1.0 * NumericTraits< ScalarValueType >::One);
    this->SetPropagationWeight(-1.0 * NumericTraits< ScalarValueType >::One);
    this->SetCurvatureWeight(NumericTraits< ScalarValueType >::One);
  }

  ImageType * GetCannyImage()
  { return m_Canny->GetOutput(); }
protected:
  CannySegmentationLevelSetFunction()
  {
    m_Variance = 0.0;
    m_Threshold = NumericTraits< ScalarValueType >::Zero;
    m_Caster = CastImageFilter< FeatureImageType, ImageType >::New();
    m_Canny = CannyEdgeDetectionImageFilter< ImageType, ImageType >::New();
    m_Distance = DanielssonDistanceMapImageFilter< ImageType, ImageType >::New();
  }

  virtual ~CannySegmentationLevelSetFunction() {}

  CannySegmentationLevelSetFunction(const Self &); //purposely not implemented
  void operator=(const Self &);                    //purposely not implemented

private:
  ScalarValueType m_Variance;
  double          m_Threshold;

  typename CannyEdgeDetectionImageFilter< ImageType, ImageType >::Pointer m_Canny;

  typename DanielssonDistanceMapImageFilter< ImageType, ImageType >::Pointer m_Distance;

  typename CastImageFilter< FeatureImageType, ImageType >::Pointer m_Caster;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCannySegmentationLevelSetFunction.txx"
#endif

#endif
