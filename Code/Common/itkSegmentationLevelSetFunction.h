/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationLevelSetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSegmentationLevelSetFunction_h_
#define __itkSegmentationLevelSetFunction_h_

#include "itkLevelSetFunction.h"

namespace itk {

template <class TImageType, class TFeatureImageType = TImageType>
class ITK_EXPORT SegmentationLevelSetFunction
  : public LevelSetFunction<TImageType>
{
public:
  /** Standard class typedefs. */
  typedef SegmentationLevelSetFunction Self;
  typedef LevelSetFunction<TImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( SegmentationLevelSetFunction, LevelSetFunction );

  //  itkNewMacro(Self);
  
  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType ImageType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef TFeatureImageType FeatureImageType;
  typedef typename Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;
  typedef typename Superclass::ScalarValueType ScalarValueType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename FeatureImageType::PixelType FeatureScalarType;
  typedef typename ImageType::IndexType IndexType;

  
  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Set/Get the image which will be used to calculate the speed function. */
  virtual FeatureImageType *GetFeatureImage() const
  { return m_FeatureImage.GetPointer(); }
  virtual void SetFeatureImage(FeatureImageType *f)
  { m_FeatureImage = f; }
  
  /** Get the image used as the speed function in the level set equation */
  virtual ImageType *GetSpeedImage() const
  { return m_SpeedImage.GetPointer(); }

  /** This method creates the appropriate member variable operators for the
   * level-set calculations.  The argument to this function is a the radius
   * necessary for performing the level-set calculations. */
  virtual void Initialize(const RadiusType &r);

  virtual void CalculateSpeedImage() = 0;

  virtual void AllocateSpeedImage();
  
protected:
  /** The image whose features will be used to create a speed image */
  typename FeatureImageType::Pointer m_FeatureImage;
  typename ImageType::Pointer        m_SpeedImage;

 /** */
 virtual ScalarValueType PropagationSpeed(
                            const NeighborhoodType& neighborhood,
                            const FloatOffsetType
                          ) const
    {
      IndexType idx = neighborhood.GetIndex();
      return m_SpeedImage->GetPixel(idx);
    }
  
  /** */
  virtual ScalarValueType PropagationSpeed(const BoundaryNeighborhoodType
                               &neighborhood, const FloatOffsetType &
                                           ) const
  {
    IndexType idx = neighborhood.GetIndex();
    return m_SpeedImage->GetPixel(idx);
  }



  virtual ~SegmentationLevelSetFunction() {}
  SegmentationLevelSetFunction()
  { m_SpeedImage = ImageType::New(); }

  
};

} // end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSegmentationLevelSetFunction.txx"
#endif

#endif
