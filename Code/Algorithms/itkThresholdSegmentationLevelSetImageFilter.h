/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdSegmentationLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkThresholdSegmentationLevelSetImageFilter_h_
#define __itkThresholdSegmentationLevelSetImageFilter_h_

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkThresholdSegmentationLevelSetFunction.h"

namespace itk {

/** \class ThresholdSegmentationLevelSetImageFilter
 *    \brief Segments structures in images based on intensity values.
 *
 *   \par IMPORTANT
 *   The SegmentationLevelSetImageFilter class and the
 *   ThresholdSegmentationLevelSetFunction class contain additional information necessary
 *   to the full understanding of how to use this filter.
 *
 *    \par OVERVIEW
 *    This class is a level set method segmentation filter.  It constructs a
 *    speed function which is close to zero at the upper and lower bounds of an
 *    intensity window, effectively locking the propagating front onto those
 *    edges.  Elsewhere, the front will propagate quickly.
 *
 *    \par INPUTS
 *    This filter requires two inputs.  The first input is a seed
 *    image.  This seed image must contain an isosurface that you want to use as the
 *    seed for your segmentation.  It can be a binary, graylevel, or floating
 *    point image.  The only requirement is that it contain a closed isosurface
 *    that you will identify as the seed by setting the IsosurfaceValue parameter
 *    of the filter.  For a binary image you will want to set your isosurface
 *    value halfway between your on and off values (i.e. for 0's and 1's, use an
 *    isosurface value of 0.5).
 *
 *    \par
 *    The second input is the feature image.  This is the image from which the
 *    speed function will be calculated.  For most applications, this is the
 *    image that you want to segment. The desired isosurface in your seed image
 *    should lie within the region of your feature image that you are trying to
 *    segment.Note that this filter does no preprocessing of the feature image
 *    before thresholding.
 *
 *    \par
 *    See SegmentationLevelSetImageFilter for more information on Inputs.
 *
 *    \par OUTPUTS
 *    The filter outputs a single, scalar, real-valued image.
 *    Positive *values in the output image are inside the segmentated region
 *    and negative *values in the image are outside of the inside region.  The
 *    zero crossings of *the image correspond to the position of the level set
 *    front.
 *
 *   \par
 *   See SparseFieldLevelSetImageFilter and
 *   SegmentationLevelSetImageFilter for more information.
 *
 *   \par PARAMETERS
 *   In addition to parameters described in SegmentationLevelSetImageFilter,
 *   this filter adds the UpperThreshold and LowerThreshold.  See
 *   ThresholdSegmentationLevelSetFunction for a description of how these values
 *   affect the segmentation.
 *
 *   \sa SegmentationLevelSetImageFilter
 *   \sa ThresholdSegmentationLevelSetFunction,
 *   \sa SparseFieldLevelSetImageFilter */
template <class TInputImage,
          class TFeatureImage,
          class TOutputPixelType = float >
class ITK_EXPORT ThresholdSegmentationLevelSetImageFilter
  : public SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, Image<TOutputPixelType, ::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:
   /** Standard class typedefs */
  typedef ThresholdSegmentationLevelSetImageFilter Self;
  typedef  SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, Image<TOutputPixelType, ::itk::GetImageDimension<TInputImage>::ImageDimension> > Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::FeatureImageType FeatureImageType;
  
  /** Type of the segmentation function */
  typedef ThresholdSegmentationLevelSetFunction<OutputImageType,
                                                FeatureImageType> ThresholdFunctionType;
  typedef typename ThresholdFunctionType::Pointer ThresholdFunctionPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ThresholdSegmentationLevelSetImageFilter, SegmentationLevelSetImageFilter);

  /** Method for creation through the object factory */
  itkNewMacro(Self);
  
  /** Get/Set the threshold values that will be used to calculate the speed function. */
  void SetUpperThreshold(ValueType v);
  void SetLowerThreshold(ValueType v);
  ValueType GetUpperThreshold() const;
  ValueType GetLowerThreshold() const;
    
protected:
  ~ThresholdSegmentationLevelSetImageFilter() {}
  ThresholdSegmentationLevelSetImageFilter();

  virtual void PrintSelf(std::ostream &os, Indent indent) const; 

  ThresholdSegmentationLevelSetImageFilter(const Self &); // purposely not impl.
   void operator=(const Self&); //purposely not implemented
private:
  ThresholdFunctionPointer m_ThresholdFunction;
};

} // end namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdSegmentationLevelSetImageFilter.txx"
#endif

#endif
