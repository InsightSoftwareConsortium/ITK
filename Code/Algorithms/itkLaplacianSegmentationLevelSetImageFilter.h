/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianSegmentationLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianSegmentationLevelSetImageFilter_h_
#define __itkLaplacianSegmentationLevelSetImageFilter_h_

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkLaplacianSegmentationLevelSetFunction.h"

namespace itk {

/**   \class LaplacianSegmentationLevelSetImageFilter
 *    \brief Segments structures in images based on a second derivative image features.
 *
 *   \par IMPORTANT
 *   The SegmentationLevelSetImageFilter class and the
 *   LaplacianSegmentationLevelSetFunction class contain additional information necessary
 *   to the full understanding of how to use this filter.
 *
 *    \par OVERVIEW
 *    This class is a level set method segmentation filter.  It constructs a
 *    speed function which is zero at image edges as detected by a Laplacian
 *    filter.  The evolving level set front will therefore tend to lock
 *    onto zero crossings in the image.  The level set front moves fastest near
 *    edges.
 *
 *   \par
 *   The Laplacian segmentation filter is intended primarily as a tool for
 *   refining existing segmentations.  The initial isosurface (as given in the
 *   seed input image) should ideally be very close to the segmentation
 *   boundary of interest.  The idea is that a rough segmentation can be
 *   refined by allowing the isosurface to deform slightly to achieve a better
 *   fit to the edge features of an image.  One example of such an application
 *   is to refine the output of a hand segmented image.
 *
 *   \par
 *   Because values in the Laplacian feature image will tend to be low except
 *   near edge features, this filter is not effective for segmenting large
 *   image regions from small seed surfaces.
 *
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
 *    segment.
 *
 *    Note that this filter does no preprocessing of the feature image before
 *    thresholding.  Because second derivative calculations are highly
 *    sensitive to noise, isotropic or anisotropic smoothing of the feature
 *    image can dramatically improve the results. 
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
 *   This filter has no parameters other than those described in
 *   SegmentationLevelSetImageFilter.
 *
 *   \sa SegmentationLevelSetImageFilter
 *   \sa LaplacianSegmentationLevelSetFunction,
 *   \sa SparseFieldLevelSetImageFilter */
template <class TInputImage,
          class TFeatureImage,
          class TOutputPixelType = float>
class ITK_EXPORT LaplacianSegmentationLevelSetImageFilter
  : public SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, Image<TOutputPixelType, ::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:
   /** Standard class typedefs */
  typedef LaplacianSegmentationLevelSetImageFilter Self;
  typedef SegmentationLevelSetImageFilter<TInputImage, TFeatureImage,
                                          TOutputPixelType> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::FeatureImageType FeatureImageType;

  
  /** Type of the segmentation function */
  typedef LaplacianSegmentationLevelSetFunction<OutputImageType,
                                                FeatureImageType> LaplacianFunctionType;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(LaplacianSegmenationLevelSetImageFilter, SegmentationLevelSetImageFilter);

  /** Method for creation through the object factory */
  itkNewMacro(Self);
  
protected:
  ~LaplacianSegmentationLevelSetImageFilter() {}
  LaplacianSegmentationLevelSetImageFilter();

  virtual void PrintSelf(std::ostream &os, Indent indent) const; 

  
private:
  typename LaplacianFunctionType::Pointer m_LaplacianFunction;  
};

} // end namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLaplacianSegmentationLevelSetImageFilter.txx"
#endif

#endif
