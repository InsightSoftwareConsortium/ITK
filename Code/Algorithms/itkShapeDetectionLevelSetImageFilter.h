/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeDetectionLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapeDetectionLevelSetImageFilter_h_
#define __itkShapeDetectionLevelSetImageFilter_h_

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkShapeDetectionLevelSetFunction.h"

namespace itk {

/** \class ShapeDetectionLevelSetImageFilter
 *  \brief Segments structures in images based on user supplied edge potential map.
 *
 *   \par IMPORTANT
 *   The SegmentationLevelSetImageFilter class and the
 *   ShapeDetectionLevelSetFunction class contain additional information necessary
 *   to the full understanding of how to use this filter.
 *
 *    \par OVERVIEW
 *    This class is a level set method segmentation filter. An initial contour
 *    is propagated outwards (or inwards) until it sticks to the shape boundaries.
 *    This is done by using a level set speed function based on a user supplied
 *    edge potential map. This approach for segmentation follows that of
 *    Malladi et al (1995).
 *
 *    \par INPUTS
 *    This filter requires two inputs.  The first input is a initial level set.
 *    The initial level set is a real image which contains the initial contour/surface
 *    as the zero level set. For example, a signed distance function from the initial
 *    contour/surface is typically used. Note that for the algorithm the initial contour
 *    has to be wholly within (or wholly outside) the structure to be segmented.
 *
 *    \par
 *    The second input is the feature image.  For this filter, this is the edge
 *    potential map. General characteristics of an edge potential map is that
 *    it has values close to zero in regions near the edges and values close
 *    to one inside the shape itself. Typically, the edge potential map is compute
 *    from the image gradient, for example:
 *
 *    \f[ g(I) = 1 / ( 1 + | (\nabla * G)(I)| ) \f]
 *    \f[ g(I) = \exp^{-|(\nabla * G)(I)|} \f]
 * 
 *    where \f$ I \f$ is image intensity and
 *    \f$ (\nabla * G) \f$ is the derivative of Gaussian operator. 
 *
 *    \par
 *    See SegmentationLevelSetImageFilter and SparseFieldLevelSetImageFilter 
 *    for more information on Inputs.
 *
 *    \par PARAMETERS
 *    The method SetUseNegatiiveFeatures() can be used to switch from propagating outwards (true)
 *    versus propagting inwards (false). 
 *
 *    The smoothness of the resulting contour/surface can be adjusting using a combination
 *    of SetPropagationScaling() and SetCurvatureScaling(). The larger the CurvatureScaling, the
 *    smoother the resulting contour. To follow the implementation in Malladi's paper,
 *    set the PropagtionScaling to 1.0 and CurvatureScaling to \f$ \epsilon \f$.
 *
 *    Note that there is no advection term for this filter. Setting the 
 *    advection scaling will have no effect.
 *
 *    \par OUTPUTS
 *    The filter outputs a single, scalar, real-valued image.
 *    Negative values in the output image are inside the segmentated region
 *    and positive values in the image are outside of the inside region.  The
 *    zero crossings of the image correspond to the position of the level set
 *    front.
 *
 *   \par
 *   See SparseFieldLevelSetImageFilter and
 *   SegmentationLevelSetImageFilter for more information.
 *
 *    \par REFERENCES
 *    \par  
 *    "Shape Modeling with Front Propagation: A Level Set Approach",
 *    R. Malladi, J. A. Sethian and B. C. Vermuri.
 *    IEEE Trans. on Pattern Analysis and Machine Intelligence,
 *    Vol 17, No. 2, pp 158-174, February 1995
 *
 *
 *   \sa SegmentationLevelSetImageFilter
 *   \sa ShapeDetectionLevelSetFunction
 *   \sa SparseFieldLevelSetImageFilter 
 *
 *   \ingroup LevelSetSegmentation
 */
template <class TInputImage,
          class TFeatureImage,
          class TOutputPixelType = float >
class ITK_EXPORT ShapeDetectionLevelSetImageFilter
  : public SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, Image<TOutputPixelType, ::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:
  /** Standard class typedefs */
  typedef ShapeDetectionLevelSetImageFilter Self;
  typedef  SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, Image<TOutputPixelType, ::itk::GetImageDimension<TInputImage>::ImageDimension> > Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::FeatureImageType FeatureImageType;
  
  /** Type of the segmentation function */
  typedef ShapeDetectionLevelSetFunction<OutputImageType,
                                         FeatureImageType> ShapeDetectionFunctionType;
  typedef typename ShapeDetectionFunctionType::Pointer ShapeDetectionFunctionPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ShapeDetectionLevelSetImageFilter, SegmentationLevelSetImageFilter);

  /** Method for creation through the object factory */
  itkNewMacro(Self);
     
protected:
  ~ShapeDetectionLevelSetImageFilter() {}
  ShapeDetectionLevelSetImageFilter();

  virtual void PrintSelf(std::ostream &os, Indent indent) const; 

  ShapeDetectionLevelSetImageFilter(const Self &); // purposely not implemented
  void operator=(const Self&); //purposely not implemented
private:
  ShapeDetectionFunctionPointer m_ShapeDetectionFunction;
};

} // end namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapeDetectionLevelSetImageFilter.txx"
#endif

#endif
