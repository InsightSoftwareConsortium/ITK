/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeodesicActiveContourLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGeodesicActiveContourLevelSetImageFilter_h_
#define __itkGeodesicActiveContourLevelSetImageFilter_h_

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkGeodesicActiveContourLevelSetFunction.h"

namespace itk {

/** \class GeodesicActiveContourLevelSetImageFilter
 * \brief Segments structures in images based on a user supplied edge potential map.
 *
 * \par IMPORTANT
 * The SegmentationLevelSetImageFilter class and the
 * GeodesicActiveContourLevelSetFunction class contain additional information necessary
 * to gain full understanding of how to use this filter.
 *
 * \par OVERVIEW
 * This class is a level set method segmentation filter. An initial contour
 * is propagated outwards (or inwards) until it ''sticks'' to the shape boundaries.
 * This is done by using a level set speed function based on a user supplied
 * edge potential map.
 *
 * \par INPUTS
 * This filter requires two inputs.  The first input is a initial level set.
 * The initial level set is a real image which contains the initial contour/surface
 * as the zero level set. For example, a signed distance function from the initial
 * contour/surface is typically used. Unlike the simpler ShapeDetectionLevelSetImageFilter
 * the initial contour does not have to lie wholly within the shape to be segmented.
 * The intiial contour is allow to overlap the shape boundary. The extra advection term
 * in the update equation behaves like a doublet and attracts the contour to the boundary.
 * This approach for segmentation follows that of Caselles et al (1997).
 *
 * \par
 * The second input is the feature image.  For this filter, this is the edge
 * potential map. General characteristics of an edge potential map is that
 * it has values close to zero in regions near the edges and values close
 * to one inside the shape itself. Typically, the edge potential map is compute
 * from the image gradient, for example:
 *
 * \f[ g(I) = 1 / ( 1 + | (\nabla * G)(I)| ) \f]
 * \f[ g(I) = \exp^{-|(\nabla * G)(I)|} \f]
 * 
 * where \f$ I \f$ is image intensity and
 * \f$ (\nabla * G) \f$ is the derivative of Gaussian operator. 
 *
 * \par
 * See SegmentationLevelSetImageFilter and SparseFieldLevelSetImageFilter 
 * for more information on Inputs.
 *
 * \par PARAMETERS
 * The PropagationScaling parameter can be used to switch from propagation outwards
 * (POSITIVE scaling parameter) versus propagating inwards (NEGATIVE scaling 
 * parameter). 
 *
 * This implementation allows the user to set the weights between the propagation, advection
 * and curvature term using methods SetPropagationScaling(), SetAdvectionScaling(),
 * SetCurvatureScaling(). In general, the larger the CurvatureScaling, the smoother the
 * resulting contour. To follow the implementation in Caselles et al paper,
 * set the PropagationScaling to \f$ c \f$ (the inflation or ballon force) and
 * AdvectionScaling and CurvatureScaling both to 1.0.
 *
 * \par OUTPUTS
 * The filter outputs a single, scalar, real-valued image.
 * Negative values in the output image represent the inside of the segmented region
 * and positive values in the image represent the outside of the segmented region.  The
 * zero crossings of the image correspond to the position of the propagating
 * front.
 *
 * \par
 * See SparseFieldLevelSetImageFilter and
 * SegmentationLevelSetImageFilter for more information.
 *
 * \par REFERENCES
 * \par  
 *    "Geodesic Active Contours",
 *    V. Caselles, R. Kimmel and G. Sapiro.
 *    International Journal on Computer Vision,
 *    Vol 22, No. 1, pp 61-97, 1997
 *
 * \sa SegmentationLevelSetImageFilter
 * \sa GeodesicActiveContourLevelSetFunction
 * \sa SparseFieldLevelSetImageFilter 
 *
 * \ingroup LevelSetSegmentation
 */
template <class TInputImage,
          class TFeatureImage,
          class TOutputPixelType = float >
class ITK_EXPORT GeodesicActiveContourLevelSetImageFilter
  : public SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, 
                                            TOutputPixelType, Image<TOutputPixelType, 
                                            ::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:
  /** Standard class typedefs */
  typedef GeodesicActiveContourLevelSetImageFilter Self;
  typedef SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, 
                                           TOutputPixelType, Image<TOutputPixelType, 
                                           ::itk::GetImageDimension<TInputImage>::ImageDimension> > Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::FeatureImageType FeatureImageType;
  
  /** Type of the segmentation function */
  typedef GeodesicActiveContourLevelSetFunction<OutputImageType,
                                                FeatureImageType > GeodesicActiveContourFunctionType;
  typedef typename GeodesicActiveContourFunctionType::Pointer
  GeodesicActiveContourFunctionPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GeodesicActiveContourLevelSetImageFilter, SegmentationLevelSetImageFilter);

  /** Method for creation through the object factory */
  itkNewMacro(Self);
     
  /** Set the value of sigma used to compute the edge potential map derivatives */
  void SetDerivativeSigma( float value )
  { 
    if ( value != m_GeodesicActiveContourFunction->GetDerivativeSigma() )
      {
      m_GeodesicActiveContourFunction->SetDerivativeSigma( value );
      this->Modified();
      }
  }

  /** Get the value of sigma used to compute the edge potential map derivatives. */
  float GetDerivativeSigma() const
  { return m_GeodesicActiveContourFunction->GetDerivativeSigma(); }
    
protected:
  ~GeodesicActiveContourLevelSetImageFilter() {}
  GeodesicActiveContourLevelSetImageFilter();

  virtual void PrintSelf(std::ostream &os, Indent indent) const; 

  GeodesicActiveContourLevelSetImageFilter(const Self &); // purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Overridden from Superclass to handle the case when PropagationScaling is zero.*/
  void GenerateData();

private:
  GeodesicActiveContourFunctionPointer m_GeodesicActiveContourFunction;
};

} // end namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGeodesicActiveContourLevelSetImageFilter.txx"
#endif

#endif
