/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeodesicActiveContourShapePriorLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGeodesicActiveContourShapePriorLevelSetImageFilter_h_
#define __itkGeodesicActiveContourShapePriorLevelSetImageFilter_h_

#include "itkShapePriorSegmentationLevelSetImageFilter.h"
#include "itkGeodesicActiveContourShapePriorLevelSetFunction.h"
#include "itkShapePriorMAPCostFunction.h"

namespace itk {

/** \class GeodesicActiveContourShapePriorLevelSetImageFilter
 * \brief Segments structures in an image based on a user supplied edge potential map
 * and user supplied shape model.
 *
 * \par IMPORTANT
 * The SegmentationLevelSetImageFilter class, ShapePriorSegmentationLevelSetImageFilter 
 * class and the
 * GeodesicActiveContourShapePrior0LevelSetFunction class contain additional 
 * information necessary to gain full understanding of how to use this filter.
 *
 * \par OVERVIEW
 * This class is a level set method segmentation filter. An initial contour
 * is propagated outwards (or inwards) until it ''sticks'' to the shape boundaries.
 * This is done by using a level set speed function based on a user supplied
 * edge potential map and a user supplied shape model.
 *
 * \par INPUTS
 * This filter requires two inputs.  The first input is a initial level set.
 * The initial level set is a real image which contains the initial contour/surface
 * as the zero level set. For example, a signed distance function from the initial
 * contour/surface is typically used. Unlike the simpler ShapeDetectionLevelSetImageFilter
 * the initial contour does not have to lie wholly within the shape to be segmented.
 * The intiial contour is allow to overlap the shape boundary. The advection term
 * in the update equation behaves like a doublet and attracts the contour to the boundary.
 * The shape prior term adds robustness by incorporating aprior information about
 * the shape to be segmented.
 * This approach for segmentation follows that of Leventon et al (2000).
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
 * curvature and shape prior term using methods SetPropagationScaling(), SetAdvectionScaling(),
 * SetCurvatureScaling() and SetShapePriorScaling. In general, the larger the CurvatureScaling, 
 * the smoother the
 * resulting contour. To follow the implementation in Leventon et al paper,
 * set the PropagationScaling to \f$ \lambda_1 \times c \f$,
 * the AdvectionScaling and CurvatureScaling both to \f$ \lambda_1 \f$ and
 * the ShapePriorScaling to \f$ \lambda_2 \f$.
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
 * Leventon, M.E. et al. "Statistical Shape Influence in Geodesic Active Contours", CVPR 2000. 
 *
 * \sa SegmentationLevelSetImageFilter
 * \sa ShapePriorSegmentationLevelSetImageFilter
 * \sa GeodesicActiveContourShapePriorLevelSetFunction
 * \sa SparseFieldLevelSetImageFilter 
 * \sa ShapeSignedDistanceFunction
 *
 * \ingroup LevelSetSegmentation
 */
template <class TInputImage,
          class TFeatureImage,
          class TOutputPixelType = float >
class ITK_EXPORT GeodesicActiveContourShapePriorLevelSetImageFilter
  : public ShapePriorSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, 
                                            TOutputPixelType >
{
public:
  /** Standard class typedefs */
  typedef GeodesicActiveContourShapePriorLevelSetImageFilter Self;
  typedef ShapePriorSegmentationLevelSetImageFilter< TInputImage, TFeatureImage, 
                                           TOutputPixelType > Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::FeatureImageType FeatureImageType;
  typedef typename Superclass::OutputPixelType OutputPixelType;
  
  /** Type of the segmentation function */
  typedef GeodesicActiveContourShapePriorLevelSetFunction< OutputImageType,
                                                           FeatureImageType > 
                                                             GeodesicActiveContourFunctionType;
  typedef typename GeodesicActiveContourFunctionType::Pointer
  GeodesicActiveContourFunctionPointer;
 
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GeodesicActiveContourShapePriorLevelSetImageFilter, 
    ShapePriorSegmentationLevelSetImageFilter);

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
  ~GeodesicActiveContourShapePriorLevelSetImageFilter() {}
  GeodesicActiveContourShapePriorLevelSetImageFilter();

  virtual void PrintSelf(std::ostream &os, Indent indent) const; 

  GeodesicActiveContourShapePriorLevelSetImageFilter(const Self &); // purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Overridden from Superclass to handle the case when PropagationScaling is zero
   * and CurvatureScaling is non-zero.*/
  void GenerateData();

private:
  GeodesicActiveContourFunctionPointer m_GeodesicActiveContourFunction;

};

} // end namespace itk



#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGeodesicActiveContourShapePriorLevelSetImageFilter.txx"
#endif

#endif
