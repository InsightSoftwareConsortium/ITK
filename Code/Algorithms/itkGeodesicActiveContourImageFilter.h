/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeodesicActiveContourImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGeodesicActiveContourImageFilter_h
#define _itkGeodesicActiveContourImageFilter_h

#include "itkShapeDetectionLevelSetFilter.h"

namespace itk
{
/** \class GeodesicActiveContourImageFilter
 * \brief Edge based shape detection using geodesic active contours.
 *
 * GeodesicActiveContourImageFilter is a level set approach for boundary
 * detection.  An initial contour is evolved to minimise a weighted curve
 * length functional, where the weight depends on the edge potential
 * function.
 *
 * The advantage of this approach over ShapeDetectionLevelSetFilter is that
 * the evolution equation has an addition term which acts like a doublet and
 * attracts the contour to the shape boundaries. This improves boundary
 * detection in cases where edge features are weak.
 *
 * This class requires three inputs: an initial level set,
 * a edge potential map and the derivatives of the edge potential.
 *
 * The initial level set is a floating point image which contains the
 * initial contour as the zero level set. For example, a signed distance
 * function from the initial front is typically used.
 *
 * The edge potential image has values close to zero in regions
 * of high image gradient and values close to one in regions with
 * relatively constant intensity.
 *
 * This class is templated on the image type which represent the 
 * level set, the type of the edge potential image and the type oft
 * the edge potential derivatives.
 *
 * This class supports narrowbanding, where at each iteration only
 * a narrow band surrounding the propagating front is updated.
 *
 * Implementation of this class is based on:
 * "Geodesic Active Contours",
 * V. Caselles, R. Kimmel and G. Sapiro.
 * International Journal on Computer Vision,
 * Vol 22, No. 1, pp 61-97, 1997
 *
 * \sa ShapeDetection
 *
 * Possible improvements:
 * - In Caselles et al (1997) it is stated that extension of the
 * velocities at each iteration is not required. However, this causes
 * the level sets to bunch with near edges causing possible instabilities.
 * Future implementation should look at including velocity extension
 * as suggested in
 * "Area and Length Minimizing Flows for Shape Segmentation"
 * K. Siddiqi, Y. Lauziere, A. Tannenbaum and S. Zucker.
 * IEEE Transaction on Image Processing
 * Vol 7, No. 3 March 1998
 *
 * - In the narrowband version, allow the user to specify the
 * number of iterations before reinitialization.
 *
 * - Add support to detect convergence.
 *
 * Note: this filter will eventually be re-implemented as part of
 * the Finite Difference Solver framework.
 *
 * \ingroup LevelSetSegmentation 
 */
template <
class TLevelSet, 
class TEdgeImage, 
class TDerivImage
>
class ITK_EXPORT GeodesicActiveContourImageFilter :
  public ShapeDetectionLevelSetFilter<TLevelSet,TEdgeImage>
{
public:
  /** Standard class typedefs. */
  typedef GeodesicActiveContourImageFilter Self;
  typedef ShapeDetectionLevelSetFilter<TLevelSet,TEdgeImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GeodesicActiveContourImageFilter, ShapeDetectionLevelSetFilter);

  /** Typedef support for level set related types. */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /** Typedef support for the EdgeImageType. */
  typedef TEdgeImage EdgeImageType;

  /** EdgeImagePointer typedef support. */
  typedef typename EdgeImageType::Pointer EdgeImagePointer;

  /** DerivImage typedef support. */
  typedef TDerivImage DerivImageType;

  /** DerivImagePointer typedef support. */
  typedef typename DerivImageType::Pointer DerivImagePointer;

  /** Determine the image dimension. */
  itkStaticConstMacro(SetDimension, unsigned int,
                      TLevelSet::ImageDimension);

  /** Set the inflation strength. The parameter control the 
   * the strength of the optional ballon force. If PropagateOutward
   * is set to false, this controls the strength of the optional
   * contraction force. Typically, the value is application dependent 
   * (e.g. noise level, shape complexity, intensity range). 
   * Default value 0.0. */
  itkSetClampMacro( InflationStrength, double, 0.0,
    NumericTraits<double>::max() );

  /** Get the inflation strength. */
  itkGetMacro( InflationStrength, double );

  /** Set the input derivative images. */
  void SetDerivativeImage( const TDerivImage *ptr );

  /** Get the input derivative images. */
  DerivImageType * GetDerivativeImage();

protected:
  GeodesicActiveContourImageFilter();
  ~GeodesicActiveContourImageFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateDataFull();
  virtual void GenerateDataNarrowBand();
  virtual void GenerateInputRequestedRegion();

private:
  GeodesicActiveContourImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  typedef ReinitializeLevelSetImageFilter<TLevelSet> ExtenderType;
  typedef typename TEdgeImage::PixelType EdgePixelType;
  typedef typename TDerivImage::PixelType DerivPixelType;
  
  typename ExtenderType::Pointer    m_Extender;
  double                            m_InflationStrength;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGeodesicActiveContourImageFilter.txx"
#endif

#endif
