/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGeodesicActiveContourImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkGeodesicActiveContourImageFilter_h
#define _itkGeodesicActiveContourImageFilter_h

#include "itkShapeDetectionLevelSetFilter.h"

namespace itk
{
/** \class GeodesicActiveContourImageFilter
 * \brief Edge based shape detection using geodesic active contours.
 *
 * GeodesicActiveContourImageFilter is a level set approach for boundary detection.
 * An initial contour is evolved to minimise a weighted curve length
 * functional, where the weight depends on the edge potential function.
 *
 * The advantage of this approach over ShapeDetectionLevelSetFilter is that the 
 * evolution equation has an addition term which acts like a doublet and 
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
  /**
   * Standard "Self" typedef
   */
  typedef GeodesicActiveContourImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ShapeDetectionLevelSetFilter<TLevelSet,TEdgeImage> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(GeodesicActiveContourImageFilter, ShapeDetectionLevelSetFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Typedef support for level set related types.
   */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /**
   * Typedef support for the EdgeImageType
   */
  typedef TEdgeImage EdgeImageType;

  /**
   * EdgeImagePointer typedef support.
   */
  typedef typename EdgeImageType::Pointer EdgeImagePointer;

  /**
   * DerivImage typedef support.
   */
  typedef TDerivImage DerivImageType;

  /**
   * DerivImagePointer typedef support.
   */
  typedef typename DerivImageType::Pointer DerivImagePointer;

  /**
   * SetDimension
   */
  enum { SetDimension = TLevelSet::ImageDimension };

  /**
   * Set the inflation strength. The parameter control the 
   * the strength of the optional ballon force. If PropagateOutward
   * is set to false, this controls the strength of the optional
   * contraction force. Typically, the value is application dependent 
   * (e.g. noise level, shape complexity, intensity range). 
   * Default value 0.0.
   */
  itkSetClampMacro( InflationStrength, double, 0.0,
    NumericTraits<double>::max() );

  /**
   * Get the inflation strength
   */
  itkGetMacro( InflationStrength, double );

  /**
   * Set the input derivative images
   */
  void SetDerivativeImage( TDerivImage *ptr, unsigned int idx=0 );

  /** 
   * Set the debugging mode
   */
  itkSetMacro( DebugOn, bool );


protected:
  GeodesicActiveContourImageFilter();
  ~GeodesicActiveContourImageFilter(){};
  GeodesicActiveContourImageFilter(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void GenerateDataFull();
  virtual void GenerateDataNarrowBand();
  virtual void GenerateInputRequestedRegion();

private:
  typedef ReinitializeLevelSetImageFilter<TLevelSet> ExtenderType;
  typedef typename TEdgeImage::PixelType EdgePixelType;
  typedef typename TDerivImage::PixelType DerivPixelType;
  
  typename ExtenderType::Pointer    m_Extender;
  typename TDerivImage::Pointer     m_DerivImages[SetDimension];
  double                            m_InflationStrength;

  bool                              m_DebugOn;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGeodesicActiveContourImageFilter.txx"
#endif

#endif
