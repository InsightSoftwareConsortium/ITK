/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeDetectionLevelSetFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkShapeDetectionLevelSetFilter_h
#define _itkShapeDetectionLevelSetFilter_h

#include "itkLevelSet.h"
#include "itkLevelSetImageFilter.h"
#include "itkExtensionVelocitiesImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class ShapeDetectionLevelSetFilter
 * \brief Edge based shape detection using a level set approach.
 *
 * ShapeDetectionLevelSetFilter is a level set approach for boundary detection.
 * An initial contour is propagated outwards until it sticks to
 * the shape boundaries. This is done by using a speed function which 
 * is based on image edge features.
 *
 * Propagation is done by the evolution of a higher-order level-set
 * with the propagation front embedded as the zero level set.
 *
 * This class requires two inputs: an initial level set and 
 * and edge potential image.
 *
 * The initial level set is a floating point image which contains
 * the initial front as the zero level set. For example, a signed
 * function from the intial front is typically used.
 *
 * A signed function image can be constructed using the
 * ReinitializeLevel class where the input is a binary image
 * with all the pixels on the inside of the front assigned a
 * value of -0.5 and all pixels on the outside assigned a
 * value of +0.5.
 *
 * The edge potential image has values close to zero in regions
 * of high image gradient and values close to one in regions with
 * relatively constant intensity. For example, functions
 * 
 * \f[ g(I) = 1 / ( 1 + | (\nabla * G)(I)| ) \f]
 * \f[ g(I) = \exp^{-|(\nabla * G)(I)|} \f]
 * 
 * are typically used, where \f$ I \f$ is image intensity and
 * \f$ (\nabla * G) \f$ is the derivative of Gaussian operator. 
 *
 * EdgePotentialImageFilter computes the potential map using the
 * latter equation with the image gradient as the input.
 *
 * The user can force the propagation to go inwards via
 * method SetPropagateOutwards( false ).
 *
 * This class is templated on the image type which represent the 
 * level set and the type of the edge potential image.
 *
 * This class supports narrowbanding, where at each iteration only
 * a narrow band surrounding the propagating front is updated.
 *
 * Implementation of this class is based on:
 * "Shape Modeling with Front Propagation: A Level Set Approach",
 * R. Malladi, J. A. Sethian and B. C. Vermuri.
 * IEEE Trans. on Pattern Analysis and Machine Intelligence,
 * Vol 17, No. 2, pp 158-174, February 1995
 *
 * Possible improvements:
 * - In the narrowband version, allow the user to specify the 
 * number of iterations before reinitialization.
 * - Add support to detect convergence.
 *
 * Note: this filter will eventually be re-implemented as part of
 * the Finite Difference Solver framework.
 *
 * \sa EdgePotentialImageFilter
 *
 * \ingroup Deprecated
 */
template <class TLevelSet, class TEdgeImage>
class ITK_EXPORT ShapeDetectionLevelSetFilter :
  public LevelSetImageFilter<TLevelSet>
{
public:
  /** Standard class typedefs. */
  typedef ShapeDetectionLevelSetFilter Self;
  typedef LevelSetImageFilter<TLevelSet> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShapeDetectionLevelSetFilter, LevelSetImageFilter);

  /** Typedef support for level set related types. */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /** Typedef support for the edge image type. */
  typedef TEdgeImage EdgeImageType;

  /** EdgeImagePointer typedef support. */
  typedef typename EdgeImageType::Pointer EdgeImagePointer;
  typedef typename EdgeImageType::ConstPointer EdgeImageConstPointer;

  /** Set/Get the edge image. */
  void SetEdgeImage( const EdgeImageType * ptr );
  const EdgeImageType * GetEdgeImage(void);

  /** Set/Get the length penalty strength. This parameter can be viewed as
   * the weighting given to length of the front in an energy functional. The
   * larger the value, the smoother the resulting front. This is the
   * "epsilon" parameter in the Malladi et al paper.  Typically, the value is
   * application dependent (e.g. noise level, shape complexity, intensity
   * range). Default value is 0.05 */
  itkSetClampMacro( LengthPenaltyStrength, double, 0.0,
    NumericTraits<double>::max() );
  itkGetMacro( LengthPenaltyStrength, double );

  /** Set/Get the propagation direction. If PropagateOutwards is true then
   * the front propagated outwards in each iteration. If set to false the
   * front is propagated inwards. By default, PropagateOutwards is set to
   * true. */
  itkSetMacro( PropagateOutwards, bool );
  itkGetMacro( PropagateOutwards, bool );

  /** Get the output narrow band. */
  NodeContainerPointer GetOutputNarrowBand()
    { return m_OutputNarrowBand; }

protected:
  ShapeDetectionLevelSetFilter();
  ~ShapeDetectionLevelSetFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual void AllocateOutput();
  void GenerateData();
  virtual void GenerateDataFull();
  virtual void GenerateDataNarrowBand();
  virtual void GenerateInputRequestedRegion();

  void SetOutputNarrowBand( NodeContainer *ptr )
    { m_OutputNarrowBand = ptr; }

private:
  ShapeDetectionLevelSetFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  typedef typename TEdgeImage::PixelType EdgePixelType;
  typedef ExtensionVelocitiesImageFilter<TLevelSet,EdgePixelType,1> 
      ExtenderType;

  double                                m_LengthPenaltyStrength;
  bool                                  m_PropagateOutwards;
  typename ExtenderType::Pointer        m_Extender;
  NodeContainerPointer                  m_OutputNarrowBand;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapeDetectionLevelSetFilter.txx"
#endif

#endif
