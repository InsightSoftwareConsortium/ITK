/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeDetectionLevelSetFilter.h
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
 * relatively constant intensity. For example, the function
 * g(I) = 1 / ( 1 + |grad(I)| ) where I = image intensity, is 
 * typically used.
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
 * \ingroup LevelSetSegmentation 
 *
 */
template <class TLevelSet, class TEdgeImage>
class ITK_EXPORT ShapeDetectionLevelSetFilter :
  public LevelSetImageFilter<TLevelSet>
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef ShapeDetectionLevelSetFilter Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef LevelSetImageFilter<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ShapeDetectionLevelSetFilter, LevelSetImageFilter);

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
   * Set the edge image
   */
  void SetEdgeImage( EdgeImageType * ptr );

  /**
   * Get the edge image
   */
  EdgeImagePointer GetEdgeImage() const
    { return m_EdgeImage; };

  /**
   * Set the length penalty strength. This parameter can be
   * viewed as the weighting given to length of the front
   * in an energy functional. The larger the value, the smoother
   * the resulting front. This is the "epsilon" parameter in the
   * Malladi et al paper.
   *
   * Typically, the value is application dependent (e.g. noise level,
   * shape complexity, intensity range). Default value is 0.05
   */
  itkSetClampMacro( LengthPenaltyStrength, double, 0.0,
    NumericTraits<double>::max() );

  /**
   * Get the length penalty strength
   */
  itkGetMacro( LengthPenaltyStrength, double );

  /**
   * Set the propagation direction. If PropagateOutwards is true then
   * the front propagated outwards in each iteration. If set to false
   * the front is propagated inwards. By default, PropagateOutwards
   * is set to true.
   */
  itkSetMacro( PropagateOutwards, bool );

  /**
   * Get the PropagateOutward flag.
   */
  itkGetMacro( PropagateOutwards, bool );

  /**
   * Get the output narrow band
   */
  NodeContainerPointer GetOutputNarrowBand()
    { return m_OutputNarrowBand; }

  /**
   * Set the debugging mode
   */
  itkSetMacro( DebugOn, bool );

protected:
  ShapeDetectionLevelSetFilter();
  ~ShapeDetectionLevelSetFilter(){};
  ShapeDetectionLevelSetFilter(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent) const;


  virtual void AllocateOutput();
  void GenerateData();
  virtual void GenerateDataFull();
  virtual void GenerateDataNarrowBand();
  virtual void GenerateInputRequestedRegion();

  void SetOutputNarrowBand( NodeContainer *ptr )
    { m_OutputNarrowBand = ptr; }

private:
  typedef typename TEdgeImage::PixelType EdgePixelType;
  typedef 
    ExtensionVelocitiesImageFilter<TLevelSet,EdgePixelType,1> 
      ExtenderType;

  EdgeImagePointer                      m_EdgeImage;
  double                                m_LengthPenaltyStrength;
  bool                                  m_PropagateOutwards;
  typename ExtenderType::Pointer        m_Extender;
  NodeContainerPointer                  m_OutputNarrowBand;

  bool                                  m_DebugOn;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapeDetectionLevelSetFilter.txx"
#endif

#endif
