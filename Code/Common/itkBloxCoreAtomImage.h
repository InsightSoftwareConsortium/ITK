/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomImage.h
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
#ifndef __itkBloxCoreAtomImage_h
#define __itkBloxCoreAtomImage_h

#include "vnl/vnl_vector_fixed.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkBloxPixel.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkBloxCoreAtomItem.h"
#include "itkBloxImage.h"

namespace itk
{

/**
 * \class BloxCoreAtomImage
 * \brief N-dimensional image class which handles BloxCoreAtomItems
 *
 * A BloxCoreAtomImage stores and processes BloxCoreAtomItem's (in BloxPixel
 * linked lists). The primary task of BloxCoreAtomImage is finding core atoms
 * and storing them in the correct blox location.
 * */

template <class TBoundaryPointImage,
  class TImageTraits = DefaultImageTraits<BloxPixel, TBoundaryPointImage::ImageDimension> >
class ITK_EXPORT BloxCoreAtomImage :
  public BloxImage<TBoundaryPointImage::ImageDimension, TImageTraits>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef BloxCoreAtomImage  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef BloxImage<TBoundaryPointImage::ImageDimension, TImageTraits>  Superclass;
  
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Dimension of the image.  This enum is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image.
   */
  enum { NDimensions = TBoundaryPointImage::ImageDimension };

  /**
   * The type of boundary point item we process
   * */
  typedef BloxBoundaryPointItem<NDimensions> TBPItemType;

  /** 
   * Pixel typedef support. Used to declare pixel type in filters
   * or other operations.
   */
  typedef BloxPixel PixelType;

  /** 
   * Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external 
   * representation.
   */
  typedef PixelType InternalPixelType;

  /** 
   *  Accessor type that convert data between internal and external
   *  representations.
   */
  typedef DefaultPixelAccessor< PixelType > AccessorType;

  /**
   * The type of Point used to convert between physical and blox space
   */
  typedef Point<double, NDimensions> TPositionType;

  /**
   * The vector between two points
   */
  typedef TPositionType::VectorType TVectorType;

  /**
   * How we represent gradients
   */
  typedef CovariantVector<double, NDimensions> TGradientType;

  /**
   * The ImageTraits for this image.
   * Note: Unlike "normal" images BloxCoreAtomImages support neither Scalar nor
   * Vector calls!!! Scalar and vector traits are not defined and do not
   * make sense for linked lists (at the present time).
   */
  typedef TImageTraits ImageTraits;

  /*@{
   * Convenient typedefs obtained from TImageTraits template parameter.
   */
  typedef typename ImageTraits::PixelContainer PixelContainer;
  typedef typename ImageTraits::SizeType SizeType;
  typedef typename ImageTraits::IndexType IndexType;
  typedef typename ImageTraits::OffsetType OffsetType;
  typedef typename ImageTraits::RegionType RegionType;
  //@}

  /**
   * A pointer to the pixel container.
   */
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(BloxCoreAtomImage, BloxImage);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /*
   * Set the boundary point image from which we derive core atoms
   */
  void SetBoundaryPointImage(typename TBoundaryPointImage::Pointer pSource){
    m_BoundaryPointImage = pSource;
    m_BPImageOrigin = m_BoundaryPointImage->GetOrigin();
    m_BPImageSpacing = m_BoundaryPointImage->GetSpacing(); };

  /**
   * Walk the source image, find core atoms, store them 
   */
  void FindCoreAtoms();

  /**
   * Find core atoms given a specific boundary point
   */
  void FindCoreAtomsAtBoundaryPoint(BloxBoundaryPointItem<NDimensions>* pItem);

  /**
   * Method to convert physical coordinates to Blox coordinates
   * Returns TRUE if the specified location lies within the image,
   * otherwise FALSE. If FALSE, the index value is unmodified
   */
  bool ConvertPhysicalToDataCoords(TPositionType physicalCoords,
                                   IndexType& dataCoords);

  /*@{
   * Gets and sets for member variables
   */
  itkSetMacro(DistanceMin, double);
  itkSetMacro(DistanceMax, double);
  itkSetMacro(Epsilon, double);
  itkSetMacro(Polarity, bool);
  //@}

protected:
  BloxCoreAtomImage();
  virtual ~BloxCoreAtomImage();
  BloxCoreAtomImage(const Self&) {}
  void operator=(const Self&) {}

private:

  /**
   * Pointer to the image that holds boundary points
   */
  typename TBoundaryPointImage::Pointer m_BoundaryPointImage;

  /**
   * The origin of the boundary point image
   */
  const double* m_BPImageOrigin;

  /**
   * The spacing of the boundary point image
   */
  const double* m_BPImageSpacing;

  
  /*@{
   * Parameters used to establish conic shell iterator regions
   * See the documentation for itkConicShellInteriorExteriorSpatialFunction
   * for how these affect the iterator
   */
  double m_DistanceMin;
  double m_DistanceMax;
  double m_Epsilon;
  bool m_Polarity;
  //@}

  /**
   * Keep track of how many core atoms we found (for debugging)
   */
  unsigned long int m_NumCoreAtoms;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxCoreAtomImage.txx"
#endif

#endif
