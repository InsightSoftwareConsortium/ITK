/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxCoreAtomImage_h
#define __itkBloxCoreAtomImage_h

#include "vnl/vnl_vector_fixed.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkBloxBoundaryPointPixel.h"
#include "itkBloxBoundaryPointImage.h"
#include "itkBloxCoreAtomItem.h"
#include "itkBloxCoreAtomPixel.h"
#include "itkBloxImage.h"

namespace itk
{

/** \class BloxCoreAtomImage
 * \brief N-dimensional image class which handles BloxCoreAtomItems
 *
 * A BloxCoreAtomImage stores and processes BloxCoreAtomItem's (in BloxPixel
 * linked lists). The primary task of BloxCoreAtomImage is finding core atoms
 * and storing them in the correct blox location.
 * \ingroup ImageObjects
 */
template <class TBoundaryPointImage>
class ITK_EXPORT BloxCoreAtomImage :
  public BloxImage<BloxCoreAtomPixel<ExtractImageDimension<TBoundaryPointImage>::ImageDimension>,
                   ExtractImageDimension<TBoundaryPointImage>::ImageDimension>
{
public:
  /** Standard class typedefs. */
  typedef BloxCoreAtomImage  Self;

  /** Dimension of the image.  This enum is used by functions that are
   * templated over image type (as opposed to being templated over pixel
   * type and dimension) when they need compile time access to the dimension
   * of the image. */
  enum { NDimensions = TBoundaryPointImage::ImageDimension };

  typedef BloxImage<BloxCoreAtomPixel<NDimensions>, NDimensions>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BloxCoreAtomImage, BloxImage);


  /** The type of boundary point item we process * */
  typedef BloxBoundaryPointItem<NDimensions> TBPItemType;

  /** Pixel typedef support. Used to declare pixel type in filters
   * or other operations. */
  typedef BloxCoreAtomPixel<NDimensions> PixelType;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external 
   * representation. */
  typedef PixelType InternalPixelType;

  /** Accessor type that convert data between internal and external
   * representations. */
  typedef DefaultPixelAccessor< PixelType > AccessorType;

  /** The type of Point used to convert between physical and blox space */
  typedef Point<double, NDimensions> TPositionType;

  /** The vector between two points */
  typedef typename TPositionType::VectorType TVectorType;

  /** How we represent gradients. */
  typedef CovariantVector<double, NDimensions> TGradientType;

  /** Convenient typedefs obtained from Superclass.
   * Note: Unlike "normal" images BloxCoreAtomImages support neither Scalar nor
   * Vector calls!!! Scalar and vector traits are not defined and do not
   * make sense for linked lists (at the present time). */
  typedef typename Superclass::PixelContainer PixelContainer;
  typedef typename Superclass::SizeType SizeType;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::OffsetType OffsetType;
  typedef typename Superclass::RegionType RegionType;
  
  /** A pointer to the pixel container. */
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** Set the boundary point image from which we derive core atoms. */
  void SetBoundaryPointImage(typename TBoundaryPointImage::Pointer pSource)
    {
      m_BoundaryPointImage = pSource;
      m_BPImageOrigin = m_BoundaryPointImage->GetOrigin();
      m_BPImageSpacing = m_BoundaryPointImage->GetSpacing(); 
    }

  /** Walk the source image, find core atoms, store them.  */
  void FindCoreAtoms();

  /** Find core atoms given a specific boundary point. */
  void FindCoreAtomsAtBoundaryPoint(BloxBoundaryPointItem<NDimensions>* pItem);

  /** Do eigenanalysis on all pixels in the image. */
  void DoEigenanalysis();

  /** Core atom voting routine. */
  void DoCoreAtomVoting();

  /** Gets and sets for member variables. */
  itkSetMacro(DistanceMin, double);
  itkSetMacro(DistanceMax, double);
  itkSetMacro(Epsilon, double);
  itkSetMacro(Polarity, bool);
  itkGetMacro(NumCoreAtoms, unsigned long int);

protected:
  BloxCoreAtomImage();
  virtual ~BloxCoreAtomImage();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  BloxCoreAtomImage(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Pointer to the image that holds boundary points. */
  typename TBoundaryPointImage::Pointer m_BoundaryPointImage;

  /** The origin of the boundary point image. */
  const double* m_BPImageOrigin;

  /** The spacing of the boundary point image. */
  const double* m_BPImageSpacing;
  
  /** Parameters used to establish conic shell iterator regions.
   * See the documentation for itkConicShellInteriorExteriorSpatialFunction
   * for how these affect the iterator. */
  double m_DistanceMin;
  double m_DistanceMax;
  double m_Epsilon;
  bool m_Polarity;
  
  /** Keep track of how many core atoms we found (for debugging) */
  unsigned long int m_NumCoreAtoms;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxCoreAtomImage.txx"
#endif

#endif
