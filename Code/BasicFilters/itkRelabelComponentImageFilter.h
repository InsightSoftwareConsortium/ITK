/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRelabelComponentImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRelabelComponentImageFilter_h
#define __itkRelabelComponentImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include <vector>

namespace itk
{

/**
 * \class RelabelComponentImageFilter
 * \brief Relabel the components in an image such that consecutive labels are used.
 *
 * RelabelComponentImageFilter remaps the labels associated with the
 * objects in an image (as from the output of
 * ConnectedComponentImageFilter) such that the label numbers are
 * consecutive with no gaps between the label number used.  By
 * default, the relabling will also sort the labels based on the size
 * of the object: the largest object will have label #1, the second
 * largest will have label #2, etc.
 *
 * RelabelComponentImageFilter is typically used on the output of the
 * ConnectedComponentImageFilter for those applications that want to
 * extract the largest object or the "k" largest objects. Any
 * particular object can be extracted from the relabeled output using
 * a BinaryThresholdImageFilter. A group of objects can be extracted
 * from the relabled output using a ThresholdImageFilter.
 *
 * Once all the objects are relabeled, the application can query the
 * number of objects and the size of each object.
 *
 *
 * \sa ConnectedComponentImageFilter, BinaryThresholdImageFilter, ThresholdImageFilter
 */

template <class TInputImage, class TOutputImage = TInputImage>
class ITK_EXPORT RelabelComponentImageFilter : 
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef RelabelComponentImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
  /**
   * Image typedef support
   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  typedef   typename TInputImage::IndexType       IndexType;
  typedef   typename TInputImage::SizeType        SizeType;
  typedef   typename TOutputImage::RegionType     RegionType;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(RelabelComponentImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** Get the number of objects in the image. This information is only
   * valid after the filter has executed. */
  itkGetMacro(NumberOfObjects, unsigned long);

  /** Get the size of each object in pixel. This information is only
   * valid after the filter has executed.  If the sorting order was
   * SortBySizeInPixels, this vector will be sorted in decreasing
   * order. If the sorting order was SortBySizeInPhysicalUnits, this
   * vector will not necessarily be sorted. */
  const std::vector<unsigned long>& GetSizeOfObjectsInPixels() const
    { return m_SizeOfObjectsInPixels; }

  /** Get the size of each object in physical space (in units of pixel
   * size). This information is only valid after the filter has
   * executed. If the sorting order was SortBySizeInPhysicalUnits,
   * this vector will be sorted in decreasing order. If the sorting
   * order was SortBySizeInPixels, this vector will not necessarily be
   * sorted. */
  const std::vector<unsigned long>& GetSizeOfObjectsInPhysicalUnits() const
    { return m_SizeOfObjectsInPhysicalUnits; }

  /** Set/Get how object size is determined.  Object size can be
   * calculated in pixels or in physical units. This mode determines
   * the sorting of objects to define labels. Default is sorting by
   * size in physical units. */
  typedef enum {SortBySizeInPixels, SortBySizeInPhysicalUnits} ObjectSortingOrderType;
  itkSetMacro(ObjectSortingOrder, ObjectSortingOrderType);
  itkGetMacro(ObjectSortingOrder, ObjectSortingOrderType);
  
protected:
  RelabelComponentImageFilter()
    : m_NumberOfObjects(0),
      m_ObjectSortingOrder(SortBySizeInPhysicalUnits)
    {}
  virtual ~RelabelComponentImageFilter() {}
  RelabelComponentImageFilter(const Self&) {}

  /**
   * Standard pipeline method. 
   */
  void GenerateData();

  /** RelabelComponentImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** Standard printself method */
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  unsigned long m_NumberOfObjects;
  std::vector<unsigned long> m_SizeOfObjectsInPixels;
  std::vector<float> m_SizeOfObjectsInPhysicalUnits;
  ObjectSortingOrderType m_ObjectSortingOrder;

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRelabelComponentImageFilter.txx"
#endif

#endif
