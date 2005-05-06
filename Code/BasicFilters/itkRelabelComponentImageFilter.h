/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRelabelComponentImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRelabelComponentImageFilter_h
#define __itkRelabelComponentImageFilter_h

#include "itkInPlaceImageFilter.h"
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
 * consecutive with no gaps between the label numbers used.  By
 * default, the relabling will also sort the labels based on the size
 * of the object: the largest object will have label #1, the second
 * largest will have label #2, etc.
 *
 * Label #0 is assumed to be background is left unaltered by the
 * relabeling. 
 *
 * RelabelComponentImageFilter is typically used on the output of the
 * ConnectedComponentImageFilter for those applications that want to
 * extract the largest object or the "k" largest objects. Any
 * particular object can be extracted from the relabeled output using
 * a BinaryThresholdImageFilter. A group of objects can be extracted
 * from the relabled output using a ThresholdImageFilter.
 *
 * Once all the objects are relabeled, the application can query the
 * number of objects and the size of each object. Object sizes are
 * returned in a vector. The size of the background is not
 * calculated. So the size of object #1 is
 * GetSizeOfObjectsInPixels()[0], the size of object #2 is
 * GetSizeOfObjectsInPixels()[1], etc.
 *
 * If user sets a minimum object size, all objects with fewer pixelss
 * than the minimum will be discarded, so that the number of objects
 * reported will be only those remaining. The
 * GetOriginalNumberOfObjects method can be called to find out how
 * many objects were present before the small ones were discarded.
 *
 * RelabelComponentImageFilter can be run as an "in place" filter,
 * where it will overwrite its output.  The default is run out of
 * place (or generate a separate output).  "In place" operation can be
 * controlled via methods in the superclass,
 * InPlaceImageFilter::InPlaceOn() and InPlaceImageFilter::InPlaceOff().
 *
 * \sa ConnectedComponentImageFilter, BinaryThresholdImageFilter, ThresholdImageFilter
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT RelabelComponentImageFilter : 
    public InPlaceImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef RelabelComponentImageFilter Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Types from the Superclass
   */
  typedef typename Superclass::InputImagePointer InputImagePointer;

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
  
  /** Get the original number of objects in the image before small
   * objects were discarded. This information is only valid after
   * the filter has executed. If the caller has not specified a
   * minimum object size, OriginalNumberOfObjects is the same as
   * NumberOfObjects. */
  itkGetMacro(OriginalNumberOfObjects, unsigned long);

  /** Get/Set the number of objects enumerated and described when the
   * filter is printed. */
  itkSetMacro(NumberOfObjectsToPrint, unsigned long);
  itkGetConstReferenceMacro(NumberOfObjectsToPrint, unsigned long);

  /** Set the minimum size in pixels for an object. All objects
   * smaller than this size will be discarded and will not appear
   * in the output label map. NumberOfObjects will count only the
   * objects whose pixel counts are greater than or equal to the
   * minimum size. Call GetOriginalNumberOfObjects to find out how
   * many objects were present in the original label map. */
  itkSetMacro(MinimumObjectSize, unsigned long);

  /** Get the caller-defined minimum size of an object in pixels.
   * If the caller has not set the minimum, 0 will be returned,
   * which is to be interpreted as meaning that no minimum exists,
   * and all objects in the original label map will be passed
   * through to the output. */
  itkGetMacro(MinimumObjectSize, unsigned long);
   
  /** Get the size of each object in pixels. This information is only
   * valid after the filter has executed.  Size of the background is
   * not calculated.  Size of object #1 is
   * GetSizeOfObjectsInPixels()[0]. Size of object #2 is
   * GetSizeOfObjectsInPixels()[1]. Etc. */
  const std::vector<unsigned long>& GetSizeOfObjectsInPixels() const
    { return m_SizeOfObjectsInPixels; }

  /** Get the size of each object in physical space (in units of pixel
   * size). This information is only valid after the filter has
   * executed. Size of the background is not calculated.  Size of
   * object #1 is GetSizeOfObjectsInPhysicalUnits()[0]. Size of object
   * #2 is GetSizeOfObjectsInPhysicalUnits()[1]. Etc. */
  const std::vector<float>& GetSizeOfObjectsInPhysicalUnits() const
    { return m_SizeOfObjectsInPhysicalUnits; }

  /** Get the size of a particular object in pixels. This information is only
   * valid after the filter has executed.  Size of the background
   * (object #0) is not calculated.  */
  unsigned long GetSizeOfObjectInPixels(unsigned long obj) const
    {
      if (obj > 0 && obj <= m_NumberOfObjects)
        {
        return m_SizeOfObjectsInPixels[obj-1];
        }
      else
        {
        return 0;
        }
    }

  /** Get the size of a particular object in physical space (in units of pixel
   * size). This information is only valid after the filter has
   * executed. Size of the background (object #0) is not calculated.  */
  float GetSizeOfObjectInPhysicalUnits(unsigned long obj) const
    { 
      if (obj > 0 && obj <= m_NumberOfObjects)
        {
        return m_SizeOfObjectsInPhysicalUnits[obj-1];
        }
      else
        {
        return 0;
        }
    }  

  
protected:
  RelabelComponentImageFilter()
    : m_NumberOfObjects(0), m_NumberOfObjectsToPrint(10),
    m_OriginalNumberOfObjects(0), m_MinimumObjectSize(0)
    { this->InPlaceOff(); }
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
  unsigned long m_NumberOfObjectsToPrint;
  unsigned long m_OriginalNumberOfObjects;
  unsigned long m_MinimumObjectSize;
  std::vector<unsigned long> m_SizeOfObjectsInPixels;
  std::vector<float> m_SizeOfObjectsInPhysicalUnits;

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRelabelComponentImageFilter.txx"
#endif

#endif
