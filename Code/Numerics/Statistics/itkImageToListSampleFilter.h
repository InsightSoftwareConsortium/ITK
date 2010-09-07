/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToListSampleFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToListSampleFilter_h
#define __itkImageToListSampleFilter_h

#include "itkListSample.h"
#include "itkPixelTraits.h"
#include "itkProcessObject.h"
#include "itkDataObject.h"
#include "itkDataObjectDecorator.h"
#include "itkFixedArray.h"

namespace itk
{
namespace Statistics
{
/** \class ImageToListSampleFilter
 *  \brief The class takes an image as input and generates a list sample as
 *  output.
 *
 *  There are differences between this class and ImageToListSampleAdaptor. This
 *  class is not an adaptor. It creates a new list sample and does not
 *  provide a pseudo interface to the actual image to make it look like a
 *  list sample.
 *
 *  The class optionally allows you to specify a mask image as an input. The
 *  list sample (if a mask is specified) is constructed from pixels that are
 *  within the mask
 *
 * \todo
 * In future allow the filter to take a Spatial object as input so a
 * generic spatial object like an ellipse etc can be used as a mask.
 * Sure the ImageMaskSpatialObject
 * can represent image masks too, so why not make SpatialObjects the default. I
 * think the ImageMaskSpatialObject is slow in terms of inefficient iteration
 * through the image.
 *
 * \sa ImageToListSampleAdaptor
 */
template< class TImage, class TMaskImage = TImage >
class ITK_EXPORT ImageToListSampleFilter:
  public ProcessObject
{
public:
  /** Standard class typedefs */
  typedef ImageToListSampleFilter    Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToListSampleFilter, ProcessObject);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Image typedefs */
  typedef TImage                           ImageType;
  typedef typename ImageType::Pointer      ImagePointer;
  typedef typename ImageType::ConstPointer ImageConstPointer;
  typedef typename ImageType::PixelType    PixelType;
  typedef typename MeasurementVectorPixelTraits<
    PixelType >::MeasurementVectorType MeasurementVectorType;

  /** Mask Image typedefs */
  typedef TMaskImage                           MaskImageType;
  typedef typename MaskImageType::Pointer      MaskImagePointer;
  typedef typename MaskImageType::ConstPointer MaskImageConstPointer;
  typedef typename MaskImageType::PixelType    MaskPixelType;

  /** Type of the output list sample */
  typedef ListSample< MeasurementVectorType > ListSampleType;

  /** return the number of components of the input image */
  unsigned int GetMeasurementVectorSize() const;

  /** Method to set/get the image */
  void SetInput(const ImageType *image);

  const ImageType * GetInput() const;

  /** Method to set/get the mask */
  void SetMaskImage(const MaskImageType *image);

  const MaskImageType * GetMaskImage() const;

  /** Method to get the list sample, the generated output. Note that this does
   * not invoke Update(). You should have called update on this class to get
   * any meaningful output. */
  const ListSampleType * GetOutput() const;

  /** Set the pixel value treated as on in the mask. If a mask has been
   * specified, only pixels with this value will be added to the list sample, if
   * no mask has been specified all pixels will be added as measurement vectors
   * to the list sample. */
  itkSetMacro(MaskValue, MaskPixelType);
  itkGetConstMacro(MaskValue, MaskPixelType);
protected:
  ImageToListSampleFilter();
  virtual ~ImageToListSampleFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Standard itk::ProcessObject subclass method. */
  typedef DataObject::Pointer DataObjectPointer;
  virtual DataObjectPointer MakeOutput(unsigned int idx);

  /** This method causes the filter to generate its output. */
  virtual void GenerateData();

  /** This method ensures that a mask image if specified has requested regions
   * that at least contain the input image's buffered region. */
  virtual void GenerateInputRequestedRegion()
  throw( InvalidRequestedRegionError );

  virtual void GenerateOutputInformation();

private:
  ImageToListSampleFilter(const Self &); //purposely not implemented
  void operator=(const Self &);          //purposely not implemented

  MaskPixelType m_MaskValue;
};  // end of class ImageToListSampleFilter
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToListSampleFilter.txx"
#endif

#endif
