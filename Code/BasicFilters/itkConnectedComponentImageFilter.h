/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedComponentImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConnectedComponentImageFilter_h
#define __itkConnectedComponentImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{

/**
 * \class ConnectedComponentImageFilter
 * \brief Label the objects in a binary image
 *
 * ConnectedComponentImageFilter labels the objects in a binary
 * image.  Each distinct object is assigned a unique label. The filter
 * makes three passes through the image.  The first pass initialized
 * the output.  The second pass labels each foreground pixel such that
 * all the pixels associated with an object either have the same label
 * or have had their labels entered into a equivalency table.  The
 * third pass through the image flattens the equivalency table such
 * that all pixels for an object have the same label.
 *
 * \sa ImageToImageFilter
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT ConnectedComponentImageFilter : 
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef ConnectedComponentImageFilter Self;
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
  typedef   std::list<IndexType>                  ListType;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(ConnectedComponentImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);


protected:
  ConnectedComponentImageFilter() {}
  virtual ~ConnectedComponentImageFilter() {}
  ConnectedComponentImageFilter(const Self&) {}

  /**
   * Standard pipeline method. 
   */
  void GenerateData();

  /** ConnectedComponentImageFilter needs the entire input. Therefore
   * it must provide an implementation GenerateInputRequestedRegion().
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion();

  /** ConnectedComponentImageFilter will produce all of the output.
   * Therefore it must provide an implementation of
   * EnlargeOutputRequestedRegion().
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output));
  
private:

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConnectedComponentImageFilter.txx"
#endif

#endif
