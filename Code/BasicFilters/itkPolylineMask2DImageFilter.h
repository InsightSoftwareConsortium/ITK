/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMask2DImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPolylineMask2DImageFilter_h
#define _itkPolylineMask2DImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
  
/** \class PolylineMask2DImageFilter
 * \brief Implements image masking operation constrained by a polyline.  The operation is applied
 * on on each 2D image.
 *
 * This class is parameterized over the types of the input image, polyline,
 * and output image.  
 * 
 * \ingroup ImageToImageFilter
 */
  template <class TInputImage, class TPolyline,
          class TOutputImage>
class ITK_EXPORT PolylineMask2DImageFilter:public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PolylineMask2DImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( PolylineMask2DImageFilter, ImageToImageFilter );

  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int,
                      TInputImage::ImageDimension);
  
  /** Some convenient typedefs for input image */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 

  /* typedef for the polyline type */
  typedef TPolyline    PolylineType;
  
  /* typedef for the output image */
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType OutputImagePixelType;

  /** Read in image and polyline inputs */
  void SetInput( const InputImageType * image);
  void SetInput( const PolylineType * polyline);

  /* Generate Data */
  void GenerateData(void);
 

protected:
  PolylineMask2DImageFilter();
  virtual ~PolylineMask2DImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  PolylineMask2DImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolylineMask2DImageFilter.txx"
#endif

#endif
