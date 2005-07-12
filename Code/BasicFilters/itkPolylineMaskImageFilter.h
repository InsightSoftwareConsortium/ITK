/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolylineMaskImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPolylineMaskImageFilter_h
#define _itkPolylineMaskImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{
  
/** \class PolylineMaskImageFilter
 * \brief Implements image masking operation constrained by a polyline on a plane
 * perpendicular to certain viewing direction
 *
 * This class is parameterized over the types of the input image, polyline, vector
 * and output image.  
 * 
 * \ingroup ImageToImageFilter
 */
  template <class TInputImage, class TPolyline, class TVector,
          class TOutputImage>
class ITK_EXPORT PolylineMaskImageFilter:public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PolylineMaskImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( PolylineMaskImageFilter, ImageToImageFilter );

  /** Number of dimensions. */
  itkStaticConstMacro(NDimensions, unsigned int,
                      TInputImage::ImageDimension);
  
  /** Some convenient typedefs for input image */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 

  /** typedef for the vector type */
  typedef TVector    VectorType;

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

  /** Read in viewing normal direction */
  itkSetMacro(Vector,VectorType);

  itkGetConstMacro(Vector,VectorType);  

  /* Generate Data */
  void GenerateData(void);
 

protected:
  PolylineMaskImageFilter();
  virtual ~PolylineMaskImageFilter() {};

private:
  PolylineMaskImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  VectorType m_Vector;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPolylineMaskImageFilter.txx"
#endif

#endif
