/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaheImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkPlaheImageFilter_h
#define __itkPlaheImageFilter_h


#include <itkImageToImageFilter.h>
#include <itkImage.h>

namespace itk
{
/**
 * /class PlaheImageFilter
 * Power Law Adaptive Histogram Equalization (PLAHE) is one of adaptive
 * histogram equalization method.  For detail description, reference
 * "Adaptive Image Contrast Enhancement using Generalizations of Histogram 
 * Equalization."  J.Alex Stark. IEEE Transactions on Image Processing, 
 * May 2000.
 */

template <class TPixel, unsigned int VImageDimension = 2>
class ITK_EXPORT PlaheImageFilter :
  public ImageToImageFilter< Image<TPixel, VImageDimension>,
                             Image<TPixel, VImageDimension> >
{
public:
 /**
  * Standard "Self" typedef.
  */ 
  typedef PlaheImageFilter Self;
  
 /**
  * Standard super class typedef support.
  */
  typedef ImageToImageFilter< Image<TPixel, VImageDimension>,
                              Image<TPixel, VImageDimension> > Superclass;

 /**
  * Smart pointer typedef support
  */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> constPointer;

 /**
  * Image type typedef support
  */
  typedef Image<TPixel, VImageDimension> ImageType;

 /**
  * Run-time type information (and related methods)
  */
  itkTypeMacro(PlaheImageFilter, ImageToImageFilter);

 /**
  * Method for creation through the object factory
  */
  itkNewMacro(Self);

 /**
  * A function which is used in GenerateData();
  */
  float CumulativeFunction(float u, float v);
   
 /**
  * Standard pipeline method.
  */
  void GenerateData();

 /**
  * Standard Get/Set macros for filter parameters
  */
  itkSetMacro(Alpha, float);
  itkGetMacro(Alpha, float);
  itkSetMacro(Beta, float);
  itkGetMacro(Beta, float);
  itkSetVectorMacro(Window, unsigned int, VImageDimension);
  itkGetVectorMacro(Window, const unsigned int, VImageDimension);

private:
 /**
  * The beta parameter of the Plahe
  */
  float m_Alpha;

 /**
  * The alpha parameter of the Plahe
  */
  float m_Beta;

 /**
  * The window size of the Plahe algorithm
  * This parameter defines the size of neighborhood around the evaluated pixel.
  */
  unsigned int m_Window[VImageDimension];

protected:
  PlaheImageFilter(){};
  virtual ~PlaheImageFilter(){};
  PlaheImageFilter(const Self&){};
  void operator=(const Self&){};
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPlaheImageFilter.txx"
#endif

#endif
