/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRandomImageSource_h
#define __itkRandomImageSource_h

#include "itkImageSource.h"

ITK_NAMESPACE_BEGIN

/** \class RandomImageSource
 * \brief Generate an n-dimensional image of random image values.
 *
 * RandomImageSource generates an image of random scalar values.
 * The output image may be of any dimension. The scalar values are
 * inserted into the image via a scalar iterator (i.e., the pixel type
 * must support GetScalar()/SetScalar()).
 */
template <class TOutputImage>
class ITK_EXPORT RandomImageSource : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RandomImageSource   Self;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RandomImageSource,ImageSource);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Specify the size of the output image.
   */
  itkSetVectorMacro(Size,unsigned long,TOutputImage::GetImageDimension());

  /** 
   * Get the size of the output image.
   */
  itkGetVectorMacro(Size,unsigned long,TOutputImage::GetImageDimension());
  
  /** 
   * Specify the spacing of the output image.
   */
  itkSetVectorMacro(Spacing,float,TOutputImage::GetImageDimension());

  /** 
   * Get the spacing of the output image.
   */
  itkGetVectorMacro(Spacing,float,TOutputImage::GetImageDimension());

  /** 
   * Specify the origin of the output image.
   */
  itkSetVectorMacro(Origin,float,TOutputImage::GetImageDimension());

  /** 
   * Get the origin of the output image.
   */
  itkGetVectorMacro(Origin,float,TOutputImage::GetImageDimension());
  
  
protected:
  RandomImageSource();
  ~RandomImageSource() {};
  RandomImageSource(const RandomImageSource&) {};
  void operator=(const RandomImageSource&) {};
  void PrintSelf(std::ostream& os, Indent indent);
  
  void Execute();

private:
  unsigned long *m_Size;    //size of the output image
  float         *m_Spacing; //spacing
  float         *m_Origin;  //origin

};

ITK_NAMESPACE_END

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomImageSource.txx"
#endif

#endif
