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

namespace itk
{

/** \class RandomImageSource
 * \brief Generate an n-dimensional image of random image values.
 *
 * RandomImageSource generates an image of random scalar values.
 * The output image may be of any dimension. The scalar values are
 * inserted into the image via a scalar iterator (i.e., the pixel type
 * must support GetScalar()/SetScalar()).
 */
template <typename TOutputImage>
class ITK_EXPORT RandomImageSource : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RandomImageSource   Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * typename typedef for the output image PixelType
   */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

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
  itkSetVectorMacro(Size,unsigned long,TOutputImage::ImageDimension);

  /** 
   * Get the size of the output image.
   */
  itkGetVectorMacro(Size,unsigned long,TOutputImage::ImageDimension);
  
  /** 
   * Specify the spacing of the output image.
   */
  itkSetVectorMacro(Spacing,float,TOutputImage::ImageDimension);

  /** 
   * Get the spacing of the output image.
   */
  itkGetVectorMacro(Spacing,float,TOutputImage::ImageDimension);

  /** 
   * Specify the origin of the output image.
   */
  itkSetVectorMacro(Origin,float,TOutputImage::ImageDimension);

  /** 
   * Get the origin of the output image.
   */
  itkGetVectorMacro(Origin,float,TOutputImage::ImageDimension);
  
  /** 
   * Set the minimum possible pixel value. By default, it is
   * NumericTraits<TOutputImage::ScalarValueType>::min().
   */
  itkSetClampMacro(Min,typename TOutputImage::ScalarValueType,
                   NumericTraits<typename TOutputImage::ScalarValueType>::min(),
                   NumericTraits<typename TOutputImage::ScalarValueType>::max());
  
  /** 
   * Get the minimum possible pixel value.
   */
  itkGetMacro(Min,typename TOutputImage::ScalarValueType);

  /** 
   * Set the maximum possible pixel value. By default, it is
   * NumericTraits<TOutputImage::ScalarValueType>::max().
   */
  itkSetClampMacro(Max,typename TOutputImage::ScalarValueType,
                   NumericTraits<typename TOutputImage::ScalarValueType>::min(),
                   NumericTraits<typename TOutputImage::ScalarValueType>::max());
  
  /** 
   * Get the maximum possible pixel value.
   */
  itkGetMacro(Max,typename TOutputImage::ScalarValueType);

protected:
  RandomImageSource();
  ~RandomImageSource();
  RandomImageSource(const RandomImageSource&) {};
  void operator=(const RandomImageSource&) {};
  void PrintSelf(std::ostream& os, Indent indent);
  
  virtual void 
  ThreadedGenerateData(const OutputImageRegion& outputRegionForThread,
                       int threadId );
  virtual void GenerateOutputInformation();

private:
  unsigned long *m_Size;    //size of the output image
  float         *m_Spacing; //spacing
  float         *m_Origin;  //origin

  typename TOutputImage::ScalarValueType m_Min; //minimum possible value
  typename TOutputImage::ScalarValueType m_Max; //maximum possible value
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomImageSource.txx"
#endif

#endif
