/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRandomImageSource_h
#define __itkRandomImageSource_h

#include "itkImageSource.h"
#include "itkNumericTraits.h"

namespace itk
{

/** \class RandomImageSource
 * \brief Generate an n-dimensional image of random pixel values.
 *
 * RandomImageSource generates an image of random pixel values.
 * This filter uses an inline random number generator since the library
 * drand48, although thread-safe, is very slow in a threaded environment.
 * The output image may be of any dimension. 
 *
 * \ingroup DataSources
 */
template <typename TOutputImage>
class ITK_EXPORT RandomImageSource : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef RandomImageSource   Self;
  typedef ImageSource<TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Typedef for the output image PixelType. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RandomImageSource,ImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Specify the size of the output image. */
  itkSetVectorMacro(Size,unsigned long,TOutputImage::ImageDimension);

  /** Get the size of the output image. */
  itkGetVectorMacro(Size,unsigned long,TOutputImage::ImageDimension);
  
  /** Specify the spacing of the output image. */
  itkSetVectorMacro(Spacing,float,TOutputImage::ImageDimension);

  /** Get the spacing of the output image. */
  itkGetVectorMacro(Spacing,float,TOutputImage::ImageDimension);

  /** Specify the origin of the output image. */
  itkSetVectorMacro(Origin,float,TOutputImage::ImageDimension);

  /** Get the origin of the output image. */
  itkGetVectorMacro(Origin,float,TOutputImage::ImageDimension);
  
  /** Set the minimum possible pixel value. By default, it is
   * NumericTraits<TOutputImage::PixelType>::min(). */
  itkSetClampMacro(Min, OutputImagePixelType,
                   NumericTraits<OutputImagePixelType>::NonpositiveMin(),
                   NumericTraits<OutputImagePixelType>::max());
  
  /** Get the minimum possible pixel value. */
  itkGetMacro(Min, OutputImagePixelType);

  /** Set the maximum possible pixel value. By default, it is
   * NumericTraits<TOutputImage::PixelType>::max(). */
  itkSetClampMacro(Max, OutputImagePixelType,
                   NumericTraits<OutputImagePixelType>::NonpositiveMin(),
                   NumericTraits<OutputImagePixelType>::max());
  
  /** Get the maximum possible pixel value. */
  itkGetMacro(Max, OutputImagePixelType);

protected:
  RandomImageSource();
  ~RandomImageSource();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  virtual void 
  ThreadedGenerateData(const OutputImageRegionType& 
                       outputRegionForThread, int threadId );
  virtual void GenerateOutputInformation();

private:
  RandomImageSource(const RandomImageSource&); //purposely not implemented
  void operator=(const RandomImageSource&); //purposely not implemented

  unsigned long *m_Size;    //size of the output image
  float         *m_Spacing; //spacing
  float         *m_Origin;  //origin

  typename TOutputImage::PixelType m_Min; //minimum possible value
  typename TOutputImage::PixelType m_Max; //maximum possible value
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomImageSource.txx"
#endif

#endif
