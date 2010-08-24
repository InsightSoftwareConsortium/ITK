/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 * NOTE: To produce deterministic results, set the number of threads
 * to 1.
 *
 * \ingroup DataSources Multithreaded
 */
template< typename TOutputImage >
class ITK_EXPORT RandomImageSource:public ImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef RandomImageSource           Self;
  typedef ImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Typedef for the output image PixelType. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(RandomImageSource, ImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Basic types from the OutputImageType */
  typedef typename TOutputImage::SizeType         SizeType;
  typedef typename TOutputImage::IndexType        IndexType;
  typedef typename TOutputImage::SpacingType      SpacingType;
  typedef typename TOutputImage::PointType        PointType;
  typedef typename SizeType::SizeValueType        SizeValueType;
  typedef SizeValueType                           SizeValueArrayType[TOutputImage::ImageDimension];
  typedef typename TOutputImage::SpacingValueType SpacingValueType;
  typedef SpacingValueType                        SpacingValueArrayType[TOutputImage::ImageDimension];
  typedef typename TOutputImage::PointValueType   PointValueType;
  typedef PointValueType                          PointValueArrayType[TOutputImage::ImageDimension];

  /** Set/Get size of the output image */
  itkSetMacro(Size, SizeType);
  virtual void SetSize(SizeValueArrayType sizeArray);

  virtual const SizeValueType * GetSize() const;

  /** Set/Get spacing of the output image */
  itkSetMacro(Spacing, SpacingType);
  virtual void SetSpacing(SpacingValueArrayType spacingArray);

  virtual const SpacingValueType * GetSpacing() const;

  /** Set/Get origin of the output image */
  itkSetMacro(Origin, PointType);
  virtual void SetOrigin(PointValueArrayType originArray);

  virtual const PointValueType * GetOrigin() const;

  /** Set the minimum possible pixel value. By default, it is
   * NumericTraits<TOutputImage::PixelType>::min(). */
  itkSetClampMacro( Min, OutputImagePixelType,
                    NumericTraits< OutputImagePixelType >::NonpositiveMin(),
                    NumericTraits< OutputImagePixelType >::max() );

  /** Get the minimum possible pixel value. */
  itkGetConstMacro(Min, OutputImagePixelType);

  /** Set the maximum possible pixel value. By default, it is
   * NumericTraits<TOutputImage::PixelType>::max(). */
  itkSetClampMacro( Max, OutputImagePixelType,
                    NumericTraits< OutputImagePixelType >::NonpositiveMin(),
                    NumericTraits< OutputImagePixelType >::max() );

  /** Get the maximum possible pixel value. */
  itkGetConstMacro(Max, OutputImagePixelType);
protected:
  RandomImageSource();
  ~RandomImageSource();
  void PrintSelf(std::ostream & os, Indent indent) const;

  virtual void
  ThreadedGenerateData(const OutputImageRegionType &
                       outputRegionForThread, int threadId);

  virtual void GenerateOutputInformation();

private:
  RandomImageSource(const RandomImageSource &); //purposely not implemented
  void operator=(const RandomImageSource &);    //purposely not implemented

  SizeType    m_Size;       //size of the output image
  SpacingType m_Spacing;    //spacing
  PointType   m_Origin;     //origin

  typename TOutputImage::PixelType m_Min; //minimum possible value
  typename TOutputImage::PixelType m_Max; //maximum possible value

  // The following variables are deprecated, and provided here just for
  // backward compatibility. It use is discouraged.
  mutable PointValueArrayType   m_OriginArray;
  mutable SpacingValueArrayType m_SpacingArray;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomImageSource.txx"
#endif

#endif
