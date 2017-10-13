/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkRandomImageSource_h
#define itkRandomImageSource_h

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
 * \ingroup DataSources MultiThreaded
 * \ingroup ITKTestKernel
 *
 * \wiki
 * \wikiexample{SimpleOperations/RandomImageSource,Produce an image of noise}
 * \endwiki
 */
template< typename TOutputImage >
class ITK_TEMPLATE_EXPORT RandomImageSource:public ImageSource< TOutputImage >
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
  typedef typename TOutputImage::DirectionType    DirectionType;
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

  itkSetMacro(Direction, DirectionType);
  itkGetMacro(Direction, DirectionType);

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
  ~RandomImageSource() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  ThreadedGenerateData(const OutputImageRegionType &
                       outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RandomImageSource);

  SizeType      m_Size;      //size of the output image
  SpacingType   m_Spacing;   //spacing
  PointType     m_Origin;    //origin
  DirectionType m_Direction; //direction

  typename TOutputImage::PixelType m_Min; //minimum possible value
  typename TOutputImage::PixelType m_Max; //maximum possible value

  // The following variables are deprecated, and provided here just for
  // backward compatibility. It use is discouraged.
  mutable PointValueArrayType   m_OriginArray;
  mutable SpacingValueArrayType m_SpacingArray;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRandomImageSource.hxx"
#endif

#endif
