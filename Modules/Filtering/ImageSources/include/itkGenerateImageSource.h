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

#ifndef __itkGenerateImageSource_h
#define __itkGenerateImageSource_h

#include "itkImageSource.h"

namespace itk
{

/** \class GenerateImageSource
 * \brief a Base class for image sources which need to have image
 * size, and other meta-data set.
 *
 * This class adds the ability for the user to set the Image's size,
 * spacing, origin, and direction cosines. It is designed to be a base
 * class for other image sources which generate an image, while there
 * is no image input to the source.
 *
 * \ingroup DataSources
 * \ingroup ITKImageSources
 */
template< typename TOutputImage >
class GenerateImageSource
  : public ImageSource< TOutputImage >
{
public:
  typedef GenerateImageSource         Self;
  typedef ImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self>   ConstPointer;

  /** Output image typedefs */
  typedef TOutputImage                            OutputImageType;
  typedef typename OutputImageType::PixelType     PixelType;
  typedef typename OutputImageType::RegionType    RegionType;
  typedef typename OutputImageType::SpacingType   SpacingType;
  typedef typename OutputImageType::PointType     PointType;
  typedef typename OutputImageType::DirectionType DirectionType;


  typedef typename TOutputImage::SizeType      SizeType;
  typedef typename TOutputImage::SizeValueType SizeValueType;

  /** Dimensionality of the output image */
  itkStaticConstMacro(NDimensions, unsigned int, TOutputImage::ImageDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GeneratemageSource, ImageSource);

  /**
   * Set/Get the size of the output image
   **/
  itkSetMacro(Size, SizeType);
  itkGetConstReferenceMacro(Size, SizeType);
  itkSetVectorMacro(Size, SizeValueType, NDimensions);

  /**
   * Set/Get the output image spacing. This parameter defaults to all ones.
   **/
  itkSetMacro(Spacing, SpacingType);
  itkGetConstReferenceMacro(Spacing, SpacingType);
  itkSetVectorMacro(Spacing, const float, NDimensions);


  /**
   * Set/Get the output image origin. This parameter defaults to all zeros.
   **/
  itkSetMacro(Origin, PointType);
  itkGetConstReferenceMacro(Origin, PointType);
  itkSetVectorMacro(Origin, const float, NDimensions);

  /**
   * Set/Get the output image direction cosine. This parameter
   * defaults to the identity matrix.
   **/
  itkSetMacro(Direction, DirectionType);
  itkGetConstReferenceMacro(Direction, DirectionType);

protected:
  GenerateImageSource();
  // virtual ~GenerateImageSource() default implementation ok
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

private:
  GenerateImageSource( const GenerateImageSource &); //purposely not implemented
  void operator=( const GenerateImageSource &);  //purposely not implemented

  SizeType      m_Size;            //size of the output image
  SpacingType   m_Spacing;         //spacing
  PointType     m_Origin;          //origin
  DirectionType m_Direction;       //direciton

};

}


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGenerateImageSource.hxx"
#endif


#endif //__itkGenerateImageSource_h
