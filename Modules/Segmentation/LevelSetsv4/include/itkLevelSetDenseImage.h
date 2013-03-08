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

#ifndef __itkLevelSetDenseImage_h
#define __itkLevelSetDenseImage_h

#include "itkDiscreteLevelSetImage.h"

namespace itk
{
/**
 *  \class LevelSetDenseImage
 *  \brief Base class for the "dense" representation of a level-set function on
 *  one image.
 *
 *  This representation is a "dense" level-set function, i.e. it defines
 *  a level-set function on a grid (more precesily the underlying structure
 *  is an Image).
 *
 *  \tparam TImage Input image type of the level set function
 *  \todo Think about using image iterators instead of GetPixel()
 *
 *  \ingroup ITKLevelSetsv4
 */
template< class TImage >
class LevelSetDenseImage :
  public DiscreteLevelSetImage< typename TImage::PixelType, TImage::ImageDimension >
  {
public:
  typedef TImage                         ImageType;
  typedef typename ImageType::Pointer    ImagePointer;
  typedef typename ImageType::IndexType  IndexType;
  typedef typename ImageType::PixelType  PixelType;
  typedef typename ImageType::RegionType RegionType;

  typedef LevelSetDenseImage                                            Self;
  typedef SmartPointer< Self >                                          Pointer;
  typedef SmartPointer< const Self >                                    ConstPointer;
  typedef DiscreteLevelSetImage< PixelType, ImageType::ImageDimension > Superclass;

  /** Method for creation through object factory */
  itkNewMacro ( Self );

  /** Run-time type information */
  itkTypeMacro ( LevelSetDenseImage, DiscreteLevelSetImage );

  itkStaticConstMacro ( Dimension, unsigned int, Superclass::Dimension );

  typedef typename Superclass::InputType        InputType;
  typedef typename Superclass::OutputType       OutputType;
  typedef typename Superclass::OutputRealType   OutputRealType;
  typedef typename Superclass::GradientType     GradientType;
  typedef typename Superclass::HessianType      HessianType;
  typedef typename Superclass::LevelSetDataType LevelSetDataType;

  virtual void SetImage( ImageType* iImage );
  itkGetModifiableObjectMacro(Image, ImageType );

  /** Returns the value of the level set function at a given location iP */
  virtual OutputType Evaluate( const InputType& iP ) const;
  virtual void Evaluate( const InputType& iP, LevelSetDataType& ioData ) const;

protected:
  LevelSetDenseImage();

  virtual ~LevelSetDenseImage();

  ImagePointer m_Image;

  virtual bool IsInsideDomain( const InputType& iP ) const;

  /** Initial the level set pointer */
  virtual void Initialize();

  /** Copy level set information from data object */
  virtual void CopyInformation(const DataObject *data);

  /** Graft data object as level set object */
  virtual void Graft( const DataObject* data );

private:

  LevelSetDenseImage( const Self& ); // purposely not implemented
  void operator = ( const Self& ); // purposely not implemented
  };
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetDenseImage.hxx"
#endif

#endif // __itkLevelSetDenseImage_h
