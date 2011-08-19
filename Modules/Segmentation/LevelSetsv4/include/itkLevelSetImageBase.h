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

#ifndef __itkLevelSetImageBase_h
#define __itkLevelSetImageBase_h

#include "itkLevelSetBase.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 *  \class LevelSetImageBase
 *  \brief Base class for the image representation of a level-set function
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
class LevelSetImageBase :
  public LevelSetBase<
      typename TImage::IndexType,
      TImage::ImageDimension,
      typename TImage::PixelType,
      TImage >
  {
public:
  typedef TImage                         ImageType;
  typedef typename ImageType::Pointer    ImagePointer;
  typedef typename ImageType::IndexType  IndexType;
  typedef typename ImageType::PixelType  PixelType;
  typedef typename ImageType::RegionType RegionType;

  typedef LevelSetImageBase                           Self;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;
  typedef LevelSetBase< IndexType,
    ImageType::ImageDimension, PixelType, ImageType > Superclass;

  /** Method for creation through object factory */
  itkNewMacro ( Self );

  /** Run-time type information */
  itkTypeMacro ( LevelSetImageBase, LevelSetBase );

  itkStaticConstMacro ( Dimension, unsigned int, Superclass::Dimension );

  typedef typename Superclass::InputType        InputType;
  typedef typename Superclass::OutputType       OutputType;
  typedef typename Superclass::OutputRealType   OutputRealType;
  typedef typename Superclass::GradientType     GradientType;
  typedef typename Superclass::HessianType      HessianType;
  typedef typename Superclass::LevelSetDataType LevelSetDataType;

  virtual void SetImage( ImageType* iImage );
  itkGetObjectMacro( Image, ImageType );

  /** Returns the value of the level set function at a given location iP */
  virtual OutputType Evaluate( const InputType& iP ) const;

  /** Returns the image gradient of the level set function at a given location iP */
  virtual GradientType EvaluateGradient( const InputType& iP ) const;

  /** Returns the image hessian of the level set function at a given location iP */
  virtual HessianType EvaluateHessian( const InputType& iP ) const;

  /** Returns the image Laplacian of the level set function at a given location iP */
  virtual OutputRealType EvaluateLaplacian( const InputType& iP ) const;

  /** Returns the value of the level set function at a given location iP */
  virtual void Evaluate( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Returns the gradient of the level set function at a given location iP
   * as part of the LevelSetDataType */
  virtual void EvaluateGradient( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Returns the Hessian of the level set function at a given location iP
   * as part of the LevelSetDataType */
  virtual void EvaluateHessian( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Returns the Laplacian of the level set function at a given location iP
   * as part of the LevelSetDataType */
  virtual void EvaluateLaplacian( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Returns the gradient of the level set function at a given location iP
   * as part of the LevelSetDataType */
  virtual void EvaluateForwardGradient( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Returns the gradient of the level set function at a given location iP
   * as part of the LevelSetDataType */
  virtual void EvaluateBackwardGradient( const InputType& iP, LevelSetDataType& ioData ) const;

protected:
  LevelSetImageBase();

  virtual ~LevelSetImageBase();

  ImagePointer m_Image;

  typedef GradientType ScalingType;
  ScalingType m_NeighborhoodScales;

private:
  /** Initial the level set pointer */
  virtual void Initialize();

  /** Copy level set information from data object */
  virtual void CopyInformation(const DataObject *data);

  /** Graft data object as level set object */
  virtual void Graft( const DataObject* data );

  LevelSetImageBase( const Self& ); // purposely not implemented
  void operator = ( const Self& ); // purposely not implemented
  };
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetImageBase.hxx"
#endif

#endif // __itkLevelSetImageBase_h
