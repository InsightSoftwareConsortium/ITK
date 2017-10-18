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

#ifndef itkDiscreteLevelSetImage_h
#define itkDiscreteLevelSetImage_h

#include "itkLevelSetImage.h"

namespace itk
{
/**
 *  \class DiscreteLevelSetImage
 *  \brief Abstract class for a level-set function on one Image.
 *
 *  \tparam TOutput OutputType of the level-set function value
 *  \tparam VDimension Dimension of the underlying Image.
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TOutput, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT DiscreteLevelSetImage :
  public LevelSetImage< Index< VDimension >, VDimension, TOutput >
{
public:
  typedef Index< VDimension >             IndexType;

  typedef DiscreteLevelSetImage                           Self;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;
  typedef LevelSetImage< IndexType, VDimension, TOutput > Superclass;

  /** Run-time type information */
  itkTypeMacro ( DiscreteLevelSetImage, LevelSetImage );

  itkStaticConstMacro ( Dimension, unsigned int, Superclass::Dimension );

  typedef typename Superclass::InputType        InputType;
  typedef typename Superclass::OutputType       OutputType;
  typedef typename Superclass::OutputRealType   OutputRealType;
  typedef typename Superclass::GradientType     GradientType;
  typedef typename Superclass::HessianType      HessianType;
  typedef typename Superclass::LevelSetDataType LevelSetDataType;

  /** Returns the gradient of the level set function at a given location inputIndex */
  virtual OutputType  Evaluate( const InputType& inputIndex ) const ITK_OVERRIDE = 0;

  /** Returns the image gradient of the level set function at a given location inputIndex */
  virtual GradientType EvaluateGradient( const InputType& inputIndex ) const ITK_OVERRIDE;

  /** Returns the image hessian of the level set function at a given location inputIndex */
  virtual HessianType EvaluateHessian( const InputType& inputIndex ) const ITK_OVERRIDE;

  /** Returns the image Laplacian of the level set function at a given location inputIndex */
  virtual OutputRealType EvaluateLaplacian( const InputType& inputIndex ) const ITK_OVERRIDE;

  /** Returns the mean curvature of the level set function at a given location inputIndex */
  virtual OutputRealType EvaluateMeanCurvature( const InputType& inputIndex ) const ITK_OVERRIDE;

  virtual GradientType EvaluateForwardGradient( const InputType& inputIndex ) const;

  virtual GradientType EvaluateBackwardGradient( const InputType& inputIndex ) const;

  /** Returns the value of the level set function at a given location inputIndex */
  virtual void Evaluate( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;

  /** Returns the gradient of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  virtual void EvaluateGradient( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;

  /** Returns the Hessian of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  virtual void EvaluateHessian( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;

  /** Returns the Hessian of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  virtual void EvaluateMeanCurvature( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;

  /** Returns the Laplacian of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  virtual void EvaluateLaplacian( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;

  /** Returns the gradient of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  virtual void EvaluateForwardGradient( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;

  /** Returns the gradient of the level set function at a given location inputIndex
   * as part of the LevelSetDataType */
  virtual void EvaluateBackwardGradient( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;

protected:
  DiscreteLevelSetImage();

  virtual ~DiscreteLevelSetImage() ITK_OVERRIDE;

  /** Initial the level set pointer */
  virtual void Initialize() ITK_OVERRIDE;

  /** Copy level set information from data object */
  virtual void CopyInformation(const DataObject *data) ITK_OVERRIDE;

  /** Graft data object as level set object */
  virtual void Graft( const DataObject* data ) ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(DiscreteLevelSetImage);
  };
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDiscreteLevelSetImage.hxx"
#endif

#endif // itkDiscreteLevelSetImage_h
