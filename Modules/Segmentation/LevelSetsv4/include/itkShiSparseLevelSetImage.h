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

#ifndef itkShiSparseLevelSetImage_h
#define itkShiSparseLevelSetImage_h

#include "itkLevelSetSparseImage.h"

namespace itk
{
/**
 *  \class ShiSparseLevelSetImage
 *  \brief Derived class for the shi representation of level-set function
 *
 *  This representation is a "sparse" level-set function, where values could
 *  only be { -3, -1, +1, +3 } and organized into 2 layers { -1, +1 }.
 *
 *  \tparam VDimension Dimension of the input space
 *  \ingroup ITKLevelSetsv4
 */
template< unsigned int VDimension >
class ITK_TEMPLATE_EXPORT ShiSparseLevelSetImage :
    public LevelSetSparseImage< int8_t, VDimension >
{
public:
  typedef ShiSparseLevelSetImage                  Self;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef LevelSetSparseImage< int8_t, VDimension >
                                                  Superclass;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShiSparseLevelSetImage, LevelSetSparseImage);

  itkStaticConstMacro ( Dimension, unsigned int, VDimension );

  typedef typename Superclass::InputType        InputType;
  typedef typename Superclass::OutputType       OutputType;
  typedef typename Superclass::OutputRealType   OutputRealType;
  typedef typename Superclass::GradientType     GradientType;
  typedef typename Superclass::HessianType      HessianType;
  typedef typename Superclass::LevelSetDataType LevelSetDataType;

  typedef typename Superclass::LayerIdType            LayerIdType;
  typedef typename Superclass::LabelObjectType        LabelObjectType;
  typedef typename Superclass::LabelObjectPointer     LabelObjectPointer;
  typedef typename Superclass::LabelObjectLengthType  LabelObjectLengthType;
  typedef typename Superclass::LabelObjectLineType    LabelObjectLineType;

  typedef typename Superclass::LabelMapType     LabelMapType;
  typedef typename Superclass::LabelMapPointer  LabelMapPointer;
  typedef typename Superclass::RegionType       RegionType;

  typedef typename Superclass::LayerType          LayerType;
  typedef typename Superclass::LayerIterator      LayerIterator;
  typedef typename Superclass::LayerConstIterator LayerConstIterator;

  typedef typename Superclass::LayerMapType           LayerMapType;
  typedef typename Superclass::LayerMapIterator       LayerMapIterator;
  typedef typename Superclass::LayerMapConstIterator  LayerMapConstIterator;

  /** Returns the value of the level set function at a given location inputIndex */
  using Superclass::Evaluate;
  virtual OutputType Evaluate( const InputType& inputIndex ) const ITK_OVERRIDE;

  /** Returns the Hessian of the level set function at a given location inputIndex */
  virtual HessianType EvaluateHessian( const InputType& inputIndex ) const ITK_OVERRIDE;

  /** Returns the Laplacian of the level set function at a given location inputIndex */
  virtual OutputRealType EvaluateLaplacian( const InputType& inputIndex ) const ITK_OVERRIDE;

  /** Returns the Laplacian of the level set function at a given location inputIndex */
  virtual OutputRealType EvaluateMeanCurvature( const InputType& inputIndex ) const ITK_OVERRIDE;

  virtual void EvaluateHessian( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;
  virtual void EvaluateLaplacian( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;
  virtual void EvaluateMeanCurvature( const InputType& inputIndex, LevelSetDataType& data ) const ITK_OVERRIDE;

  static inline LayerIdType MinusThreeLayer() { return -3; }
  static inline LayerIdType MinusOneLayer() { return -1; }
  static inline LayerIdType PlusOneLayer() { return 1; }
  static inline LayerIdType PlusThreeLayer() { return 3; }

protected:

  ShiSparseLevelSetImage();

  virtual ~ShiSparseLevelSetImage() ITK_OVERRIDE;

  /** Initialize the sparse field layers */
  virtual void InitializeLayers() ITK_OVERRIDE;

  virtual void InitializeInternalLabelList() ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(ShiSparseLevelSetImage);
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShiSparseLevelSetImage.hxx"
#endif

#endif // itkShiSparseLevelSetImage_h
