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

#ifndef itkMalcolmSparseLevelSetImage_h
#define itkMalcolmSparseLevelSetImage_h

#include "itkImage.h"
#include "itkLevelSetSparseImage.h"

#include "itkLabelObject.h"
#include "itkLabelMap.h"

namespace itk
{
/**
 *  \class MalcolmSparseLevelSetImage
 *  \brief Derived class for the Malcolm representation of level-set function
 *
 *  This representation is a "sparse" level-set function, where values could
 *  only be { -1, 0, +1 } and organized into 1 layer { 0 }.
 *
 *  \tparam VDimension Dimension of the input space
 *  \ingroup ITKLevelSetsv4
 */
template< unsigned int VDimension >
class ITK_TEMPLATE_EXPORT MalcolmSparseLevelSetImage :
    public LevelSetSparseImage< int8_t, VDimension >
{
public:
  typedef MalcolmSparseLevelSetImage                Self;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;
  typedef LevelSetSparseImage< int8_t, VDimension > Superclass;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MalcolmSparseLevelSetImage, LevelSetSparseImage);

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

  /** Returns the value of the level set function at a given location inputPixel */
  using Superclass::Evaluate;
  virtual OutputType Evaluate( const InputType& inputPixel ) const ITK_OVERRIDE;

  /** Returns the Hessian of the level set function at a given location inputPixel */
  virtual HessianType EvaluateHessian( const InputType& inputPixel ) const ITK_OVERRIDE;

  /** Returns the Laplacian of the level set function at a given location inputPixel */
  virtual OutputRealType EvaluateLaplacian( const InputType& inputPixel ) const ITK_OVERRIDE;

  /** Returns the MeanCurvature of the level set function at a given location inputPixel */
  virtual OutputRealType EvaluateMeanCurvature( const InputType& inputPixel ) const ITK_OVERRIDE;

  virtual void EvaluateHessian( const InputType& inputPixel, LevelSetDataType& data ) const ITK_OVERRIDE;
  virtual void EvaluateLaplacian( const InputType& inputPixel, LevelSetDataType& data ) const ITK_OVERRIDE;
  virtual void EvaluateMeanCurvature( const InputType& inputPixel, LevelSetDataType& data ) const ITK_OVERRIDE;

  static inline LayerIdType MinusOneLayer() { return -1; }
  static inline LayerIdType ZeroLayer() { return 0; }
  static inline LayerIdType PlusOneLayer() { return 1; }

protected:

  MalcolmSparseLevelSetImage();

  virtual ~MalcolmSparseLevelSetImage() ITK_OVERRIDE;

  /** Initialize the sparse field layers */
  virtual void InitializeLayers() ITK_OVERRIDE;

  virtual void InitializeInternalLabelList() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MalcolmSparseLevelSetImage);
};
}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMalcolmSparseLevelSetImage.hxx"
#endif

#endif // itkMalcolmSparseLevelSetImage_h
