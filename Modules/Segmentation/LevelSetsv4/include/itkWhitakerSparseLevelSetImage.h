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

#ifndef itkWhitakerSparseLevelSetImage_h
#define itkWhitakerSparseLevelSetImage_h

#include "itkLevelSetSparseImage.h"
#include "itkLabelObject.h"
#include "itkLabelMap.h"

namespace itk
{
/**
 *  \class WhitakerSparseLevelSetImage
 *  \brief Derived class for the sparse-field representation of level-set function
 *
 *  This representation is a "sparse" level-set function, where values are
 *  real in between [ -3, +3 ] and organized into several layers { -2, -1,
 *  0, +1, +2 }.
 *
 *  \tparam TOutput Output type (float or double) of the level set function
 *  \tparam VDimension Dimension of the input space
 *  \ingroup ITKLevelSetsv4
 */
template< typename TOutput, unsigned int VDimension >
class ITK_TEMPLATE_EXPORT WhitakerSparseLevelSetImage :
    public LevelSetSparseImage< TOutput, VDimension >
{
public:
  typedef WhitakerSparseLevelSetImage                 Self;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;
  typedef LevelSetSparseImage< TOutput, VDimension >  Superclass;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(WhitakerSparseLevelSetImage, LevelSetSparseImage);

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

  /** Returns the value of the level set function at a given location iP */
  using Superclass::Evaluate;
  virtual OutputType Evaluate( const InputType& inputIndex ) const ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking

  itkConceptMacro( DoubleConvertible,
                    ( Concept::Convertible< OutputRealType, OutputType > ) );

  // End concept checking
#endif // ITK_USE_CONCEPT_CHECKING

  static inline LayerIdType MinusThreeLayer() { return -3; }
  static inline LayerIdType MinusTwoLayer() { return -2; }
  static inline LayerIdType MinusOneLayer() { return -1; }
  static inline LayerIdType ZeroLayer() { return 0; }
  static inline LayerIdType PlusOneLayer() { return 1; }
  static inline LayerIdType PlusTwoLayer() { return 2; }
  static inline LayerIdType PlusThreeLayer() { return 3; }

  /** Return the label object pointer with a given id */
  template< typename TLabel >
  typename LabelObject< TLabel, Dimension >::Pointer
  GetAsLabelObject()
    {
    typedef LabelObject< TLabel, Dimension > OutputLabelObjectType;
    typename OutputLabelObjectType::Pointer object = OutputLabelObjectType::New();

    for( LayerIdType status = this->MinusThreeLayer(); status < this->PlusOneLayer(); ++status )
      {
      LabelObjectPointer labelObject = this->m_LabelMap->GetLabelObject( status );

      for( SizeValueType i = 0; i < labelObject->GetNumberOfLines(); ++i )
        {
        object->AddLine( labelObject->GetLine( i ) );
        }
      }
    object->Optimize();

    return object;
    }

protected:
  WhitakerSparseLevelSetImage();
  virtual ~WhitakerSparseLevelSetImage() ITK_OVERRIDE;

  /** Initialize the sparse field layers */
  virtual void InitializeLayers() ITK_OVERRIDE;

  virtual void InitializeInternalLabelList() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(WhitakerSparseLevelSetImage);

};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWhitakerSparseLevelSetImage.hxx"
#endif

#endif // itkWhitakerSparseLevelSetImage_h
