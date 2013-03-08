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

#ifndef __itkLevelSetSparseImage_h
#define __itkLevelSetSparseImage_h

#include "itkDiscreteLevelSetImage.h"
#include "itkObjectFactory.h"

#include "itkLabelObject.h"
#include "itkLabelMap.h"

namespace itk
{
/**
 *  \class LevelSetSparseImage
 *  \brief Base class for the sparse representation of a level-set function on one Image.
 *
 *  \tparam TImage Input image type of the level set function
 *  \todo Think about using image iterators instead of GetPixel()
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TOutput, unsigned int VDimension >
class LevelSetSparseImage :
  public DiscreteLevelSetImage< TOutput, VDimension >
{
public:
  typedef LevelSetSparseImage                           Self;
  typedef SmartPointer< Self >                          Pointer;
  typedef SmartPointer< const Self >                    ConstPointer;
  typedef DiscreteLevelSetImage< TOutput, VDimension >  Superclass;

  /** Run-time type information */
  itkTypeMacro ( LevelSetSparseImage, DiscreteLevelSetImage );

  itkStaticConstMacro ( Dimension, unsigned int, Superclass::Dimension );

  typedef typename Superclass::InputType        InputType;
  typedef typename Superclass::OutputType       OutputType;
  typedef typename Superclass::OutputRealType   OutputRealType;
  typedef typename Superclass::GradientType     GradientType;
  typedef typename Superclass::HessianType      HessianType;
  typedef typename Superclass::LevelSetDataType LevelSetDataType;

  typedef int8_t                                  LayerIdType;
  typedef std::list< LayerIdType >                LayerIdListType;

  typedef LabelObject< LayerIdType, VDimension >  LabelObjectType;
  typedef typename LabelObjectType::Pointer       LabelObjectPointer;
  typedef typename LabelObjectType::LengthType    LabelObjectLengthType;
  typedef typename LabelObjectType::LineType      LabelObjectLineType;

  typedef LabelMap< LabelObjectType >         LabelMapType;
  typedef typename LabelMapType::Pointer      LabelMapPointer;
  typedef typename LabelMapType::RegionType   RegionType;

  typedef std::map< InputType, OutputType,
                    Functor::IndexLexicographicCompare< VDimension > >
                                                  LayerType;
  typedef typename LayerType::iterator            LayerIterator;
  typedef typename LayerType::const_iterator      LayerConstIterator;

  typedef std::map< LayerIdType, LayerType >      LayerMapType;
  typedef typename LayerMapType::iterator         LayerMapIterator;
  typedef typename LayerMapType::const_iterator   LayerMapConstIterator;

  /** Returns the layer affiliation of a given location iP */
  virtual LayerIdType Status( const InputType& iP ) const;

  /** Return the const pointer to a layer map with given id  */
  const LayerType& GetLayer( LayerIdType iVal ) const;

  /** Return the pointer to a layer map with given id  */
  LayerType& GetLayer( LayerIdType iVal );

  /** Set a layer map with id to the given layer pointer */
  void SetLayer( LayerIdType iVal, const LayerType& iLayer );

  /** Set/Get the label map for computing the sparse representation */
  virtual void SetLabelMap( LabelMapType* iLabelMap );
  itkGetModifiableObjectMacro(LabelMap, LabelMapType );

  /** Graft data object as level set object */
  virtual void Graft( const DataObject* data );

  /** Return the label object pointer with a given id */
  template< class TLabel >
  typename LabelObject< TLabel, VDimension >::Pointer GetAsLabelObject();

protected:
  LevelSetSparseImage();

  virtual ~LevelSetSparseImage();

  LayerMapType      m_Layers;
  LabelMapPointer   m_LabelMap;
  LayerIdListType   m_InternalLabelList;

  /** Initialize the sparse field layers */
  virtual void InitializeLayers() = 0;

  virtual void InitializeInternalLabelList() = 0;

  virtual bool IsInsideDomain( const InputType& iP ) const;

  /** Initialize the label map point and the sparse-field layers */
  virtual void Initialize();

  /** Copy level set information from data object */
  virtual void CopyInformation( const DataObject* data );

private:

  LevelSetSparseImage( const Self& ); // purposely not implemented
  void operator = ( const Self& ); // purposely not implemented
  };
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetSparseImage.hxx"
#endif

#endif // __itkLevelSetSparseImage_h
