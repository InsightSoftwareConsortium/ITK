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

#ifndef __itkShiSparseLevelSetBase_h
#define __itkShiSparseLevelSetBase_h

#include "itkLevelSetBase.h"
#include "itkLabelObject.h"
#include "itkLabelMap.h"

namespace itk
{
/**
 *  \class ShiSparseLevelSetBase
 *  \brief Derived class for the shi representation of level-set function
 *
 *  This representation is a "sparse" level-set function, where values could
 *  only be { -3, -1, +1, +3 } and organized into 2 layers { -1, +1 }.
 *
 *  \tparam VDimension Dimension of the input space
 *  \ingroup ITKLevelSetsv4
 */
template< unsigned int VDimension >
class ShiSparseLevelSetBase :
    public LevelSetBase< Index< VDimension >,
                         VDimension,
                         int8_t,
                         ImageBase< VDimension > >
{
public:
  typedef Index< VDimension >                     IndexType;
  typedef int8_t                                  OutputType;
  typedef ImageBase< VDimension >                 ImageBaseType;

  typedef ShiSparseLevelSetBase                   Self;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef LevelSetBase< IndexType, VDimension,
                        OutputType, ImageBaseType >
                                                  Superclass;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShiSparseLevelSetBase, LevelSetBase);

  itkStaticConstMacro ( Dimension, unsigned int,
                        VDimension );

  typedef typename Superclass::InputType        InputType;
  typedef typename Superclass::OutputRealType   OutputRealType;
  typedef typename Superclass::GradientType     GradientType;
  typedef typename Superclass::HessianType      HessianType;
  typedef typename Superclass::LevelSetDataType LevelSetDataType;

  typedef int8_t                                 LayerIdType;
  typedef LabelObject< LayerIdType, VDimension > LabelObjectType;
  typedef typename LabelObjectType::Pointer      LabelObjectPointer;
  typedef typename LabelObjectType::LengthType   LabelObjectLengthType;
  typedef typename LabelObjectType::LineType     LabelObjectLineType;

  typedef LabelMap< LabelObjectType >         LabelMapType;
  typedef typename LabelMapType::Pointer      LabelMapPointer;
  typedef typename LabelMapType::RegionType   RegionType;

  typedef std::map< IndexType, OutputType, Functor::IndexLexicographicCompare< VDimension > >
                                                  LayerType;
  typedef typename LayerType::iterator            LayerIterator;
  typedef typename LayerType::const_iterator      LayerConstIterator;

  typedef std::map< LayerIdType, LayerType >      LayerMapType;
  typedef typename LayerMapType::iterator         LayerMapIterator;
  typedef typename LayerMapType::const_iterator   LayerMapConstIterator;

  /** Returns the layer affiliation of a given location iP */
  virtual LayerIdType Status( const InputType& iP ) const;

  /** Returns the value of the level set function at a given location iP */
  virtual OutputType Evaluate( const InputType& iP ) const;

  /** Returns the gradient of the level set function at a given location iP */
  virtual GradientType EvaluateGradient( const InputType& iP ) const;

  /** Returns the Hessian of the level set function at a given location iP */
  virtual HessianType EvaluateHessian( const InputType& iP ) const;

  /** Returns the Laplacian of the level set function at a given location iP */
  virtual OutputRealType EvaluateLaplacian( const InputType& iP ) const;

  virtual void Evaluate( const InputType& iP, LevelSetDataType& ioData ) const;
  virtual void EvaluateGradient( const InputType& iP, LevelSetDataType& ioData ) const;
  virtual void EvaluateHessian( const InputType& iP, LevelSetDataType& ioData ) const;
  virtual void EvaluateLaplacian( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Returns the gradient of the level set function at a given location iP
   * as part of the LevelSetDataType
   * \todo to be implemented */
  virtual void EvaluateForwardGradient( const InputType& iP, LevelSetDataType& ioData ) const;

  /** Returns the gradient of the level set function at a given location iP
   * as part of the LevelSetDataType
   * \todo to be implemented */
  virtual void EvaluateBackwardGradient( const InputType& iP, LevelSetDataType& ioData ) const;

  static inline LayerIdType MinusThreeLayer() { return -3; }
  static inline LayerIdType MinusOneLayer() { return -1; }
  static inline LayerIdType PlusOneLayer() { return 1; }
  static inline LayerIdType PlusThreeLayer() { return 3; }

  /** Return the const pointer to a layer map with given id  */
  const LayerType& GetLayer( LayerIdType iVal ) const;

  /** Return the pointer to a layer map with given id  */
  LayerType& GetLayer( LayerIdType iVal );

  /** Set a layer map with id to the given layer pointer */
  void SetLayer( LayerIdType iVal, const LayerType& iLayer );

  /** Return the label object pointer with a given id */
  template< class TLabel >
  typename LabelObject< TLabel, Dimension >::Pointer
  GetAsLabelObject()
    {
    typedef LabelObject< TLabel, Dimension > OutputLabelObjectType;
    typename OutputLabelObjectType::Pointer object = OutputLabelObjectType::New();

    LabelObjectPointer labelObject = m_LabelMap->GetLabelObject( MinusThreeLayer() );

    for( SizeValueType i = 0; i < labelObject->GetNumberOfLines(); i++ )
      {
      object->AddLine( labelObject->GetLine( i ) );
      }

    labelObject = m_LabelMap->GetLabelObject( MinusOneLayer() );

    for( SizeValueType i = 0; i < labelObject->GetNumberOfLines(); i++ )
      {
      object->AddLine( labelObject->GetLine( i ) );
      }
    object->Optimize();

    return object;
    }

  /** Set/Get the label map for computing the sparse representation */
  virtual void SetLabelMap( LabelMapType* iLabelMap );
  itkGetObjectMacro( LabelMap, LabelMapType );

  /** Graft data object as level set object */
  virtual void Graft( const DataObject* data );

protected:

  ShiSparseLevelSetBase();

  virtual ~ShiSparseLevelSetBase();

  LayerMapType     m_Layers;
  LabelMapPointer  m_LabelMap;

  typedef GradientType ScalingType;
  ScalingType m_NeighborhoodScales;

  /** Initialize the sparse field layers */
  void InitializeLayers();

private:
  /** Initialize the label map point and the sparse-field layers */
  virtual void Initialize();

  /** Copy level set information from data object */
  virtual void CopyInformation( const DataObject* data );

  ShiSparseLevelSetBase( const Self& ); //purposely not implemented
  void operator = ( const Self& ); //purposely not implemented
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShiSparseLevelSetBase.hxx"
#endif

#endif // __itkShiSparseLevelSetBase_h
