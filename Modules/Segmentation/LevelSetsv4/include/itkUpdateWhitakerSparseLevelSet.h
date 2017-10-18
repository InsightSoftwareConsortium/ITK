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

#ifndef itkUpdateWhitakerSparseLevelSet_h
#define itkUpdateWhitakerSparseLevelSet_h

#include "itkImage.h"
#include "itkDiscreteLevelSetImage.h"
#include "itkWhitakerSparseLevelSetImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkLabelImageToLabelMapFilter.h"

namespace itk
{
/**
 *  \class UpdateWhitakerSparseLevelSet
 *  \brief Base class for updating the level-set function
 *
 *  \tparam VDimension Dimension of the input space
 *  \tparam TLevelSetValueType Output type (float or double) of the levelset function
 *  \tparam TEquationContainer Container of the system of levelset equations
 *  \ingroup ITKLevelSetsv4
 */
template< unsigned int VDimension,
          typename TLevelSetValueType,
          typename TEquationContainer >
class ITK_TEMPLATE_EXPORT UpdateWhitakerSparseLevelSet : public Object
{
public:
  typedef UpdateWhitakerSparseLevelSet  Self;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;
  typedef Object                        Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( UpdateWhitakerSparseLevelSet, Object );

  itkStaticConstMacro( ImageDimension, unsigned int, VDimension );

  typedef TLevelSetValueType  LevelSetOutputType;

  typedef WhitakerSparseLevelSetImage< LevelSetOutputType, ImageDimension >
                                                       LevelSetType;
  typedef typename LevelSetType::Pointer               LevelSetPointer;
  typedef typename LevelSetType::InputType             LevelSetInputType;
  typedef typename LevelSetType::OffsetType            LevelSetOffsetType;

  typedef typename LevelSetType::LabelMapType          LevelSetLabelMapType;
  typedef typename LevelSetType::LabelMapPointer       LevelSetLabelMapPointer;

  typedef typename LevelSetType::LabelObjectType       LevelSetLabelObjectType;
  typedef typename LevelSetType::LabelObjectPointer    LevelSetLabelObjectPointer;
  typedef typename LevelSetType::LabelObjectLengthType LevelSetLabelObjectLengthType;
  typedef typename LevelSetType::LabelObjectLineType   LevelSetLabelObjectLineType;

  typedef typename LevelSetType::LayerIdType           LevelSetLayerIdType;
  typedef typename LevelSetType::LayerType             LevelSetLayerType;
  typedef typename LevelSetType::LayerIterator         LevelSetLayerIterator;
  typedef typename LevelSetType::LayerConstIterator    LevelSetLayerConstIterator;
  typedef typename LevelSetType::OutputRealType        LevelSetOutputRealType;

  typedef typename LevelSetType::LayerMapType           LevelSetLayerMapType;
  typedef typename LevelSetType::LayerMapIterator       LevelSetLayerMapIterator;
  typedef typename LevelSetType::LayerMapConstIterator  LevelSetLayerMapConstIterator;

  typedef TEquationContainer                      EquationContainerType;
  typedef typename EquationContainerType::Pointer EquationContainerPointer;

  typedef typename EquationContainerType::TermContainerType     TermContainerType;
  typedef typename EquationContainerType::TermContainerPointer  TermContainerPointer;

  typedef Image< LevelSetLayerIdType, ImageDimension >  LabelImageType;
  typedef typename LabelImageType::Pointer              LabelImagePointer;

  typedef LabelMapToLabelImageFilter< LevelSetLabelMapType, LabelImageType >  LabelMapToLabelImageFilterType;
  typedef LabelImageToLabelMapFilter< LabelImageType, LevelSetLabelMapType >  LabelImageToLabelMapFilterType;

  itkGetModifiableObjectMacro(OutputLevelSet, LevelSetType );

  /** Update function for initializing and computing the output level set */
  void Update();

  /** Set/Get the sparse levet set image */
  itkSetObjectMacro( InputLevelSet, LevelSetType );
  itkGetModifiableObjectMacro(InputLevelSet, LevelSetType );

  /** Set/Get the TimeStep for the update */
  itkSetMacro( TimeStep, LevelSetOutputType );
  itkGetMacro( TimeStep, LevelSetOutputType );

  /** Set/Get the RMS change for the update */
  itkGetMacro( RMSChangeAccumulator, LevelSetOutputType );

  /** Set/Get the Equation container for computing the update */
  itkSetObjectMacro( EquationContainer, EquationContainerType );
  itkGetModifiableObjectMacro(EquationContainer, EquationContainerType );

  /** Set/Get the current level set id */
  itkSetMacro( CurrentLevelSetId, IdentifierType );
  itkGetMacro( CurrentLevelSetId, IdentifierType );

  /** Set the update map for all points in the zero layer */
  void SetUpdate( const LevelSetLayerType& update );

protected:
  UpdateWhitakerSparseLevelSet();
  virtual ~UpdateWhitakerSparseLevelSet() ITK_OVERRIDE;

  /** Update zero level set layer by moving relevant points to layers -1 or 1 */
  void UpdateLayerZero();

  /** Update -1 level set layer by moving relevant points to layers -2 or 0 */
  void UpdateLayerMinus1();

  /** Update +1 level set layer by moving relevant points to layers 0 or 2 */
  void UpdateLayerPlus1();

  /** Update zero level set layer by moving relevant points to layers -3 or -1 */
  void UpdateLayerMinus2();

  /** Update +2 level set layer by moving relevant points to layers 1 or 3 */
  void UpdateLayerPlus2();

  /** Move identified points into 0 level set layer */
  void MovePointIntoZeroLevelSet();

  /** Move identified points into -1 level set layer */
  void MovePointFromMinus1();

  /** Move identified points into +1 level set layer */
  void MovePointFromPlus1();

  /** Move identified points into -2 level set layer */
  void MovePointFromMinus2();

  /** Move identified points into +2 level set layer */
  void MovePointFromPlus2();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(UpdateWhitakerSparseLevelSet);

  LevelSetOutputType m_TimeStep;
  LevelSetOutputType m_RMSChangeAccumulator;
  IdentifierType     m_CurrentLevelSetId;

  EquationContainerPointer m_EquationContainer;

  LevelSetLayerType  m_Update;
  LevelSetPointer    m_InputLevelSet;
  LevelSetPointer    m_OutputLevelSet;

  LevelSetPointer   m_TempLevelSet;
  LevelSetLayerType m_TempPhi;

  LevelSetLayerIdType m_MinStatus;
  LevelSetLayerIdType m_MaxStatus;

  LabelImagePointer m_InternalImage;

  LevelSetOffsetType m_Offset;

  typedef ShapedNeighborhoodIterator< LabelImageType > NeighborhoodIteratorType;

  typedef std::pair< LevelSetInputType, LevelSetOutputType > NodePairType;
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUpdateWhitakerSparseLevelSet.hxx"
#endif
#endif // itkUpdateWhitakerSparseLevelSet_h
