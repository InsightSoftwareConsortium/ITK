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

#ifndef itkUpdateMalcolmSparseLevelSet_h
#define itkUpdateMalcolmSparseLevelSet_h

#include "itkImage.h"
#include "itkDiscreteLevelSetImage.h"
#include "itkMalcolmSparseLevelSetImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkLabelImageToLabelMapFilter.h"

namespace itk
{
/**
 *  \class UpdateMalcolmSparseLevelSet
 *  \brief Base class for updating the Malcolm representation of level-set function
 *
 *  \tparam VDimension Dimension of the input space
 *  \tparam TEquationContainer Container of the system of levelset equations
 *  \ingroup ITKLevelSetsv4
 */
template< unsigned int VDimension,
          typename TEquationContainer >
class ITK_TEMPLATE_EXPORT UpdateMalcolmSparseLevelSet : public Object
{
public:
  typedef UpdateMalcolmSparseLevelSet   Self;
  typedef SmartPointer< Self >          Pointer;
  typedef SmartPointer< const Self >    ConstPointer;
  typedef Object                        Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( UpdateMalcolmSparseLevelSet, Object );

  itkStaticConstMacro( ImageDimension, unsigned int, VDimension );

  typedef MalcolmSparseLevelSetImage< ImageDimension > LevelSetType;
  typedef typename LevelSetType::Pointer               LevelSetPointer;
  typedef typename LevelSetType::InputType             LevelSetInputType;
  typedef typename LevelSetType::OutputType            LevelSetOutputType;
  typedef typename LevelSetType::OffsetType            LevelSetOffsetType;

  typedef typename LevelSetType::LabelMapType          LevelSetLabelMapType;
  typedef typename LevelSetType::LabelMapPointer       LevelSetLabelMapPointer;

  typedef typename LevelSetType::LabelObjectType       LevelSetLabelObjectType;
  typedef typename LevelSetType::LabelObjectPointer    LevelSetLabelObjectPointer;
  typedef typename LevelSetType::LabelObjectLengthType LevelSetLabelObjectLengthType;
  typedef typename LevelSetType::LabelObjectLineType   LevelSetLabelObjectLineType;

  typedef typename LevelSetType::LayerType             LevelSetLayerType;
  typedef typename LevelSetType::LayerIterator         LevelSetLayerIterator;
  typedef typename LevelSetType::LayerConstIterator    LevelSetLayerConstIterator;
  typedef typename LevelSetType::OutputRealType        LevelSetOutputRealType;

  typedef typename LevelSetType::LayerMapType           LevelSetLayerMapType;
  typedef typename LevelSetType::LayerMapIterator       LevelSetLayerMapIterator;
  typedef typename LevelSetType::LayerMapConstIterator  LevelSetLayerMapConstIterator;

  typedef TEquationContainer                                    EquationContainerType;
  typedef typename EquationContainerType::Pointer               EquationContainerPointer;
  typedef typename EquationContainerType::TermContainerPointer  TermContainerPointer;

  itkGetModifiableObjectMacro(OutputLevelSet, LevelSetType );

  /** Update function for initializing and computing the output level set */
  void Update();

  /** Set/Get the sparse levet set image */
  itkSetObjectMacro( InputLevelSet, LevelSetType );
  itkGetModifiableObjectMacro(InputLevelSet, LevelSetType );

  /** Set/Get the RMS change for the update */
  itkGetMacro( RMSChangeAccumulator, LevelSetOutputRealType );

  /** Set/Get the Equation container for computing the update */
  itkSetObjectMacro( EquationContainer, EquationContainerType );
  itkGetModifiableObjectMacro(EquationContainer, EquationContainerType );

  /** Set/Get the current level set id */
  itkSetMacro( CurrentLevelSetId, IdentifierType );
  itkGetMacro( CurrentLevelSetId, IdentifierType );

protected:
  UpdateMalcolmSparseLevelSet();
  virtual ~UpdateMalcolmSparseLevelSet() ITK_OVERRIDE;

  // output
  LevelSetPointer   m_OutputLevelSet;

  LevelSetLayerType m_Update;

  IdentifierType           m_CurrentLevelSetId;
  LevelSetOutputRealType   m_RMSChangeAccumulator;
  EquationContainerPointer m_EquationContainer;

  typedef Image< int8_t, ImageDimension >   LabelImageType;
  typedef typename LabelImageType::Pointer  LabelImagePointer;

  LabelImagePointer m_InternalImage;

  typedef ShapedNeighborhoodIterator< LabelImageType > NeighborhoodIteratorType;

  bool m_IsUsingUnPhasedPropagation;

  /** Compute the updates for all points in the 0 layer and store in UpdateContainer */
  void FillUpdateContainer();

  /** Update the zero layer for all points with values stored in UpdateContainer
   *  Move points to -1 or +1 layers */
  void EvolveWithUnPhasedPropagation();

  /** Update separately the zero layer for points with positive/negative update values
   *  Move points to -1 or +1 layers */
  void EvolveWithPhasedPropagation( LevelSetLayerType& ioList,
                                    LevelSetLayerType& ioUpdate,
                                    const bool& iContraction );

  /** Make sure the layers are of single pixel thickness only. This method is related
    to the minimal interface function described in the original paper. */
  void CompactLayersToSinglePixelThickness();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(UpdateMalcolmSparseLevelSet);

  // input
  LevelSetPointer   m_InputLevelSet;

  LevelSetOffsetType m_Offset;

  typedef std::pair< LevelSetInputType, LevelSetOutputType > NodePairType;

};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUpdateMalcolmSparseLevelSet.hxx"
#endif

#endif // itkUpdateMalcolmSparseLevelSet_h
