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

#ifndef __itkBinaryImageToWhitakerSparseLevelSetAdaptor_h
#define __itkBinaryImageToWhitakerSparseLevelSetAdaptor_h

#include "itkImage.h"
#include "itkLevelSetImageBase.h"
#include "itkWhitakerSparseLevelSetBase.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include <list>
#include "itkObject.h"

namespace itk
{
/** \class BinaryImageToWhitakerSparseLevelSetAdaptor
 *  \brief Convenient adaptor class to convert a binary mask into a sparse leve-set
 *  function.
 *
 *  \tparam TInputImage Input image to be converted
 *  \tparam TLevelSetValueType Ouput value type for the level-set function
 *
 *  \todo Make it as filter to benefit from the pipeline architecture
 *  \ingroup ITKLevelSetsv4
 */
template< class TInputImage, typename TLevelSetValueType >
class BinaryImageToWhitakerSparseLevelSetAdaptor : public Object
{
public:
  typedef BinaryImageToWhitakerSparseLevelSetAdaptor  Self;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;
  typedef Object                                      Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( BinaryImageToWhitakerSparseLevelSetAdaptor, Object );

  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef typename InputImageType::IndexType    InputImageIndexType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename NumericTraits< InputImagePixelType >::RealType
                                                InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int,
                       InputImageType::ImageDimension );

  typedef TLevelSetValueType  LevelSetOutputType;

  typedef WhitakerSparseLevelSetBase< LevelSetOutputType, ImageDimension >
                                                       LevelSetType;
  typedef typename LevelSetType::Pointer               LevelSetPointer;
  typedef typename LevelSetType::InputType             LevelSetInputType;

  typedef typename LevelSetType::LabelObjectType       LevelSetLabelObjectType;
  typedef typename LevelSetLabelObjectType::LabelType  LayerIdType;
  typedef typename LevelSetType::LabelObjectPointer    LevelSetLabelObjectPointer;
  typedef typename LevelSetType::LabelObjectLengthType LevelSetLabelObjectLengthType;
  typedef typename LevelSetType::LabelObjectLineType   LevelSetLabelObjectLineType;

  typedef typename LevelSetType::LabelMapType          LevelSetLabelMapType;
  typedef typename LevelSetType::LabelMapPointer       LevelSetLabelMapPointer;

  typedef typename LevelSetType::LayerType             LevelSetLayerType;
  typedef typename LevelSetType::LayerIterator         LevelSetLayerIterator;
  typedef typename LevelSetType::LayerConstIterator    LevelSetLayerConstIterator;

  /** This is the same as "Procedure 1" as described in
   * "Sparse Field Methods - Technical Report"
   * http://www.shawnlankton.com/2009/04/sfm-and-active-contours/
   *
   * Input is a binary image m_InputImage
   * Output is a WhitakerSparseLevelSetBasePointer  */
  void Initialize();

  /** Get the sparse levet set function */
  itkGetObjectMacro( SparseLevelSet, LevelSetType );

  /** Set/Get the input image*/
  itkSetObjectMacro( InputImage, InputImageType );
  itkGetObjectMacro( InputImage, InputImageType );

protected:
  /** Constructor */
  BinaryImageToWhitakerSparseLevelSetAdaptor();

  /** Destructor */
  ~BinaryImageToWhitakerSparseLevelSetAdaptor();

  InputImagePointer       m_InputImage;
  LevelSetPointer         m_SparseLevelSet;
  LevelSetLabelMapPointer m_LabelMap;

  typedef Image< char, ImageDimension >         InternalImageType;
  typedef typename InternalImageType::Pointer   InternalImagePointer;

  InternalImagePointer m_InternalImage;

  typedef std::pair< LevelSetInputType, LevelSetOutputType >  LayerPairType;

  typedef ImageRegionIteratorWithIndex< InputImageType >      InputIteratorType;
  typedef ImageRegionIteratorWithIndex< InternalImageType >   InternalIteratorType;

  typedef ShapedNeighborhoodIterator< InternalImageType > NeighborhoodIteratorType;

  /** Fill layer adjacent (OutputLayer) to the layer (LayerToBeScanned) */
  void PropagateToOuterLayers( LayerIdType LayerToBeScanned, LayerIdType OutputLayer, LayerIdType TestValue );

  /** Fill the layer corresponding to zero level set */
  void FindActiveLayer();

  /** Fill layers adjacent to the zero level set (i.e. layer -1 and +1 )*/
  void FindPlusOneMinusOneLayer();

private:
  BinaryImageToWhitakerSparseLevelSetAdaptor( const Self& ); // purposely not implemented
  void operator = ( const Self& ); // purposely not implemented


};
}
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryImageToWhitakerSparseLevelSetAdaptor.hxx"
#endif

#endif // __itkBinaryImageToWhitakerSparseLevelSetAdaptor_h
