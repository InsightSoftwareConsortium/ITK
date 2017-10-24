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

#ifndef itkBinaryImageToLevelSetImageAdaptor_h
#define itkBinaryImageToLevelSetImageAdaptor_h

#include "itkBinaryImageToLevelSetImageAdaptorBase.h"

#include "itkLevelSetDenseImage.h"
#include "itkImageToImageFilter.h"

#include "itkWhitakerSparseLevelSetImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkShapedNeighborhoodIterator.h"

#include "itkShiSparseLevelSetImage.h"
#include "itkMalcolmSparseLevelSetImage.h"

namespace itk
{
/** \class BinaryImageToLevelSetImageAdator
 *  \brief Converts one binary image to the appropriate level-set type
 *  provided by the template argument TLevelSet.
 *
 *  \tparam TInputImage Binary Input Image Type
 *  \tparam TLevelSet   Output Level-Set Type
 *
 *  \note TLevelSet must inherits from LevelSetImage
 *
 *  \sa LevelSetImage
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInputImage, typename TLevelSet >
class ITK_TEMPLATE_EXPORT BinaryImageToLevelSetImageAdaptor
{};


/** \brief Partial template specialization for LevelSetDenseImage
 */
template< typename TInputImage, typename TLevelSetImage >
class ITK_TEMPLATE_EXPORT BinaryImageToLevelSetImageAdaptor<
    TInputImage,
    LevelSetDenseImage< TLevelSetImage > > :
public BinaryImageToLevelSetImageAdaptorBase<
    TInputImage,
    LevelSetDenseImage< TLevelSetImage > >
{
public:
  typedef LevelSetDenseImage< TLevelSetImage >  LevelSetType;

  typedef BinaryImageToLevelSetImageAdaptor         Self;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;
  typedef BinaryImageToLevelSetImageAdaptorBase<
    TInputImage, LevelSetType >                     Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( BinaryImageToLevelSetImageAdaptorBase, Object );

  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef typename InputImageType::IndexType    InputImageIndexType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename NumericTraits< InputImagePixelType >::RealType
                                                InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int,
                       InputImageType::ImageDimension );

  typedef typename LevelSetType::Pointer    LevelSetPointer;
  typedef typename LevelSetType::ImageType  LevelSetImageType;

  typedef ImageToImageFilter< InputImageType, LevelSetImageType >  SignedDistanceTransformFilterType;
  typedef typename SignedDistanceTransformFilterType::Pointer      SignedDistanceTransformFilterPointer;

  /** Set the signed distance image filter.  Defaults to a
   * SignedMaurerDistanceMapImageFilter. */
  itkSetObjectMacro( SignedDistanceTransformFilter, SignedDistanceTransformFilterType );
  itkGetModifiableObjectMacro(SignedDistanceTransformFilter, SignedDistanceTransformFilterType );

  /**
   * Input is a binary image m_InputImage
   * Output is a WhitakerSparseLevelSetImagePointer  */
  void Initialize() ITK_OVERRIDE;

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptor();

  /** Destructor */
  virtual ~BinaryImageToLevelSetImageAdaptor() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLevelSetImageAdaptor);

  SignedDistanceTransformFilterPointer   m_SignedDistanceTransformFilter;
};

////////////////////////////////////////////////////////////////////////////////

/** \class BinaryImageToSparseLevelSetImageAdaptorBase
 *  \brief Abstract class for converting binary image to sparse level-set
 *
 *  \ingroup ITKLevelSetsv4
 */
template< typename TInput, typename TOutput >
class ITK_TEMPLATE_EXPORT BinaryImageToSparseLevelSetImageAdaptorBase :
    public BinaryImageToLevelSetImageAdaptorBase< TInput, TOutput >
{
public:
  typedef BinaryImageToSparseLevelSetImageAdaptorBase Self;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;
  typedef BinaryImageToLevelSetImageAdaptorBase< TInput, TOutput >
    Superclass;

  /** Run-time type information */
  itkTypeMacro( BinaryImageToSparseLevelSetImageAdaptorBase,
                BinaryImageToLevelSetImageAdaptorBase );

  typedef typename Superclass::InputImageType       InputImageType;
  typedef typename Superclass::InputImagePixelType  InputImagePixelType;
  typedef typename Superclass::InputImageIndexType  InputImageIndexType;
  typedef typename Superclass::InputImagePointer    InputImagePointer;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;
  typedef typename Superclass::InputPixelRealType   InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int,
                       InputImageType::ImageDimension );

  typedef typename Superclass::LevelSetType             LevelSetType;
  typedef typename Superclass::LevelSetPointer          LevelSetPointer;

  typedef typename LevelSetType::InputType              LevelSetInputType;
  typedef typename LevelSetType::OutputType             LevelSetOutputType;

  typedef typename LevelSetType::LabelObjectType        LevelSetLabelObjectType;
  typedef typename LevelSetLabelObjectType::LabelType   LayerIdType;
  typedef typename LevelSetType::LabelObjectPointer     LevelSetLabelObjectPointer;
  typedef typename LevelSetType::LabelObjectLengthType  LevelSetLabelObjectLengthType;
  typedef typename LevelSetType::LabelObjectLineType    LevelSetLabelObjectLineType;

  typedef typename LevelSetType::LabelMapType           LevelSetLabelMapType;
  typedef typename LevelSetType::LabelMapPointer        LevelSetLabelMapPointer;

  typedef typename LevelSetType::LayerType              LevelSetLayerType;
  typedef typename LevelSetType::LayerIterator          LevelSetLayerIterator;
  typedef typename LevelSetType::LayerConstIterator     LevelSetLayerConstIterator;

  typedef Image< signed char, ImageDimension >  InternalImageType;
  typedef typename InternalImageType::Pointer   InternalImagePointer;

  typedef std::pair< LevelSetInputType, LevelSetOutputType >  LayerPairType;

  typedef ImageRegionIteratorWithIndex< InputImageType >      InputIteratorType;
  typedef ImageRegionIteratorWithIndex< InternalImageType >   InternalIteratorType;

  typedef ShapedNeighborhoodIterator< InternalImageType > NeighborhoodIteratorType;

protected:
  BinaryImageToSparseLevelSetImageAdaptorBase() : Superclass() {}
  virtual ~BinaryImageToSparseLevelSetImageAdaptorBase() ITK_OVERRIDE {}

  LevelSetLabelMapPointer m_LabelMap;

  InternalImagePointer m_InternalImage;

private:
  BinaryImageToSparseLevelSetImageAdaptorBase( const Self& );
  void operator = ( const Self& );
};

////////////////////////////////////////////////////////////////////////////////
/** \brief Partial template specialization for WhitakerSparseLevelSetImage
 */
template< typename TInput, typename TOutput >
class ITK_TEMPLATE_EXPORT BinaryImageToLevelSetImageAdaptor<
    TInput,
    WhitakerSparseLevelSetImage< TOutput, TInput::ImageDimension > > :
  public BinaryImageToSparseLevelSetImageAdaptorBase<
      TInput,
      WhitakerSparseLevelSetImage< TOutput, TInput::ImageDimension > >
  {
public:
  typedef WhitakerSparseLevelSetImage< TOutput, TInput::ImageDimension >
    LevelSetType;

  typedef BinaryImageToLevelSetImageAdaptor       Self;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef BinaryImageToSparseLevelSetImageAdaptorBase<
    TInput, LevelSetType >                        Superclass;


  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( BinaryImageToLevelSetImageAdaptor,
                BinaryImageToSparseLevelSetImageAdaptorBase );

  typedef typename Superclass::InputImageType       InputImageType;
  typedef typename Superclass::InputImagePixelType  InputImagePixelType;
  typedef typename Superclass::InputImageIndexType  InputImageIndexType;
  typedef typename Superclass::InputImagePointer    InputImagePointer;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;
  typedef typename Superclass::InputPixelRealType   InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int,
                        InputImageType::ImageDimension );

  typedef typename Superclass::LevelSetPointer                LevelSetPointer;

  typedef typename Superclass::LevelSetInputType              LevelSetInputType;
  typedef typename Superclass::LevelSetOutputType             LevelSetOutputType;

  typedef typename Superclass::LevelSetLabelObjectType        LevelSetLabelObjectType;
  typedef typename Superclass::LayerIdType                    LayerIdType;
  typedef typename Superclass::LevelSetLabelObjectPointer     LevelSetLabelObjectPointer;
  typedef typename Superclass::LevelSetLabelObjectLengthType  LevelSetLabelObjectLengthType;
  typedef typename Superclass::LevelSetLabelObjectLineType    LevelSetLabelObjectLineType;

  typedef typename Superclass::LevelSetLabelMapType           LevelSetLabelMapType;
  typedef typename Superclass::LevelSetLabelMapPointer        LevelSetLabelMapPointer;

  typedef typename Superclass::LevelSetLayerType              LevelSetLayerType;
  typedef typename Superclass::LevelSetLayerIterator          LevelSetLayerIterator;
  typedef typename Superclass::LevelSetLayerConstIterator     LevelSetLayerConstIterator;

  typedef typename Superclass::InternalImageType        InternalImageType;
  typedef typename Superclass::InternalImagePointer     InternalImagePointer;

  typedef typename Superclass::LayerPairType            LayerPairType;

  typedef typename Superclass::InputIteratorType        InputIteratorType;
  typedef typename Superclass::InternalIteratorType     InternalIteratorType;

  typedef typename Superclass::NeighborhoodIteratorType NeighborhoodIteratorType;

  void Initialize() ITK_OVERRIDE;

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptor();

  /** Destructor */
  virtual ~BinaryImageToLevelSetImageAdaptor() ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLevelSetImageAdaptor);

  /** Fill layer adjacent (OutputLayer) to the layer (LayerToBeScanned) */
  void PropagateToOuterLayers( LayerIdType LayerToBeScanned, LayerIdType OutputLayer, LayerIdType TestValue );

  /** Fill the layer corresponding to zero level set */
  void FindActiveLayer();

  /** Fill layers adjacent to the zero level set (i.e. layer -1 and +1 )*/
  void FindPlusOneMinusOneLayer();

};

////////////////////////////////////////////////////////////////////////////////
/** \brief Partial template specialization for ShiSparseLevelSetImage
 */
template< typename TInput >
class ITK_TEMPLATE_EXPORT BinaryImageToLevelSetImageAdaptor<
    TInput,
    ShiSparseLevelSetImage< TInput::ImageDimension > > :
public BinaryImageToSparseLevelSetImageAdaptorBase<
    TInput,
    ShiSparseLevelSetImage< TInput::ImageDimension > >
{
public:
  typedef ShiSparseLevelSetImage< TInput::ImageDimension > LevelSetType;

  typedef BinaryImageToLevelSetImageAdaptor       Self;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef BinaryImageToSparseLevelSetImageAdaptorBase<
    TInput, LevelSetType >                        Superclass;

  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( BinaryImageToLevelSetImageAdaptor,
                BinaryImageToSparseLevelSetImageAdaptorBase );

  typedef typename Superclass::InputImageType       InputImageType;

  typedef typename Superclass::InputImagePixelType  InputImagePixelType;
  typedef typename Superclass::InputImageIndexType  InputImageIndexType;
  typedef typename Superclass::InputImagePointer    InputImagePointer;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;
  typedef typename Superclass::InputPixelRealType   InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int,
                       InputImageType::ImageDimension );

//  typedef typename Superclass::LevelSetType             LevelSetType;
  typedef typename Superclass::LevelSetPointer                LevelSetPointer;

  typedef typename Superclass::LevelSetInputType              LevelSetInputType;
  typedef typename Superclass::LevelSetOutputType             LevelSetOutputType;

  typedef typename Superclass::LevelSetLabelObjectType        LevelSetLabelObjectType;
  typedef typename Superclass::LayerIdType                    LayerIdType;
  typedef typename Superclass::LevelSetLabelObjectPointer     LevelSetLabelObjectPointer;
  typedef typename Superclass::LevelSetLabelObjectLengthType  LevelSetLabelObjectLengthType;
  typedef typename Superclass::LevelSetLabelObjectLineType    LevelSetLabelObjectLineType;

  typedef typename Superclass::LevelSetLabelMapType           LevelSetLabelMapType;
  typedef typename Superclass::LevelSetLabelMapPointer        LevelSetLabelMapPointer;

  typedef typename Superclass::LevelSetLayerType              LevelSetLayerType;
  typedef typename Superclass::LevelSetLayerIterator          LevelSetLayerIterator;
  typedef typename Superclass::LevelSetLayerConstIterator     LevelSetLayerConstIterator;

  typedef typename Superclass::InternalImageType        InternalImageType;
  typedef typename Superclass::InternalImagePointer     InternalImagePointer;

  typedef typename Superclass::LayerPairType            LayerPairType;

  typedef typename Superclass::InputIteratorType        InputIteratorType;
  typedef typename Superclass::InternalIteratorType     InternalIteratorType;

  typedef typename Superclass::NeighborhoodIteratorType NeighborhoodIteratorType;

  void Initialize() ITK_OVERRIDE;

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptor();

  /** Destructor */
  ~BinaryImageToLevelSetImageAdaptor() ITK_OVERRIDE;

  /** Find the active layer separating the foreground and background regions */
  void FindActiveLayer();

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLevelSetImageAdaptor);
};


////////////////////////////////////////////////////////////////////////////////
/** \brief Partial template specialization for MalcolmSparseLevelSetImage
 */
template< typename TInput >
class ITK_TEMPLATE_EXPORT BinaryImageToLevelSetImageAdaptor<
    TInput,
    MalcolmSparseLevelSetImage< TInput::ImageDimension > > :
  public BinaryImageToSparseLevelSetImageAdaptorBase< TInput, MalcolmSparseLevelSetImage< TInput::ImageDimension > >
{
public:
  typedef MalcolmSparseLevelSetImage< TInput::ImageDimension > LevelSetType;

  typedef BinaryImageToLevelSetImageAdaptor       Self;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;
  typedef BinaryImageToSparseLevelSetImageAdaptorBase<
    TInput, LevelSetType >                        Superclass;


  /** Method for creation through object factory */
  itkNewMacro( Self );

  /** Run-time type information */
  itkTypeMacro( BinaryImageToLevelSetImageAdaptor,
                BinaryImageToSparseLevelSetImageAdaptorBase );

  typedef typename Superclass::InputImageType       InputImageType;

  typedef typename Superclass::InputImagePixelType  InputImagePixelType;
  typedef typename Superclass::InputImageIndexType  InputImageIndexType;
  typedef typename Superclass::InputImagePointer    InputImagePointer;
  typedef typename Superclass::InputImageRegionType InputImageRegionType;
  typedef typename Superclass::InputPixelRealType   InputPixelRealType;

  itkStaticConstMacro ( ImageDimension, unsigned int,
                       InputImageType::ImageDimension );


  typedef typename Superclass::LevelSetPointer                LevelSetPointer;
  typedef typename Superclass::LevelSetInputType              LevelSetInputType;
  typedef typename Superclass::LevelSetOutputType             LevelSetOutputType;

  typedef typename Superclass::LevelSetLabelObjectType        LevelSetLabelObjectType;
  typedef typename Superclass::LayerIdType                    LayerIdType;
  typedef typename Superclass::LevelSetLabelObjectPointer     LevelSetLabelObjectPointer;
  typedef typename Superclass::LevelSetLabelObjectLengthType  LevelSetLabelObjectLengthType;
  typedef typename Superclass::LevelSetLabelObjectLineType    LevelSetLabelObjectLineType;

  typedef typename Superclass::LevelSetLabelMapType           LevelSetLabelMapType;
  typedef typename Superclass::LevelSetLabelMapPointer        LevelSetLabelMapPointer;

  typedef typename Superclass::LevelSetLayerType              LevelSetLayerType;
  typedef typename Superclass::LevelSetLayerIterator          LevelSetLayerIterator;
  typedef typename Superclass::LevelSetLayerConstIterator     LevelSetLayerConstIterator;

  typedef typename Superclass::InternalImageType        InternalImageType;
  typedef typename Superclass::InternalImagePointer     InternalImagePointer;

  typedef typename Superclass::LayerPairType            LayerPairType;

  typedef typename Superclass::InputIteratorType        InputIteratorType;
  typedef typename Superclass::InternalIteratorType     InternalIteratorType;

  typedef typename Superclass::NeighborhoodIteratorType NeighborhoodIteratorType;

  void Initialize() ITK_OVERRIDE;

protected:
  /** Constructor */
  BinaryImageToLevelSetImageAdaptor();

  /** Destructor */
  virtual ~BinaryImageToLevelSetImageAdaptor() ITK_OVERRIDE;

  /** Find the active layer separating the foreground and background regions */
  void FindActiveLayer();

  /** Ensure that the 0 level set layer is only of single pixel thickness */
  void CreateMinimalInterface();

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryImageToLevelSetImageAdaptor);
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryImageToLevelSetImageAdaptor.hxx"
#endif
#endif // itkBinaryImageToLevelSetImageAdaptorBase_h
