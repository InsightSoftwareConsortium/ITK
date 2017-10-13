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
#ifndef itkIsoContourDistanceImageFilter_h
#define itkIsoContourDistanceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNarrowBand.h"
#include "itkNeighborhoodIterator.h"
#include "itkBarrier.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class IsoContourDistanceImageFilter
 *  \brief Compute an approximate distance from an interpolated isocontour
 *  to the close grid points.
 *
 * For standard level set algorithms, it is useful to periodically
 * reinitialize the evolving image to prevent numerical accuracy
 * problems in computing derivatives.
 * This reinitialization is done by computing a signed distance map
 * to the current level set.
 * This class provides the first step in this reinitialization by
 * computing an estimate of the distance from the interpolated isocontour
 * to the pixels (or voxels) that are close to it, i.e. for which the
 * isocontour crosses a segment between them and one of their direct
 * neighbors.
 * This class supports narrowbanding. If the input narrowband is provided,
 * the algorithm will only locate the level set within the input narrowband.
 *
 * Implementation of this class is based on
 * Fast and Accurate Redistancing for Level Set Methods
 *`Krissian K. and Westin C.F.',
 * EUROCAST NeuroImaging Workshop Las Palmas Spain,
 * Ninth International Conference on Computer Aided Systems Theory , pages 48-51, Feb 2003.
 *
 *
 * \ingroup LevelSetSegmentation
 *
 * \ingroup ITKDistanceMap
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT IsoContourDistanceImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef IsoContourDistanceImageFilter                   Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(IsoContourDistanceImageFilter, ImageToImageFilter);

  /**Typedefs from the superclass */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename OutputImageType::PixelType                 PixelType;
  typedef typename InputImageType::PixelType                  InputPixelType;
  typedef typename NumericTraits< InputPixelType >::RealType  PixelRealType;

  typedef typename OutputImageType::RegionType OutputImageRegionType;

  typedef typename InputImageType::SizeType   InputSizeType;
  typedef typename OutputImageType::SizeType  SizeType;

  typedef typename InputImageType::IndexType  InputIndexType;
  typedef typename OutputImageType::IndexType IndexType;

  typedef typename InputImageType::SpacingType InputSpacingType;

  /** NarrowBand typedef support. */
  typedef BandNode< IndexType, PixelType >       BandNodeType;
  typedef NarrowBand< BandNodeType >             NarrowBandType;
  typedef typename NarrowBandType::Pointer       NarrowBandPointer;
  typedef typename NarrowBandType::RegionType    RegionType;
  typedef typename NarrowBandType::ConstIterator ConstBandIterator;
  typedef typename NarrowBandType::Iterator      BandIterator;

  /** Set/Get the value of the level set to be located. The default value is
    *  0. */
  itkSetMacro(LevelSetValue, PixelRealType);
  itkGetConstMacro(LevelSetValue, PixelRealType);

  /** Set/Get the value of the level set to be located. The default value is
  *  0. */
  itkSetMacro(FarValue, PixelType);
  itkGetConstMacro(FarValue, PixelType);

  /** Set/Get the narrowbanding flag. By default, narrowbanding is switched
   * off. */
  itkSetMacro(NarrowBanding, bool);
  itkGetConstMacro(NarrowBanding, bool);
  itkBooleanMacro(NarrowBanding);

  /** Set/Get the narrowband. */
  void SetNarrowBand(NarrowBandType *ptr);

  NarrowBandPointer GetNarrowBand() const
  { return m_NarrowBand; }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputPixelType > ) );
  itkConceptMacro( OutputEqualityComparableCheck,
                   ( Concept::EqualityComparable< PixelType > ) );
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< ImageDimension, OutputImageDimension > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, PixelType > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< InputPixelType, PixelType > ) );
  itkConceptMacro( OutputAdditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< PixelType > ) );
  itkConceptMacro( InputOStreamWritableCheck,
                   ( Concept::OStreamWritable< InputPixelType > ) );
  itkConceptMacro( OutputOStreamWritableCheck,
                   ( Concept::OStreamWritable< PixelType > ) );
  // End concept checking
#endif

protected:
  IsoContourDistanceImageFilter();
  ~IsoContourDistanceImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  void ThreadedGenerateDataFull(const OutputImageRegionType & outputRegionForThread,
                                ThreadIdType threadId);

  void ThreadedGenerateDataBand(const OutputImageRegionType & outputRegionForThread,
                                ThreadIdType threadId);

  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  virtual void EnlargeOutputRequestedRegion(DataObject *) ITK_OVERRIDE;

  typedef ConstNeighborhoodIterator< InputImageType > InputNeighbordIteratorType;
  typedef NeighborhoodIterator< OutputImageType >     OutputNeighborhoodIteratorType;

  void ComputeValue( const InputNeighbordIteratorType& inNeigIt,
                     OutputNeighborhoodIteratorType& outNeigIt,
                     unsigned int center,
                     const std::vector< OffsetValueType >& stride );

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IsoContourDistanceImageFilter);

  PixelRealType  m_LevelSetValue;
  PixelType      m_FarValue;

  InputSpacingType m_Spacing;

  bool                      m_NarrowBanding;
  NarrowBandPointer         m_NarrowBand;
  std::vector< RegionType > m_NarrowBandRegion;

  /** A global barrier used for synchronization between all threads. */
  typename Barrier::Pointer m_Barrier;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIsoContourDistanceImageFilter.hxx"
#endif

#endif
