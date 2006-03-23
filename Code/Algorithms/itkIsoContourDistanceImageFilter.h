/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIsoContourDistanceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkIsoContourDistanceImageFilter_h
#define _itkIsoContourDistanceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNarrowBand.h"
#include "itkBarrier.h"

namespace itk
{
/** \class IsoContourDistanceImageFilter
 *  \brief Compute an approximate distance from an interpolated isocontour to the close grid points.
 *
 * For standard level set algorithms, it is useful to periodically
 * reinitialize the evolving image to prevent numerical accuracy
 * problems in computing derivatives.
 * This reinitialization is done by computing a signed distance map
 * to the current level set.
 * This class provides the first step in this reinitialization by
 * computing an estimate of the distance from the interpolated isocontour
 * to the pixels (or voxels) that are close to it, i.e. for which the isocontour
 * crosses a segment between them and one of their direct neighbors.
 *
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
 */ 
 
template <class TInputImage, class TOutputImage>
class ITK_EXPORT IsoContourDistanceImageFilter :
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef IsoContourDistanceImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(IsoContourDistanceImageFilter, ImageToImageFilter);
  
  /**Typedefs from the superclass */
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  
  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
   /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename OutputImageType::PixelType PixelType;
  typedef typename InputImageType::PixelType InputPixelType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  /** Some typedef */
  typedef typename InputImageType::SizeType  InputSizeType;
  typedef typename OutputImageType::SizeType SizeType;
  typedef typename InputImageType::IndexType InputIndexType;
  typedef typename OutputImageType::IndexType IndexType;

  /** NarrowBand typedef support. */
  typedef BandNode<IndexType,PixelType> BandNodeType;
  typedef NarrowBand<BandNodeType> NarrowBandType;
  typedef typename NarrowBandType::Pointer NarrowBandPointer;
  typedef typename NarrowBandType::RegionType RegionType;
  typedef typename NarrowBandType::ConstIterator ConstBandIterator;
  typedef typename NarrowBandType::Iterator BandIterator;


 /** Set/Get the value of the level set to be located. The default value is
   *  0. */
  itkSetMacro( LevelSetValue, InputPixelType );
  itkGetMacro( LevelSetValue, InputPixelType );
  
   /** Set/Get the value of the level set to be located. The default value is
   *  0. */
  itkSetMacro( FarValue, PixelType );
  itkGetMacro( FarValue, PixelType );


  /** Set/Get the narrowbanding flag. By default, narrowbanding is switched
   * off. */
  itkSetMacro( NarrowBanding, bool );
  itkGetMacro( NarrowBanding, bool );
  itkBooleanMacro( NarrowBanding );


  /** Set/Get the narrowband. */
  void SetNarrowBand( NarrowBandType * ptr );
  NarrowBandPointer GetNarrowBand() const
  { return m_NarrowBand; }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputEqualityComparableCheck,
    (Concept::EqualityComparable<InputPixelType>));
  itkConceptMacro(OutputEqualityComparableCheck,
    (Concept::EqualityComparable<PixelType>));
  itkConceptMacro(SameDimensionCheck,
    (Concept::SameDimension<ImageDimension, OutputImageDimension>));
  itkConceptMacro(DoubleConvertibleToOutputCheck,
    (Concept::Convertible<double, PixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible<InputPixelType, PixelType>));
  itkConceptMacro(OutputAdditiveOperatorsCheck,
    (Concept::AdditiveOperators<PixelType>));
  /** End concept checking */
#endif

protected:
  IsoContourDistanceImageFilter();
  ~IsoContourDistanceImageFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );
  void ThreadedGenerateDataFull(const OutputImageRegionType& outputRegionForThread,
                            int threadId );
  void ThreadedGenerateDataBand(const OutputImageRegionType& outputRegionForThread,
                            int threadId );
  void BeforeThreadedGenerateData();

  virtual void GenerateInputRequestedRegion();
  virtual void EnlargeOutputRequestedRegion( DataObject * );

private:
  IsoContourDistanceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  InputPixelType            m_LevelSetValue;
  PixelType                 m_FarValue;
  
  bool                      m_NarrowBanding;
  NarrowBandPointer         m_NarrowBand;
  std::vector<RegionType>   m_NarrowBandRegion;
  
  /** A global barrier used for synchronization between all threads. */
  typename Barrier::Pointer m_Barrier;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIsoContourDistanceImageFilter.txx"
#endif

#endif
