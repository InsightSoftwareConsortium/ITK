/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIsoContourDistanceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkIsoContourDistanceImageFilter_h
#define _itkIsoContourDistanceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNarrowBand.h"

namespace itk
{
/** \class IsoContourDistanceImageFilter
 *  \brief Reinitialize the level set to the signed distance function.
 *
 * IsoContourDistanceImageFilter reinitializes the input level set to
 * the approximated signed distance function from a particular 
 * level set. The output is a level set of the same type as the input.
 *
 * For some level set algorithms, it is useful to periodically
 * reinitialize the level set function to prevent numerical accuracy
 * problems in computing derivatives and curvature values where level
 * sets are densely bunched together.
 *
 * This class is templated over the image type which represents
 * the level set.
 *
 * This class supports narrowbanding. If the input narrowband is provided,
 * the algorithm will only locate the level set within the input narrowband.
 * For the output, the reinitialize level set is only valid for a distance
 * of OutputNarrowBandwidth / 2 of either side of the level set of interest.
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
  
   /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename OutputImageType::PixelType PixelType;
  typedef typename InputImageType::PixelType InputPixelType;
  
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


  /** LevelSetType typedef support. */
//  typedef LevelSetTypeDefault<TInputImage>  LevelSetType;
//  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
//  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
//  typedef typename LevelSetType::LevelSetConstPointer  LevelSetConstPointer;
//  typedef typename LevelSetType::NodeType NodeType;
//  typedef typename LevelSetType::NodeContainer NodeContainer;
//  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;
//  typedef typename LeveSetType::

  /** SetDimension enumeration. */
//  itkStaticConstMacro(SetDimension, unsigned int,
//                      SuperClass::SetDimension);

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

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIsoContourDistanceImageFilter.txx"
#endif

#endif
