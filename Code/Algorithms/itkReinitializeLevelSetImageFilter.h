/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReinitializeLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkReinitializeLevelSetImageFilter_h
#define _itkReinitializeLevelSetImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkLevelSet.h"
#include "itkLevelSetNeighborhoodExtractor.h"
#include "itkFastMarchingImageFilter.h"

namespace itk
{
/** \class ReinitializeLevelSetImageFilter
 *  \brief Reinitialize the level set to the signed distance function.
 *
 * ReinitializeLevelSetImageFilter reinitializes the input level set to
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
 * Implementation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \ingroup LevelSetSegmentation 
 *
 */
template <class TLevelSet>
class ITK_EXPORT ReinitializeLevelSetImageFilter :
    public ImageToImageFilter<TLevelSet,TLevelSet>
{
public:
  /** Standard class typedefs. */
  typedef ReinitializeLevelSetImageFilter Self;
  typedef ImageToImageFilter<TLevelSet,TLevelSet> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ReinitializeLevelSetImageFilter, ImageToImageFilter);

  /** LevelSetType typedef support. */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::LevelSetConstPointer  LevelSetConstPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /** SetDimension enumeration. */
  itkStaticConstMacro(SetDimension, unsigned int,
                      LevelSetType::SetDimension);

  /** Set/Get the value of the level set to be located. The default value is
   *  0. */
  itkSetMacro( LevelSetValue, double );
  itkGetMacro( LevelSetValue, double );

  /** Set/Get the narrowbanding flag. By default, narrowbanding is switched
   * off. */
  itkSetMacro( NarrowBanding, bool );
  itkGetMacro( NarrowBanding, bool );
  itkBooleanMacro( NarrowBanding );

  /** Set/Get the input narrow bandwidth. The default value is 12. */
  itkSetClampMacro( InputNarrowBandwidth, double, 0.0, 
                    NumericTraits<double>::max());
  itkGetMacro( InputNarrowBandwidth, double );

  /** Set/Get the output narrow bandwidth. The default value is 12. */
  itkSetClampMacro( OutputNarrowBandwidth, double, 0.0, 
                    NumericTraits<double>::max());
  itkGetMacro( OutputNarrowBandwidth, double );

  /** Set the bandwidth for both the input and output narrowband,
   * By default, both the input and output are set to 12. */
  void SetNarrowBandwidth( double value )
  {
    this->SetInputNarrowBandwidth(value);
    this->SetOutputNarrowBandwidth(value);
  }

  /** Set/Get the input narrowband. */
  void SetInputNarrowBand( NodeContainer * ptr );
  NodeContainerPointer GetInputNarrowBand() const
  { return m_InputNarrowBand; }

  /** Get the output narrowband. */
  NodeContainerPointer GetOutputNarrowBand() const
  { return m_OutputNarrowBand; }

protected:
  ReinitializeLevelSetImageFilter();
  ~ReinitializeLevelSetImageFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Internal typedefs. SpeedImageType defined to work around the Borland
   * compiler's improper handling of default template parameters that use
   * dependent non-type templates. */
  typedef Image<float, itkGetStaticConstMacro(SetDimension) > SpeedImageType;
  typedef LevelSetNeighborhoodExtractor<TLevelSet> LocatorType;
  typedef FastMarchingImageFilter<TLevelSet, SpeedImageType> FastMarchingImageFilterType;

  void GenerateData();
  virtual void GenerateDataFull();
  virtual void GenerateDataNarrowBand();
  virtual void AllocateOutput();

  virtual void GenerateInputRequestedRegion();
  virtual void EnlargeOutputRequestedRegion( DataObject * );

  void SetOutputNarrowBand( NodeContainer *ptr )
  { m_OutputNarrowBand = ptr; }

private:
  ReinitializeLevelSetImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  double                                m_LevelSetValue;

  typename LocatorType::Pointer                    m_Locator;
  typename FastMarchingImageFilterType::Pointer    m_Marcher;

  bool                                  m_NarrowBanding;
  double                                m_InputNarrowBandwidth;
  double                                m_OutputNarrowBandwidth;
  NodeContainerPointer                  m_InputNarrowBand;
  NodeContainerPointer                  m_OutputNarrowBand;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkReinitializeLevelSetImageFilter.txx"
#endif

#endif
