/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReinitializeLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
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
 */
template <class TLevelSet>
class ITK_EXPORT ReinitializeLevelSetImageFilter :
  public ImageToImageFilter<TLevelSet,TLevelSet>
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef ReinitializeLevelSetImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ImageToImageFilter<TLevelSet,TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ReinitializeLevelSetImageFilter, ImageToImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * LevelSetType typedef support.
   */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /**
   * SetDimension enumeration.
   */
  enum { SetDimension = LevelSetType::SetDimension};

  /**
   * Set the value of the level set to be located. The default value is 0.
   */
  itkSetMacro( LevelSetValue, double );

  /**
   * Get the value of the level set to be located.
   */
  itkGetMacro( LevelSetValue, double );

  /**
   * Set the narrowbanding flag. By default, narrowbanding is switched
   * off.
   */
  itkSetMacro( NarrowBanding, bool );

  /**
   * Get the narrowbanding flag
   */
  itkGetMacro( NarrowBanding, bool );
  itkBooleanMacro( NarrowBanding );

  /**
   * Set the input narrow bandwidth. The default value is 12.
   */
  itkSetClampMacro( InputNarrowBandwidth, double, 0.0, 
    NumericTraits<double>::max());

  /**
   * Get the input narrow bandwidth.
   */
  itkGetMacro( InputNarrowBandwidth, double );

  /**
   * Set the output narrow bandwidth. The default value is 12.
   */
  itkSetClampMacro( OutputNarrowBandwidth, double, 0.0, 
    NumericTraits<double>::max());

  /**
   * Get the output narrow bandwidth.
   */
  itkGetMacro( OutputNarrowBandwidth, double );

  /**
   * Set the bandwidth for both the input and output narrowband,
   * By default, both the input and output are set to 12.
   */
  void SetNarrowBandwidth( double value )
    {
      this->SetInputNarrowBandwidth(value);
      this->SetOutputNarrowBandwidth(value);
    }

  /**
   * Set the input narrowband.
   */
  void SetInputNarrowBand( NodeContainer * ptr );

  /**
   * Get the input narrowband.
   */
  NodeContainerPointer GetInputNarrowBand() const
    { return m_InputNarrowBand; }

  /**
   * Get the output narrowband.
   */
  NodeContainerPointer GetOutputNarrowBand() const
    { return m_OutputNarrowBand; }


protected:
  ReinitializeLevelSetImageFilter();
  ~ReinitializeLevelSetImageFilter(){};
  ReinitializeLevelSetImageFilter(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent);

  typedef LevelSetNeighborhoodExtractor<TLevelSet> LocatorType;
  typedef FastMarchingImageFilter<TLevelSet> FastMarchingImageFilterType;

  virtual void GenerateData();
  virtual void GenerateDataFull();
  virtual void GenerateDataNarrowBand();
  virtual void AllocateOutput();

  virtual void GenerateInputRequestedRegion();
  virtual void EnlargeOutputRequestedRegion( DataObject * );

  void SetOutputNarrowBand( NodeContainer *ptr )
    { m_OutputNarrowBand = ptr; }

private:
  double                                m_LevelSetValue;

  typename LocatorType::Pointer         m_Locator;
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
