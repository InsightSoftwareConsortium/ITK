/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkRGRegion_h
#define _itkRGRegion_h

#include "itkObject.h"

namespace itk
{

/** \class RGRegion
 * \brief Base class for RGRegion object
 * 
 * itkRGRegion is the base class for the RGRegion objects. It provides
 * the basic function definitons that are inherent to a RGRegion objects.
 * A region object is defined by the label it owns. We use integer labels
 * to represent a region. No two regions can have the same labels This 
 * object stores the region label. The user can get the area, mean region 
 * intensity and a unique label associate with the region through access
 * functions provided publicly. The unique label parameter is used at the 
 * end of a region growing class to associate a region with a new label  
 * that characterises the region after several regions have been merged. 
 * This class provides an interface method called ApplyRGRegion to enable
 * future extension of the region object.
 *
 */

template <class TInputImage, class TOutputImage>
class ITK_EXPORT RGRegion : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RGRegion   Self;
  
  /**
   * Standard "Superclass" typedef
   */
  typedef Object Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RGRegion,Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Type definition for the input image.
   */
  typedef typename TInputImage::Pointer   InputImageType;

  /**
   * Type definition for the input image pixel type.
   */
  typedef typename TInputImage::PixelType InputImagePixelType;

  /**
   * Type definition for an double vector.
   */
  typedef typename vnl_matrix<double> VecDblType;

  /**
   * Define a virtual RGRegion function that is meant to be
   * used in derived classes if some operation needs to be
   * performed on a region object.
   */
  virtual void ApplyRGRegion(){};

  /**
   * Set the region with parameter values
   * defining the region.
   */
  itkSetMacro(RegionLabel, unsigned int);

  /**
   * Get the label associated with the region. 
   */
  itkGetMacro(RegionLabel, unsigned int);

  /**
   * Set the area of the region.
   */
  itkSetMacro(RegionArea, unsigned int);

  /**
   * Get the area of the region.
   */
  itkGetMacro(RegionArea, unsigned int);

  /**
   * Set the area of the region.
   */
  itkSetMacro(UniqueLabel, unsigned int);

  /**
   * Get the area of the region.
   */
  itkGetMacro(UniqueLabel, unsigned int);

  /**
   * Get the mean pixel intensity in the region
   */
  void SetMeanRegionIntensity(VecDblType averageRegionIntensity);

  /**
   * Get the mean pixel intensity in the region
   */
  VecDblType GetMeanRegionIntensity();

  /**
   * Constructor
   */
  RGRegion();

  /**
   * Destructor
   */
  ~RGRegion();

  /**
   * Copy constructor
   */
  RGRegion(const Self&) {}

  /**
   * Assignment operator
   */
  void operator=(const Self&) {}

protected:
  /**
   * Print self identity
   */      
  void PrintSelf(std::ostream& os, Indent indent);

private:
  unsigned int    m_RegionLabel;
  unsigned int    m_RegionArea;
  unsigned int    m_UniqueLabel; 
  VecDblType      m_MeanVec; 

}; // class RGRegion


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGRegion.txx"
#endif



#endif




