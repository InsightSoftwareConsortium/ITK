/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationRegion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSegmentationRegion_h
#define _itkSegmentationRegion_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class SegmentationRegion
 * \brief Base class for SegmentationRegion object
 * 
 * itkSegmentationRegion is the base class for the SegmentationRegion
 * objects. It provides the basic function definitons that are inherent to a
 * SegmentationRegion objects.  A region object is defined by the label it
 * owns. We use integer labels to represent a region. No two regions can have
 * the same labels This object stores the region label. The user can get the
 * area, mean region intensity and a unique label associate with the region
 * through access functions provided publicly. The unique label parameter is
 * used at the end of a region growing class to associate a region with a new
 * label that characterises the region after several regions have been
 * merged.  This class provides an interface method called
 * ApplySegmentationRegion is defined as virtual to enable future extension 
 * of the region object.  
 * This object can be used where region in images needs to be defined.
 * Chlid classes can be derived from this class for tailoring them towards 
 * specific algorithmic needs.
 *
 * \ingroup RegionGrowingSegmentation 
 */
class ITKCommon_EXPORT SegmentationRegion : public Object
{
public:
  /** Standard class typedefs. */
  typedef SegmentationRegion   Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SegmentationRegion,Object);

  /** Type definition for an double vector. */
  typedef vnl_matrix<double> VectorOfDoubleType;

  /** Define a virtual SegmentationRegion function that is meant to be
   * used in derived classes if some operation needs to be
   * performed on a region object. */
  virtual void ApplySegmentationRegion(){};

  /** Set the region with parameter values
   * defining the region. */
  itkSetMacro(RegionLabel, unsigned int);

  /** Get the label associated with the region.  */
  itkGetMacro(RegionLabel, unsigned int);

  /** Set/Get the area of the region. */
  itkSetMacro(RegionArea, unsigned int);
  itkGetMacro(RegionArea, unsigned int);

  /** Set/Get the area of the region. */
  itkSetMacro(UniqueLabel, unsigned int);
  itkGetMacro(UniqueLabel, unsigned int);

  /** Set/Get the mean pixel intensity in the region. */
  itkSetMacro(MeanRegionIntensity, VectorOfDoubleType)
  itkGetMacro(MeanRegionIntensity, VectorOfDoubleType);

protected: 
  SegmentationRegion();
  ~SegmentationRegion();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  SegmentationRegion(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  unsigned int    m_RegionLabel;
  unsigned int    m_RegionArea;
  unsigned int    m_UniqueLabel; 
  VectorOfDoubleType      m_MeanRegionIntensity; 

}; // class SegmentationRegion


} // namespace itk


#endif




