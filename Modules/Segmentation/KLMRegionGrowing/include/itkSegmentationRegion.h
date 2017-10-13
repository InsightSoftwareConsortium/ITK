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
#ifndef itkSegmentationRegion_h
#define itkSegmentationRegion_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "ITKKLMRegionGrowingExport.h"

namespace itk
{
/** \class SegmentationRegion
 * \brief Base class for SegmentationRegion object
 *
 * itkSegmentationRegion is the base class for the SegmentationRegion
 * objects. It provides the basic function definitons that are inherent to a
 * SegmentationRegion objects.  A region object is defined by the label it
 * owns.  We use integer labels to represent a region.
 * This object stores the region label.  The user can get the
 * area and a label associated with the region
 * through access functions provided publicly.
 * This class provides an interface method called
 * ApplySegmentationRegion is defined as virtual to enable future extension
 * of the region object.
 * This object can be used where region in images needs to be defined.
 * Child classes can be derived from this class for tailoring them towards
 * specific algorithmic needs.
 *
 * \ingroup RegionGrowingSegmentation
 * \ingroup ITKKLMRegionGrowing
 */
class ITKKLMRegionGrowing_EXPORT SegmentationRegion:public Object
{
public:
  /** Standard class typedefs. */
  typedef SegmentationRegion         Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SegmentationRegion, Object);

  /** Type definition for a segmentation region label. */
  typedef unsigned int RegionLabelType;

  /** Define a virtual SegmentationRegion function that is meant to be
   * used in derived classes if some operation needs to be
   * performed on a region object. */
  virtual void ApplySegmentationRegion(){}

  /** Set/Get the region with parameter values
   * defining the region. */
  itkSetMacro(RegionLabel, RegionLabelType);
  itkGetConstReferenceMacro(RegionLabel, RegionLabelType);

  /** Set/Get the area of the region. */
  itkSetMacro(RegionArea, double);
  itkGetConstReferenceMacro(RegionArea, double);

protected:
  SegmentationRegion();
  ~SegmentationRegion() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SegmentationRegion);

  RegionLabelType m_RegionLabel;
  double          m_RegionArea;
};
} // end namespace itk

#endif
