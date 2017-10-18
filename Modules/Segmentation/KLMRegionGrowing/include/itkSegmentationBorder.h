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
#ifndef itkSegmentationBorder_h
#define itkSegmentationBorder_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "ITKKLMRegionGrowingExport.h"

namespace itk
{
/** \class SegmentationBorder
 * \brief Base class for SegmentationBorder object
 *
 * itkSegmentationBorder is the base class for the SegmentationBorder
 * objects. It provides the basic function definitons that are inherent to a
 * SegmentationBorder objects.  This object stores information relevant to
 * the borders that are used in the region growing class. This class allows
 * access to the parameter that defines the length of the border associated
 * with this object.  In order to use this object in an application, the
 * object must be created within the application and then used to
 * initialize/store/use the various parameters accessible through the public
 * methods. For usage also see itkRegionGrowImageFiltering class.
 * This object can be used where a border of a region needs to be defined.
 * Child classes can be derived from this class for tailoring them towards
 * specific algorithmic needs.
 *
 * \ingroup RegionGrowingSegmentation
 * \ingroup ITKKLMRegionGrowing
 */
class ITKKLMRegionGrowing_EXPORT SegmentationBorder:public Object
{
public:
  /** Standard class typedefs. */
  typedef SegmentationBorder         Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SegmentationBorder, Object);

  /** Set/Get the length of a border object. */
  itkSetMacro(BorderLength, double);
  itkGetConstReferenceMacro(BorderLength, double);

  /** Define a virtual SegmentationBorder function. This function allows
   * access to specific instantiations of other border representations. This
   * is the function should be overloaded in any derived classes for the user
   * to access the various methods supported by the method. */
  virtual void ApplySegmentationBorder(){}

protected:
  SegmentationBorder();
  ~SegmentationBorder() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SegmentationBorder);

  double m_BorderLength;
};
} // end namespace itk

#endif
