/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkROISpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkROISpatialObject_h
#define __itkROISpatialObject_h
#include "itkGroupSpatialObject.h"
#include "itkROIOrientation.h"
#include "itkROIStrandSpatialObject.h"

/**
 * \class ROISpatialObject
 * \brief Implements a Region Of Interest Type
 *
 * This class implements a Region of Interest, which is simply a space
 * in a medical image that has some significance as a component of the
 * scan.  Different components of brain anatomy, for instance are Regions
 * of Interest in the human brain.
 * The primary difference between ROISpatialObject and other descendants
 * of SpatialObject are these:
 * 1. For now, an ROI needs to comprise a series of slices parallel to
 * a plane swept by any two of the major axes. In other words, the points
 * in each slice (or strand) all need to have one of the X,Y,Z values to
 * be constant.
 * 2. The IsInside method implements the concept of spatial 'insidedness'
 * where other descendents of SpatialObject have a concept of insidedness
 * defined in a rather ad-hoc manner. ROISpatialObjects define IsInside
 * to mean that the given point resides in space inside the surface of
 * the ROISpatialObject.
 */
namespace itk
{
  template <unsigned int TDimension = 3> class 
    ROISpatialObject
    :public GroupSpatialObject<TDimension>
    {
    public:
      typedef ROISpatialObject< TDimension >             Self;
      typedef BlobSpatialObject< TDimension >            Superclass;
      typedef SmartPointer < Self >                      Pointer;
      typedef SmartPointer < const Self >                ConstPointer;
      typedef typename Superclass::PointType             PointType;
      typedef typename Superclass::BoundingBoxType       BoundingBoxType;
      /** Method for creation through the object factory. */
      itkNewMacro( Self );

      /** Method for creation through the object factory. */
      itkTypeMacro( Self, Superclass );
  
    /** Return true if the given ROIStrandSpatialObject is successfully
     * added to the ROI.
     */
    bool AddStrand(ROIStrandSpatialObject<TDimension> *toAdd);

    /** Return true if the given ROIStrandSpatialObject is successfully
     * removed from the ROI
     */
    bool DeleteStrand(ROIStrandSpatialObject<TDimension> *toDelete);

    /** Return true if the given ROIStrandSpatialObject successfully
     * replaces the ROIStrand given in toReplace. This will fail if
     * toReplace is not a strand in the ROIObject.
     */
    bool ReplaceStrand(ROIStrandSpatialObject<TDimension> *toReplace,
                       ROIStrandSpatialObject<TDimension> *replacement);

    /** Return true if all constituent ROIStrands are closed. */
    bool IsClosed();
    /** returns the number of ROIStrands in this ROI */
    unsigned int NumberOfStrands();
    /** Volume of this ROI, which is the sum of the volume of all
     * its constituent ROIStrands
     */
    double Volume();
    /** Same as Volume, above.*/
    double MeasureVolume();
    /** Test whether a point is inside or outside the object. */ 
    virtual bool IsInside( const PointType & point,
                           unsigned int depth=0,
                           char * name = NULL) const;
    };
}
#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkROISpatialObject.txx" 
#endif 

#endif
