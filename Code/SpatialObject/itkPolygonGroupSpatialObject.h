/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolygonGroupSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPolygonGroupSpatialObject_h
#define __itkPolygonGroupSpatialObject_h
#include "itkGroupSpatialObject.h"
#include "itkPolygonGroupOrientation.h"
#include "itkPolygonSpatialObject.h"

namespace itk
{


/**
 * \class PolygonGroupSpatialObject
 * \brief Implements a Region Of Interest Type
 *
 * This class implements a Region of Interest, which is simply a space
 * in a medical image that has some significance as a component of the
 * scan.  Different components of brain anatomy, for instance are Regions
 * of Interest in the human brain.
 * The primary difference between PolygonGroupSpatialObject and other descendants
 * of SpatialObject are these:
 * 1. For now, an PolygonGroup needs to comprise a series of slices parallel to
 * a plane swept by any two of the major axes. In other words, the points
 * in each slice (or strand) all need to have one of the X,Y,Z values to
 * be constant.
 * 2. The IsInside method implements the concept of spatial 'insidedness'
 * where other descendents of SpatialObject have a concept of insidedness
 * defined in a rather ad-hoc manner. PolygonGroupSpatialObjects define IsInside
 * to mean that the given point resides in space inside the surface of
 * the PolygonGroupSpatialObject.
 */


template <unsigned int TDimension = 3> class 
  PolygonGroupSpatialObject
  :public GroupSpatialObject<TDimension>
  {
    public:
    typedef PolygonGroupSpatialObject< TDimension >             Self;
    typedef BlobSpatialObject< TDimension >            Superclass;
    typedef SmartPointer < Self >                      Pointer;
    typedef SmartPointer < const Self >                ConstPointer;
    typedef typename Superclass::PointType             PointType;
    typedef typename Superclass::BoundingBoxType       BoundingBoxType;
    /** Method for creation through the object factory. */
    itkNewMacro( Self );

    /** Method for creation through the object factory. */
    itkTypeMacro( Self, Superclass );
  
    /** Return true if the given PolygonSpatialObject is successfully
     * added to the PolygonGroup.
     */
    bool AddStrand(PolygonSpatialObject<TDimension> *toAdd);

    /** Return true if the given PolygonSpatialObject is successfully
     * removed from the PolygonGroup
     */
    bool DeleteStrand(PolygonSpatialObject<TDimension> *toDelete);

    /** Return true if the given PolygonSpatialObject successfully
     * replaces the Polygon given in toReplace. This will fail if
     * toReplace is not a strand in the PolygonGroupObject.
     */
    bool ReplaceStrand(PolygonSpatialObject<TDimension> *toReplace,
                       PolygonSpatialObject<TDimension> *replacement);

    /** Return true if all constituent Polygons are closed. */
    bool IsClosed();
    /** returns the number of Polygons in this PolygonGroup */
    unsigned int NumberOfStrands();
    /** Volume of this PolygonGroup, which is the sum of the volume of all
     * its constituent Polygons
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
#include "itkPolygonGroupSpatialObject.txx" 
#endif 

#endif
