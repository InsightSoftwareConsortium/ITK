/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkTubeSpatialObject_h
#define __itkTubeSpatialObject_h

#include <list>

#include "itkSpatialObject.h"
#include "itkTubeSpatialObjectPoint.h"

namespace itk 
{

/** 
* \class TubeSpatialObject
* \brief Representation of a tube based on the spatial object classes.
*
* The tube is basically defined by a set of points. Each tube can
* be connected to a tube network, by using the AddSpatialObject() methods
* of a TubeSpatialObject Object. A tube is also identified by an id number when connected
* to a network.
*
* \also TubeSpatialObjectPoint 
*/

template < unsigned int TDimension = 3 >
class TubeSpatialObject 
:public SpatialObject< TDimension >
{

public:

  typedef TubeSpatialObject                            Self;
  typedef SpatialObject< TDimension >                  Superclass;
  typedef SmartPointer < Self >                        Pointer;
  typedef SmartPointer < const Self >                  ConstPointer;
  typedef double                                       ScalarType;
  typedef itk::TubeSpatialObjectPoint< TDimension >    TubePointType;
  typedef std::list < TubePointType >                  PointListType;
  typedef PointListType *                              PointListPointer;
  typedef typename Superclass::PointType               PointType;
  typedef VectorContainer<unsigned long,PointType>     PointContainerType;
  typedef SmartPointer<PointContainerType>             PointContainerPointer;
  typedef typename Superclass::VectorType              VectorType;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Method for creation through the object factory. */
  itkTypeMacro( Self, Superclass );
    
  /** Returns a reference to the list of the tube points.*/
  PointListType & GetPoints( void );

  /** Returns a reference to the list of the tube points.*/
  const PointListType & GetPoints( void ) const;

  /** Set the list of tube points.*/
  //void SetPoints( PointListPointer newPoints );
  void SetPoints( PointListType & newPoints );

  /** Remove the list of tube points */
  void Clear(void);

  /** Calculate the normalized tangent */
  bool CalcTangent( void );

   /** Calculate the normal vector of the tube.*/
  bool CalcNormal( void );

  /** Returns true if the tube is evaluable at the requested point, 
   *  false otherwise. */
  bool IsEvaluableAt( const PointType & point,
                      unsigned int depth=0, char * name=NULL) const;

  /** Returns the value of the tube at that point.
   *  Currently this function returns a binary value,
   *  but it might want to return a degree of membership
   *  in case of fuzzy tubes. */
  bool ValueAt( const PointType & point, double & value,
                unsigned int depth=0, char * name=NULL) const;

  /** Returns true if the point is inside the tube, false otherwise. */
  bool IsInside( const PointType & point,
                 unsigned int depth=0, char * name=NULL) const;

  /** Compute the boundaries of the tube. */
  bool ComputeBoundingBox( unsigned int depth=0, char * name=NULL);

  /** Set/Get the parent point which corresponds to the 
   *  position of the point in the parent's points list */
  itkSetMacro(ParentPoint,int);
  itkGetMacro(ParentPoint,int);

protected:

  PointListType     m_Points;
  int               m_ParentPoint;

  TubeSpatialObject();
  virtual ~TubeSpatialObject();

  /** Method to print the object.*/
  virtual void PrintSelf( std::ostream& os, Indent indent ) const;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkTubeSpatialObject.txx" 
#endif 

#endif // __itkTubeSpatialObject_h
