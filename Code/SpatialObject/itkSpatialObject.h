/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
 
#ifndef __itkSpatialObject_h 
#define __itkSpatialObject_h 
 
#include "itkNDimensionalSpatialObject.h"
#include "itkBoundingBox.h"
#include "itkPoint.h"
#include "itkAffineTransform.h"
#include "itkSmartPointer.h" 
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "itkExceptionObject.h" 
#include "itkSpatialObjectProperty.h" 
 
namespace itk  
{ 

/** 
* \class SpatialObject
* \brief Implementation of the composite pattern
*
* The purpose of this class is to implement the composite pattern within itk,  
* so that it becomes easy to create a whole environment, 
* and to manipulate this one, or any of the objects inside it.  
* An object has a list of transformations to transform local coordinates to 
* the corresponding coordinates in the real world coordinates system, and a
* list of inverse transformation to go backward.
* Each object can be plug to a composite spatial object, and it will then
* be affected by all modifications ( translation, rotation,...)
* applied to its parent object.
* To implement your own spatial object, you need to derive from the following class,
* which imply the definition of just a few pure virtual function, like for instance
* ValueAt(), IsEvaluableAt(), and IsInside() which are specific to each particular
* object.
*/ 
 
template< unsigned int NDimensions = 3, 
          typename TTransform = AffineTransform< double, 
                                                 NDimensions
          > ,
          unsigned int PipelineDimension = 3 
        > 
class SpatialObject 
:public NDimensionalSpatialObject<PipelineDimension>
{ 

public: 

  typedef double ScalarType;

  typedef SpatialObject<NDimensions,TTransform,PipelineDimension> Self;
  typedef Object Superclass; 
  
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef SpatialObjectProperty< float > PropertyType; 
  typedef typename PropertyType::Pointer  PropertyPointer; 
  
  typedef Point < ScalarType, NDimensions > PointType; 
  typedef PointType * PointPointer; 
  
  typedef Vector< ScalarType, NDimensions > VectorType; 
  typedef VectorType * VectorPointer;

  typedef CovariantVector< double, NDimensions > OutputVectorType; 
  typedef OutputVectorType * OutputVectorPointer;

  typedef TTransform            TransformType;
  typedef typename TransformType::Pointer  TransformPointer;
  typedef const TransformType *  TransformConstPointer;
  
  typedef std::list< const TransformType * > TransformListType;
  typedef TransformListType * TransformListPointer;
  
  typedef VectorContainer< unsigned long int, PointType > VectorContainerType;
  
  typedef BoundingBox< unsigned long int, NDimensions, ScalarType, VectorContainerType > BoundingBoxType; 
  typedef typename BoundingBoxType::Pointer BoundingBoxPointer; 


  /** Method for creation through the object factory. */
  itkNewMacro( Self );
 
  /** Run-time type information (and related methods). */ 
  itkTypeMacro( Self, Superclass );

  /** Set the bounding box of the object. */
  void SetBounds( BoundingBoxPointer bounds ); 

  /** Get the bounding box of the object. */
  BoundingBoxType * GetBounds( void ); 

  /*
  * Returns a pointer to the property object applied
  * to this class.
  */
  PropertyType * GetProperty( void );

  /*
  * Set the property applied to the object.
  */
  void SetProperty( const PropertyType * property ); 

  void SetLocalToGlobalTransform( const TransformType * transform ); 
  const TransformType * GetLocalToGlobalTransform( void ); 
  
  void SetGlobalToLocalTransform( const TransformType * transform ); 
  const TransformType * GetGlobalToLocalTransform( void ); 


  /** 
  * Returns a degree of membership to the object. 
  * That's useful for fuzzy objects.
  */ 
  virtual void ValueAt( const PointType & point, double & value ) = 0; //purposely not implemented
     
  /*
  * return ture if the object provides a method to evaluate the value 
  * at the specified point, else otherwise.
  */
  virtual bool IsEvaluableAt( const PointType & point ) = 0; // purposely not implemented

  /** 
  * Test whether a point is inside or outside the object. 
  */ 
  virtual bool IsInside( const PointType & point ) = 0; // purposely not implemented

  /**
  * Set the pointer to the parent object in the tree hierarchy
  * used for the spatial object patter.
  */
  void SetParent( const Self * parent );

  /**
  * Return true if the object has a parent object. Basically, only
  * the root object , or some isolated objects should return false.
  */
  bool HasParent( void ) const;

  /**
  * Return the n-th order derivative value at the specified point.
  */
  virtual void DerivativeAt( const PointType & point, short unsigned int order, OutputVectorType & value );

  /** 
  * Returns the coordinates of the point passed as argument in the object
  * local coordinate system.
  */
  void TransformPointToLocalCoordinate( PointType & p ) const;

  /** 
  * Returns the coordinates of the point passed as argument in the object
  * local coordinate system.
  */
  void TransformPointToGlobalCoordinate( PointType & p ) const; 

  /**
  * Build the list of local to global transforms to applied to the SpatialObject.
  * If init equals false, then the list will be initialized.
  */
  void BuildLocalToGlobalTransformList( TransformListPointer list, bool init ) const;

  /**
  * Build the list of global to local transforms applied to the SpatialObject.
  * If init equals false, then the list will be initialized.
  */
  void BuildGlobalToLocalTransformList( TransformListPointer list, bool init ) const;

  /**
  * Returns the list of local to global transforms.
  */
  TransformListPointer GetLocalToGlobalTransformList( void );

  /**
  * Returns the list of global to local transforms.
  */
  TransformListPointer GetGlobalToLocalTransformList( void );

  /** 
  * This function has to be implemented in the deriving class. 
  * It should provide a method to get the boundaries of 
  * a specific object. Basically, this function need to be called
  * every time one of the object component is changed. 
  */ 
  virtual void ComputeBounds( void ) = 0; // purposely not implemented 

  /**
  * Returns the latest modified time of the spatial object, and 
  * any of its components.
  */
  unsigned long GetMTime( void ) const;

  /**
  * Rebuild the list of transform applied to the object to switch from the local
  * coordinate system, to the real world coordinate system.
  */
  virtual void RebuildLocalToGlobalTransformList( void ) const;

  /**
  * Rebuild the list of transforms applied to the object to switch from the real
  * world coordinate systemn to the local coordinate system.
  */
  virtual void RebuildGlobalToLocalTransformList( void ) const;

  /**
  * Rebuild all the transforms list. Basically, this function is performed every time
  * an object is plugged or unplugged to a hierarchy of objects.
  */
  virtual void RebuildAllTransformLists( void ) const;


protected: 

  BoundingBoxPointer m_Bounds; 
  PropertyPointer m_Property; 
  ConstPointer m_Parent; 
  TimeStamp m_BoundsMTime;

  TransformListPointer m_LocalToGlobalTransformList;
  TransformListPointer m_GlobalToLocalTransformList;

  TransformConstPointer m_LocalToGlobalTransform; 
  TransformConstPointer m_GlobalToLocalTransform; 

  /** Constructor. */ 
  SpatialObject(); 

  /** Destructor. */ 
  virtual ~SpatialObject(); 

  virtual void PrintSelf( std::ostream& os, Indent indent ) const; 

}; 

} // end of namespace itk
 
#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkSpatialObject.txx" 
#endif 
 
#endif // __itkSpatialObject_h
