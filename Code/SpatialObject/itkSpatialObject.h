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
#include <list> 

namespace itk  
{ 

/** 
* \class SpatialObject
* \brief Implementation of the composite pattern
*
* The purpose of this class is to implement the composite pattern
* within itk, so that it becomes easy to create an environment, and to
* manipulate the environment as a whole or any of its components.  An
* object has a list of transformations to transform local coordinates
* to the corresponding coordinates in the real world coordinate
* system, and a list of inverse transformation to go backward.  Any
* spatial objects can be plugged to a spatial object as children.  To
* implement your own spatial object, you need to derive from the
* following class, which requires the definition of just a few pure
* virtual functions.  Examples of such functions are ValueAt(),
* IsEvaluableAt(), and IsInside(), each of which has a meaning
* specific to each particular object type.
*/ 
 
template< unsigned int NDimensions = 3, 
          unsigned int SpaceDimension = 3 
        > 
class SpatialObject 
:public NDimensionalSpatialObject<NDimensions>
{ 

public: 

  typedef double ScalarType;

  itkStaticConstMacro(MaximumDepth, unsigned int, 9999999);

  typedef SpatialObject<NDimensions,SpaceDimension> Self;
  typedef NDimensionalSpatialObject<NDimensions> Superclass; 
  
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  
  typedef Point < ScalarType, NDimensions > PointType; 
  typedef PointType * PointPointer; 
  
  typedef Vector< ScalarType, NDimensions > VectorType; 
  typedef VectorType * VectorPointer;

  typedef CovariantVector< double, NDimensions > OutputVectorType; 
  typedef OutputVectorType * OutputVectorPointer;

  typedef AffineTransform< double, NDimensions>   TransformType;
  typedef typename TransformType::Pointer  TransformPointer;
  typedef const TransformType*             TransformConstPointer;
  
  typedef std::list< TransformType * > TransformListType;
  
  typedef VectorContainer< unsigned long int, PointType > VectorContainerType;
  
  typedef BoundingBox< unsigned long int, NDimensions, ScalarType,
                       VectorContainerType > BoundingBoxType; 
  typedef typename BoundingBoxType::Pointer BoundingBoxPointer; 

  typedef std::list< Self * > ChildrenListType; 
  typedef ChildrenListType* ChildrenListPointer; 

  /** Dimension of the object.  This constant is used by functions that are
   * templated over spatialObject type when they need compile time access 
   * to the dimension of the object. */
  itkStaticConstMacro(ObjectDimension, unsigned int, NDimensions);

  /** Method for creation through the object factory. */
  itkNewMacro( Self );
 
  /** Run-time type information (and related methods). */ 
  itkTypeMacro( Self, Superclass );

  /** Set the bounding box of the object. */
  void SetBoundingBox( BoundingBoxPointer bounds ); 

  /** Get the bounding box of the object. */
  virtual BoundingBoxType * GetBoundingBox( void ) const; 

  /** This is the transform applied from the origin of the parent
   *  if the origin of the child is different from the origin of the parent
   *  (i.e. m_Origin) then the transformation is applied from this origin
   *  this origin is transformed by the parent's transform before applying
   *  any transformation */
  void SetTransform( TransformType * transform ); 
  TransformType * GetTransform( void ); 
  const TransformType * GetTransform( void ) const; 

  /** This defines the transformation from the global coordinate frame.
   *  By setting this transform, the local transform is computed */
  void SetGlobalTransform( TransformType * transform );
  TransformType * GetGlobalTransform( void );
  const TransformType * GetGlobalTransform( void ) const;

  /** Set the center of the rotation */
  itkSetMacro(CenterOfRotation,PointType);
  itkGetMacro(CenterOfRotation,PointType);

  /** Returns a degree of membership to the object. 
   *  That's useful for fuzzy objects. */ 
  virtual bool ValueAt( const PointType & point, double & value,
                        unsigned int depth=0,
                        char * name = NULL) const;
     
  /** Return tru if the object provides a method to evaluate the value 
   *  at the specified point, else otherwise. */
  virtual bool IsEvaluableAt( const PointType & point,
                              unsigned int depth=0,
                              char * name = NULL) const;

  /** Test whether a point is inside or outside the object. */ 
  virtual bool IsInside( const PointType & point,
                         unsigned int depth=0,
                         char * name = NULL) const;

  /** Set the pointer to the parent object in the tree hierarchy
   *  used for the spatial object patter. */
  void SetParent( const Superclass * parent );

  /** Return the n-th order derivative value at the specified point. */
  void DerivativeAt( const PointType & point,
                     short unsigned int order,
                     OutputVectorType & value,
                     unsigned int depth=0,
                     char * name = NULL);

  /** Returns the coordinates of the point passed as argument in the object
   * local coordinate system. */
  void TransformPointToLocalCoordinate( PointType & p ) const;

  /** Returns the coordinates of the point passed as argument in the object
   * local coordinate system. */
  void TransformPointToGlobalCoordinate( PointType & p ) const; 

  /** Returns the list of local to global transforms */
  TransformListType & GetGlobalTransformList( void );

  /** 
   * Compute an axis-aligned bounding box for the object and its selected
   * children, down to a specified depth.  After computation, the
   * resulting bounding box is stored in this->m_Bounds.  Once this
   * function is called with a specific value of \p depth and \p name,
   * future calls, irrespective of the parameters, will leave the
   * bounding box unchanged until the spatial object is modified and the
   * modification time updated.
   *
   * This function has to be implemented in the deriving class. 
   *
   * \param depth Include children down to this depth.  If \p depth = 0,
   * include only the object itself.
   * \param name Include only objects whose type string contains \p
   * name.  
   * \return \c true if, after the function completes, the bounding box
   * reflects object information, and \c false if the bounding box is
   * still in an initial state.  The return value is mainly used by recursive
   * calls of this function.
   *  every time one of the object component is changed.  
   */ 
  virtual bool ComputeBoundingBox( unsigned int depth=0,
                                   char * name = NULL);

  /** Set the Spacing of the spatial object */
  void SetSpacing( const double spacing[ObjectDimension] );

  /** Set the Scale of the spatial object */
  void SetScale( const double scale[ObjectDimension] );

  /** Get the spacing of the spatial object */
  const double* GetSpacing() const {return m_Spacing;}
  
  /** Get the spacing of the spatial object */
  const double* GetScale() const {return m_Scale;}

  /** Get the spacing of the spatial object */
  const double* GetGlobalScale() const {return m_GlobalScale;}

  /** Returns the latest modified time of the spatial object, and 
   * any of its components. */
  unsigned long GetMTime( void ) const;

  /** Compute the Global transform when the local transform is set
   *  This function should be called each time the local transform
   *  has been modified */
  void ComputeGlobalTransform(void);

  /** Compute the Local transform when the global transform is set */
  void ComputeTransform(void);

  /** Add an object to the list of children. */ 
  void AddSpatialObject( Self * pointer ); 
     
  /** Remove the object passed as arguments from the list of 
   * children. May this function 
   * should return a false value if the object to remove is 
   * not found in the list. */ 
  void RemoveSpatialObject( Self * object ); 

  /** Returns a list of pointer to the children affiliated to this object. 
   * A depth of 0 returns the immediate childred. A depth of 1 returns the
   * children and those children's children. */ 
  virtual ChildrenListType * GetChildren( unsigned int depth=0,
                                          char * name = NULL) const;

  /** Returns the number of children currently assigned to the object. */ 
  unsigned int GetNumberOfChildren( unsigned int depth=0,
                                    char * name = NULL);

  /** Set the list of pointers to children to the list passed as argument. */ 
  void SetChildren( ChildrenListType & children ); 

  /** Clear the spatial object by deleting all lists of children
   * and subchildren */
  virtual void Clear(void);

  /** Return the Modified time of the LocalToGlobalTransform */
  unsigned long GetTransformMTime(void);

  /** Return the Modified time of the GlobalToLocalTransform */
  unsigned long GetGlobalTransformMTime(void);

protected: 
  
  BoundingBoxPointer  m_Bounds; 
  unsigned long       m_BoundsMTime;
  double              m_Spacing[ObjectDimension];
  double              m_Scale[ObjectDimension];
  double              m_GlobalScale[ObjectDimension];
  PointType           m_CenterOfRotation;
  TransformListType   m_GlobalTransformList;

  TransformPointer    m_Transform;
  TransformPointer    m_TransformWithCoR;
  TransformPointer    m_GlobalTransform; 

  /** Constructor. */ 
  SpatialObject(); 

  /** Destructor. */ 
  virtual ~SpatialObject(); 

  virtual void PrintSelf( std::ostream& os, Indent indent ) const; 

  /** List of the children object plug to the composite 
   *  spatial object. */
  ChildrenListType m_Children; 


}; 

} // end of namespace itk
 
#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkSpatialObject.txx" 
#endif 
 
#endif // __itkSpatialObject_h
