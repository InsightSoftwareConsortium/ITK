/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompositeSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
 
#ifndef __CompositeSpatialObject_h 
#define __CompositeSpatialObject_h 
 
#include "itkSpatialObject.h" 
 
#include <list> 
 
namespace itk 
{ 

/** \class CompositeSpatialObject
* \brief Object composed of spatial objects
*
* This class represent a composite object to which it's possible
* to plug any kind of spatial object. A composite object has exactly
* the same specifications that a simple spatial object. It can be considered
* more or less like an interface to manage a list of spatial objects.
* \also SpatialObject
*/ 
 
template< unsigned int NDimensions = 3, 
          class TransformType = AffineTransform< double,
                                                 NDimensions 
                                               >,
          class OutputType = double
        > 
class CompositeSpatialObject 
: public SpatialObject< NDimensions, TransformType, OutputType >
{ 
 
public: 

  typedef CompositeSpatialObject< NDimensions, TransformType, OutputType > Self; 
  typedef SpatialObject< NDimensions, TransformType, OutputType > Superclass; 
  typedef typename Superclass::Pointer SuperclassPointer; 
  typedef SmartPointer< Self > Pointer; 
  typedef SmartPointer< const Self > ConstPointer; 
  typedef std::list< SuperclassPointer > ChildrenListType; 
     
  /** Method for creation through the object factory */ 
  itkNewMacro(Self); 
  itkTypeMacro(Self, Superclass); 

  /** 
  * Add an object to the list of children. 
  */ 
  void AddSpatialObject( SuperclassPointer pointer ); 
     
  /** 
  * Remove the object passed as arguments from the list of 
  * children. May this function 
  * should return a false value if the object to remove is 
  * not found in the list. 
  */ 
  void RemoveSpatialObject( SuperclassPointer object ); 

  /** 
  * Execute the IsInside() function for each children of this 
  * object until one 
  * returns that the point is inside. If the point is inside 
  * of none of the objects contains in this composite object, 
  * it returns false. 
  */ 
  bool IsInside( const PointType &  point ); 

  /**
  * Check if any of the children of this spatial object is 
  * evaluable at the point passed as reference and returns true
  * if any of them is evaluable at that point. If none of them
  * is evaluable, this function returns false.
  */ 
  bool IsEvaluableAt( const PointType & point );

  /**
  * First, this functions looks for a children evaluable at the 
  * the specified point, and then try to compute the n-order derivative
  * at this point. If the derivtive cannot be computed at that place, 
  * this function throws an exception.
  */
  void DerivativeAt( const PointType & point, short unsigned int order, OutputVectorType & value ); 

  //OutputVectorType ComputeDerivative( OutputVectorType & v2, OutputVectorType & v1 );

  /**
  * Find if there is an evaluable children for the specified point,
  * and then returns the value at that point. If no children is evaluable
  * it throws an exception.
  */
  virtual void ValueAt( const PointType & point, OutputType & value );

  /** 
  * Returns a list of pointer to the children affiliated to this object. 
  */ 
  ChildrenListType & GetChildren( void );

  /**
  * Returns the number of children currently assigned to the composite object.
  */ 
  unsigned int GetNumberOfChildren( void ); 

  /** 
  * Set the list of pointers to children to the list passed as argument. 
  */ 
  void SetChildren( ChildrenListType & children ); 

  /**
  * Compute the bounding box containing all the bounding boxes of
  * the children of this composite object.
  */
  void ComputeBounds( void );

  /**
  * Returns the latest modified time of all the objects contained in this composite
  * object.
  */
  unsigned long GetMTime( void ) const;

  /**
  * Rebuild the list of transform applied to the object to switch from the local
  * coordinate system, to the real world coordinate system.
  */
  virtual void RebuildLocalToGlobalTransformList( void );
  
  /**
  * Rebuild the list of transforms applied to the object to switch from the real
  * world coordinate systemn to the local coordinate system.
  */
  virtual void RebuildGlobalToLocalTransformList( void );

protected: 

  /**
  * List of the children object plug to the composite spatial object.
  */
  ChildrenListType m_Children; 

  /** 
  * constructor 
  */ 
  CompositeSpatialObject(); 

  /** 
  * destructor 
  */ 
  virtual ~CompositeSpatialObject();

  /**
  * Print the object informations in a stream.
  */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const; 
}; 

} // end of namespace itk
 
#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkCompositeSpatialObject.txx" 
#endif 
 
#endif // __CompositeSpatialObject_h 
