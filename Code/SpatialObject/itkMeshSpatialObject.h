/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMeshSpatialObject_h
#define __itkMeshSpatialObject_h

#include "itkMesh.h"
#include "itkExceptionObject.h"
#include "itkSpatialObject.h"

namespace itk
{
  
/** \class MeshSpatialObject
 * \brief Implementation of an Mesh as spatial object.
 *
 * This class combines functionnalities from a spatial object,
 * and an itkMesh.
 *
 * \sa SpatialObject
 */

template <class TMesh = Mesh<int> >
class MeshSpatialObject 
: public SpatialObject< ::itk::GetMeshDimension<TMesh>::PointDimension >
{

public:
 
  typedef double ScalarType; 
  typedef MeshSpatialObject< TMesh>                  Self;
   
  itkStaticConstMacro(Dimension, unsigned int,TMesh::PointDimension);

  typedef SpatialObject< itkGetStaticConstMacro(Dimension) > Superclass;
  typedef SmartPointer< Self >                       Pointer;
  typedef SmartPointer< const Self >                 ConstPointer;

  typedef TMesh                                      MeshType;
  typedef typename MeshType::Pointer                 MeshPointer;
  typedef typename Superclass::TransformType         TransformType;
  typedef typename Superclass::PointType             PointType;
  typedef typename Superclass::BoundingBoxType       BoundingBoxType;

  typedef VectorContainer< unsigned long, PointType> PointContainerType;
  typedef typename PointContainerType::Pointer       PointContainerPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( MeshSpatialObject, SpatialObject );

  /** Set the Mesh. */
  void SetMesh( MeshType * Mesh );

  /** Get a pointer to the Mesh currently attached to the object. */
  MeshType * GetMesh( void );

  /** Return true if the object is evaluable at the requested point, 
   *  and else otherwise. */
  bool IsEvaluableAt( const PointType & point, 
                      unsigned int depth=0, char *name=NULL) const;

  /** Returns the value of the Mesh at the requested point. 
   *  If the point is not inside the object, then an exception is thrown.
   * \sa ExceptionObject */
  bool ValueAt( const PointType & point, double & value, 
                unsigned int depth=0, char *name=NULL) const;
  
  /** Returns true if the point is inside, false otherwise. */
  bool IsInside( const PointType & point,
                 unsigned int depth, char *name) const;

  /** Test whether a point is inside or outside the object 
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */ 
  virtual bool IsInside( const PointType & point) const;
 
  /** Compute the boundaries of the iamge spatial object. */
  bool ComputeLocalBoundingBox() const;

  /** Returns the latest modified time of the object and its component. */
  unsigned long GetMTime( void ) const;

  /** Return the type of pixel used */
  const char* GetPixelType()
    {
    return m_PixelType.c_str();
    }

  /** Set/Get the precision for the IsInside function. 
   *  This is used when the cell is a triangle, in this case, it's more likely 
   *  that the given point will not be falling exactly on the triangle surface.
   *  If the distance from the point to the surface is <= to 
   *  m_IsInsidePrecision the point is considered inside the mesh. 
   *  The default value is 1. */
  itkSetMacro(IsInsidePrecision, double);
  itkGetMacro(IsInsidePrecision, double);

protected:

  MeshPointer m_Mesh;
  std::string m_PixelType;
  double      m_IsInsidePrecision;

  MeshSpatialObject();
  virtual ~MeshSpatialObject();

  void PrintSelf( std::ostream& os, Indent indent ) const;

};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeshSpatialObject.txx"
#endif

#endif //__itkMeshSpatialObject_h
