/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAffineTransformMeshFilter_h
#define __itkAffineTransformMeshFilter_h

#include "itkMeshToMeshFilter.h"
#include "itkAffineTransform.h"

namespace itk
{

/** \class AffineTransformMeshFilter
 * \brief 
 *
 * AffineTransformMeshFilter applies an Affine Transform to all the points
 * of a mesh.
 *
 * The additional content of the mesh is passed untouched. Including the 
 * connectivity and the additional information contained on cells and points.
 * 
 * Meshes that have added information like normal vector on the points, will
 * have to take care of transforming this data by other means.
 * 
 */
template <class TInputMesh, class TOutputMesh>
class ITK_EXPORT AffineTransformMeshFilter : 
                              public MeshToMeshFilter<TInputMesh,TOutputMesh>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AffineTransformMeshFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef MeshToMeshFilter<TInputMesh,TOutputMesh> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /** 
   * Type of the Affine Transform
   */
  typedef AffineTransform< typename TInputMesh::CoordRepType,
                           TInputMesh::PointDimension> AffineTransformType;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(AffineTransformMeshFilter,MeshToMeshFilter);

  /** 
   * Generate Requested Data
   */
  virtual void GenerateData( void );

  /** 
   * Set Affine Transform
   */
  void SetAffineTransform( const AffineTransformType & transform ) 
  { m_AffineTransform = transform;  this->Modified(); }

  /** 
   * Get Affine Transform
   */
  const AffineTransformType & GetAffineTransform( void ) const
  { return m_AffineTransform; }


protected:
  AffineTransformMeshFilter();
  ~AffineTransformMeshFilter() {};
  AffineTransformMeshFilter(const AffineTransformMeshFilter&) {};
  void operator=(const AffineTransformMeshFilter&) {};
  void PrintSelf(std::ostream& os, Indent indent);
  
 /**
  *  Affine transform to apply to all the mesh points
  */
  AffineTransformType         m_AffineTransform;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAffineTransformMeshFilter.txx"
#endif

#endif
