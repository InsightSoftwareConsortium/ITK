/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkParametricSpaceToImageSpaceMeshFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkParametricSpaceToImageSpaceMeshFilter_h
#define __itkParametricSpaceToImageSpaceMeshFilter_h

#include "itkMeshToMeshFilter.h"

namespace itk
{

/** \class ParametricSpaceToImageSpaceMeshFilter
 * \brief 
 *
 * ParametricSpaceToImageSpaceMeshFilter takes an itk::Mesh on which 
 * the point Data is expected to contain itk::Index of itk::Image pixels
 * associated with each point of the Mesh, and construct with them a new
 * mesh whose points are in the coordinates of those pixels.
 *
 * The input mesh is assumed to represent pixels in some parametric space.
 * The output mesh is suitable to be superimposed as an overlay to the
 * associated image.
 * by the user. 
 *
 * The additional content of the mesh is passed untouched. Including the 
 * connectivity and the additional information contained on cells and points.
 * 
 * \warning This filter also assumes that the Output Mesh has as type 
 * for the PointDataType the PointType of the input mesh.
 * 
 * \ingroup MeshFilters
 */
template <class TInputMesh, class TOutputMesh >
class ITK_EXPORT ParametricSpaceToImageSpaceMeshFilter : 
    public MeshToMeshFilter<TInputMesh,TOutputMesh>
{
public:
  /** Standard class typedefs. */
  typedef ParametricSpaceToImageSpaceMeshFilter  Self;
  typedef MeshToMeshFilter<TInputMesh,TOutputMesh> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Type for representing coordinates. */
  typedef typename TInputMesh::CoordRepType  CoordRepType;

  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ParametricSpaceToImageSpaceMeshFilter,MeshToMeshFilter);

protected:
  ParametricSpaceToImageSpaceMeshFilter();
  ~ParametricSpaceToImageSpaceMeshFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Generate requested data. */
  virtual void GenerateData( void );

  /** Generate additional information in the output  */
  virtual void GenerateOutputInformation( void );

private:
  ParametricSpaceToImageSpaceMeshFilter(const ParametricSpaceToImageSpaceMeshFilter&); //purposely not implemented
  void operator=(const ParametricSpaceToImageSpaceMeshFilter&); //purposely not implemented
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkParametricSpaceToImageSpaceMeshFilter.txx"
#endif

#endif
