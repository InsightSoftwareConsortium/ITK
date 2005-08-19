/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformableSimplexMesh3DGradientConstraintForceFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _DeformableSimplexMesh3DGradientConstraintForceFilter_h
#define _DeformableSimplexMesh3DGradientConstraintForceFilter_h

#include "itkDeformableSimplexMesh3DFilter.h"
#include "itkMesh.h"
#include "itkVector.h"
#include "itkImage.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkCovariantVector.h"

#include <set>
#include <vector>
namespace itk
{
class ImageVoxel 
{
  
public:
  // voxel coordinates
  unsigned int vpos[3];
  // subvoxel coordinates (in cartesian space)
  double  spos[3];
  // voxel value converted to a double
  double value;
  // distance from line origin
  double distance;
  // index
  unsigned int index;

  ImageVoxel() {};
  ImageVoxel(int *pos, double *subpos, double value, double distance, unsigned int index)
  {
    this->vpos[0] = pos[0];
    this->vpos[1] = pos[1];
    this->vpos[2] = pos[2];
    this->spos[0] = subpos[0];
    this->spos[1] = subpos[1];
    this->spos[2] = subpos[2];
    this->value = value;
    this->distance = distance;
    this->index = index;
  };

  /// returns voxel X coordinate (voxel column)
  unsigned int getX(void) const { return vpos[0]; }
  /// returns voxel Y coordinate (voxel row)
  unsigned int getY(void) const { return vpos[1]; }
  /// returns voxel Z coordinate (voxel plane)
  unsigned int getZ(void) const { return vpos[2]; }
  /// returns voxel distance to origin
  double getDistance(void) const { return distance; }
  /// returns voxel value
  double getValue(void) const { return value; }
  /// returns voxel position
  
  /// set the value of the voxel
  void setValue(const double val) { value = val; }
  
};
  
/** \class DeformableSimplexMesh3DGradientConstraintForceFilter
 * \brief
 * Additional to its superclass this class reimplemets the external forces methos
 * in which the scan line algorithm is used to find highest gradient is found in 
 * the direction of the normal to each vertex within a specified range.
 *
 * \author Leila Baghdadi. Mouse Imaging Centre, Hospital for Sick Children, Toronto, Ontario,Canada.
 *  I acknowledge the helpful insights of Herve Delingette of INRIA, France.
 */

template <class TInputMesh, class TOutputMesh>
class DeformableSimplexMesh3DGradientConstraintForceFilter : public DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh>
{
public:
  /** Standard "Self" typedefs. */
  typedef DeformableSimplexMesh3DGradientConstraintForceFilter Self;

  /** Standard "Superclass" typedef. */
  typedef  DeformableSimplexMesh3DFilter<TInputMesh, TOutputMesh> Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self); 

  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformableSimplexMesh3DGradientConstraintForceFilter, DeformableSimplexMesh3DFilter); 

  /** Some typedefs. */
  typedef TInputMesh InputMeshType;
  typedef TOutputMesh OutputMeshType;
  typedef typename Superclass::PointType                  PointType;
  typedef typename Superclass::GradientIndexType          GradientIndexType;
  typedef typename Superclass::GradientIndexValueType     GradientIndexValueType;

  /* Mesh pointer definition. */
  typedef typename InputMeshType::Pointer     InputMeshPointer;
  typedef typename OutputMeshType::Pointer    OutputMeshPointer;

  typedef typename InputMeshType::PixelType         PixelType;

  typedef Image<PixelType, 3>                                   GradientIntensityImageType;
  typedef typename GradientIntensityImageType::Pointer          GradientIntensityImagePointer;

  typedef Image<float, 3>                        OriginalImageType;
  typedef typename OriginalImageType::IndexType           OriginalImageIndexType;
  typedef typename OriginalImageIndexType::IndexValueType      ImageIndexValueType;
  typedef typename OriginalImageType::Pointer             OriginalImagePointer;
  /** control the range of search for Bresenham at normal line */
  itkSetMacro(Range, int);
  itkGetMacro(Range, int);
  
  // full segment or half segment direction
  enum SIDE
  {
    // half segment in direction
    NORMAL,
    // half segment in -direction
    INVERSE,
    // complete segment
    BOTH
  };

  /**
   * Set Original image
   */ 
  itkSetMacro(Image, OriginalImagePointer);

protected:
  DeformableSimplexMesh3DGradientConstraintForceFilter();
  ~DeformableSimplexMesh3DGradientConstraintForceFilter();
  DeformableSimplexMesh3DGradientConstraintForceFilter(const Self&) 
  {
  }
  void operator=(const Self&){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Compute the external force component
      */
  virtual void ComputeExternalForce(SimplexMeshGeometry* data);  

  /** 
   * Range of search for Bresenham algorithm (normal line at each vertex)
   */
  int m_Range;

private:
  double NextVoxel(const double* pp, int *ic, double *x, double *y, double *z);
    
  int Signi(double a);
  
  void Clear();

  // line starting voxel
  ImageVoxel *m_StartVoxel;
  // line voxels in direction
  std::vector<ImageVoxel *> m_Positive;
  // line voxels in -direction
  std::vector<ImageVoxel *> m_Negative;

  OriginalImagePointer m_Image;

}; // end of class

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableSimplexMesh3DGradientConstraintForceFilter.txx"
#endif

#endif /* __DeformableSimplexMesh3DGradientConstraintForceFilter_h */
