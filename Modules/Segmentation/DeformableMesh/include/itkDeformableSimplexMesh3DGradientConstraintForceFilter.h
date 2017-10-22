/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkDeformableSimplexMesh3DGradientConstraintForceFilter_h
#define itkDeformableSimplexMesh3DGradientConstraintForceFilter_h

#include "itkDeformableSimplexMesh3DFilter.h"
#include "itkMesh.h"
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
  unsigned int m_Vpos[3];
  // subvoxel coordinates (in cartesian space)
  double m_Spos[3];
  // voxel value converted to a double
  double m_Value;
  // distance from line origin
  double m_Distance;
  // index
  unsigned int m_Index;

  ImageVoxel() {}
  ImageVoxel(int *pos, double *subpos, double val, double dist, unsigned int ind)
  {
    this->m_Vpos[0] = pos[0];
    this->m_Vpos[1] = pos[1];
    this->m_Vpos[2] = pos[2];
    this->m_Spos[0] = subpos[0];
    this->m_Spos[1] = subpos[1];
    this->m_Spos[2] = subpos[2];
    this->m_Value = val;
    this->m_Distance = dist;
    this->m_Index = ind;
  }

  /// returns voxel X coordinate (voxel column)
  unsigned int GetX(void) const { return m_Vpos[0]; }
  /// returns voxel Y coordinate (voxel row)
  unsigned int GetY(void) const { return m_Vpos[1]; }
  /// returns voxel Z coordinate (voxel plane)
  unsigned int GetZ(void) const { return m_Vpos[2]; }
  /// returns voxel distance to origin
  double GetDistance(void) const { return m_Distance; }
  /// returns voxel value
  double GetValue(void) const { return m_Value; }
  /// returns voxel position

  /// set the value of the voxel
  void SetValue(const double val) { m_Value = val; }
};

/** \class DeformableSimplexMesh3DGradientConstraintForceFilter
 * \brief
 * Additional to its superclass this class reimplemets the external forces methos
 * in which the scan line algorithm is used to find highest gradient is found in
 * the direction of the normal to each vertex within a specified range.
 *
 * \author Leila Baghdadi. Mouse Imaging Centre, Hospital for Sick Children, Toronto, Ontario,Canada.
 *  I acknowledge the helpful insights of Herve Delingette of INRIA, France.
 * \ingroup ITKDeformableMesh
 */

template< typename TInputMesh, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT DeformableSimplexMesh3DGradientConstraintForceFilter :
  public DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh >
{
public:
  /** Standard "Self" typedefs. */
  typedef DeformableSimplexMesh3DGradientConstraintForceFilter Self;

  /** Standard "Superclass" typedef. */
  typedef  DeformableSimplexMesh3DFilter< TInputMesh, TOutputMesh > Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DeformableSimplexMesh3DGradientConstraintForceFilter, DeformableSimplexMesh3DFilter);

  /** Some typedefs. */
  typedef TInputMesh  InputMeshType;
  typedef TOutputMesh OutputMeshType;

  typedef typename Superclass::PointType              PointType;
  typedef typename Superclass::GradientIndexType      GradientIndexType;
  typedef typename Superclass::GradientIndexValueType GradientIndexValueType;
  typedef typename Superclass::GradientType           GradientType;
  typedef typename Superclass::GradientImageType      GradientImageType;

  /* Mesh pointer definition. */
  typedef typename InputMeshType::Pointer  InputMeshPointer;
  typedef typename OutputMeshType::Pointer OutputMeshPointer;

  typedef typename InputMeshType::PixelType PixelType;

  typedef Image< PixelType, 3 >                        GradientIntensityImageType;
  typedef typename GradientIntensityImageType::Pointer GradientIntensityImagePointer;

  typedef Image< float, 3 >                               OriginalImageType;
  typedef typename OriginalImageType::IndexType           OriginalImageIndexType;
  typedef typename OriginalImageIndexType::IndexValueType ImageIndexValueType;
  typedef typename OriginalImageType::ConstPointer        OriginalImagePointer;

  /** control the range of search for Bresenham at normal line */
  itkSetMacro(Range, int);
  itkGetConstMacro(Range, int);

  // full segment or half segment direction
  enum SIDE {
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
  itkSetConstObjectMacro(Image, OriginalImageType);

protected:
  DeformableSimplexMesh3DGradientConstraintForceFilter();
  ~DeformableSimplexMesh3DGradientConstraintForceFilter() ITK_OVERRIDE;
  DeformableSimplexMesh3DGradientConstraintForceFilter(const Self &) {}
  void operator=(const Self &){}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**
   * Compute the external force component
   */
  virtual void ComputeExternalForce(SimplexMeshGeometry *data, const GradientImageType *gradientImage) ITK_OVERRIDE;

  /**
   * Range of search for Bresenham algorithm (normal line at each vertex)
   */
  int m_Range;

private:
  double NextVoxel(const double *pp, int *ic, double *x, double *y, double *z);

  int Signi(double a);

  void Clear();

  // line starting voxel
  ImageVoxel *m_StartVoxel;
  // line voxels in direction
  std::vector< ImageVoxel * > m_Positive;
  // line voxels in -direction
  std::vector< ImageVoxel * > m_Negative;

  OriginalImagePointer m_Image;
}; // end of class
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformableSimplexMesh3DGradientConstraintForceFilter.hxx"
#endif

#endif /* __DeformableSimplexMesh3DGradientConstraintForceFilter_h */
