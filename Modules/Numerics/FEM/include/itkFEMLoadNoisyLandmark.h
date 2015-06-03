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

#ifndef itkFEMLoadNoisyLandmark_h
#define itkFEMLoadNoisyLandmark_h

#include "itkFEMLoadLandmark.h"
#include "ITKFEMExport.h"

namespace itk
{
namespace fem
{
/**
 * \class itkFEMLoadNoisyLandmark
 * \brief This landmark is derived from the motion of a specific landmark, but
 * allows the existance of noise or outliers
 *
 * \author Yixun Liu
 *
 * \ingroup ITKFEM
 */
class ITKFEM_EXPORT LoadNoisyLandmark : public LoadLandmark
{
public:
  /** Standard class typedefs. */
  typedef LoadNoisyLandmark             Self;
  typedef LoadLandmark                  Superclass;
  typedef SmartPointer<Self>            Pointer;
  typedef SmartPointer<const Self>      ConstPointer;

  /** Some convenient typedefs */
  typedef  Element::VectorType          VectorType;
  typedef  Element::MatrixType          MatrixType;

  /** Method for creation through the object factory. */
  itkSimpleNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoadNoisyLandmark, LoadLandmark);

  /** Outlier or not */
  void SetOutlier(bool outlier)
    {
    m_IsOutlier = outlier;
    }

  bool IsOutlier() const
    {
    return m_IsOutlier;
    }

  /** Set/Get Error norm */
  void SetErrorNorm(float errorNorm)
    {
    m_ErrorNorm = errorNorm;
    }

  float GetErrorNorm() const
    {
    return m_ErrorNorm;
    }

  /** Set/Get Confidence */
  void SetConfidence(float confidence)
    {
    m_Confidence = confidence;
    }

  float GetConfidence() const
    {
    return m_Confidence;
    }

  /** Set/Get real displacement */
  void SetRealDisplacement(const VectorType & displacement)
    {
    m_RealDisplacement = displacement;
    }

  const VectorType & GetRealDisplacement() const
    {
    return m_RealDisplacement;
    }

  /** Set/Get simulated displacement */
  void SetSimulatedDisplacement(const VectorType & displacement)
    {
    m_SimulatedDisplacement = displacement;
    }

  const VectorType & GetSimulatedDisplacement() const
    {
    return m_SimulatedDisplacement;
    }

  /** Set/Get Shape function */
  void SetShape(const VectorType & shape)
    {
    m_Shape = shape;
    }

  const VectorType & GetShape() const
    {
    return m_Shape;
    }

  /** Set/Get flag for outside of the mesh */
  void SetIsOutOfMesh(bool out)
    {
    m_IsOutOfMesh = out;
    }

  bool IsOutOfMesh() const
    {
    return m_IsOutOfMesh;
    }

  /** Set/Get Structure tensor */
  void SetStructureTensor(const MatrixType& structureTensor)
    {
    m_StructureTensor = structureTensor;
    m_HasStructureTensor = true;
    }

  const MatrixType& GetStructureTensor() const
    {
    return m_StructureTensor;
    }

  bool HasStructureTensor() const
    {
    return m_HasStructureTensor;
    }

  /** Set/Get Landmark tensor */
  void SetLandmarkTensor(const MatrixType& landmarkTensor)
    {
    m_LandmarkTensor = landmarkTensor;
    }

  const MatrixType & GetLandmarkTensor() const
    {
    return m_LandmarkTensor;
    }

protected:

  /**
   * Default constructors
   */
  LoadNoisyLandmark():
    m_Confidence(1.0),
    m_ErrorNorm(0.0),
    m_IsOutlier(false),
    m_IsOutOfMesh(false),
    m_HasStructureTensor(false)
    {
    this->m_Element.resize(1);
    }

  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;

private:

  /**
   * Confidence of the landmark
   */
  float m_Confidence;

  /**
   * Real displacement of the landmark
   */
  VectorType m_RealDisplacement;

  /**
   * Simulated displacement of the landmark
   */
  VectorType m_SimulatedDisplacement;

  /**
   * Shape function vector
   */
  VectorType m_Shape;

  /**
   * Magnitude of the error
   */
  float m_ErrorNorm;

  /**
   * Outlier or not
   */
  bool m_IsOutlier;

  /**
   * Outside of mesh
   */
  bool m_IsOutOfMesh;

  /**
   * Has structure tensor or not
   */
  bool m_HasStructureTensor;

  /**
   * Structure tensor
   */
  MatrixType m_StructureTensor;

  /**
   * Landmark tensor, which can be the stiffness tensor or the product
   * of the stiffness tensor and the structure tensor
   */
  MatrixType m_LandmarkTensor;

};

}
}  // end namespace itk::fem

#endif
