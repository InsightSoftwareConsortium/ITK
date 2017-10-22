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
#ifndef itkLevelSetVelocityNeighborhoodExtractor_h
#define itkLevelSetVelocityNeighborhoodExtractor_h

#include "itkLevelSetNeighborhoodExtractor.h"

namespace itk
{
/** \class LevelSetVelocityNeighborhoodExtractor
 * \brief Locate pixels of a particular level set.
 *
 * LevelSetVelocityNeighborhoodExtractor extends the functionality of
 * LevelSetNeighborhoodExtractor by also extracting the values
 * of velocity variables at the specified level set. Specifically,
 * it populates two containers: one containing the value of velocity
 * variables immediately inside the contour defined by the level set and the
 * other containing values for velocity variables immediately outside.
 *
 * The containers AuxInsideValues() and AuxOutsideValues() can
 * be used in conjunction with Superclass::InsidePoints() and
 * Superclass::OutsidePoints() in FastMarchingExtensionImageFilter
 * to produce images which extends the velocity variables smoothly
 * from the specified level set.
 *
 * This class is templated over the image type representing
 * the level set, the type of the auxiliary/velocity variables and the
 * number of auxiliary/velocity variables.
 *
 * Implementation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \ingroup LevelSetSegmentation
 *
 * \ingroup ITKLevelSets
 */
template<
  typename TLevelSet,
  typename TAuxValue,
  unsigned int VAuxDimension = 1
  >
class ITK_TEMPLATE_EXPORT LevelSetVelocityNeighborhoodExtractor:
  public LevelSetNeighborhoodExtractor< TLevelSet >
{
public:
  /** Standard class typdedefs. */
  typedef LevelSetVelocityNeighborhoodExtractor      Self;
  typedef LevelSetNeighborhoodExtractor< TLevelSet > Superclass;
  typedef SmartPointer< Self >                       Pointer;
  typedef SmartPointer< const Self >                 ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetVelocityNeighborhoodExtractor,
               LevelSetNeighborhoodExtractor);

  /** The type of the level set. */
  typedef LevelSetTypeDefault< TLevelSet > LevelSetType;

  /** The dimension of the level set. */
  itkStaticConstMacro(SetDimension, unsigned int,
                      LevelSetType::SetDimension);

  /** Index typedef support */
  typedef::itk::Index< itkGetStaticConstMacro(SetDimension) > Index;

  /** AuxVarType typedef support. */
  typedef AuxVarTypeDefault< TAuxValue, VAuxDimension, itkGetStaticConstMacro(SetDimension) >
  AuxVarType;
  typedef typename AuxVarType::AuxValueType         AuxValueType;
  typedef typename AuxVarType::AuxValueVectorType   AuxValueVectorType;
  typedef typename AuxVarType::AuxValueContainer    AuxValueContainer;
  typedef typename AuxVarType::AuxImageType         AuxImageType;
  typedef typename AuxVarType::AuxImagePointer      AuxImagePointer;
  typedef typename AuxVarType::AuxImageConstPointer AuxImageConstPointer;

  /** Set the auxiliary images. */
  void SetAuxImage(const AuxImageType *ptr, unsigned int idx = 0)
  {
    if ( idx < VAuxDimension && m_AuxImage[idx] != ptr )
      {
      m_AuxImage[idx] = ptr;
      }
    this->Modified();
  }

  /** Get the auxiliary images. */
  AuxImageConstPointer GetAuxImage(unsigned int idx = 0)
  {
    if ( idx >= VAuxDimension )
      {
      return ITK_NULLPTR;
      }
    else
      {
      return m_AuxImage[idx];
      }
  }

  /** Get the container of auxiliary values associated with the inside
   *  points. */
  itkGetModifiableObjectMacro(AuxInsideValues, AuxValueContainer);

  /** Get the container of auxiliary values associate with the outside
   *  points. */
  itkGetModifiableObjectMacro(AuxOutsideValues, AuxValueContainer);

protected:
  LevelSetVelocityNeighborhoodExtractor();
  ~LevelSetVelocityNeighborhoodExtractor() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void Initialize() ITK_OVERRIDE;

  virtual double CalculateDistance(Index & index) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetVelocityNeighborhoodExtractor);

  typename AuxValueContainer::Pointer m_AuxInsideValues;
  typename AuxValueContainer::Pointer m_AuxOutsideValues;
  AuxImageConstPointer m_AuxImage[VAuxDimension];
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetVelocityNeighborhoodExtractor.hxx"
#endif

#endif
