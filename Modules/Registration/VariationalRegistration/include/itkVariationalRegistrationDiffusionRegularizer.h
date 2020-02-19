/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkVariationalRegistrationDiffusionRegularizer_h
#define itkVariationalRegistrationDiffusionRegularizer_h

#include "itkVariationalRegistrationRegularizer.h"

namespace itk
{

/** \class itk::VariationalRegistrationDiffusionRegularizer
 *
 *  \brief This class performs diffusive regularization of a vector field.
 *
 *  This class implements the diffusive regularization as described in
 *  <em>Fischer and Modersitzki. "Fast diffusion registration." Contemporary
 *  Mathematics 313 (2002): 117-128.</em>.
 *
 *  We efficiently compute \f$u^{out}=(Id - A)^{-1}[u^{in}]\f$ with
 *  \f$A[u]=\alpha\Delta u\f$ using additive operator splitting (AOS).
 *  Please note that \f$\alpha\f$ corresponds to \f$\tau\alpha\f$ in Eq.(2)
 *  in VariationalRegistrationFilter.
 *
 *  \sa VariationalRegistrationFilter
 *  \sa VariationalRegistrationRegularizer
 *
 *  \ingroup VariationalRegistration
 *
 *  \note This class was developed with funding from the German Research
 *  Foundation (DFG: EH 224/3-1 and HA 235/9-1).
 *  \author Alexander Schmidt-Richberg
 *  \author Rene Werner
 *  \author Jan Ehrhardt
 */
template <typename TDisplacementField>
class VariationalRegistrationDiffusionRegularizer : public VariationalRegistrationRegularizer<TDisplacementField>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(VariationalRegistrationDiffusionRegularizer);

  /** Standard class type alias */
  using Self = VariationalRegistrationDiffusionRegularizer;
  using Superclass = VariationalRegistrationRegularizer<TDisplacementField>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VariationalRegistrationDiffusionRegularizer, VariationalRegistrationRegularizer);

  /** Dimensionality of input and output data is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TDisplacementField::ImageDimension;

  /** Deformation field types, inherited from Superclass. */
  using DisplacementFieldType = typename Superclass::DisplacementFieldType;
  using DisplacementFieldPointer = typename Superclass::DisplacementFieldPointer;
  using DisplacementFieldConstPointer = typename Superclass::DisplacementFieldConstPointer;
  using PixelType = typename Superclass::PixelType;
  using ValueType = typename Superclass::ValueType;

  /** Types for buffer image. */
  using BufferImageType = Image<ValueType, ImageDimension>;
  using BufferImagePointer = typename BufferImageType::Pointer;
  using BufferImageRegionType = typename BufferImageType::RegionType;

  /** Set the regularization weight alpha */
  itkSetMacro(Alpha, ValueType);

  /** Get the regularization weight alpha */
  itkGetConstMacro(Alpha, ValueType);

protected:
  VariationalRegistrationDiffusionRegularizer();
  ~VariationalRegistrationDiffusionRegularizer() override = default;

  /** Print information about the filter. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Execute regularization. This method is multi-threaded but does not
   * use ThreadedGenerateData(). */
  void
  GenerateData() override;

  /** Method for initialization. Buffer images are allocated and the matrices
   * calculated in this method. */
  void
  Initialize() override;

  /** Calculation and LU decomposition of the tridiagonal matrices
   * using the Thomas algorithm (TDMA). */
  virtual void
  InitLUMatrices(ValueType ** alpha, ValueType ** beta, ValueType ** gamma, int n, int dim);

  /** Regularize a given component (dimension) of the deformation field. This
   *  is called for each ImageDimension by GenerateData(). */
  virtual void
  RegularizeComponent(const int component);

  /** A struct to store parameters for multithreaded function call. */
  struct CalcBufferThreadStruct
  {
    VariationalRegistrationDiffusionRegularizer * Filter;
    unsigned int                                  component; // The current dimension.
    BufferImagePointer                            bPtr;      // Pointer to the image buffer.
  };

  /** A struct to store parameters for multithreaded function call. */
  struct RegularizeThreadStruct
  {
    VariationalRegistrationDiffusionRegularizer * Filter;
    unsigned int                                  direction; // Current direction.
    ValueType *                                   alpha;     // Pointer to matrix diagonal.
    ValueType *                                   beta;      // Pointer to matrix subdiagonal.
    ValueType *                                   gamma;     // Pointer to matrix superdiagonal.
    BufferImagePointer                            bPtr;      // Pointer to force field image buffer.
    BufferImagePointer                            vPtr;      // Pointer to temporal result image buffer.
  };

  /** A struct to store parameters for multithreaded function call. */
  struct MergeDirectionsThreadStruct
  {
    VariationalRegistrationDiffusionRegularizer * Filter;
    unsigned int                                  component; // The current dimension.
    BufferImagePointer *                          vPtr;      // Pointer to temporal image buffers.
  };

  /** Method for multi-threaded calculation of the image buffer. */
  static ITK_THREAD_RETURN_TYPE
  CalcBufferCallback(void * arg);

  /** Method for multi-threaded regularization of the image buffer. */
  static ITK_THREAD_RETURN_TYPE
  RegularizeDirectionCallback(void * arg);

  /** Method for multi-threaded calculation of the final field. */
  static ITK_THREAD_RETURN_TYPE
  MergeDirectionsCallback(void * arg);

  /** Split the boundary face orthogonal to "inDir" into "num" pieces, returning
   * region "i" as "splitRegion". This method is called "num" times. The
   * regions must not overlap. The method returns the number of pieces that
   * the routine is capable of splitting the output RequestedRegion,
   * i.e. return value is less than or equal to "num". */
  virtual int
  SplitBoundaryFaceRegion(int i, int num, int inDir, BufferImageRegionType & splitRegion);

private:
  /** Weight of the regularization term. */
  ValueType m_Alpha;

  /** The size of the displacement field. */
  typename DisplacementFieldType::SizeType m_Size;

  /** The spacing of the displacement field. */
  typename DisplacementFieldType::SpacingType m_Spacing;

  // Attributes for AOS calculation
  /** Pointer to a temporal image for the regularization.  */
  BufferImagePointer m_BufferImage;

  /** Buffers for the regularized fields in each direction. Stored in separate
   * images instead of a vector image for better memory management. */
  BufferImagePointer m_V[ImageDimension];

  /** Array for the diagonals of the factorized matrices for each dimension */
  ValueType * m_MatrixAlpha[ImageDimension];

  /** Array for the subdiagonals of the factorized matrices for each dimension */
  ValueType * m_MatrixBeta[ImageDimension];

  /** Array for the superdiagonals of the factorized matrices for each dimension */
  ValueType * m_MatrixGamma[ImageDimension];
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationDiffusionRegularizer.hxx"
#endif

#endif
