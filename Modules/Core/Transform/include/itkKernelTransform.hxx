/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkKernelTransform_hxx
#define itkKernelTransform_hxx

namespace itk
{

template <typename TParametersValueType, unsigned int VDimension>
KernelTransform<TParametersValueType, VDimension>::KernelTransform()
  : Superclass(VDimension)
// the second VDimension is associated is provided as
// a tentative number for initializing the Jacobian.
// The matrix can be resized at run time so this number
// here is irrelevant. The correct size of the Jacobian
// will be VDimension X VDimension.NumberOfLandMarks.
{
  this->m_I.set_identity();
  this->m_SourceLandmarks = PointSetType::New();
  this->m_TargetLandmarks = PointSetType::New();
  this->m_Displacements = VectorSetType::New();
  this->m_WMatrixComputed = false;

  this->m_Stiffness = 0.0;
}

template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::SetSourceLandmarks(PointSetType * landmarks)
{
  itkDebugMacro("setting SourceLandmarks to " << landmarks);
  if (this->m_SourceLandmarks != landmarks)
  {
    this->m_SourceLandmarks = landmarks;
    this->UpdateParameters();
    this->Modified();
  }
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::SetTargetLandmarks(PointSetType * landmarks)
{
  itkDebugMacro("setting TargetLandmarks to " << landmarks);
  if (this->m_TargetLandmarks != landmarks)
  {
    this->m_TargetLandmarks = landmarks;
    this->UpdateParameters();
    this->Modified();
  }
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ComputeG(const InputVectorType &,
                                                            GMatrixType & itkNotUsed(gmatrix)) const
{
  itkExceptionMacro("ComputeG(vector,gmatrix) must be reimplemented"
                    << " in subclasses of KernelTransform.");
}


template <typename TParametersValueType, unsigned int VDimension>
const typename KernelTransform<TParametersValueType, VDimension>::GMatrixType &
  KernelTransform<TParametersValueType, VDimension>::ComputeReflexiveG(PointsIterator) const
{
  m_GMatrix.fill(NumericTraits<TParametersValueType>::ZeroValue());
  m_GMatrix.fill_diagonal(m_Stiffness);

  return m_GMatrix;
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ComputeDeformationContribution(const InputPointType & thisPoint,
                                                                                  OutputPointType &      result) const
{
  /*
   * Default implementation of the the method. This can be overloaded
   * in transforms whose kernel produce diagonal G matrices.
   */
  PointIdentifier numberOfLandmarks = this->m_SourceLandmarks->GetNumberOfPoints();

  PointsIterator sp = this->m_SourceLandmarks->GetPoints()->Begin();

  GMatrixType Gmatrix;

  for (unsigned int lnd = 0; lnd < numberOfLandmarks; ++lnd)
  {
    this->ComputeG(thisPoint - sp->Value(), Gmatrix);
    for (unsigned int dim = 0; dim < VDimension; ++dim)
    {
      for (unsigned int odim = 0; odim < VDimension; ++odim)
      {
        result[odim] += Gmatrix(dim, odim) * m_DMatrix(dim, lnd);
      }
    }
    ++sp;
  }
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ComputeD()
{
  PointIdentifier numberOfLandmarks = this->m_SourceLandmarks->GetNumberOfPoints();

  PointsIterator sp = this->m_SourceLandmarks->GetPoints()->Begin();
  PointsIterator tp = this->m_TargetLandmarks->GetPoints()->Begin();
  PointsIterator end = this->m_SourceLandmarks->GetPoints()->End();

  this->m_Displacements->Reserve(numberOfLandmarks);
  typename VectorSetType::Iterator vt = this->m_Displacements->Begin();

  while (sp != end)
  {
    vt->Value() = tp->Value() - sp->Value();
    ++vt;
    ++sp;
    ++tp;
  }
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ComputeWMatrix()
{
  using SVDSolverType = vnl_svd<TParametersValueType>;

  this->ComputeL();
  this->ComputeY();
  SVDSolverType svd(this->m_LMatrix, 1e-8);
  this->m_WMatrix = svd.solve(this->m_YMatrix);

  this->ReorganizeW();
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ComputeL()
{
  PointIdentifier numberOfLandmarks = this->m_SourceLandmarks->GetNumberOfPoints();

  vnl_matrix<TParametersValueType> O2(VDimension * (VDimension + 1), VDimension * (VDimension + 1), 0);

  this->ComputeP();
  this->ComputeK();

  this->m_LMatrix.set_size(VDimension * (numberOfLandmarks + VDimension + 1),
                           VDimension * (numberOfLandmarks + VDimension + 1));

  this->m_LMatrix.fill(0.0);

  this->m_LMatrix.update(this->m_KMatrix, 0, 0);
  this->m_LMatrix.update(this->m_PMatrix, 0, this->m_KMatrix.columns());
  this->m_LMatrix.update(this->m_PMatrix.transpose(), this->m_KMatrix.rows(), 0);
  this->m_LMatrix.update(O2, this->m_KMatrix.rows(), this->m_KMatrix.columns());
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ComputeK()
{
  PointIdentifier numberOfLandmarks = this->m_SourceLandmarks->GetNumberOfPoints();


  this->ComputeD();

  this->m_KMatrix.set_size(VDimension * numberOfLandmarks, VDimension * numberOfLandmarks);

  this->m_KMatrix.fill(0.0);

  PointsIterator p1 = this->m_SourceLandmarks->GetPoints()->Begin();
  PointsIterator end = this->m_SourceLandmarks->GetPoints()->End();

  GMatrixType G;
  // K matrix is symmetric, so only evaluate the upper triangle and
  // store the values in bot the upper and lower triangle
  unsigned int i = 0;
  while (p1 != end)
  {
    PointsIterator p2 = p1; // start at the diagonal element
    unsigned int   j = i;

    // Compute the block diagonal element, i.e. kernel for pi->pi
    G = this->ComputeReflexiveG(p1);
    const vnl_matrix_ref<TParametersValueType> G_reference_alias{
      G.as_ref()
    }; // different interfaces require different representations of values.
    this->m_KMatrix.update(
      G_reference_alias, i * VDimension, i * VDimension); // update only accepts `const vnl_matrix<T> &`
    ++p2;
    ++j;

    // Compute the upper (and copy into lower) triangular part of K
    while (p2 != end)
    {
      const InputVectorType s = p1.Value() - p2.Value();
      this->ComputeG(s, G);
      // write value in upper and lower triangle of matrix
      this->m_KMatrix.update(G_reference_alias, i * VDimension, j * VDimension);
      this->m_KMatrix.update(G_reference_alias, j * VDimension, i * VDimension);
      ++p2;
      ++j;
    }
    ++p1;
    ++i;
  }
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ComputeP()
{
  const PointIdentifier                  numberOfLandmarks = this->m_SourceLandmarks->GetNumberOfPoints();
  const vnl_matrix<TParametersValueType> I{ IMatrixType().set_identity().as_matrix() };

  InputPointType p{};

  this->m_PMatrix.set_size(VDimension * numberOfLandmarks, VDimension * (VDimension + 1));
  this->m_PMatrix.fill(0.0);
  for (unsigned long i = 0; i < numberOfLandmarks; ++i)
  {
    this->m_SourceLandmarks->GetPoint(i, &p);
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      const vnl_matrix<TParametersValueType> temp_matrix{ I * p[j] }; // be explicit about conversions that occur.
      this->m_PMatrix.update(
        temp_matrix, i * VDimension, j * VDimension); // update only takes 'const vnl_matrix<T> &' as input
    }
    this->m_PMatrix.update(I, i * VDimension, VDimension * VDimension);
  }
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ComputeY()
{
  PointIdentifier numberOfLandmarks = this->m_SourceLandmarks->GetNumberOfPoints();

  typename VectorSetType::ConstIterator displacement = this->m_Displacements->Begin();

  this->m_YMatrix.set_size(VDimension * (numberOfLandmarks + VDimension + 1), 1);

  this->m_YMatrix.fill(0.0);
  for (unsigned int i = 0; i < numberOfLandmarks; ++i)
  {
    for (unsigned int j = 0; j < VDimension; ++j)
    {
      this->m_YMatrix.put(i * VDimension + j, 0, displacement.Value()[j]);
    }
    ++displacement;
  }
  for (unsigned int i = 0; i < VDimension * (VDimension + 1); ++i)
  {
    this->m_YMatrix.put(numberOfLandmarks * VDimension + i, 0, 0);
  }
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ReorganizeW()
{
  PointIdentifier numberOfLandmarks = this->m_SourceLandmarks->GetNumberOfPoints();

  // The deformable (non-affine) part of the registration goes here
  this->m_DMatrix.set_size(VDimension, numberOfLandmarks);
  unsigned int ci = 0;
  for (unsigned int lnd = 0; lnd < numberOfLandmarks; ++lnd)
  {
    for (unsigned int dim = 0; dim < VDimension; ++dim)
    {
      this->m_DMatrix(dim, lnd) = this->m_WMatrix(ci++, 0);
    }
  }
  // This matrix holds the rotational part of the Affine component
  for (unsigned int j = 0; j < VDimension; ++j)
  {
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      this->m_AMatrix(i, j) = this->m_WMatrix(ci++, 0);
    }
  }
  // This vector holds the translational part of the Affine component
  for (unsigned int k = 0; k < VDimension; ++k)
  {
    this->m_BVector(k) = this->m_WMatrix(ci++, 0);
  }

  // release WMatrix memory by assigning a small one.
  this->m_WMatrix = WMatrixType(1, 1);
}


template <typename TParametersValueType, unsigned int VDimension>
auto
KernelTransform<TParametersValueType, VDimension>::TransformPoint(const InputPointType & thisPoint) const
  -> OutputPointType
{
  OutputPointType result;

  using ValueType = typename OutputPointType::ValueType;

  result.Fill(NumericTraits<ValueType>::ZeroValue());

  // TODO:  It is unclear if the following line is needed.
  this->ComputeDeformationContribution(thisPoint, result);

  // Add the rotational part of the Affine component
  for (unsigned int j = 0; j < VDimension; ++j)
  {
    for (unsigned int i = 0; i < VDimension; ++i)
    {
      result[i] += this->m_AMatrix(i, j) * thisPoint[j];
    }
  }
  // This vector holds the translational part of the Affine component
  for (unsigned int k = 0; k < VDimension; ++k)
  {
    result[k] += this->m_BVector(k) + thisPoint[k];
  }

  return result;
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::ComputeJacobianWithRespectToParameters(const InputPointType &,
                                                                                          JacobianType & jacobian) const
{
  jacobian.Fill(0.0);

  // FIXME: TODO
  // The Jacobian should be computable in terms of the matrices
  // used to Transform points...
  itkExceptionMacro("Get[Local]Jacobian must be implemented in subclasses"
                    << " of KernelTransform.");
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::SetParameters(const ParametersType & parameters)
{
  // Set the parameters
  // NOTE that in this transformation both the Source and Target
  // landmarks could be considered as parameters. It is assumed
  // here that the Target landmarks are provided by the user and
  // are not changed during the optimization process required for
  // registration.

  // Save parameters. Needed for proper operation of TransformUpdateParameters.
  if (&parameters != &(this->m_Parameters))
  {
    const size_t                 parameterSize = this->GetParameters().Size();
    const NumberOfParametersType offset{};
    this->CopyInParameters(&(parameters.data_block())[offset], &(parameters.data_block())[offset] + parameterSize);
  }

  auto               landmarks = PointsContainer::New();
  const unsigned int numberOfLandmarks = parameters.Size() / VDimension;
  landmarks->Reserve(numberOfLandmarks);

  PointsIterator itr = landmarks->Begin();
  PointsIterator end = landmarks->End();

  InputPointType landMark;

  unsigned int pcounter = 0;
  while (itr != end)
  {
    for (unsigned int dim = 0; dim < VDimension; ++dim)
    {
      landMark[dim] = parameters[pcounter];
      ++pcounter;
    }
    itr.Value() = landMark;
    ++itr;
  }

  this->m_SourceLandmarks->SetPoints(landmarks);

  // Modified is always called since we just have a pointer to the
  // parameters and cannot know if the parameters have changed.
  this->Modified();
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::SetFixedParameters(const FixedParametersType & parameters)
{
  // Set the fixed parameters
  // Since the API of the SetParameters() function sets the
  // source landmarks, this function was added to support the
  // setting of the target landmarks, and allowing the Transform
  // I/O mechanism to be supported.

  auto               landmarks = PointsContainer::New();
  const unsigned int numberOfLandmarks = parameters.Size() / VDimension;

  landmarks->Reserve(numberOfLandmarks);

  PointsIterator itr = landmarks->Begin();
  PointsIterator end = landmarks->End();

  InputPointType landMark;

  unsigned int pcounter = 0;
  while (itr != end)
  {
    for (unsigned int dim = 0; dim < VDimension; ++dim)
    {
      landMark[dim] = parameters[pcounter];
      ++pcounter;
    }
    itr.Value() = landMark;
    ++itr;
  }

  this->m_TargetLandmarks->SetPoints(landmarks);
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::UpdateParameters() const
{
  this->m_Parameters = ParametersType(this->m_SourceLandmarks->GetNumberOfPoints() * VDimension);

  PointsIterator itr = this->m_SourceLandmarks->GetPoints()->Begin();
  PointsIterator end = this->m_SourceLandmarks->GetPoints()->End();

  unsigned int pcounter = 0;
  while (itr != end)
  {
    InputPointType landmark = itr.Value();
    for (unsigned int dim = 0; dim < VDimension; ++dim)
    {
      this->m_Parameters[pcounter] = landmark[dim];
      ++pcounter;
    }
    ++itr;
  }
}


template <typename TParametersValueType, unsigned int VDimension>
auto
KernelTransform<TParametersValueType, VDimension>::GetParameters() const -> const ParametersType &
{
  this->UpdateParameters();
  return this->m_Parameters;
}


template <typename TParametersValueType, unsigned int VDimension>
auto
KernelTransform<TParametersValueType, VDimension>::GetFixedParameters() const -> const FixedParametersType &
{
  // Get the fixed parameters
  // This returns the target landmark locations
  // This was added to support the Transform Reader/Writer mechanism
  this->m_FixedParameters = ParametersType(this->m_TargetLandmarks->GetNumberOfPoints() * VDimension);

  PointsIterator itr = this->m_TargetLandmarks->GetPoints()->Begin();
  PointsIterator end = this->m_TargetLandmarks->GetPoints()->End();

  unsigned int pcounter = 0;
  while (itr != end)
  {
    InputPointType landmark = itr.Value();
    for (unsigned int dim = 0; dim < VDimension; ++dim)
    {
      this->m_FixedParameters[pcounter] = landmark[dim];
      ++pcounter;
    }
    ++itr;
  }

  return this->m_FixedParameters;
}


template <typename TParametersValueType, unsigned int VDimension>
void
KernelTransform<TParametersValueType, VDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if (this->m_SourceLandmarks)
  {
    os << indent << "SourceLandmarks: " << std::endl;
    this->m_SourceLandmarks->Print(os, indent.GetNextIndent());
  }
  if (this->m_TargetLandmarks)
  {
    os << indent << "TargetLandmarks: " << std::endl;
    this->m_TargetLandmarks->Print(os, indent.GetNextIndent());
  }
  if (this->m_Displacements)
  {
    os << indent << "Displacements: " << std::endl;
    this->m_Displacements->Print(os, indent.GetNextIndent());
  }
  os << indent << "Stiffness: " << this->m_Stiffness << std::endl;
}

} // end namespace itk

#endif
