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

#ifndef itkTileMontage_hxx
#define itkTileMontage_hxx

#include "itkTileMontage.h"
#include <algorithm>

namespace itk
{
template<typename TImageType>
TileMontage<TImageType>
::TileMontage()
{
  m_PCM = PCMType::New();
  m_PCMOperator = PCMOperatorType::New();
  m_PCMOptimizer = PCMOptimizerType::New();

  m_FinishedTiles = 0;
  SizeType initialSize;
  initialSize.Fill(1);
  initialSize[0] = 2;
  this->SetMontageSize(initialSize);

  //required for GenerateOutputInformation to be called
  this->SetNthOutput(0, this->MakeOutput(0).GetPointer());
}

template<typename TImageType>
void
TileMontage<TImageType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Montage size: " << m_MontageSize << std::endl;
  os << indent << "Linear Montage size: " << m_LinearMontageSize << std::endl;  
  os << indent << "Finished Tiles: " << m_FinishedTiles << std::endl;
  
  auto nullCount = std::count(m_FFTCache.begin(), m_FFTCache.end(), nullptr);
  os << indent << "FFTCache (filled/capcity): " << m_FFTCache.size() - nullCount
      << "/" << m_FFTCache.size() << std::endl;

  os << indent << "PhaseCorrelationImageRegistrationMethod: " << m_PCM.GetPointer() << std::endl;
  os << indent << "PCM Optimizer: " << m_PCMOptimizer.GetPointer() << std::endl;
  os << indent << "PCM Operator: " << m_PCMOperator.GetPointer() << std::endl;
}

template<typename TImageType>
void
TileMontage<TImageType>
::SetMontageSize(SizeType montageSize)
{
  if (m_MontageSize != montageSize)
    {
    m_LinearMontageSize = 1u;
    for (unsigned d = 0; d < ImageDimension; d++)
      {
      m_LinearMontageSize *= montageSize[d];
      }
    this->SetNumberOfRequiredInputs(m_LinearMontageSize);
    this->SetNumberOfRequiredOutputs(m_LinearMontageSize);
    m_MontageSize = montageSize;
    m_FFTCache.resize(m_LinearMontageSize);
    this->Modified();
    }
}

template<typename TImageType>
DataObject::DataObjectPointerArraySizeType
TileMontage<TImageType>
::nDIndexToLinearIndex(IndexType nDIndex) const
{
  DataObjectPointerArraySizeType ind = 0;
  SizeValueType stride = 1u;
  for (unsigned d = 0; d < ImageDimension; d++)
    {
    itkAssertOrThrowMacro(nDIndex[d] < m_MontageSize[d], "Tile index " << nDIndex
        << " exceeds tile size " << m_MontageSize << " at dimension " << d);
    ind += nDIndex[d] * stride;
    stride *= m_MontageSize[d];
    }
  return ind;
}

template<typename TImageType>
typename TileMontage<TImageType>::IndexType
TileMontage<TImageType>
::LinearIndexTonDIndex(DataObject::DataObjectPointerArraySizeType linearIndex) const
{
  IndexType ind;
  SizeValueType stride = 1u;
  for (unsigned d = 0; d < ImageDimension; d++)
    {
    stride *= m_MontageSize[d];
    ind[d] = linearIndex % stride;
    }
  itkAssertOrThrowMacro(linearIndex < stride, "Linear tile index " << linearIndex
      << " exceeds total montage size " << stride);
  return ind;
}

template<typename TImageType>
typename TileMontage<TImageType>::TransformPointer
TileMontage<TImageType>
::RegisterPair(IndexType fixed, IndexType moving)
{
  DataObjectPointerArraySizeType lFixedInd = nDIndexToLinearIndex(fixed);
  DataObjectPointerArraySizeType lMovingInd = nDIndexToLinearIndex(moving);

  m_PCM->SetFixedImage(static_cast<ImageType *>(this->GetInput(lFixedInd)));
  m_PCM->SetMovingImage(static_cast<ImageType *>(this->GetInput(lMovingInd)));
  m_PCM->SetFixedImageFFT(m_FFTCache[lFixedInd]); //maybe null
  m_PCM->SetMovingImageFFT(m_FFTCache[lMovingInd]); //maybe null
  //m_PCM->DebugOn();
  m_PCM->Update();

  m_FFTCache[lFixedInd] = m_PCM->GetFixedImageFFT(); //certainly not null
  m_FFTCache[lMovingInd] = m_PCM->GetMovingImageFFT(); //certrainly not null
  
  const TransformType* regTr = m_PCM->GetOutput()->Get();
  return const_cast<TransformType*>(regTr);
}

template<typename TImageType>
void
TileMontage<TImageType>
::MontageLinear(unsigned dim, TransformPointer initialTransform,
    IndexType initialTile, int dir)
{
  SizeValueType indLower, indHigher;
  IndexType fixed = initialTile;
  IndexType moving = initialTile;
  if (dir<0)
    {
    indLower = initialTile[dim] - 1;
    indHigher = initialTile[dim];
    moving[dim] = initialTile[dim] - 1;
    }
  else
    {
    indLower = initialTile[dim];
    indHigher = initialTile[dim] + 1;
    moving[dim] = initialTile[dim] + 1;
    }

  TransformPointer oldT = initialTransform;
  SizeValueType i = initialTile[dim];
  while (indHigher > 0 && indLower < m_MontageSize[dim] - 1)
    {
    TransformPointer t = this->RegisterPair(fixed, moving);
    t->Compose(oldT, true);
    this->WriteOutTransform(moving, t);
    oldT = t;

    indLower += dir;
    indHigher += dir;
    fixed[dim] += dir;
    moving[dim] += dir;
    }  
}

template<typename TImageType>
void
TileMontage<TImageType>
::MontageDimension(unsigned d, TransformPointer tInitial, IndexType initialTile)
{
  IndexType ind = initialTile;
  if (d == 0)
    {
    MontageLinear(d, tInitial, ind, +1);
    }
  else // d>0
    {
    ind[d] = 0; //montage first index in lower dimension
    MontageDimension(d - 1, tInitial, ind);

    TransformPointer oldT = tInitial;
    for (unsigned i = 1; i < m_MontageSize[d]; i++)
      {
      //register i-th tile to i-1 along this dimension
      IndexType indF = ind;
      indF[d] = i - 1;
      ind[d] = i;
      TransformPointer t = this->RegisterPair(indF, ind);
      t->Compose(oldT, true);

      this->WriteOutTransform(ind, t);

      oldT = t; //save current transform for next i iteration

      //montage this index in lower dimension
      MontageDimension(d - 1, t, ind);
      }
    }
}

template<typename TImageType>
void
TileMontage<TImageType>
::WriteOutTransform(IndexType index, TransformPointer transform)
{
  auto dOut = this->GetOutput(this->nDIndexToLinearIndex(index));
  const auto cOut = static_cast<TransformOutputType *>(dOut);
  auto decorator = const_cast<TransformOutputType *>(cOut);
  decorator->Set(transform);
  m_FinishedTiles++;
  this->UpdateProgress(float(m_FinishedTiles) / m_LinearMontageSize);
}

template<typename TImageType>
void
TileMontage<TImageType>
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  std::vector<double> sizes(ImageDimension); //default initialized to 0
  SizeType maxSizes;
  maxSizes.Fill(0);
  for (unsigned i = 0; i < m_LinearMontageSize; i++)
    {
    if (i > 0) //otherwise primary output has same modification time as this class
      { //and GenerateData does not get called
      this->SetNthOutput(i, this->MakeOutput(i).GetPointer());
      }
    //the rest of this code determines average and maximum tile sizes
    auto input = static_cast<const TImageType*>(this->GetInput(i));
    typename TImageType::RegionType reg = input->GetRequestedRegion();
    for (unsigned d = 0; d < ImageDimension; d++)
      {
      sizes[d] += reg.GetSize(d);
      maxSizes[d] = std::max(maxSizes[d], reg.GetSize(d));
      }
    }
  
  //divide by count to get average
  for (unsigned d = 0; d < ImageDimension; d++)
    {
    sizes[d] /= m_LinearMontageSize;
    }

  //if maximum size is more than twice the average along any dimension,
  //we will not pad all the images to maxSize
  //in most cases images will be of similar or exactly the same size
  bool forceSame = true;
  for (unsigned d = 0; d < ImageDimension; d++)
    {
    if (sizes[d] * 2 < maxSizes[d])
      {
      forceSame = false;
      }
    }
  if (forceSame)
    {
    maxSizes = m_PCM->RoundUpToFFTSize(maxSizes);
    m_PCM->SetPadToSize(maxSizes);
    }

  //we connect these classes here in case user has provided new versions
  m_PCM->SetOperator(m_PCMOperator);
  m_PCM->SetOptimizer(m_PCMOptimizer);
}

template<typename TImageType>
void
TileMontage<TImageType>
::GenerateData()
{
  TransformType::Pointer t0 = TransformType::New();
  IndexType ind;
  ind.Fill(0);
  m_FinishedTiles = 0;
  this->WriteOutTransform(ind, t0); //write identity (no translation) for tile 0

  this->MontageDimension(this->ImageDimension - 1, t0, ind);

  ////clear cache after montaging is finished
  //for (unsigned i = 0; i < m_LinearMontageSize; i++)
  //  {
  //  m_FFTCache[i] = nullptr;
  //  }
}

} //namespace itk

#endif //itkTileMontage_hxx
