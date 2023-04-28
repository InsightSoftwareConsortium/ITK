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

#ifndef itkLevelSetEquationBinaryMaskTerm_hxx
#define itkLevelSetEquationBinaryMaskTerm_hxx


namespace itk
{

template <typename TInput, typename TLevelSetContainer>
LevelSetEquationBinaryMaskTerm<TInput, TLevelSetContainer>::LevelSetEquationBinaryMaskTerm()
{
  this->m_TermName = "Binary mask term";
  this->m_RequiredData.insert("Value");
}

template <typename TInput, typename TLevelSetContainer>
void
LevelSetEquationBinaryMaskTerm<TInput, TLevelSetContainer>::Update()
{}

template <typename TInput, typename TLevelSetContainer>
void
LevelSetEquationBinaryMaskTerm<TInput, TLevelSetContainer>::InitializeParameters()
{
  this->SetUp();
}


template <typename TInput, typename TLevelSetContainer>
void
LevelSetEquationBinaryMaskTerm<TInput, TLevelSetContainer>::Initialize(const LevelSetInputIndexType & itkNotUsed(index))
{}


template <typename TInput, typename TLevelSetContainer>
void
LevelSetEquationBinaryMaskTerm<TInput, TLevelSetContainer>::UpdatePixel(
  const LevelSetInputIndexType & itkNotUsed(index),
  const LevelSetOutputRealType & itkNotUsed(oldValue),
  const LevelSetOutputRealType & itkNotUsed(newValue))
{}

template <typename TInput, typename TLevelSetContainer>
auto
LevelSetEquationBinaryMaskTerm<TInput, TLevelSetContainer>::Value(const LevelSetInputIndexType & index)
  -> LevelSetOutputRealType
{
  const InputPixelType   pixel = this->m_Mask->GetPixel(index);
  LevelSetOutputRealType value;
  if (pixel > 0)
  {
    value = NumericTraits<LevelSetOutputRealType>::ZeroValue();
  }
  else
  {
    value = NumericTraits<LevelSetOutputRealType>::OneValue();
  }
  return value;
}

template <typename TInput, typename TLevelSetContainer>
auto
LevelSetEquationBinaryMaskTerm<TInput, TLevelSetContainer>::Value(const LevelSetInputIndexType & index,
                                                                  const LevelSetDataType &       itkNotUsed(data))
  -> LevelSetOutputRealType
{
  const InputPixelType   pixel = this->m_Mask->GetPixel(index);
  LevelSetOutputRealType value;
  if (pixel > 0)
  {
    value = NumericTraits<LevelSetOutputRealType>::ZeroValue();
  }
  else
  {
    value = NumericTraits<LevelSetOutputRealType>::OneValue();
  }
  return value;
}

} // namespace itk
#endif
