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
#ifndef itkScalarRegionBasedLevelSetFunction_hxx
#define itkScalarRegionBasedLevelSetFunction_hxx


namespace itk
{

template <typename TInputImage, typename TFeatureImage, typename TSharedData>
typename ScalarRegionBasedLevelSetFunction<TInputImage, TFeatureImage, TSharedData>::ScalarValueType
ScalarRegionBasedLevelSetFunction<TInputImage, TFeatureImage, TSharedData>::ComputeOverlapParameters(
  const FeatureIndexType & globalIndex,
  ScalarValueType &        product)
{
  // This conditional statement computes the amount of overlap s
  // and the presence of background pr
  unsigned int fId = this->m_FunctionId;

  // accumulates the overlap across all functions
  ScalarValueType sum = 0;

  product = 1.;

  ListPixelType L;
  L = this->m_SharedData->m_NearestNeighborListImage->GetPixel(globalIndex);

  InputPixelType hVal;
  InputIndexType otherIndex;

  for (const auto & it : L)
  {
    if (it != fId)
    {
      otherIndex = this->m_SharedData->m_LevelSetDataPointerVector[it]->GetIndex(globalIndex);
      hVal =
        this->m_SharedData->m_LevelSetDataPointerVector[it]->m_HeavisideFunctionOfLevelSetImage->GetPixel(otherIndex);

      sum += (1 - hVal);
      product *= (1 - hVal);
    }
  }

  return sum;
}

template <typename TInputImage, typename TFeatureImage, typename TSharedData>
void
ScalarRegionBasedLevelSetFunction<TInputImage, TFeatureImage, TSharedData>::UpdatePixel(
  const unsigned int                  idx,
  NeighborhoodIterator<TInputImage> & iterator,
  InputPixelType &                    newValue,
  bool &                              itkNotUsed(status))
{
  unsigned int fId = this->m_FunctionId;

  // For each affected h val: h val = new hval (this will dirty some cvals)
  InputIndexType   inputIndex = iterator.GetIndex(idx);
  FeatureIndexType globalIndex = this->m_SharedData->m_LevelSetDataPointerVector[fId]->GetFeatureIndex(inputIndex);

  FeaturePixelType featureVal = this->m_FeatureImage->GetPixel(inputIndex);

  ScalarValueType oldH =
    this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_HeavisideFunctionOfLevelSetImage->GetPixel(inputIndex);
  ScalarValueType newH = this->m_DomainFunction->Evaluate(-newValue);
  ScalarValueType change = newH - oldH;

  // update the foreground constant for current level-set function
  UpdateSharedDataInsideParameters(fId, featureVal, change);

  // Compute the product factor
  ListPixelType   L = this->m_SharedData->m_NearestNeighborListImage->GetPixel(globalIndex);
  InputIndexType  itInputIndex;
  ScalarValueType hVal;

  InputPixelType product = 1;
  for (ListPixelType::const_iterator it = L.begin(); it != L.end(); ++it)
  {
    if (*it != fId)
    {
      itInputIndex = this->m_SharedData->m_LevelSetDataPointerVector[*it]->GetIndex(globalIndex);
      hVal = this->m_SharedData->m_LevelSetDataPointerVector[*it]->m_HeavisideFunctionOfLevelSetImage->GetPixel(
        itInputIndex);
      product *= (1 - hVal);
    }
  }
  // Determine the change in the product factor
  ScalarValueType productChange = -(product * change);

  // update the background constant of all level-set functions
  for (ListPixelType::const_iterator it = L.begin(); it != L.end(); ++it)
  {
    UpdateSharedDataOutsideParameters(*it, featureVal, productChange);
  }

  this->m_SharedData->m_LevelSetDataPointerVector[fId]->m_HeavisideFunctionOfLevelSetImage->SetPixel(inputIndex, newH);
}
} // end namespace itk
#endif
