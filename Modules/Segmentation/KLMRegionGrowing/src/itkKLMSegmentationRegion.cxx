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
#include "itkKLMSegmentationRegion.h"

namespace itk
{
KLMSegmentationRegion
::KLMSegmentationRegion(void)
{
  m_MeanRegionIntensity = 0;
}

KLMSegmentationRegion
::~KLMSegmentationRegion()
{}

void
KLMSegmentationRegion
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Mean region intensity   : " << m_MeanRegionIntensity
     << std::endl;
  os << indent << "Region border object" << std::endl;
} // end PrintSelf

void
KLMSegmentationRegion
::SetRegionParameters(MeanRegionIntensityType meanRegionIntensity,
                      double regionArea,
                      RegionLabelType label)
{
  // Set the area, mean, and label associated with the region
  this->SetRegionArea(regionArea);
  this->SetMeanRegionIntensity(meanRegionIntensity);
  this->SetRegionLabel(label);
} // end SetRegionParameters

void
KLMSegmentationRegion
::CombineRegionParameters(const Self *region)
{
  // Reset the area and mean associated with the merged region

  MeanRegionIntensityType region1Mean = this->GetMeanRegionIntensity();
  MeanRegionIntensityType region2Mean = region->GetMeanRegionIntensity();

  double region1Area = this->GetRegionArea();
  double region2Area = region->GetRegionArea();

  double                  mergedRegionArea = region1Area + region2Area;
  MeanRegionIntensityType mergedRegionMean =
    region1Mean * region1Area + region2Mean * region2Area;

  if ( mergedRegionArea <= 0 ) { itkExceptionMacro(<< "Invalid region area"); }
  mergedRegionMean /= mergedRegionArea;
  this->SetRegionArea(mergedRegionArea);
  this->SetMeanRegionIntensity(mergedRegionMean);
} // end CombineRegionParameters

double
KLMSegmentationRegion
::EnergyFunctional(const Self *region)
{
  MeanRegionIntensityType region1_2MeanDiff =
    this->m_MeanRegionIntensity - region->m_MeanRegionIntensity;

  // Assuming equal weights to all the channels
  // FIXME: For different channel weights modify this part of the code.
  double cost = region1_2MeanDiff.squared_magnitude();

  double region1Area = this->GetRegionArea();
  double region2Area = region->GetRegionArea();

  double scaleArea = ( region1Area * region2Area )
                     / ( region1Area + region2Area );

  return scaleArea * cost;
}

void
KLMSegmentationRegion
::DeleteRegionBorder(KLMSegmentationBorder *pBorderCandidate)
{
  if ( pBorderCandidate == ITK_NULLPTR )
    {
    itkExceptionMacro(<< "Null pointer to segmentation region border");
    }

  RegionBorderVectorIterator
    regionBorderVectorIt    = m_RegionBorderVector.begin();
  RegionBorderVectorIterator
    regionBorderVectorItEnd = m_RegionBorderVector.end();

  // Erase the region border that matches the pBorderCandidate
  bool foundBorderCandidate = false;
  while ( regionBorderVectorIt != regionBorderVectorItEnd )
    {
    if ( *regionBorderVectorIt == pBorderCandidate )
      {
      m_RegionBorderVector.erase(regionBorderVectorIt);
      foundBorderCandidate = true;
      break;
      }
    ++regionBorderVectorIt;
    }

  if ( !foundBorderCandidate )
    {
    itkExceptionMacro(<< "Border candidate not in region borders list");
    }
} // end DeleteRegionBorder()

void
KLMSegmentationRegion
::PushBackRegionBorder(KLMSegmentationBorder *pBorderCandidate)
{
  if ( pBorderCandidate == ITK_NULLPTR )
    {
    itkExceptionMacro(<< "Null pointer to segmentation region border");
    }
  m_RegionBorderVector.push_back(pBorderCandidate);
}

void
KLMSegmentationRegion
::PushFrontRegionBorder(KLMSegmentationBorder *pBorderCandidate)
{
  if ( pBorderCandidate == ITK_NULLPTR )
    {
    itkExceptionMacro(<< "Null pointer to segmentation region border");
    }
  m_RegionBorderVector.insert(m_RegionBorderVector.begin(), pBorderCandidate);
}

void
KLMSegmentationRegion
::InsertRegionBorder(KLMSegmentationBorder *pBorderCandidate)
{
  // Ensure that the border candidate is not a null pointer
  if ( pBorderCandidate == ITK_NULLPTR )
    {
    itkExceptionMacro(<< "Null pointer to segmentation region border");
    }

  // The m_RegionBorderVec is a ordered vector of pointers to the borders.
  // Ordering is based on regions labels.

  // The region border vector is empty, there is only one region border
  if ( m_RegionBorderVector.empty() )
    {
    m_RegionBorderVector.push_back(pBorderCandidate);
    }

  // There are many region borders
  else
    {
    // Iterator set to the first element of the region border element
    RegionBorderVectorIterator
      regionBorderVectorIt    = m_RegionBorderVector.begin();
    RegionBorderVectorIterator
      regionBorderVectorItEnd = m_RegionBorderVector.end();

    bool insertAtEnd = true;
    while ( regionBorderVectorIt != regionBorderVectorItEnd )
      {
      // The region border should be inserted.
      if ( ( pBorderCandidate->GetRegion1()->GetRegionLabel() <
             ( *regionBorderVectorIt )->GetRegion1()->GetRegionLabel() )
           || ( pBorderCandidate->GetRegion1()->GetRegionLabel() ==
                ( *regionBorderVectorIt )->GetRegion1()->GetRegionLabel()
                && pBorderCandidate->GetRegion2()->GetRegionLabel() <
                ( *regionBorderVectorIt )->GetRegion2()->GetRegionLabel() ) )
        {
        m_RegionBorderVector.insert(regionBorderVectorIt, pBorderCandidate);
        insertAtEnd = false;
        break;
        } // end of if

      ++regionBorderVectorIt;
      } // end of while

    // It should be inserted at the end
    if ( insertAtEnd )
      {
      m_RegionBorderVector.push_back(pBorderCandidate);
      }
    } // end of else for the case with many region borders
}     // end InsertRegionBorder

void
KLMSegmentationRegion
::InsertRegionBorder(RegionBorderVectorIterator RegionBorderVectorIt,
                     KLMSegmentationBorder *pBorderCandidate)
{
  // Ensure that the border candidate is not a null pointer
  if ( pBorderCandidate == ITK_NULLPTR )
    {
    itkExceptionMacro(<< "Null pointer to segmentation region border");
    }

  // The m_RegionBorderVec is a ordered vector of pointers to the
  // borders. Insert a valid region border into the region border vector
  // assuming calling function has correctly identified the new
  // element position.
  m_RegionBorderVector.insert(RegionBorderVectorIt, pBorderCandidate);
} // end InsertRegionBorder

void
KLMSegmentationRegion
::ResetRegionLabelAndUpdateBorders(Self *region)
{
  // Assign new equivalence label to the old region
  this->SetRegionLabel( region->GetRegionLabel() );

  // Update the region borders

  RegionBorderVectorIterator
    oldRegionBordersIt    = this->GetRegionBorderItBegin();
  RegionBorderVectorIterator
    endOfOldRegionBorders = this->GetRegionBorderItEnd();

  // Ensure that all borders associated with the old region
  // are in the correct order
  while ( oldRegionBordersIt != endOfOldRegionBorders )
    {
    // Make sure the neighbors now point to the new region, and
    // reorder the region borders of the neighbors since they are
    // no longer in ascending order

    // Ensure that region 1 label is less than  region 2 label
    if ( ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel() ==
         ( *oldRegionBordersIt )->GetRegion2()->GetRegionLabel() )
      {
      itkExceptionMacro(<< "Invalid region border list");
      }

    // Correct order
    if ( ( *oldRegionBordersIt )->GetRegion1()->GetRegionLabel()  >
         ( *oldRegionBordersIt )->GetRegion2()->GetRegionLabel() )
      {
      // Swap the regions pointed to by the border
      KLMSegmentationRegion *ptmpRegion = ( *oldRegionBordersIt )->GetRegion1();
      ( *oldRegionBordersIt )->SetRegion1( ( *oldRegionBordersIt )->GetRegion2() );
      ( *oldRegionBordersIt )->SetRegion2(ptmpRegion);
      } // end if (correct order)

    // Point to the newly merged region, and since the region labels
    // have been switched (if the region 1 label of a border is found to be
    // greater than the corresponding region 2 label), the list of borders for
    // the neighbor region has changed.

    // The border's region1 is the neighbor
    if ( ( *oldRegionBordersIt )->GetRegion2() == this )
      {
      // Make the border's region 2 point to the new region
      ( *oldRegionBordersIt )->SetRegion2(region);

      // Reorder the region borders of the neighbor:
      // remove the region border, then re-insert it.
      ( *oldRegionBordersIt )->GetRegion1()
      ->DeleteRegionBorder(*oldRegionBordersIt);
      ( *oldRegionBordersIt )->GetRegion1()
      ->InsertRegionBorder(*oldRegionBordersIt);
      } // end if (the border's region1 is the neighbor)

    // The border's region2 is the neighbor
    else if ( ( *oldRegionBordersIt )->GetRegion1() == this )
      {
      // Make the border's region 1 point to the new region
      ( *oldRegionBordersIt )->SetRegion1(region);

      // Reorder the region borders of the neighbor
      // remove the region border, then re-insert it
      ( *oldRegionBordersIt )->GetRegion2()
      ->DeleteRegionBorder(*oldRegionBordersIt);
      ( *oldRegionBordersIt )->GetRegion2()
      ->InsertRegionBorder(*oldRegionBordersIt);
      } // end else if (the border's region2 is the neighbor)

    else
      {
      itkExceptionMacro(<< "Invalid region border list");
      } // end else

    // Go to the next region border pointed by the iterator
    ++oldRegionBordersIt;
    } // end while
}     // end ResetRegionLabelAndUpdateBorders

void
KLMSegmentationRegion
::SpliceRegionBorders(Self *region)
{
  // Do the actual union of the borders

  // Copy the container into a temp variable to avoid iterator confusion
  RegionBorderVectorType thisRegionBorder = m_RegionBorderVector;

  // clear output
  m_RegionBorderVector.resize(0);

  // Initialize the region iterators

  RegionBorderVectorConstIterator
    thisRegionBordersIt    = thisRegionBorder.begin();
  RegionBorderVectorConstIterator
    endOfThisRegionBorders = thisRegionBorder.end();

  RegionBorderVectorConstIterator
    thatRegionBordersIt    = region->GetRegionBorderConstItBegin();
  RegionBorderVectorConstIterator
    endOfThatRegionBorders = region->GetRegionBorderConstItEnd();

  // Merge the two sets of region borders into the newly merged region

  while ( ( thisRegionBordersIt != endOfThisRegionBorders )
          && ( thatRegionBordersIt != endOfThatRegionBorders ) )
    {
    // Ensure that there are no common borders in the new region
    if ( ( ( *thisRegionBordersIt )->GetRegion1() ==
           ( *thisRegionBordersIt )->GetRegion2() )
         || ( ( *thatRegionBordersIt )->GetRegion1() ==
              ( *thatRegionBordersIt )->GetRegion2() )
         || ( ( *thisRegionBordersIt ) == ( *thatRegionBordersIt ) ) )
      {
      itkExceptionMacro(<< "Invalid region border list");
      }

    // The two borders point to the same regions: DUPLICATE case.
    // They must be merged into one border, and any reference to
    // to the duplicate border must be purged.

    if ( ( ( *thisRegionBordersIt )->GetRegion1() ==
           ( *thatRegionBordersIt )->GetRegion1() )
         && ( ( *thisRegionBordersIt )->GetRegion2() ==
              ( *thatRegionBordersIt )->GetRegion2() ) )
      {
      // Add the lengths of the borders
      double newLength = ( *thatRegionBordersIt )->GetBorderLength()
                         + ( *thisRegionBordersIt )->GetBorderLength();

      ( *thisRegionBordersIt )->SetBorderLength(newLength);

      m_RegionBorderVector.push_back(*thisRegionBordersIt);

      // The border's region1 is the neighbor
      if ( ( *thatRegionBordersIt )->GetRegion2() == this )
        {
        ( *thatRegionBordersIt )->GetRegion1()
        ->DeleteRegionBorder( ( *thatRegionBordersIt ) );
        } // end if (the border's region1 is the neighbor )

      // The border's region2 is the neighbor
      else if ( ( *thatRegionBordersIt )->GetRegion1() == this )
        {
        ( *thatRegionBordersIt )->GetRegion2()
        ->DeleteRegionBorder(*thatRegionBordersIt);
        } // end else if (the border's region2 is the neighbor )

      else
        {
        itkExceptionMacro(<< "Invalid region border list");
        } // end else

      // Nullify the duplicate border so it can be identified and removed.
      ( *thatRegionBordersIt )->SetRegion1(ITK_NULLPTR);
      ( *thatRegionBordersIt )->SetRegion2(ITK_NULLPTR);
      ( *thatRegionBordersIt )->SetLambda(-1.0);

      thisRegionBordersIt++;
      thatRegionBordersIt++;
      } // end if loop for case when two borders point to same region

    // This neighbor region label is less then that neighbor region label
    else if ( ( ( *thisRegionBordersIt )->GetRegion1()->GetRegionLabel() <
                ( *thatRegionBordersIt )->GetRegion1()->GetRegionLabel() ) ||

              ( ( ( *thisRegionBordersIt )->GetRegion1()->GetRegionLabel() ==
                  ( *thatRegionBordersIt )->GetRegion1()->GetRegionLabel() ) &&

                ( ( *thisRegionBordersIt )->GetRegion2()->GetRegionLabel() <
                  ( *thatRegionBordersIt )->GetRegion2()->GetRegionLabel() ) ) )
      {
      m_RegionBorderVector.push_back(*thisRegionBordersIt);
      thisRegionBordersIt++;
      } // end else if

    // That neighbor region label is less then this neighbor region label
    else if ( ( ( *thatRegionBordersIt )->GetRegion1()->GetRegionLabel() <
                ( *thisRegionBordersIt )->GetRegion1()->GetRegionLabel() ) ||

              ( ( ( *thatRegionBordersIt )->GetRegion1()->GetRegionLabel() ==
                  ( *thisRegionBordersIt )->GetRegion1()->GetRegionLabel() ) &&

                ( ( *thatRegionBordersIt )->GetRegion2()->GetRegionLabel() <
                  ( *thisRegionBordersIt )->GetRegion2()->GetRegionLabel() ) ) )
      {
      m_RegionBorderVector.push_back(*thatRegionBordersIt);
      thatRegionBordersIt++;
      } // end else if
    else
      {
      itkExceptionMacro(<< "Invalid region border");
      } // end else
    }   // end of while

  // If any borders remain in thisRegionBordersIt, put them in the back
  while ( thisRegionBordersIt != endOfThisRegionBorders )
    {
    m_RegionBorderVector.push_back(*thisRegionBordersIt);
    thisRegionBordersIt++;
    }

  // If any borders remain in thatRegionBorders, put them to the back
  while ( thatRegionBordersIt != endOfThatRegionBorders )
    {
    m_RegionBorderVector.push_back(*thatRegionBordersIt);
    thatRegionBordersIt++;
    }
} // end SpliceRegionBorders

void
KLMSegmentationRegion
::UpdateRegionBorderLambda()
{
  // Check if the number of borders for this region is ITK_NULLPTR
  if ( m_RegionBorderVector.empty() )
    {
    itkExceptionMacro(<< "The region border for computing Lambda is ITK_NULLPTR");
    }

  // Set up the iterator to loop through the region border vector
  RegionBorderVectorIterator
    regionBorderVectorIt    = m_RegionBorderVector.begin();
  RegionBorderVectorIterator
    regionBorderVectorItEnd = m_RegionBorderVector.end();

  // Loop through the entire border list and update the Lambda values
  while ( regionBorderVectorIt != regionBorderVectorItEnd )
    {
    ( *regionBorderVectorIt )->EvaluateLambda();
    ++regionBorderVectorIt;
    } // End while loop
}     // end UpdateRegionBorderLambda

void
KLMSegmentationRegion
::DeleteAllRegionBorders()
{
  m_RegionBorderVector.resize(0);
} // end DeleteAllRegionBorders

KLMSegmentationRegion::RegionBorderVectorIterator
KLMSegmentationRegion
::GetRegionBorderItBegin()
{
  return m_RegionBorderVector.begin();
} // end GetRegionBorderItBegin

KLMSegmentationRegion::RegionBorderVectorConstIterator
KLMSegmentationRegion
::GetRegionBorderConstItBegin()
{
  return m_RegionBorderVector.begin();
} // end GetRegionBorderConstItBegin

KLMSegmentationRegion::RegionBorderVectorIterator
KLMSegmentationRegion
::GetRegionBorderItEnd()
{
  return m_RegionBorderVector.end();
} // end GetRegionBorderItEnd

KLMSegmentationRegion::RegionBorderVectorConstIterator
KLMSegmentationRegion
::GetRegionBorderConstItEnd()
{
  return m_RegionBorderVector.end();
} // end GetRegionBorderConstItEnd

KLMSegmentationRegion::RegionBorderVectorSizeType
KLMSegmentationRegion
::GetRegionBorderSize() const
{
  return m_RegionBorderVector.size();
} // end GetRegionBorderSize

void
KLMSegmentationRegion
::PrintRegionInfo()
{
  int region1label;
  int region2label;

  // If there are border pointers print the results
  RegionBorderVectorIterator tempVectorIt;

  std::cout << "------------------------------" << std::endl;
  std::cout << "Location   : " << this << std::endl;
  std::cout << "Label      : " << ( this->GetRegionLabel() ) << std::endl;
  std::cout << "Area       : " << ( this->GetRegionArea() ) << std::endl;
  std::cout << "Mean       : " << ( this->GetMeanRegionIntensity() ) << std::endl;
  std::cout << "Num Borders: " << static_cast< int >( m_RegionBorderVector.size() ) << std::endl;
  std::cout << "++++++++++++++++++++++++++++++" << std::endl;

  // If there are border pointers print the results
  tempVectorIt = m_RegionBorderVector.begin();
  for ( unsigned int k = 0; k < m_RegionBorderVector.size(); k++ )
    {
    region1label = ( *tempVectorIt )->GetRegion1()->GetRegionLabel();
    region2label = ( *tempVectorIt )->GetRegion2()->GetRegionLabel();

    std::cout << "Border Ptr :" << ( *tempVectorIt ) << "( "
              << region1label << " - " << region2label << " )" << " Lambda = "
              << ( *tempVectorIt )->GetLambda() << std::endl;

    tempVectorIt++;
    } // end for

  std::cout << "------------------------------" << std::endl;
} //end PrintRegionInfo
} // namespace itk
