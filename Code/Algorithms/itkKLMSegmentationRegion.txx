/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationRegion.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkKLMSegmentationRegion_txx
#define _itkKLMSegmentationRegion_txx

namespace itk
{

template<class TInputImage, class TOutputImage>
KLMSegmentationRegion<TInputImage,TOutputImage>
::KLMSegmentationRegion( void )
{

}

template<class TInputImage, class TOutputImage>
KLMSegmentationRegion<TInputImage,TOutputImage>
::~KLMSegmentationRegion()
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
KLMSegmentationRegion<TInputImage,TOutputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{

  Superclass::PrintSelf(os,indent);
  os << indent << "Region border object" << std::endl;

}// end PrintSelf

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMSegmentationRegion<TInputImage,TOutputImage>
::SetRegion( VecDblType regionMeanIntensity, 
             unsigned int regionArea,
             unsigned int label )
{
  //Set the area and the mean associated with the region
  this->SetRegionArea( regionArea );
  this->SetMeanRegionIntensity( regionMeanIntensity );

  // Assign region label
  this->SetRegionLabel( label );

        
}//end Set Region

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMSegmentationRegion<TInputImage,TOutputImage>
::SetRegionBorder( KLMSegmentationBorder<TInputImage,TOutputImage> *pnewRegionBorder )
{

  // If this is the first border being added to the region

  if( m_RegionBorderVec.empty() )
  {

    m_RegionBorderVec.push_back( pnewRegionBorder ); 
  
  }
  // If this is not the first border being added to the region 
  // The new region belongs at the head of the list 
  // else if(newBorderRegion2Label < firstBorderRegion2Label)

  else if(
           ( pnewRegionBorder->GetRegion2()->GetRegionLabel() ) <
           ( this->GetFirstRegionBorder()->GetRegion2()->GetRegionLabel() )
         )
  {

    // Iterator set to the first element of the region border element

    RegionBorderVecType::iterator RegionBorderVecIt = m_RegionBorderVec.begin();
    m_RegionBorderVec.insert( RegionBorderVecIt, pnewRegionBorder );

  }// end else if

  // The new border should be the second element of the region border list,
  // see internal documentation of calling function 
  else 
  {  
    // Iterator set to the first element of the region border element
    RegionBorderVecType::iterator RegionBorderVecIt = m_RegionBorderVec.begin();

    // The offset 1 is to point the interator to the second vector element
    // so the insert function inserts 
    if( ( RegionBorderVecIt + 1 ) <= m_RegionBorderVec.end() )
    {
      m_RegionBorderVec.insert( RegionBorderVecIt + 1, pnewRegionBorder );
    }//end if
    else
    {
      m_RegionBorderVec.push_back( pnewRegionBorder );
    }//end else
  }//end else
 
}//end SetRegionBorder

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
KLMSegmentationBorder<TInputImage,TOutputImage> *
KLMSegmentationRegion<TInputImage,TOutputImage>
::GetFirstRegionBorder()
{

  return m_RegionBorderVec[0];

}// end GetFirstRegionBorder

//----------------------------------------------------------------------

template<class TInputImage, class TOutputImage>
void
KLMSegmentationRegion<TInputImage,TOutputImage>
::DeleteRegionBorder( KLMSegmentationBorder<TInputImage,TOutputImage> *pBorderCandidate )
{

  if( pBorderCandidate == NULL )
  {
    throw ExceptionObject(__FILE__, __LINE__);
  }

  // The m_RegionBorderVec is a ordered vector of pointers to the
  // borders. If index points to the current border, index-1 points to the 
  // previous border and index+1 point to the next border
  unsigned int index = 0;

  while( m_RegionBorderVec[index] != pBorderCandidate ) 
  {
    index++;
  }//end

  if(index > m_RegionBorderVec.size())
  {
    throw ExceptionObject(__FILE__, __LINE__);
  }

  // Iterator set to the first element of the region border element
  RegionBorderVecIterator RegionBorderVecIt = m_RegionBorderVec.begin();

  if( index <= m_RegionBorderVec.size() )
  {

    // Erase the region border that matches the pBorderCandidate
    m_RegionBorderVec.erase( RegionBorderVecIt + index );
  
  }
  else
  {

    std::cout << "Border candidate not in region borders list" << std::endl;
  
  }//end else

}// end DeleteRegionBorder()

template<class TInputImage, class TOutputImage>
void
KLMSegmentationRegion<TInputImage,TOutputImage>
::ReorderRegionBorders( KLMSegmentationBorder<TInputImage,TOutputImage> *pBorderCandidate )
{
  // Ensure that the border candidate is not a null pointer
  if( pBorderCandidate == NULL )
  {
    throw ExceptionObject(__FILE__, __LINE__);
  }

  // First delete the border from the region border list
  DeleteRegionBorder( pBorderCandidate );

  // The m_RegionBorderVec is a ordered vector of pointers to the
  // borders. If index points to the current border, index-1 points to the 
  // previous border and index+1 point to the next border
  unsigned int index = 0;

  // Iterator set to the first element of the region border element
  RegionBorderVecIterator regionBorderVecIt    = m_RegionBorderVec.begin();  
  RegionBorderVecIterator regionBorderVecItEnd = m_RegionBorderVec.end();  

  // If the region border vector is empty, there is only one region border
  if( m_RegionBorderVec.empty() )
  {
    m_RegionBorderVec.insert(regionBorderVecIt,pBorderCandidate);
    index++;
  }

  // If there are many region borders
  else
  {
    while( ( regionBorderVecIt + index ) != regionBorderVecItEnd )
    {
      //The region border should be inserted
      if( ( pBorderCandidate->GetRegion1()->GetRegionLabel() <
            ( *regionBorderVecIt )->GetRegion1()->GetRegionLabel() ) ||
          ( pBorderCandidate->GetRegion1()->GetRegionLabel() ==
            (*regionBorderVecIt)->GetRegion1()->GetRegionLabel() &&
            pBorderCandidate->GetRegion2()->GetRegionLabel() ==
            (*regionBorderVecIt)->GetRegion2()->GetRegionLabel() ) )
      {
        //Insert region border at the head of the list
        if(index == 0)
        {
          m_RegionBorderVec.insert(regionBorderVecIt,pBorderCandidate);
        }

        //Insert the region border in the middle of the list
        else
        {
          m_RegionBorderVec.insert(regionBorderVecIt+index,pBorderCandidate); 
        }

        pBorderCandidate = NULL;
        break;

      }// end of the big if clasue 

      index++;
    }// end of while

    // It should always be inserted at the end
    if( pBorderCandidate != NULL )
    m_RegionBorderVec.push_back( pBorderCandidate );

  }// end of the big else for the case with many region borders

}// end ReorderRegionBorders

template<class TInputImage, class TOutputImage>
void
KLMSegmentationRegion<TInputImage,TOutputImage>
::InsertRegionBorder(RegionBorderVecIterator RegionBorderVecIt,
                     KLMSegmentationBorder<TInputImage,TOutputImage> *pBorderCandidate )
{
  // Ensure that the border candidate is not a null pointer
  if( pBorderCandidate == NULL )
  {
    throw ExceptionObject(__FILE__, __LINE__);
  }

  // The m_RegionBorderVec is a ordered vector of pointers to the
  // borders. Insert a valid region border into the region border vector
  if( RegionBorderVecIt <= m_RegionBorderVec.end() )
    m_RegionBorderVec.insert( RegionBorderVecIt, pBorderCandidate );
  else
    m_RegionBorderVec.push_back( pBorderCandidate );

}// end InsertRegionBorder()

template<class TInputImage, class TOutputImage>
void
KLMSegmentationRegion<TInputImage,TOutputImage>
::UpdateRegionBorderLambda()
{
  // Check if the number of borders for this region is NULL
  if( m_RegionBorderVec.empty() )
  {
    std::cout << "The region border for computing lambda is NULL" << std::endl;
  }

  // Set up the iterator to loop through the region border vector
  RegionBorderVecIterator regionBorderVecIt    = m_RegionBorderVec.begin();  
  RegionBorderVecIterator regionBorderVecItEnd = m_RegionBorderVec.end(); 

  // Loop through the entire border list and update the lambda values
  while( regionBorderVecIt != regionBorderVecItEnd )
  {
    ( *regionBorderVecIt )->EvaluateLambda();
    regionBorderVecIt++; 
  }// End while loop

} // end UpdateRegionBorderLambda


template<class TInputImage, class TOutputImage>
KLMSegmentationRegion<TInputImage,TOutputImage>::RegionBorderVecIterator
KLMSegmentationRegion<TInputImage,TOutputImage>
::GetRegionBorderItBegin()
{

  return m_RegionBorderVec.begin();

}// end GetRegionBorderItBegin

template<class TInputImage, class TOutputImage>
KLMSegmentationRegion<TInputImage,TOutputImage>::RegionBorderVecIterator
KLMSegmentationRegion<TInputImage,TOutputImage>
::GetRegionBorderItEnd()
{

  return m_RegionBorderVec.end();

}// end GetRegionBorderItBegin

template<class TInputImage, class TOutputImage>
void
KLMSegmentationRegion<TInputImage,TOutputImage>
::PrintRegionInfo()
{
  std::cout << "------------------------------" << std::endl;
  std::cout << "Location   : " << this << std::endl;
  std::cout << "Label      : " << (this->GetRegionLabel()) << std::endl;
  std::cout << "Area       : " << m_RegionArea << std::endl;
  std::cout << "Mean       : " << m_MeanVec << std::endl;
  std::cout << "Num Borders: " << m_RegionBorderVec.size() << std::endl;
  std::cout << "++++++++++++++++++++++++++++++" << std::endl;

  // If there are border pointers print the results
  RegionBorderVecIterator tempVecIt = m_RegionBorderVec.begin();
  for( int k = 0; k < m_RegionBorderVec.size(); k++ )
  {       
    std::cout << "Border Ptr :" << (*tempVecIt) << std::endl;
    tempVecIt++;
  }// end while
          
  std::cout << "------------------------------" << std::endl;
  std::cout << "------------------------------" << std::endl;

}//end PrintRegionInfo

} // namespace itk




#endif
