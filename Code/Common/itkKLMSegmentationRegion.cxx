/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationRegion.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkKLMSegmentationRegion_cxx
#define _itkKLMSegmentationRegion_cxx

#include "itkKLMSegmentationRegion.h"

namespace itk
{


KLMSegmentationRegion
::KLMSegmentationRegion( void )
{

}


KLMSegmentationRegion
::~KLMSegmentationRegion()
{

}

/**
* PrintSelf
*/

void
KLMSegmentationRegion
::PrintSelf( std::ostream& os, Indent indent ) const
{

  Superclass::PrintSelf(os,indent);
  os << indent << "Region border object" << std::endl;

}// end PrintSelf

//----------------------------------------------------------------------


void
KLMSegmentationRegion
::SetRegion( VectorOfDoubleType regionMeanIntensity, 
             unsigned int regionArea,
             unsigned int label )
{
  //Set the area and the mean associated with the region
  this->SetRegionArea( regionArea );
  this->SetMeanRegionIntensity( regionMeanIntensity );

  // Assign region label
  this->SetRegionLabel( label );

  //Indicate that the region has been modified
  this->Modified();

}//end Set Region

//----------------------------------------------------------------------


void
KLMSegmentationRegion
::SetRegionBorder( KLMSegmentationBorder *pnewRegionBorder )
{

  // If this is the first border being added to the region

  if( m_RegionBorderVector.empty() )
    {

    m_RegionBorderVector.push_back( pnewRegionBorder ); 

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

    RegionBorderVectorType::iterator RegionBorderVectorIt = m_RegionBorderVector.begin();
    m_RegionBorderVector.insert( RegionBorderVectorIt, pnewRegionBorder );

    }// end else if

  // The new border should be the second element of the region border list,
  // see internal documentation of calling function 
  else 
    {  
    // Iterator set to the first element of the region border element
    RegionBorderVectorType::iterator RegionBorderVectorIt = m_RegionBorderVector.begin();

    // The offset 1 is to point the interator to the second vector element
    // so the insert function inserts 
    if( ( RegionBorderVectorIt + 1 ) <= m_RegionBorderVector.end() )
      {
      m_RegionBorderVector.insert( RegionBorderVectorIt + 1, pnewRegionBorder );
      }//end if
    else
      {
      m_RegionBorderVector.push_back( pnewRegionBorder );
      }//end else
    }//end else

    //Indicate that the region border has been modified
    this->Modified();

}//end SetRegionBorder

//----------------------------------------------------------------------


void
KLMSegmentationRegion
::SetRegionBorder3d( KLMSegmentationBorder *pnewRegionBorder )
{


  // If this is the first border being added to the region. This 
  // possibility should never be encountered. But the assertion will
  // catch it if it does.

  if( m_RegionBorderVector.empty() )
    {

    throw ExceptionObject(__FILE__, __LINE__); 

    }
  // 
  // The new region belongs at the head of the list 
  // else if(newBorderRegion1Label < firstBorderRegion1Label)

  else if(
    ( pnewRegionBorder->GetRegion1()->GetRegionLabel() ) <
    ( this->GetFirstRegionBorder()->GetRegion1()->GetRegionLabel() )
    )
    {

    // Iterator set to the first element of the region border element

    RegionBorderVectorType::iterator RegionBorderVectorIt = m_RegionBorderVector.begin();
    m_RegionBorderVector.insert( RegionBorderVectorIt, pnewRegionBorder );

    }// end else if

  // The new border should be going at the end of the list,
  // see internal documentation of calling function 
  else 
    {  

    // Iterator set to the first element of the region border element
    m_RegionBorderVector.begin();
    m_RegionBorderVector.push_back( pnewRegionBorder );

    }//end else

  //Indicate that the region border has been modified
  this->Modified();

}//end SetRegionBorder3d

//----------------------------------------------------------------------


KLMSegmentationBorder *
KLMSegmentationRegion
::GetFirstRegionBorder()
{

  return m_RegionBorderVector[0];

}// end GetFirstRegionBorder

//----------------------------------------------------------------------


void
KLMSegmentationRegion
::DeleteRegionBorder( KLMSegmentationBorder *pBorderCandidate )
{

  if( pBorderCandidate == NULL )
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }

  // The m_RegionBorderVec is a ordered vector of pointers to the
  // borders. If index points to the current border, index-1 points to the 
  // previous border and index+1 point to the next border
  unsigned int index = 0;

  while( m_RegionBorderVector[index] != pBorderCandidate ) 
    {
    index++;
    }//end

  if(index > m_RegionBorderVector.size())
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }

  // Iterator set to the first element of the region border element
  RegionBorderVectorIterator RegionBorderVectorIt = m_RegionBorderVector.begin();

  if( index <= m_RegionBorderVector.size() )
    {

    // Erase the region border that matches the pBorderCandidate
    m_RegionBorderVector.erase( RegionBorderVectorIt + index );

    }
  else
    {

    itkDebugMacro(<< "Border candidate not in region borders list" );

    }//end else

}// end DeleteRegionBorder()


void
KLMSegmentationRegion
::ReorderRegionBorders( KLMSegmentationBorder *pBorderCandidate )
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
  RegionBorderVectorIterator regionBorderVectorIt    = m_RegionBorderVector.begin();  
  RegionBorderVectorIterator regionBorderVectorItEnd = m_RegionBorderVector.end();  

// If the region border vector is empty, there is only one region border
  if( m_RegionBorderVector.empty() )
    {
    m_RegionBorderVector.insert(regionBorderVectorIt,pBorderCandidate);
    }

  // If there are many region borders
  else
    {
    while( ( regionBorderVectorIt + index ) != regionBorderVectorItEnd )
      {

      //The region border should be inserted
      if( ( pBorderCandidate->GetRegion1()->GetRegionLabel() <
            ( *(regionBorderVectorIt + index) )->GetRegion1()->GetRegionLabel() ) ||
          ( pBorderCandidate->GetRegion1()->GetRegionLabel() ==
            ( *(regionBorderVectorIt + index) )->GetRegion1()->GetRegionLabel() &&
            pBorderCandidate->GetRegion2()->GetRegionLabel() ==
            ( *(regionBorderVectorIt + index) )->GetRegion2()->GetRegionLabel() ) )
        {

        //Insert region border at the head of the list
        if(index == 0)
          {
          m_RegionBorderVector.insert(regionBorderVectorIt,pBorderCandidate);
          }

        //Insert the region border in the middle of the list
        else
          {
          m_RegionBorderVector.insert(regionBorderVectorIt+index,pBorderCandidate); 
          }

        pBorderCandidate = NULL;
        break;

        }// end of the big if clasue 

      index++;
      }// end of while

    // It should always be inserted at the end
    if( pBorderCandidate != NULL )
      m_RegionBorderVector.push_back( pBorderCandidate );

    }// end of the big else for the case with many region borders

}// end ReorderRegionBorders


void
KLMSegmentationRegion
::InsertRegionBorder(RegionBorderVectorIterator RegionBorderVectorIt,
                     KLMSegmentationBorder *pBorderCandidate )
{
  // Ensure that the border candidate is not a null pointer
  if( pBorderCandidate == NULL )
    {
    throw ExceptionObject(__FILE__, __LINE__);
    }

  // The m_RegionBorderVec is a ordered vector of pointers to the
  // borders. Insert a valid region border into the region border vector
  if( RegionBorderVectorIt <= m_RegionBorderVector.end() )
    m_RegionBorderVector.insert( RegionBorderVectorIt, pBorderCandidate );
  else
    m_RegionBorderVector.push_back( pBorderCandidate );

}// end InsertRegionBorder()


void
KLMSegmentationRegion
::UpdateRegionBorderLambda()
{
  // Check if the number of borders for this region is NULL
  if( m_RegionBorderVector.empty() )
    {
    itkDebugMacro(<< "The region border for computing lambda is NULL" );
    }

  // Set up the iterator to loop through the region border vector
  RegionBorderVectorIterator regionBorderVectorIt    = m_RegionBorderVector.begin();  
  RegionBorderVectorIterator regionBorderVectorItEnd = m_RegionBorderVector.end(); 

// Loop through the entire border list and update the lambda values
  while( regionBorderVectorIt != regionBorderVectorItEnd )
    {
    ( *regionBorderVectorIt )->EvaluateLambda();
    regionBorderVectorIt++; 
    }// End while loop

} // end UpdateRegionBorderLambda

void
KLMSegmentationRegion
::DeleteAllRegionBorders()
{
  m_RegionBorderVector.resize( 0 );
}

KLMSegmentationRegion::RegionBorderVectorIterator
KLMSegmentationRegion
::GetRegionBorderItBegin()
{

  return m_RegionBorderVector.begin();

}// end GetRegionBorderItBegin


KLMSegmentationRegion::RegionBorderVectorIterator
KLMSegmentationRegion
::GetRegionBorderItEnd()
{
  return m_RegionBorderVector.end();
}// end GetRegionBorderItBegin


void
KLMSegmentationRegion
::PrintRegionInfo()
{
  int region1label;
  int region2label;
  this->DebugOff();

  itkDebugMacro(<< "------------------------------" );
  itkDebugMacro(<< "Location   : " << this );
  itkDebugMacro(<< "Label      : " << (this->GetRegionLabel()) );
  itkDebugMacro(<< "Area       : " << (this->GetRegionArea()) );
  itkDebugMacro(<< "Mean       : " << (this->GetMeanRegionIntensity()) );
  itkDebugMacro(<< "Num Borders: " << static_cast<int>( m_RegionBorderVector.size() ) );
  itkDebugMacro(<< "++++++++++++++++++++++++++++++" );

  // If there are border pointers print the results
  RegionBorderVectorIterator tempVectorIt = m_RegionBorderVector.begin();
  for( unsigned int k = 0; k < m_RegionBorderVector.size(); k++ )
    {      
    region1label = (*tempVectorIt)->GetRegion1()->GetRegionLabel();
    region2label = (*tempVectorIt)->GetRegion2()->GetRegionLabel(); 

    itkDebugMacro(<< "Border Ptr :" << (*tempVectorIt) << "( " << 
      region1label << " - " << region2label << " )" );
    tempVectorIt++;
    }// end while

  this->DebugOff();

  
  std::cout << "------------------------------" << std::endl;
  std::cout << "Location   : " << this << std::endl;
  std::cout << "Label      : " << (this->GetRegionLabel()) << std::endl;
  std::cout << "Area       : " << (this->GetRegionArea()) << std::endl;
  std::cout << "Mean       : " << (this->GetMeanRegionIntensity()) << std::endl;
  std::cout << "Num Borders: " << static_cast<int>( m_RegionBorderVector.size() ) << std::endl;
  std::cout << "++++++++++++++++++++++++++++++" << std::endl;

  // If there are border pointers print the results
  tempVectorIt = m_RegionBorderVector.begin();
  for( unsigned int k = 0; k < m_RegionBorderVector.size(); k++ )
    {      
    region1label = (*tempVectorIt)->GetRegion1()->GetRegionLabel();
    region2label = (*tempVectorIt)->GetRegion2()->GetRegionLabel(); 

    std::cout << "Border Ptr :" << (*tempVectorIt) << "( " << 
      region1label << " - " << region2label << " )" << " L = " << 
      (*tempVectorIt)->GetLambda() << std::endl;

    tempVectorIt++;
    }// end while

  std::cout << "------------------------------" << std::endl;


}//end PrintRegionInfo

} // namespace itk




#endif
