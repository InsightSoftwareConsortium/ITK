

#ifndef _itkSparseLevelSetNode_h
#define _itkSparseLevelSetNode_h

#include "itkLevelSet.h"


namespace itk
{
  
template<class TPixel, unsigned int VSetDimension=2>
class SparseLevelSetNode : public LevelSetNode<TPixel, VSetDimension>
{

public:

  /* 
   *  standard typedef for "self" and "superclass"
   */
  typedef SparseLevelSetNode Self;
  typedef LevelSetNode<TPixel, VSetDimension>  Superclass;

  typedef SmartPointer<Self> Pointer;
  typedef typename Superclass::IndexType IndexType;
  typedef typename Superclass::PixelType PixelType;


  PixelType& GetPreviousValue()
   { return m_PreviousValue; }  
  
  void SetPreviousValue( const PixelType & input)
   { m_SetPreviousValue = input;}

  int GetStatus()
   { return m_Status; }
  
  void SetStatus(const int& input)
   { m_Status = input;}   
  
  SparseLevelSetNode()
   { 
     m_Status = 0; 
     m_PreviousValue = 0;
     this->SetValue(0);
   }
  SparseLevelSetNode(const IndexType& index)
   {  this->SetIndex(index); }
  SparseLevelSetNode( const IndexType& index, PixelType & value)
   {  this->SetIndex(index); this->SetValue(value); }

  /** Operator =. Two nodes are equal if both their value and index fields
   * are the same. */
  Self& operator= ( const Self& rhs )
    {
      if( this == &rhs ) {return *this;}
  
      this->SetValue(rhs.m_Value);
      this->SetIndex(rhs.m_Index);
      this->SetPreviousValue(rhs.m_PreviousValue);
      this->SetStatus(rhs.m_Status);
      return *this;
    }

  Self& operator= ( const Self * rhs )
    {
      if( this == &rhs ) {return *this;}
  
      this->SetValue(rhs->m_Value);
      this->SetIndex(rhs->m_Index);
      this->SetPreviousValue(rhs->m_PreviousValue);
      this->SetStatus(rhs->m_Status);
      return *this;
    }


private: 
  

  PixelType m_PreviousValue;
  int     m_Status;

};





}// end of namespace itk


#endif
