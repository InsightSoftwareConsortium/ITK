/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScene.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
 
#ifndef __Scene_h 
#define __Scene_h 

#include "itkSpatialObject.h"  
 
#include <list>

namespace itk 
{ 

/** \class Scene
* \brief a Scene has a list of SpatialObjects
*
* This class represent a Scene object into which one can
* plug any kind of spatial object.
*
* \also SpatialObject
*/ 
 
template <unsigned int SpaceDimension = 3>
class Scene 
: public Object
{ 
 
public: 

  typedef Scene<SpaceDimension> Self; 
  typedef Object Superclass; 
  typedef Superclass::Pointer SuperclassPointer; 
  typedef SmartPointer< Self > Pointer; 
  typedef SmartPointer< const Self > ConstPointer; 

  typedef SpatialObject<SpaceDimension> ObjectType;
  typedef SpatialObject<SpaceDimension> SpatialObjectType;

  typedef std::list< SpatialObject<SpaceDimension> * > 
          ObjectListType; 
     
  itkStaticConstMacro(MaximumDepth, unsigned int, 9999999);

  /** Method for creation through the object factory */ 
  itkNewMacro(Self); 
  itkTypeMacro(Self, Superclass); 

  /** Add an object to the list of children. */ 
  void AddSpatialObject( SpatialObject<SpaceDimension> * pointer ); 
     
  /** Remove the object passed as arguments from the list of 
   *  children. May this function 
   *  should return a false value if the object to remove is 
   *  not found in the list. */ 
  void RemoveSpatialObject( SpatialObject<SpaceDimension> * object ); 

  /** Returns a list of pointer to the children affiliated to this object.*/ 
  ObjectListType * GetObjects( unsigned int depth=MaximumDepth,
                               char * name=NULL );

  /** Returns the number of children currently assigned to the Scene object.*/ 
  unsigned int GetNumberOfObjects( unsigned int depth=MaximumDepth,
                                   char * name=NULL ); 

  /** Set the list of pointers to children to the list passed as argument.*/ 
  void SetObjects( ObjectListType & children ); 

  /** Returns the latest modified time of all the objects contained in this Scene
   *  object. */
  unsigned long GetMTime( void ) const;

  /** Get/Set the ParentID */
  void SetParentId(int parentid) {m_ParentId=parentid;}
  int  GetParentId(void) {return m_ParentId;}

  /** Return a SpatialObject in the scene given its ID */
  SpatialObject<SpaceDimension> * GetObjectById(int Id);

  bool FixHierarchy(void);

protected: 

  /** List of the children object plug to the Scene spatial object. */
  ObjectListType m_Objects; 

  /** constructor */ 
  Scene(); 

  /** destructor */ 
  virtual ~Scene();

  /** Print the object informations in a stream. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const; 

  /** Parent ID : default = -1 */
  int m_ParentId;

}; 

} // end of namespace itk
 

#ifndef ITK_MANUAL_INSTANTIATION 
  #include "itkScene.txx" 
#endif 

#endif // __Scene_h 
