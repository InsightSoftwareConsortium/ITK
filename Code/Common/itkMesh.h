/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMesh.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkMesh represents unstrucured data such as polygonal or finite element
 * meshes. The conceptual model of the mesh is a hierarchically arranged set
 * of entities, ranging through vertex, edge, face, and region (with
 * n-dimensional hyper-regions possible). Each entity of dimension n refers
 * to face (or boundary) entities of dimension (n-1). The meshes can vary in
 * dimensionality, for example, representing a mixed mesh of tetrahedra and
 * quadrilaterals. Other important functionality incudes the ability to
 * associate a "pixel type" with each entity (e.g., itkScalar). This
 * information can be used in a variety of ways including associating scalar,
 * vector, and/or other attribute data with each entity; controlling entity
 * "visibility"; and grouping of entities. (The functionality of this
 * information depends on the nature of the pixel type.)
 *
 * An itkMesh is created from a set of n-dimensional entities. Each entity in
 * turn is created from other entities of dimension n-1. This proceeds
 * recursively until an entity type of VERTEX is encountered. In this
 * hierarchy, an entity is instantiated with two template parameters: one
 * defines the pixel type associated with the entity, and the other the pixel
 * types of the entities forming its n-1 boundary. A vertex is defined by its
 * dimension (which must match TMeshDimension), its representation type
 * (e.g., float), and an associated pixel type. This class itkMesh is created
 * with a single template parameter. The type TPixelEntity is the pixel type
 * associated with the entities forming the mesh.  
 */

#ifndef __itkMesh_h
#define __itkMesh_h

#include "itkMeshBase.h"

template <class TPixelEntity, unsigned int TMeshDimension=2>
class ITK_EXPORT itkMesh : public itkMeshBase
{
public:
  /** 
   * Smart pointer typedef support. 
   */
  typedef typename itkSmartPointer<itkMesh> Pointer;

  /** 
   * Create an empty image. 
   */
  static Pointer New();

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkMesh, itkDataObject);

  /** 
   * Restore object to initialized state.
   */
  void Initialize();

  

protected:
  itkMesh();
  ~itkMesh();
  itkMesh(const itkMesh&) {};
  void operator=(const itkMesh&) {};
  virtual void PrintSelf(std::ostream& os, itkIndent indent);

private:
  
};

#endif

#ifndef _MESH_H_
#define _MESH_H_

// Algorithm Oriented Mesh Database - B. Kaan Karamete Mar. 2000

#ifdef DEBUG
#define _DEBUG_
#endif

#include "dEntity.h"
#include "dEntities.h"
#include "dEntityContainer.h"
class dIDGenerator;

class dMesh  {
 private:
  dMeshContainer* e[4]; 
  dIDGenerator* idgen;
 public:
  dMesh();
  int isCreated(dEntity::EntityType) const ;
  void createContainer(dEntity::EntityType) ;
  int entityExists(dEntity*);
  dEntity* entityExists(dEntity::EntityType,
                        dLowerEntityContainer* ents)  ;

  dEntity* createEntity(dEntity::EntityType,
			dLowerEntityContainer*,dEntity* klas=0);
 
  dMeshContainer::iter firstEntityIter(dEntity::EntityType etype) ;
  dMeshContainer::iter lastEntityIter(dEntity::EntityType etype) ;
  dMeshContainer::iter nextEntityIter(dEntity::EntityType etype,
                        dMeshContainer::iter) ;

  dVertex* createVertex(double x, double y, double z,dEntity* klas=0);
  dEdge* createEdge(dVertex* v1, dVertex* v2, dEntity* klas=0);
  dFace* createFace(dVertex* v1, dVertex* v2, dVertex* v3, dEntity* klas=0);
  dRegion* createRegion(dVertex* v1, dVertex* v2, dVertex* v3, dVertex* v4,
			dEntity* klas=0,dElement::ElementType e=dElement::TETRAHEDRON);
  int createFromTo(dEntity::EntityType from, dEntity::EntityType To);

  // Important Note: when deleted an entity from mesh 
  // the higher order attachments of lowerorder entities of that entity 
  // are checked to also remove the entity.

  int setEntityID();
  void getEntityID(int id);

  void deleteVertex(dVertex* vertex);
  void deleteEdge(dEdge* edge);
  void deleteFace(dFace* face);
  void deleteRegion(dRegion* region);

  dMeshContainer::iter begin(dEntity::EntityType) ;
  dMeshContainer::iter end(dEntity::EntityType) ;

  int size(dEntity::EntityType etype) const;
  dEntity* get(dEntity::EntityType etype, int i);
  virtual ~dMesh();
};

typedef dMesh dModel;

#endif
