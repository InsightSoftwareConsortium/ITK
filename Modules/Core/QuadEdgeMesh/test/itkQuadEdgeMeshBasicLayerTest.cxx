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

// Program test for the basic QE layer.
#include <iostream>
#include "itkGeometricalQuadEdge.h"

class itkQuadEdgeMeshBasicLayerTestHelper
{
public:
  typedef itk::GeometricalQuadEdge< int, int, bool, bool >  PrimalType;
  typedef PrimalType::DualType                              DualType;

  static PrimalType * MakeQuadEdges()
    {
    PrimalType * e1 = new PrimalType();
    DualType   * e2 = new DualType();
    PrimalType * e3 = new PrimalType();
    DualType   * e4 = new DualType();

    e1->SetRot( e2 );
    e2->SetRot( e3 );
    e3->SetRot( e4 );
    e4->SetRot( e1 );

    e1->SetOnext( e1 );
    e2->SetOnext( e4 );
    e3->SetOnext( e3 );
    e4->SetOnext( e4 );

    return e1;
    }
};

int itkQuadEdgeMeshBasicLayerTest( int , char* [] )
{
  typedef itkQuadEdgeMeshBasicLayerTestHelper::PrimalType  PrimalType;

  PrimalType* e[5];

  //////////////////////////////////////////////////////////
  std::cout << "Creating edges" << std::endl;
  for(int i=0; i < 5; i++)
    {
    e[i] = new PrimalType;
    }
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout << "Testing MakeEdge" << std::endl;
  for( int i=0; i < 5; i++ )
    {
    e[i] = itkQuadEdgeMeshBasicLayerTestHelper::MakeQuadEdges();
    }
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout << "Testing ring for Origin() unset values" << std::endl;
  for(int i=0; i < 5; i++)
    {
    if( e[i]->IsOriginSet() )
      {
      std::cout << "IsOriginSet() should not be set for edge number " << i
                << ". Failed" << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Setting Origin() and Destination() values... " << std::endl;
  int  org[5] = { 0, 1, 2, 3, 0};
  int dest[5] = { 1, 2, 3, 0, 2};

  for(int i=0; i < 5; i++ )
    {
    e[i]->SetOrigin( org[i] );
    e[i]->SetDestination( dest[i] );
    }
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Testing ring for Origin() set values... " << std::endl;
  for( int i=0; i < 5; i++ )
    {
    if ( ! e[i]->IsOriginSet() )
      {
      std::cout << "IsOriginSet() ITK_NULLPTR value failed for edge number "
                << i << ". Failed" << std::endl;
      return EXIT_FAILURE;
      } //fi
    } // rof
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Splicing... " << std::endl;
  e[0]->Splice( e[4] );
  e[4]->Splice( e[3]->GetSym( ) );
  e[1]->Splice( e[0]->GetSym( ) );
  e[2]->Splice( e[4]->GetSym( ) );
  e[4]->GetSym( )->Splice( e[1]->GetSym( ) );
  e[3]->Splice( e[2]->GetSym( ) );
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Testing Origin() and Destination() adjacency... "
             << std::endl;
  for( int i=0; i < 5; i++ )
    {
    if ( e[i]->GetOrigin() != org[i] )
      {
      std::cout << std::endl
                << "Erroneous GetOrigin() on edge number " << i
                << ". Was expecting " << org[i]
                << " but got " << e[i]->GetOrigin()
                << ". Failed" << std::endl;
      return EXIT_FAILURE;
      } //fi
    if ( e[i]->GetDestination() != dest[i] )
      {
      std::cout << std::endl
                << "Erroneous GetDestination() on edge number " << i
                << ". Was expecting " << dest[i]
                << " but got " << e[i]->GetDestination()
                << ". Failed" << std::endl;
      return EXIT_FAILURE;
      } //fi
    } // rof
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Setting faces... " << std::endl;
  e[0]->SetLeft( 0 );
  e[1]->SetLeft( 0 );
  e[4]->SetRight( 0 );
  e[2]->SetLeft( 1 );
  e[3]->SetLeft( 1 );
  e[4]->SetLeft( 1 );
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  typedef PrimalType::IteratorGeom IteratorGeom;
  std::cout  << "Testing Onext iterators... " << std::endl;
  int onextDestination[5][3] = { { 1, 2, 3 },
                                 { 2, 0, 0 },  // Last 0 is a dummy
                                 { 3, 0, 1 },
                                 { 0, 2, 0 },  // Last 0 is a dummy
                                 { 2, 3, 1 } };
  for( int edge = 0; edge < 5; edge++ )
    {
    int test = 0;
    for( IteratorGeom itOnext = e[edge]->BeginGeomOnext( );
         itOnext != e[edge]->EndGeomOnext( );
         itOnext++, test++ )
      {
      if ( itOnext.Value( )->GetDestination( ) != onextDestination[edge][test] )
        {
        std::cout << std::endl
                  << "Erroneous GetDestination() on edge number " << edge
                  << ". Was expecting " << onextDestination[edge][test]
                  << " but got " << itOnext.Value( )->GetDestination( )
                  << ". Failed" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }
  std::cout << "Passed" << std::endl;

  //////////////////////////////////////////////////////////
  std::cout  << "Testing Lnext iterators... " << std::endl;
  int lnextDestination[5][3] = { { 0, 1, 2 },
                                 { 1, 2, 0 },
                                 { 2, 3, 0 },
                                 { 3, 0, 2 },
                                 { 0, 2, 3 } };
  for( int edge = 0; edge < 5; edge++ )
    {
    int test = 0;
    for( IteratorGeom itLnext = e[edge]->BeginGeomLnext( );
         itLnext != e[edge]->EndGeomLnext( );
         itLnext++, test++ )
      {
      if ( *itLnext != lnextDestination[edge][test] )
        {
        std::cout << std::endl
                  << "Erroneous GetDestination() on edge number " << edge
                  << ". Was expecting " << lnextDestination[edge][test]
                  << " but got " << *itLnext
                  << ". Failed" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  std::cout << "on Sym()... " << std::endl;
  int lnextDestinationOnSym[3] = { 2, 0, 1 };
  int test = 0;
  for( IteratorGeom itLnext = e[4]->GetSym()->BeginGeomLnext( );
       itLnext != e[4]->GetSym()->EndGeomLnext( );
       itLnext++, test++ )
    {
    if ( *itLnext != lnextDestinationOnSym[test] )
      {
      std::cout << std::endl
                << "Erroneous GetDestination() on edge number 4. "
                << "Was expecting " << lnextDestinationOnSym[test]
                << " but got " << *itLnext
                << ". Failed" << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << "Passed" << std::endl;


  //////////////////////////////////////////////////////////
  std::cout  << "Testing Sym iterators... " << std::endl;
  int symDestination[5][3] = { { 0, 1 },
                               { 1, 2 },
                               { 2, 3 },
                               { 3, 0 },
                               { 0, 2 } };
  for( int edge = 0; edge < 5; edge++ )
    {
    test = 0;
    for( IteratorGeom itSym = e[edge]->BeginGeomSym( );
         itSym != e[edge]->EndGeomSym( );
         itSym++, test++ )
      {
      if ( *itSym != symDestination[edge][test] )
        {
        std::cout << std::endl
                  << "Erroneous GetDestination() on edge number " << edge
                  << ". Was expecting " << symDestination[edge][test]
                  << " but got " << *itSym
                  << ". Failed" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }
  std::cout << "Passed" << std::endl;

  return EXIT_SUCCESS;
}
