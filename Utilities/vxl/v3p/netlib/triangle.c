/*****************************************************************************/
/*                                                                           */
/*      888888888        ,o,                          / 888                  */
/*         888    88o88o  "    o8888o  88o8888o o88888o 888  o88888o         */
/*         888    888    888       88b 888  888 888 888 888 d888  88b        */
/*         888    888    888  o88^o888 888  888 "88888" 888 8888oo888        */
/*         888    888    888 C888  888 888  888  /      888 q888             */
/*         888    888    888  "88o^888 888  888 Cb      888  "88oooo"        */
/*                                              "8oo8D                       */
/*                                                                           */
/*  A Two-Dimensional Quality Mesh Generator and Delaunay Triangulator.      */
/*  (triangle.c)                                                             */
/*                                                                           */
/*  Version 1.3                                                              */
/*  July 19, 1996                                                            */
/*                                                                           */
/*  Copyright 1996                                                           */
/*  Jonathan Richard Shewchuk                                                */
/*  School of Computer Science                                               */
/*  Carnegie Mellon University                                               */
/*  5000 Forbes Avenue                                                       */
/*  Pittsburgh, Pennsylvania  15213-3891                                     */
/*  jrs@cs.cmu.edu                                                           */
/*                                                                           */
/*  This program may be freely redistributed under the condition that the    */
/*    copyright notices (including this entire header and the copyright      */
/*    notice printed when the `-h' switch is selected) are not removed, and  */
/*    no compensation is received.  Private, research, and institutional     */
/*    use is free.  You may distribute modified versions of this code UNDER  */
/*    THE CONDITION THAT THIS CODE AND ANY MODIFICATIONS MADE TO IT IN THE   */
/*    SAME FILE REMAIN UNDER COPYRIGHT OF THE ORIGINAL AUTHOR, BOTH SOURCE   */
/*    AND OBJECT CODE ARE MADE FREELY AVAILABLE WITHOUT CHARGE, AND CLEAR    */
/*    NOTICE IS GIVEN OF THE MODIFICATIONS.  Distribution of this code as    */
/*    part of a commercial system is permissible ONLY BY DIRECT ARRANGEMENT  */
/*    WITH THE AUTHOR.  (If you are not directly supplying this code to a    */
/*    customer, and you are instead telling them how they can obtain it for  */
/*    free, then you are not required to make any arrangement with me.)      */
/*                                                                           */
/*  Hypertext instructions for Triangle are available on the Web at          */
/*                                                                           */
/*      http://www.cs.cmu.edu/~quake/triangle.html                           */
/*                                                                           */
/*  Some of the references listed below are marked [*].  These are available */
/*    for downloading from the Web page                                      */
/*                                                                           */
/*      http://www.cs.cmu.edu/~quake/triangle.research.html                  */
/*                                                                           */
/*  A paper discussing some aspects of Triangle is available.  See Jonathan  */
/*    Richard Shewchuk, "Triangle:  Engineering a 2D Quality Mesh Generator  */
/*    and Delaunay Triangulator," First Workshop on Applied Computational    */
/*    Geometry, ACM, May 1996.  [*]                                          */
/*                                                                           */
/*  Triangle was created as part of the Archimedes project in the School of  */
/*    Computer Science at Carnegie Mellon University.  Archimedes is a       */
/*    system for compiling parallel finite element solvers.  For further     */
/*    information, see Anja Feldmann, Omar Ghattas, John R. Gilbert, Gary L. */
/*    Miller, David R. O'Hallaron, Eric J. Schwabe, Jonathan R. Shewchuk,    */
/*    and Shang-Hua Teng, "Automated Parallel Solution of Unstructured PDE   */
/*    Problems."  To appear in Communications of the ACM, we hope.           */
/*                                                                           */
/*  The quality mesh generation algorithm is due to Jim Ruppert, "A          */
/*    Delaunay Refinement Algorithm for Quality 2-Dimensional Mesh           */
/*    Generation," Journal of Algorithms 18(3):548-585, May 1995.  [*]       */
/*                                                                           */
/*  My implementation of the divide-and-conquer and incremental Delaunay     */
/*    triangulation algorithms follows closely the presentation of Guibas    */
/*    and Stolfi, even though I use a triangle-based data structure instead  */
/*    of their quad-edge data structure.  (In fact, I originally implemented */
/*    Triangle using the quad-edge data structure, but switching to a        */
/*    triangle-based data structure sped Triangle by a factor of two.)  The  */
/*    mesh manipulation primitives and the two aforementioned Delaunay       */
/*    triangulation algorithms are described by Leonidas J. Guibas and Jorge */
/*    Stolfi, "Primitives for the Manipulation of General Subdivisions and   */
/*    the Computation of Voronoi Diagrams," ACM Transactions on Graphics     */
/*    4(2):74-123, April 1985.                                               */
/*                                                                           */
/*  Their O(n log n) divide-and-conquer algorithm is adapted from Der-Tsai   */
/*    Lee and Bruce J. Schachter, "Two Algorithms for Constructing the       */
/*    Delaunay Triangulation," International Journal of Computer and         */
/*    Information Science 9(3):219-242, 1980.  The idea to improve the       */
/*    divide-and-conquer algorithm by alternating between vertical and       */
/*    horizontal cuts was introduced by Rex A. Dwyer, "A Faster Divide-and-  */
/*    Conquer Algorithm for Constructing Delaunay Triangulations,"           */
/*    Algorithmica 2(2):137-151, 1987.                                       */
/*                                                                           */
/*  The incremental insertion algorithm was first proposed by C. L. Lawson,  */
/*    "Software for C1 Surface Interpolation," in Mathematical Software III, */
/*    John R. Rice, editor, Academic Press, New York, pp. 161-194, 1977.     */
/*    For point location, I use the algorithm of Ernst P. Mucke, Isaac       */
/*    Saias, and Binhai Zhu, "Fast Randomized Point Location Without         */
/*    Preprocessing in Two- and Three-dimensional Delaunay Triangulations,"  */
/*    Proceedings of the Twelfth Annual Symposium on Computational Geometry, */
/*    ACM, May 1996.  [*]  If I were to randomize the order of point         */
/*    insertion (I currently don't bother), their result combined with the   */
/*    result of Leonidas J. Guibas, Donald E. Knuth, and Micha Sharir,       */
/*    "Randomized Incremental Construction of Delaunay and Voronoi           */
/*    Diagrams," Algorithmica 7(4):381-413, 1992, would yield an expected    */
/*    O(n^{4/3}) bound on running time.                                      */
/*                                                                           */
/*  The O(n log n) sweepline Delaunay triangulation algorithm is taken from  */
/*    Steven Fortune, "A Sweepline Algorithm for Voronoi Diagrams",          */
/*    Algorithmica 2(2):153-174, 1987.  A random sample of edges on the      */
/*    boundary of the triangulation are maintained in a splay tree for the   */
/*    purpose of point location.  Splay trees are described by Daniel        */
/*    Dominic Sleator and Robert Endre Tarjan, "Self-Adjusting Binary Search */
/*    Trees," Journal of the ACM 32(3):652-686, July 1985.                   */
/*                                                                           */
/*  The algorithms for exact computation of the signs of determinants are    */
/*    described in Jonathan Richard Shewchuk, "Adaptive Precision Floating-  */
/*    Point Arithmetic and Fast Robust Geometric Predicates," Technical      */
/*    Report CMU-CS-96-140, School of Computer Science, Carnegie Mellon      */
/*    University, Pittsburgh, Pennsylvania, May 1996.  [*]  (Submitted to    */
/*    Discrete & Computational Geometry.)  An abbreviated version appears as */
/*    Jonathan Richard Shewchuk, "Robust Adaptive Floating-Point Geometric   */
/*    Predicates," Proceedings of the Twelfth Annual Symposium on Computa-   */
/*    tional Geometry, ACM, May 1996.  [*]  Many of the ideas for my exact   */
/*    arithmetic routines originate with Douglas M. Priest, "Algorithms for  */
/*    Arbitrary Precision Floating Point Arithmetic," Tenth Symposium on     */
/*    Computer Arithmetic, 132-143, IEEE Computer Society Press, 1991.  [*]  */
/*    Many of the ideas for the correct evaluation of the signs of           */
/*    determinants are taken from Steven Fortune and Christopher J. Van Wyk, */
/*    "Efficient Exact Arithmetic for Computational Geometry," Proceedings   */
/*    of the Ninth Annual Symposium on Computational Geometry, ACM,          */
/*    pp. 163-172, May 1993, and from Steven Fortune, "Numerical Stability   */
/*    of Algorithms for 2D Delaunay Triangulations," International Journal   */
/*    of Computational Geometry & Applications 5(1-2):193-213, March-June    */
/*    1995.                                                                  */
/*                                                                           */
/*  For definitions of and results involving Delaunay triangulations,        */
/*    constrained and conforming versions thereof, and other aspects of      */
/*    triangular mesh generation, see the excellent survey by Marshall Bern  */
/*    and David Eppstein, "Mesh Generation and Optimal Triangulation," in    */
/*    Computing and Euclidean Geometry, Ding-Zhu Du and Frank Hwang,         */
/*    editors, World Scientific, Singapore, pp. 23-90, 1992.                 */
/*                                                                           */
/*  The time for incrementally adding PSLG (planar straight line graph)      */
/*    segments to create a constrained Delaunay triangulation is probably    */
/*    O(n^2) per segment in the worst case and O(n) per edge in the common   */
/*    case, where n is the number of triangles that intersect the segment    */
/*    before it is inserted.  This doesn't count point location, which can   */
/*    be much more expensive.  (This note does not apply to conforming       */
/*    Delaunay triangulations, for which a different method is used to       */
/*    insert segments.)                                                      */
/*                                                                           */
/*  The time for adding segments to a conforming Delaunay triangulation is   */
/*    not clear, but does not depend upon n alone.  In some cases, very      */
/*    small features (like a point lying next to a segment) can cause a      */
/*    single segment to be split an arbitrary number of times.  Of course,   */
/*    floating-point precision is a practical barrier to how much this can   */
/*    happen.                                                                */
/*                                                                           */
/*  The time for deleting a point from a Delaunay triangulation is O(n^2) in */
/*    the worst case and O(n) in the common case, where n is the degree of   */
/*    the point being deleted.  I could improve this to expected O(n) time   */
/*    by "inserting" the neighboring vertices in random order, but n is      */
/*    usually quite small, so it's not worth the bother.  (The O(n) time     */
/*    for random insertion follows from L. Paul Chew, "Building Voronoi      */
/*    Diagrams for Convex Polygons in Linear Expected Time," Technical       */
/*    Report PCS-TR90-147, Department of Mathematics and Computer Science,   */
/*    Dartmouth College, 1990.                                               */
/*                                                                           */
/*  Ruppert's Delaunay refinement algorithm typically generates triangles    */
/*    at a linear rate (constant time per triangle) after the initial        */
/*    triangulation is formed.  There may be pathological cases where more   */
/*    time is required, but these never arise in practice.                   */
/*                                                                           */
/*  The segment intersection formulae are straightforward.  If you want to   */
/*    see them derived, see Franklin Antonio.  "Faster Line Segment          */
/*    Intersection."  In Graphics Gems III (David Kirk, editor), pp. 199-    */
/*    202.  Academic Press, Boston, 1992.                                    */
/*                                                                           */
/*  If you make any improvements to this code, please please please let me   */
/*    know, so that I may obtain the improvements.  Even if you don't change */
/*    the code, I'd still love to hear what it's being used for.             */
/*                                                                           */
/*  Disclaimer:  Neither I nor Carnegie Mellon warrant this code in any way  */
/*    whatsoever.  This code is provided "as-is".  Use at your own risk.     */
/*                                                                           */
/*  Modifications: Ian Scott 10 Jan 2003 - attempt to quash 64 bit           */
/*                 conversion warnings. Replaced most (unsigned long) with   */
/*                 (ptr_sized_int).                                          */
/*                 Amitha Perera 13 Jan 2003 - replace ptr_sized_int with    */
/*                 intptr_t                                                  */
/*                                                                           */
/*****************************************************************************/

/* For single precision (which will save some memory and reduce paging),     */
/*   define the symbol SINGLE by using the -DSINGLE compiler switch or by    */
/*   writing "#define SINGLE" below.                                         */
/*                                                                           */
/* For double precision (which will allow you to refine meshes to a smaller  */
/*   edge length), leave SINGLE undefined.                                   */
/*                                                                           */
/* Double precision uses more memory, but improves the resolution of the     */
/*   meshes you can generate with Triangle.  It also reduces the likelihood  */
/*   of a floating exception due to overflow.  Finally, it is much faster    */
/*   than single precision on 64-bit architectures like the DEC Alpha.  I    */
/*   recommend double precision unless you want to generate a mesh for which */
/*   you do not have enough memory.                                          */

/* #define SINGLE */

/* If yours is not a Unix system, define the NO_TIMER compiler switch to     */
/*   remove the Unix-specific timing code.                                   */

#define NO_TIMER

/* To insert lots of self-checks for internal errors, define the SELF_CHECK  */
/*   symbol.  This will slow down the program significantly.  It is best to  */
/*   define the symbol using the -DSELF_CHECK compiler switch, but you could */
/*   write "#define SELF_CHECK" below.  If you are modifying this code, I    */
/*   recommend you turn self-checks on.                                      */

/* #define SELF_CHECK */

/* To compile Triangle as a callable object library (triangle.o), define the */
/*   TRILIBRARY symbol.  Read the file triangle.h for details on how to call */
/*   the procedure triangulate() that results.                               */

#define TRILIBRARY

/* It is possible to generate a smaller version of Triangle using one or     */
/*   both of the following symbols.  Define the REDUCED symbol to eliminate  */
/*   all features that are primarily of research interest; specifically, the */
/*   -i, -F, -s, and -C switches.  Define the CDT_ONLY symbol to eliminate   */
/*   all meshing algorithms above and beyond constrained Delaunay            */
/*   triangulation; specifically, the -r, -q, -a, -S, and -s switches.       */
/*   These reductions are most likely to be useful when generating an object */
/*   library (triangle.o) by defining the TRILIBRARY symbol.                 */

/* #define REDUCED */
/* #define CDT_ONLY */

/* On some machines, the exact arithmetic routines might be defeated by the  */
/*   use of internal extended precision floating-point registers.  Sometimes */
/*   this problem can be fixed by defining certain values to be volatile,    */
/*   thus forcing them to be stored to memory and rounded off.  This isn't   */
/*   a great solution, though, as it slows Triangle down.                    */
/*                                                                           */
/* To try this out, write "#define INEXACT volatile" below.  Normally,       */
/*   however, INEXACT should be defined to be nothing.  ("#define INEXACT".) */

#define INEXACT /* Nothing */
/* #define INEXACT volatile */

/* Maximum number of characters in a file name (including the null).         */

#define FILENAMESIZE 1024

/* Maximum number of characters in a line read from a file (including the    */
/*   null).                                                                  */

#define INPUTLINESIZE 4096

/* For efficiency, a variety of data structures are allocated in bulk.  The  */
/*   following constants determine how many of each structure is allocated   */
/*   at once.                                                                */

#define TRIPERBLOCK 4092           /* Number of triangles allocated at once. */
#define SHELLEPERBLOCK 508       /* Number of shell edges allocated at once. */
#define POINTPERBLOCK 4092            /* Number of points allocated at once. */
#define VIRUSPERBLOCK 1020   /* Number of virus triangles allocated at once. */
/* Number of encroached segments allocated at once. */
#define BADSEGMENTPERBLOCK 252
/* Number of skinny triangles allocated at once. */
#define BADTRIPERBLOCK 4092
/* Number of splay tree nodes allocated at once. */
#define SPLAYNODEPERBLOCK 508

/* The point marker DEADPOINT is an arbitrary number chosen large enough to  */
/*   (hopefully) not conflict with user boundary markers.  Make sure that it */
/*   is small enough to fit into your machine's integer size.                */

#define DEADPOINT -1073741824

/* The next line is used to outsmart some very stupid compilers.  If your    */
/*   compiler is smarter, feel free to replace the "int" with "void".        */
/*   Not that it matters.                                                    */

#define VOID int

/* Two constants for algorithms based on random sampling.  Both constants    */
/*   have been chosen empirically to optimize their respective algorithms.   */

/* Used for the point location scheme of Mucke, Saias, and Zhu, to decide    */
/*   how large a random sample of triangles to inspect.                      */
#define SAMPLEFACTOR 11
/* Used in Fortune's sweepline Delaunay algorithm to determine what fraction */
/*   of boundary edges should be maintained in the splay tree for point      */
/*   location on the front.                                                  */
#define SAMPLERATE 10

/* A number that speaks for itself, every kissable digit.                    */

#define PI 3.141592653589793238462643383279502884197169399375105820974944592308

/* Another fave.                                                             */

#define SQUAREROOTTWO 1.4142135623730950488016887242096980785696718753769480732

/* And here's one for those of you who are intimidated by math.              */

#define ONETHIRD 0.333333333333333333333333333333333333333333333333333333333333

#include <stdio.h>
#include <string.h>

/* Borland compiler provides a "poly" function in math.h.  This
   conflicts with the variable name used below.  Move the symbol out
   of the way.  */
#ifdef __BORLANDC__
# define poly borland_poly
# include <math.h>
# undef poly
#else
# include <math.h>
#endif

#ifndef NO_TIMER
#include <sys/time.h>
#endif /* NO_TIMER */
#ifdef TRILIBRARY
#include "triangle.h"
#endif /* TRILIBRARY */

/* The following obscenity seems to be necessary to ensure that this program */
/* will port to Dec Alphas running OSF/1, because their stdio.h file commits */
/* the unpardonable sin of including stdlib.h.  Hence, malloc(), free(), and */
/* exit() may or may not already be defined at this point.  I declare these  */
/* functions explicitly because some non-ANSI C compilers lack stdlib.h.     */

#ifndef _MSC_VER
#ifndef _STDLIB_H_
extern void *malloc();
extern void free();
extern void exit();
extern double strtod();
extern long strtol();
#endif /* _STDLIB_H_ */
#else
# include <stdlib.h>
#endif

/* A few forward declarations.                                               */

void poolrestart();
#ifndef TRILIBRARY
char *readline();
char *findfield();
#endif /* not TRILIBRARY */

/* Labels that signify whether a record consists primarily of pointers or of */
/*   floating-point words.  Used to make decisions about data alignment.     */

enum wordtype {POINTER, FLOATINGPOINT};

/* Labels that signify the result of point location.  The result of a        */
/*   search indicates that the point falls in the interior of a triangle, on */
/*   an edge, on a vertex, or outside the mesh.                              */

enum locateresult {INTRIANGLE, ONEDGE, ONVERTEX, OUTSIDE};

/* Labels that signify the result of site insertion.  The result indicates   */
/*   that the point was inserted with complete success, was inserted but     */
/*   encroaches on a segment, was not inserted because it lies on a segment, */
/*   or was not inserted because another point occupies the same location.   */

enum insertsiteresult {SUCCESSFULPOINT, ENCROACHINGPOINT, VIOLATINGPOINT,
                       DUPLICATEPOINT};

/* Labels that signify the result of direction finding.  The result          */
/*   indicates that a segment connecting the two query points falls within   */
/*   the direction triangle, along the left edge of the direction triangle,  */
/*   or along the right edge of the direction triangle.                      */

enum finddirectionresult {WITHIN, LEFTCOLLINEAR, RIGHTCOLLINEAR};

/* Labels that signify the result of the circumcenter computation routine.   */
/*   The return value indicates which edge of the triangle is shortest.      */

enum circumcenterresult {OPPOSITEORG, OPPOSITEDEST, OPPOSITEAPEX};

/*****************************************************************************/
/*                                                                           */
/*  The basic mesh data structures                                           */
/*                                                                           */
/*  There are three:  points, triangles, and shell edges (abbreviated        */
/*  `shelle').  These three data structures, linked by pointers, comprise    */
/*  the mesh.  A point simply represents a point in space and its properties.*/
/*  A triangle is a triangle.  A shell edge is a special data structure used */
/*  to represent impenetrable segments in the mesh (including the outer      */
/*  boundary, boundaries of holes, and internal boundaries separating two    */
/*  triangulated regions).  Shell edges represent boundaries defined by the  */
/*  user that triangles may not lie across.                                  */
/*                                                                           */
/*  A triangle consists of a list of three vertices, a list of three         */
/*  adjoining triangles, a list of three adjoining shell edges (when shell   */
/*  edges are used), an arbitrary number of optional user-defined floating-  */
/*  point attributes, and an optional area constraint.  The latter is an     */
/*  upper bound on the permissible area of each triangle in a region, used   */
/*  for mesh refinement.                                                     */
/*                                                                           */
/*  For a triangle on a boundary of the mesh, some or all of the neighboring */
/*  triangles may not be present.  For a triangle in the interior of the     */
/*  mesh, often no neighboring shell edges are present.  Such absent         */
/*  triangles and shell edges are never represented by NULL pointers; they   */
/*  are represented by two special records:  `dummytri', the triangle that   */
/*  fills "outer space", and `dummysh', the omnipresent shell edge.          */
/*  `dummytri' and `dummysh' are used for several reasons; for instance,     */
/*  they can be dereferenced and their contents examined without causing the */
/*  memory protection exception that would occur if NULL were dereferenced.  */
/*                                                                           */
/*  However, it is important to understand that a triangle includes other    */
/*  information as well.  The pointers to adjoining vertices, triangles, and */
/*  shell edges are ordered in a way that indicates their geometric relation */
/*  to each other.  Furthermore, each of these pointers contains orientation */
/*  information.  Each pointer to an adjoining triangle indicates which face */
/*  of that triangle is contacted.  Similarly, each pointer to an adjoining  */
/*  shell edge indicates which side of that shell edge is contacted, and how */
/*  the shell edge is oriented relative to the triangle.                     */
/*                                                                           */
/*  Shell edges are found abutting edges of triangles; either sandwiched     */
/*  between two triangles, or resting against one triangle on an exterior    */
/*  boundary or hole boundary.                                               */
/*                                                                           */
/*  A shell edge consists of a list of two vertices, a list of two           */
/*  adjoining shell edges, and a list of two adjoining triangles.  One of    */
/*  the two adjoining triangles may not be present (though there should      */
/*  always be one), and neighboring shell edges might not be present.        */
/*  Shell edges also store a user-defined integer "boundary marker".         */
/*  Typically, this integer is used to indicate what sort of boundary        */
/*  conditions are to be applied at that location in a finite element        */
/*  simulation.                                                              */
/*                                                                           */
/*  Like triangles, shell edges maintain information about the relative      */
/*  orientation of neighboring objects.                                      */
/*                                                                           */
/*  Points are relatively simple.  A point is a list of floating point       */
/*  numbers, starting with the x, and y coordinates, followed by an          */
/*  arbitrary number of optional user-defined floating-point attributes,     */
/*  followed by an integer boundary marker.  During the segment insertion    */
/*  phase, there is also a pointer from each point to a triangle that may    */
/*  contain it.  Each pointer is not always correct, but when one is, it     */
/*  speeds up segment insertion.  These pointers are assigned values once    */
/*  at the beginning of the segment insertion phase, and are not used or     */
/*  updated at any other time.  Edge swapping during segment insertion will  */
/*  render some of them incorrect.  Hence, don't rely upon them for          */
/*  anything.  For the most part, points do not have any information about   */
/*  what triangles or shell edges they are linked to.                        */
/*                                                                           */
/*****************************************************************************/

/*****************************************************************************/
/*                                                                           */
/*  Handles                                                                  */
/*                                                                           */
/*  The oriented triangle (`triedge') and oriented shell edge (`edge') data  */
/*  structures defined below do not themselves store any part of the mesh.   */
/*  The mesh itself is made of `triangle's, `shelle's, and `point's.         */
/*                                                                           */
/*  Oriented triangles and oriented shell edges will usually be referred to  */
/*  as "handles".  A handle is essentially a pointer into the mesh; it       */
/*  allows you to "hold" one particular part of the mesh.  Handles are used  */
/*  to specify the regions in which one is traversing and modifying the mesh.*/
/*  A single `triangle' may be held by many handles, or none at all.  (The   */
/*  latter case is not a memory leak, because the triangle is still          */
/*  connected to other triangles in the mesh.)                               */
/*                                                                           */
/*  A `triedge' is a handle that holds a triangle.  It holds a specific side */
/*  of the triangle.  An `edge' is a handle that holds a shell edge.  It     */
/*  holds either the left or right side of the edge.                         */
/*                                                                           */
/*  Navigation about the mesh is accomplished through a set of mesh          */
/*  manipulation primitives, further below.  Many of these primitives take   */
/*  a handle and produce a new handle that holds the mesh near the first     */
/*  handle.  Other primitives take two handles and glue the corresponding    */
/*  parts of the mesh together.  The exact position of the handles is        */
/*  important.  For instance, when two triangles are glued together by the   */
/*  bond() primitive, they are glued by the sides on which the handles lie.  */
/*                                                                           */
/*  Because points have no information about which triangles they are        */
/*  attached to, I commonly represent a point by use of a handle whose       */
/*  origin is the point.  A single handle can simultaneously represent a     */
/*  triangle, an edge, and a point.                                          */
/*                                                                           */
/*****************************************************************************/

/* The triangle data structure.  Each triangle contains three pointers to    */
/*   adjoining triangles, plus three pointers to vertex points, plus three   */
/*   pointers to shell edges (defined below; these pointers are usually      */
/*   `dummysh').  It may or may not also contain user-defined attributes     */
/*   and/or a floating-point "area constraint".  It may also contain extra   */
/*   pointers for nodes, when the user asks for high-order elements.         */
/*   Because the size and structure of a `triangle' is not decided until     */
/*   runtime, I haven't simply defined the type `triangle' to be a struct.   */

typedef REAL **triangle;            /* Really:  typedef triangle *triangle   */

/* An oriented triangle:  includes a pointer to a triangle and orientation.  */
/*   The orientation denotes an edge of the triangle.  Hence, there are      */
/*   three possible orientations.  By convention, each edge is always        */
/*   directed to point counterclockwise about the corresponding triangle.    */

struct triedge {
  triangle *tri;
  int orient;                                         /* Ranges from 0 to 2. */
};

/* The shell data structure.  Each shell edge contains two pointers to       */
/*   adjoining shell edges, plus two pointers to vertex points, plus two     */
/*   pointers to adjoining triangles, plus one shell marker.                 */

typedef REAL **shelle;                  /* Really:  typedef shelle *shelle   */

/* An oriented shell edge:  includes a pointer to a shell edge and an        */
/*   orientation.  The orientation denotes a side of the edge.  Hence, there */
/*   are two possible orientations.  By convention, the edge is always       */
/*   directed so that the "side" denoted is the right side of the edge.      */

struct edge {
  shelle *sh;
  int shorient;                                       /* Ranges from 0 to 1. */
};

/* The point data structure.  Each point is actually an array of REALs.      */
/*   The number of REALs is unknown until runtime.  An integer boundary      */
/*   marker, and sometimes a pointer to a triangle, is appended after the    */
/*   REALs.                                                                  */

typedef REAL *point;

/* A queue used to store encroached segments.  Each segment's vertices are   */
/*   stored so that one can check whether a segment is still the same.       */

struct badsegment {
  struct edge encsegment;                          /* An encroached segment. */
  point segorg, segdest;                                /* The two vertices. */
  struct badsegment *nextsegment;     /* Pointer to next encroached segment. */
};

/* A queue used to store bad triangles.  The key is the square of the cosine */
/*   of the smallest angle of the triangle.  Each triangle's vertices are    */
/*   stored so that one can check whether a triangle is still the same.      */

struct badface {
  struct triedge badfacetri;                              /* A bad triangle. */
  REAL key;                             /* cos^2 of smallest (apical) angle. */
  point faceorg, facedest, faceapex;                  /* The three vertices. */
  struct badface *nextface;                 /* Pointer to next bad triangle. */
};

/* A node in a heap used to store events for the sweepline Delaunay          */
/*   algorithm.  Nodes do not point directly to their parents or children in */
/*   the heap.  Instead, each node knows its position in the heap, and can   */
/*   look up its parent and children in a separate array.  The `eventptr'    */
/*   points either to a `point' or to a triangle (in encoded format, so that */
/*   an orientation is included).  In the latter case, the origin of the     */
/*   oriented triangle is the apex of a "circle event" of the sweepline      */
/*   algorithm.  To distinguish site events from circle events, all circle   */
/*   events are given an invalid (smaller than `xmin') x-coordinate `xkey'.  */

struct event {
  REAL xkey, ykey;                              /* Coordinates of the event. */
  VOID *eventptr;       /* Can be a point or the location of a circle event. */
  int heapposition;              /* Marks this event's position in the heap. */
};

/* A node in the splay tree.  Each node holds an oriented ghost triangle     */
/*   that represents a boundary edge of the growing triangulation.  When a   */
/*   circle event covers two boundary edges with a triangle, so that they    */
/*   are no longer boundary edges, those edges are not immediately deleted   */
/*   from the tree; rather, they are lazily deleted when they are next       */
/*   encountered.  (Since only a random sample of boundary edges are kept    */
/*   in the tree, lazy deletion is faster.)  `keydest' is used to verify     */
/*   that a triangle is still the same as when it entered the splay tree; if */
/*   it has been rotated (due to a circle event), it no longer represents a  */
/*   boundary edge and should be deleted.                                    */

struct splaynode {
  struct triedge keyedge;                  /* Lprev of an edge on the front. */
  point keydest;            /* Used to verify that splay node is still live. */
  struct splaynode *lchild, *rchild;              /* Children in splay tree. */
};

/* A type used to allocate memory.  firstblock is the first block of items.  */
/*   nowblock is the block from which items are currently being allocated.   */
/*   nextitem points to the next slab of free memory for an item.            */
/*   deaditemstack is the head of a linked list (stack) of deallocated items */
/*   that can be recycled.  unallocateditems is the number of items that     */
/*   remain to be allocated from nowblock.                                   */
/*                                                                           */
/* Traversal is the process of walking through the entire list of items, and */
/*   is separate from allocation.  Note that a traversal will visit items on */
/*   the "deaditemstack" stack as well as live items.  pathblock points to   */
/*   the block currently being traversed.  pathitem points to the next item  */
/*   to be traversed.  pathitemsleft is the number of items that remain to   */
/*   be traversed in pathblock.                                              */
/*                                                                           */
/* itemwordtype is set to POINTER or FLOATINGPOINT, and is used to suggest   */
/*   what sort of word the record is primarily made up of.  alignbytes       */
/*   determines how new records should be aligned in memory.  itembytes and  */
/*   itemwords are the length of a record in bytes (after rounding up) and   */
/*   words.  itemsperblock is the number of items allocated at once in a     */
/*   single block.  items is the number of currently allocated items.        */
/*   maxitems is the maximum number of items that have been allocated at     */
/*   once; it is the current number of items plus the number of records kept */
/*   on deaditemstack.                                                       */

struct memorypool {
  VOID **firstblock, **nowblock;
  VOID *nextitem;
  VOID *deaditemstack;
  VOID **pathblock;
  VOID *pathitem;
  enum wordtype itemwordtype;
  int alignbytes;
  int itembytes, itemwords;
  int itemsperblock;
  long items, maxitems;
  int unallocateditems;
  int pathitemsleft;
};

/* Variables used to allocate memory for triangles, shell edges, points,     */
/*   viri (triangles being eaten), bad (encroached) segments, bad (skinny    */
/*   or too large) triangles, and splay tree nodes.                          */

static struct memorypool triangles;
static struct memorypool shelles;
static struct memorypool points;
static struct memorypool viri;
static struct memorypool badsegments;
static struct memorypool badtriangles;
static struct memorypool splaynodes;

/* Variables that maintain the bad triangle queues.  The tails are pointers  */
/*   to the pointers that have to be filled in to enqueue an item.           */

static struct badface *queuefront[64];
static struct badface **queuetail[64];

static REAL xmin, xmax, ymin, ymax;                       /* x and y bounds. */
static REAL xminextreme; /* Nonexistent x value used as a flag in sweepline. */
static int inpoints;                              /* Number of input points. */
static int inelements;                         /* Number of input triangles. */
static int insegments;                          /* Number of input segments. */
static int holes;                                  /* Number of input holes. */
static int regions;                              /* Number of input regions. */
static long edges;                                /* Number of output edges. */
static int mesh_dim;                           /* Dimension (ought to be 2). */
static int nextras;                       /* Number of attributes per point. */
static int eextras;                    /* Number of attributes per triangle. */
static long hullsize;                     /* Number of edges of convex hull. */
static int triwords;                            /* Total words per triangle. */
static int shwords;                           /* Total words per shell edge. */
static int pointmarkindex;      /* Index to find boundary marker of a point. */
static int point2triindex;  /* Index to find a triangle adjacent to a point. */
static int highorderindex;/* Index to find extra nodes for high-order elements. */
static int elemattribindex;       /* Index to find attributes of a triangle. */
static int areaboundindex;        /* Index to find area bound of a triangle. */
static int checksegments;    /* Are there segments in the triangulation yet? */
static int readnodefile;                      /* Has a .node file been read? */
static long samples;         /* Number of random samples for point location. */
static unsigned long randomseed;              /* Current random number seed. */

static REAL splitter;/* Used to split REAL factors for exact multiplication. */
static REAL epsilon;                      /* Floating-point machine epsilon. */
static REAL resulterrbound;
static REAL ccwerrboundA, ccwerrboundB, ccwerrboundC;
static REAL iccerrboundA, iccerrboundB, iccerrboundC;

static long incirclecount;            /* Number of incircle tests performed. */
static long counterclockcount;/* Number of counterclockwise tests performed. */
static long hyperbolacount; /* Number of right-of-hyperbola tests performed. */
static long circumcentercount;/* Number of circumcenter calculations performed. */
static long circletopcount;  /* Number of circle top calculations performed. */

/* Switches for the triangulator.                                            */
/*   poly: -p switch.  refine: -r switch.                                    */
/*   quality: -q switch.                                                     */
/*     minangle: minimum angle bound, specified after -q switch.             */
/*     goodangle: cosine squared of minangle.                                */
/*   vararea: -a switch without number.                                      */
/*   fixedarea: -a switch with number.                                       */
/*     maxarea: maximum area bound, specified after -a switch.               */
/*   regionattrib: -A switch.  convex: -c switch.                            */
/*   firstnumber: inverse of -z switch.  All items are numbered starting     */
/*     from firstnumber.                                                     */
/*   edgesout: -e switch.  voronoi: -v switch.                               */
/*   neighbors: -n switch.  geomview: -g switch.                             */
/*   nobound: -B switch.  nopolywritten: -P switch.                          */
/*   nonodewritten: -N switch.  noelewritten: -E switch.                     */
/*   noiterationnum: -I switch.  noholes: -O switch.                         */
/*   noexact: -X switch.                                                     */
/*   order: element order, specified after -o switch.                        */
/*   nobisect: count of how often -Y switch is selected.                     */
/*   steiner: maximum number of Steiner points, specified after -S switch.   */
/*     steinerleft: number of Steiner points not yet used.                   */
/*   incremental: -i switch.  sweepline: -F switch.                          */
/*   dwyer: inverse of -l switch.                                            */
/*   splitseg: -s switch.                                                    */
/*   docheck: -C switch.                                                     */
/*   quiet: -Q switch.  verbose: count of how often -V switch is selected.   */
/*   useshelles: -p, -r, -q, or -c switch; determines whether shell edges    */
/*     are used at all.                                                      */
/*                                                                           */
/* Read the instructions to find out the meaning of these switches.          */

static int poly, refine, quality, vararea, fixedarea, regionattrib, convex;
static int firstnumber;
static int edgesout, voronoi, neighbors, geomview;
static int nobound, nopolywritten, nonodewritten, noelewritten, noiterationnum;
static int noholes, noexact;
static int incremental, sweepline, dwyer;
static int splitseg;
static int docheck;
static int quiet, verbose;
static int useshelles;
static int order;
static int nobisect;
static int steiner, steinerleft;
static REAL minangle, goodangle;
static REAL maxarea;

/* Variables for file names.                                                 */

#ifndef TRILIBRARY
char innodefilename[FILENAMESIZE];
char inelefilename[FILENAMESIZE];
char inpolyfilename[FILENAMESIZE];
char areafilename[FILENAMESIZE];
char outnodefilename[FILENAMESIZE];
char outelefilename[FILENAMESIZE];
char outpolyfilename[FILENAMESIZE];
char edgefilename[FILENAMESIZE];
char vnodefilename[FILENAMESIZE];
char vedgefilename[FILENAMESIZE];
char neighborfilename[FILENAMESIZE];
char offfilename[FILENAMESIZE];
#endif /* not TRILIBRARY */

/* Triangular bounding box points.                                           */

static point infpoint1, infpoint2, infpoint3;

/* Pointer to the `triangle' that occupies all of "outer space".             */

static triangle *dummytri;
static triangle *dummytribase;      /* Keep base address so we can free() it later. */

/* Pointer to the omnipresent shell edge.  Referenced by any triangle or     */
/*   shell edge that isn't really connected to a shell edge at that          */
/*   location.                                                               */

static shelle *dummysh;
static shelle *dummyshbase;         /* Keep base address so we can free() it later. */

/* Pointer to a recently visited triangle.  Improves point location if       */
/*   proximate points are inserted sequentially.                             */

static struct triedge recenttri;


/* Deal with point types that are not unsigned long                          */
#ifdef _MSC_VER
# if _MSC_VER <= 1200
   typedef unsigned long intptr_t;
# else
#  include <stddef.h>
#  include <stdlib.h> /* for malloc and friends */
# endif
#else
# if defined(__alpha) /* there is no inttypes.h here */
   typedef unsigned long intptr_t;
# elif defined(__CYGWIN__)
#  include <sys/types.h> /* for intptr_t on Cygwin */
# elif defined(__BORLANDC__)
#  if __BORLANDC__ < 0x0560
    typedef unsigned long intptr_t;
#  else
#   include <stdint.h> /* for intptr_t on Borland 5.6. */
#  endif
# else
#  include <inttypes.h> /* for intptr_t on e.g. SGI, Linux, Solaris */
# endif
#endif

/*****************************************************************************/
/*                                                                           */
/*  Mesh manipulation primitives.  Each triangle contains three pointers to  */
/*  other triangles, with orientations.  Each pointer points not to the      */
/*  first byte of a triangle, but to one of the first three bytes of a       */
/*  triangle.  It is necessary to extract both the triangle itself and the   */
/*  orientation.  To save memory, I keep both pieces of information in one   */
/*  pointer.  To make this possible, I assume that all triangles are aligned */
/*  to four-byte boundaries.  The `decode' routine below decodes a pointer,  */
/*  extracting an orientation (in the range 0 to 2) and a pointer to the     */
/*  beginning of a triangle.  The `encode' routine compresses a pointer to a */
/*  triangle and an orientation into a single pointer.  My assumptions that  */
/*  triangles are four-byte-aligned and that the `unsigned long' type is     */
/*  long enough to hold a pointer are two of the few kludges in this program.*/
/*                                                                           */
/*  Shell edges are manipulated similarly.  A pointer to a shell edge        */
/*  carries both an address and an orientation in the range 0 to 1.          */
/*                                                                           */
/*  The other primitives take an oriented triangle or oriented shell edge,   */
/*  and return an oriented triangle or oriented shell edge or point; or they */
/*  change the connections in the data structure.                            */
/*                                                                           */
/*****************************************************************************/

/********* Mesh manipulation primitives begin here                   *********/
/**                                                                         **/
/**                                                                         **/

/* Fast lookup arrays to speed some of the mesh manipulation primitives.     */

static int plus1mod3[3] = {1, 2, 0};
static int minus1mod3[3] = {2, 0, 1};

/********* Primitives for triangles                                  *********/
/*                                                                           */
/*                                                                           */


/* decode() converts a pointer to an oriented triangle.  The orientation is  */
/*   extracted from the two least significant bits of the pointer.           */

#define decode(ptr, triedge)                                                  \
  (triedge).orient = (int) ((intptr_t) (ptr) & (intptr_t) 3l);      \
  (triedge).tri = (triangle *)                                                \
                  ((intptr_t) (ptr) ^ (intptr_t) (triedge).orient)

/* encode() compresses an oriented triangle into a single pointer.  It       */
/*   relies on the assumption that all triangles are aligned to four-byte    */
/*   boundaries, so the two least significant bits of (triedge).tri are zero.*/

#define encode(triedge)                                                       \
  (triangle) ((intptr_t) (triedge).tri | (intptr_t) (triedge).orient)

/* The following edge manipulation primitives are all described by Guibas    */
/*   and Stolfi.  However, they use an edge-based data structure, whereas I  */
/*   am using a triangle-based data structure.                               */

/* sym() finds the abutting triangle, on the same edge.  Note that the       */
/*   edge direction is necessarily reversed, because triangle/edge handles   */
/*   are always directed counterclockwise around the triangle.               */

#define sym(triedge1, triedge2)                                               \
  ptr = (triedge1).tri[(triedge1).orient];                                    \
  decode(ptr, triedge2);

#define symself(triedge)                                                      \
  ptr = (triedge).tri[(triedge).orient];                                      \
  decode(ptr, triedge);

/* lnext() finds the next edge (counterclockwise) of a triangle.             */

#define lnext(triedge1, triedge2)                                             \
  (triedge2).tri = (triedge1).tri;                                            \
  (triedge2).orient = plus1mod3[(triedge1).orient]

#define lnextself(triedge)                                                    \
  (triedge).orient = plus1mod3[(triedge).orient]

/* lprev() finds the previous edge (clockwise) of a triangle.                */

#define lprev(triedge1, triedge2)                                             \
  (triedge2).tri = (triedge1).tri;                                            \
  (triedge2).orient = minus1mod3[(triedge1).orient]

#define lprevself(triedge)                                                    \
  (triedge).orient = minus1mod3[(triedge).orient]

/* onext() spins counterclockwise around a point; that is, it finds the next */
/*   edge with the same origin in the counterclockwise direction.  This edge */
/*   will be part of a different triangle.                                   */

#define onext(triedge1, triedge2)                                             \
  lprev(triedge1, triedge2);                                                  \
  symself(triedge2);

#define onextself(triedge)                                                    \
  lprevself(triedge);                                                         \
  symself(triedge);

/* oprev() spins clockwise around a point; that is, it finds the next edge   */
/*   with the same origin in the clockwise direction.  This edge will be     */
/*   part of a different triangle.                                           */

#define oprev(triedge1, triedge2)                                             \
  sym(triedge1, triedge2);                                                    \
  lnextself(triedge2);

#define oprevself(triedge)                                                    \
  symself(triedge);                                                           \
  lnextself(triedge);

/* dnext() spins counterclockwise around a point; that is, it finds the next */
/*   edge with the same destination in the counterclockwise direction.  This */
/*   edge will be part of a different triangle.                              */

#define dnext(triedge1, triedge2)                                             \
  sym(triedge1, triedge2);                                                    \
  lprevself(triedge2);

#define dnextself(triedge)                                                    \
  symself(triedge);                                                           \
  lprevself(triedge);

/* dprev() spins clockwise around a point; that is, it finds the next edge   */
/*   with the same destination in the clockwise direction.  This edge will   */
/*   be part of a different triangle.                                        */

#define dprev(triedge1, triedge2)                                             \
  lnext(triedge1, triedge2);                                                  \
  symself(triedge2);

#define dprevself(triedge)                                                    \
  lnextself(triedge);                                                         \
  symself(triedge);

/* rnext() moves one edge counterclockwise about the adjacent triangle.      */
/*   (It's best understood by reading Guibas and Stolfi.  It involves        */
/*   changing triangles twice.)                                              */

#define rnext(triedge1, triedge2)                                             \
  sym(triedge1, triedge2);                                                    \
  lnextself(triedge2);                                                        \
  symself(triedge2);

#define rnextself(triedge)                                                    \
  symself(triedge);                                                           \
  lnextself(triedge);                                                         \
  symself(triedge);

/* rnext() moves one edge clockwise about the adjacent triangle.             */
/*   (It's best understood by reading Guibas and Stolfi.  It involves        */
/*   changing triangles twice.)                                              */

#define rprev(triedge1, triedge2)                                             \
  sym(triedge1, triedge2);                                                    \
  lprevself(triedge2);                                                        \
  symself(triedge2);

#define rprevself(triedge)                                                    \
  symself(triedge);                                                           \
  lprevself(triedge);                                                         \
  symself(triedge);

/* These primitives determine or set the origin, destination, or apex of a   */
/* triangle.                                                                 */

#define org(triedge, pointptr)                                                \
  pointptr = (point) (triedge).tri[plus1mod3[(triedge).orient] + 3]

#define dest(triedge, pointptr)                                               \
  pointptr = (point) (triedge).tri[minus1mod3[(triedge).orient] + 3]

#define apex(triedge, pointptr)                                               \
  pointptr = (point) (triedge).tri[(triedge).orient + 3]

#define setorg(triedge, pointptr)                                             \
  (triedge).tri[plus1mod3[(triedge).orient] + 3] = (triangle) pointptr

#define setdest(triedge, pointptr)                                            \
  (triedge).tri[minus1mod3[(triedge).orient] + 3] = (triangle) pointptr

#define setapex(triedge, pointptr)                                            \
  (triedge).tri[(triedge).orient + 3] = (triangle) pointptr

#define setvertices2null(triedge)                                             \
  (triedge).tri[3] = (triangle) NULL;                                         \
  (triedge).tri[4] = (triangle) NULL;                                         \
  (triedge).tri[5] = (triangle) NULL;

/* Bond two triangles together.                                              */

#define bond(triedge1, triedge2)                                              \
  (triedge1).tri[(triedge1).orient] = encode(triedge2);                       \
  (triedge2).tri[(triedge2).orient] = encode(triedge1)

/* Dissolve a bond (from one side).  Note that the other triangle will still */
/*   think it's connected to this triangle.  Usually, however, the other     */
/*   triangle is being deleted entirely, or bonded to another triangle, so   */
/*   it doesn't matter.                                                      */

#define dissolve(triedge)                                                     \
  (triedge).tri[(triedge).orient] = (triangle) dummytri

/* Copy a triangle/edge handle.                                              */

#define triedgecopy(triedge1, triedge2)                                       \
  (triedge2).tri = (triedge1).tri;                                            \
  (triedge2).orient = (triedge1).orient

/* Test for equality of triangle/edge handles.                               */

#define triedgeequal(triedge1, triedge2)                                      \
  (((triedge1).tri == (triedge2).tri) &&                                      \
   ((triedge1).orient == (triedge2).orient))

/* Primitives to infect or cure a triangle with the virus.  These rely on    */
/*   the assumption that all shell edges are aligned to four-byte boundaries.*/

#define infect(triedge)                                                       \
  (triedge).tri[6] = (triangle)                                               \
                     ((intptr_t) (triedge).tri[6] | (intptr_t) 2l)

#define uninfect(triedge)                                                     \
  (triedge).tri[6] = (triangle)                                               \
                     ((intptr_t) (triedge).tri[6] & ~ (intptr_t) 2l)

/* Test a triangle for viral infection.                                      */

#define infected(triedge)                                                     \
  (((intptr_t) (triedge).tri[6] & (intptr_t) 2l) != 0)

/* Check or set a triangle's attributes.                                     */

#define elemattribute(triedge, attnum)                                        \
  ((REAL *) (triedge).tri)[elemattribindex + (attnum)]

#define setelemattribute(triedge, attnum, value)                              \
  ((REAL *) (triedge).tri)[elemattribindex + (attnum)] = value

/* Check or set a triangle's maximum area bound.                             */

#define areabound(triedge)  ((REAL *) (triedge).tri)[areaboundindex]

#define setareabound(triedge, value)                                          \
  ((REAL *) (triedge).tri)[areaboundindex] = value

/********* Primitives for shell edges                                *********/
/*                                                                           */
/*                                                                           */

/* sdecode() converts a pointer to an oriented shell edge.  The orientation  */
/*   is extracted from the least significant bit of the pointer.  The two    */
/*   least significant bits (one for orientation, one for viral infection)   */
/*   are masked out to produce the real pointer.                             */

#define sdecode(sptr, edge)                                                   \
  (edge).shorient = (int) ((intptr_t) (sptr) & (intptr_t) 1l);      \
  (edge).sh = (shelle *)                                                      \
              ((intptr_t) (sptr) & ~ (intptr_t) 3l)

/* sencode() compresses an oriented shell edge into a single pointer.  It    */
/*   relies on the assumption that all shell edges are aligned to two-byte   */
/*   boundaries, so the least significant bit of (edge).sh is zero.          */

#define sencode(edge)                                                         \
  (shelle) ((intptr_t) (edge).sh | (intptr_t) (edge).shorient)

/* ssym() toggles the orientation of a shell edge.                           */

#define ssym(edge1, edge2)                                                    \
  (edge2).sh = (edge1).sh;                                                    \
  (edge2).shorient = 1 - (edge1).shorient

#define ssymself(edge)                                                        \
  (edge).shorient = 1 - (edge).shorient

/* spivot() finds the other shell edge (from the same segment) that shares   */
/*   the same origin.                                                        */

#define spivot(edge1, edge2)                                                  \
  sptr = (edge1).sh[(edge1).shorient];                                        \
  sdecode(sptr, edge2)

#define spivotself(edge)                                                      \
  sptr = (edge).sh[(edge).shorient];                                          \
  sdecode(sptr, edge)

/* snext() finds the next shell edge (from the same segment) in sequence;    */
/*   one whose origin is the input shell edge's destination.                 */

#define snext(edge1, edge2)                                                   \
  sptr = (edge1).sh[1 - (edge1).shorient];                                    \
  sdecode(sptr, edge2)

#define snextself(edge)                                                       \
  sptr = (edge).sh[1 - (edge).shorient];                                      \
  sdecode(sptr, edge)

/* These primitives determine or set the origin or destination of a shell    */
/*   edge.                                                                   */

#define sorg(edge, pointptr)                                                  \
  pointptr = (point) (edge).sh[2 + (edge).shorient]

#define sdest(edge, pointptr)                                                 \
  pointptr = (point) (edge).sh[3 - (edge).shorient]

#define setsorg(edge, pointptr)                                               \
  (edge).sh[2 + (edge).shorient] = (shelle) pointptr

#define setsdest(edge, pointptr)                                              \
  (edge).sh[3 - (edge).shorient] = (shelle) pointptr

/* These primitives read or set a shell marker.  Shell markers are used to   */
/*   hold user boundary information.                                         */

#define mark(edge)  (* (int *) ((edge).sh + 6))

#define setmark(edge, value)                                                  \
  * (int *) ((edge).sh + 6) = value

/* Bond two shell edges together.                                            */

#define sbond(edge1, edge2)                                                   \
  (edge1).sh[(edge1).shorient] = sencode(edge2);                              \
  (edge2).sh[(edge2).shorient] = sencode(edge1)

/* Dissolve a shell edge bond (from one side).  Note that the other shell    */
/*   edge will still think it's connected to this shell edge.                */

#define sdissolve(edge)                                                       \
  (edge).sh[(edge).shorient] = (shelle) dummysh

/* Copy a shell edge.                                                        */

#define shellecopy(edge1, edge2)                                              \
  (edge2).sh = (edge1).sh;                                                    \
  (edge2).shorient = (edge1).shorient

/* Test for equality of shell edges.                                         */

#define shelleequal(edge1, edge2)                                             \
  (((edge1).sh == (edge2).sh) &&                                              \
   ((edge1).shorient == (edge2).shorient))

/********* Primitives for interacting triangles and shell edges      *********/
/*                                                                           */
/*                                                                           */

/* tspivot() finds a shell edge abutting a triangle.                         */

#define tspivot(triedge, edge)                                                \
  sptr = (shelle) (triedge).tri[6 + (triedge).orient];                        \
  sdecode(sptr, edge)

/* stpivot() finds a triangle abutting a shell edge.  It requires that the   */
/*   variable `ptr' of type `triangle' be defined.                           */

#define stpivot(edge, triedge)                                                \
  ptr = (triangle) (edge).sh[4 + (edge).shorient];                            \
  decode(ptr, triedge)

/* Bond a triangle to a shell edge.                                          */

#define tsbond(triedge, edge)                                                 \
  (triedge).tri[6 + (triedge).orient] = (triangle) sencode(edge);             \
  (edge).sh[4 + (edge).shorient] = (shelle) encode(triedge)

/* Dissolve a bond (from the triangle side).                                 */

#define tsdissolve(triedge)                                                   \
  (triedge).tri[6 + (triedge).orient] = (triangle) dummysh

/* Dissolve a bond (from the shell edge side).                               */

#define stdissolve(edge)                                                      \
  (edge).sh[4 + (edge).shorient] = (shelle) dummytri

/********* Primitives for points                                     *********/
/*                                                                           */
/*                                                                           */

#define pointmark(pt)  ((int *) (pt))[pointmarkindex]

#define setpointmark(pt, value)                                               \
  ((int *) (pt))[pointmarkindex] = value

#define point2tri(pt)  ((triangle *) (pt))[point2triindex]

#define setpoint2tri(pt, value)                                               \
  ((triangle *) (pt))[point2triindex] = value

/**                                                                         **/
/**                                                                         **/
/********* Mesh manipulation primitives end here                     *********/

/********* User interaction routines begin here                      *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  syntax()   Print list of command line switches.                          */
/*                                                                           */
/*****************************************************************************/

#ifndef TRILIBRARY

void syntax()
{
#ifdef CDT_ONLY
#ifdef REDUCED
  printf("triangle [-pAcevngBPNEIOXzo_lQVh] input_file\n");
#else /* not REDUCED */
  printf("triangle [-pAcevngBPNEIOXzo_iFlCQVh] input_file\n");
#endif /* not REDUCED */
#else /* not CDT_ONLY */
#ifdef REDUCED
  printf("triangle [-prq__a__AcevngBPNEIOXzo_YS__lQVh] input_file\n");
#else /* not REDUCED */
  printf("triangle [-prq__a__AcevngBPNEIOXzo_YS__iFlsCQVh] input_file\n");
#endif /* not REDUCED */
#endif /* not CDT_ONLY */

  printf("    -p  Triangulates a Planar Straight Line Graph (.poly file).\n");
#ifndef CDT_ONLY
  printf("    -r  Refines a previously generated mesh.\n");
  printf(
    "    -q  Quality mesh generation.  A minimum angle may be specified.\n");
  printf("    -a  Applies a maximum triangle area constraint.\n");
#endif /* not CDT_ONLY */
  printf(
    "    -A  Applies attributes to identify elements in certain regions.\n");
  printf("    -c  Encloses the convex hull with segments.\n");
  printf("    -e  Generates an edge list.\n");
  printf("    -v  Generates a Voronoi diagram.\n");
  printf("    -n  Generates a list of triangle neighbors.\n");
  printf("    -g  Generates an .off file for Geomview.\n");
  printf("    -B  Suppresses output of boundary information.\n");
  printf("    -P  Suppresses output of .poly file.\n");
  printf("    -N  Suppresses output of .node file.\n");
  printf("    -E  Suppresses output of .ele file.\n");
  printf("    -I  Suppresses mesh iteration numbers.\n");
  printf("    -O  Ignores holes in .poly file.\n");
  printf("    -X  Suppresses use of exact arithmetic.\n");
  printf("    -z  Numbers all items starting from zero (rather than one).\n");
  printf("    -o2 Generates second-order subparametric elements.\n");
#ifndef CDT_ONLY
  printf("    -Y  Suppresses boundary segment splitting.\n");
  printf("    -S  Specifies maximum number of added Steiner points.\n");
#endif /* not CDT_ONLY */
#ifndef REDUCED
  printf("    -i  Uses incremental method, rather than divide-and-conquer.\n");
  printf("    -F  Uses Fortune's sweepline algorithm, rather than d-and-c.\n");
#endif /* not REDUCED */
  printf("    -l  Uses vertical cuts only, rather than alternating cuts.\n");
#ifndef REDUCED
#ifndef CDT_ONLY
  printf(
    "    -s  Force segments into mesh by splitting (instead of using CDT).\n");
#endif /* not CDT_ONLY */
  printf("    -C  Check consistency of final mesh.\n");
#endif /* not REDUCED */
  printf("    -Q  Quiet:  No terminal output except errors.\n");
  printf("    -V  Verbose:  Detailed information on what I'm doing.\n");
  printf("    -h  Help:  Detailed instructions for Triangle.\n");
  exit(0);
}

#endif /* not TRILIBRARY */

/*****************************************************************************/
/*                                                                           */
/*  info()   Print out complete instructions.                                */
/*                                                                           */
/*****************************************************************************/

#ifndef TRILIBRARY

void info()
{
  printf("Triangle\n");
  printf("A Two-Dimensional Quality Mesh Generator and Delaunay Triangulator.\n");
  printf("Version 1.3\n\n");

  printf("Copyright 1996 Jonathan Richard Shewchuk  (bugs/comments to jrs@cs.cmu.edu)\n");
  printf("School of Computer Science / Carnegie Mellon University\n");
  printf("5000 Forbes Avenue / Pittsburgh, Pennsylvania  15213-3891\n");
  printf("Created as part of the Archimedes project (tools for parallel FEM).\n");
  printf("Supported in part by NSF Grant CMS-9318163 and an NSERC 1967 Scholarship.\n");
  printf("There is no warranty whatsoever.  Use at your own risk.\n");
#ifdef SINGLE
  printf("This executable is compiled for single precision arithmetic.\n\n\n");
#else /* not SINGLE */
  printf("This executable is compiled for double precision arithmetic.\n\n\n");
#endif /* not SINGLE */
  printf("Triangle generates exact Delaunay triangulations, constrained Delaunay\n");
  printf("triangulations, and quality conforming Delaunay triangulations.  The latter\n");
  printf("can be generated with no small angles, and are thus suitable for finite\n");
  printf("element analysis.  If no command line switches are specified, your .node\n");
  printf("input file will be read, and the Delaunay triangulation will be returned in\n");
  printf(".node and .ele output files.  The command syntax is:\n\n");
#ifdef CDT_ONLY
#ifdef REDUCED
  printf("triangle [-pAcevngBPNEIOXzo_lQVh] input_file\n\n");
#else /* not REDUCED */
  printf("triangle [-pAcevngBPNEIOXzo_iFlCQVh] input_file\n\n");
#endif /* not REDUCED */
#else /* not CDT_ONLY */
#ifdef REDUCED
  printf("triangle [-prq__a__AcevngBPNEIOXzo_YS__lQVh] input_file\n\n");
#else /* not REDUCED */
  printf("triangle [-prq__a__AcevngBPNEIOXzo_YS__iFlsCQVh] input_file\n\n");
#endif /* not REDUCED */
#endif /* not CDT_ONLY */
  printf("Underscores indicate that numbers may optionally follow certain switches;\n");
  printf("do not leave any space between a switch and its numeric parameter.\n");
  printf("input_file must be a file with extension .node, or extension .poly if the\n");
  printf("-p switch is used.  If -r is used, you must supply .node and .ele files,\n");
  printf("and possibly a .poly file and .area file as well.  The formats of these\n");
  printf("files are described below.\n\n");

  printf("Command Line Switches:\n\n");

  printf("    -p  Reads a Planar Straight Line Graph (.poly file), which can specify\n");
  printf("        points, segments, holes, and regional attributes and area\n");
  printf("        constraints.  Will generate a constrained Delaunay triangulation\n");
  printf("        fitting the input; or, if -s, -q, or -a is used, a conforming\n");
  printf("        Delaunay triangulation.  If -p is not used, Triangle reads a .node\n");
  printf("        file by default.\n");
  printf("    -r  Refines a previously generated mesh.  The mesh is read from a .node\n");
  printf("        file and an .ele file.  If -p is also used, a .poly file is read\n");
  printf("        and used to constrain edges in the mesh.  Further details on\n");
  printf("        refinement are given below.\n");
  printf("    -q  Quality mesh generation by Jim Ruppert's Delaunay refinement\n");
  printf("        algorithm.  Adds points to the mesh to ensure that no angles\n");
  printf("        smaller than 20 degrees occur.  An alternative minimum angle may be\n");
  printf("        specified after the `q'.  If the minimum angle is 20.7 degrees or\n");
  printf("        smaller, the triangulation algorithm is theoretically guaranteed to\n");
  printf("        terminate (assuming infinite precision arithmetic - Triangle may\n");
  printf("        fail to terminate if you run out of precision).  In practice, the\n");
  printf("        algorithm often succeeds for minimum angles up to 33.8 degrees.\n");
  printf("        For highly refined meshes, however, it may be necessary to reduce\n");
  printf("        the minimum angle to well below 20 to avoid problems associated\n");
  printf("        with insufficient floating-point precision.  The specified angle\n");
  printf("        may include a decimal point.\n");
  printf("    -a  Imposes a maximum triangle area.  If a number follows the `a', no\n");
  printf("        triangle will be generated whose area is larger than that number.\n");
  printf("        If no number is specified, an .area file (if -r is used) or .poly\n");
  printf("        file (if -r is not used) specifies a number of maximum area\n");
  printf("        constraints.  An .area file contains a separate area constraint for\n");
  printf("        each triangle, and is useful for refining a finite element mesh\n");
  printf("        based on a posteriori error estimates.  A .poly file can optionally\n");
  printf("        contain an area constraint for each segment-bounded region, thereby\n");
  printf("        enforcing triangle densities in a first triangulation.  You can\n");
  printf("        impose both a fixed area constraint and a varying area constraint\n");
  printf("        by invoking the -a switch twice, once with and once without a\n");
  printf("        number following.  Each area specified may include a decimal point.\n");
  printf("    -A  Assigns an additional attribute to each triangle that identifies\n");
  printf("        what segment-bounded region each triangle belongs to.  Attributes\n");
  printf("        are assigned to regions by the .poly file.  If a region is not\n");
  printf("        explicitly marked by the .poly file, triangles in that region are\n");
  printf("        assigned an attribute of zero.  The -A switch has an effect only\n");
  printf("        when the -p switch is used and the -r switch is not.\n");
  printf("    -c  Creates segments on the convex hull of the triangulation.  If you\n");
  printf("        are triangulating a point set, this switch causes a .poly file to\n");
  printf("        be written, containing all edges in the convex hull.  (By default,\n");
  printf("        a .poly file is written only if a .poly file is read.)  If you are\n");
  printf("        triangulating a PSLG, this switch specifies that the interior of\n");
  printf("        the convex hull of the PSLG should be triangulated.  If you do not\n");
  printf("        use this switch when triangulating a PSLG, it is assumed that you\n");
  printf("        have identified the region to be triangulated by surrounding it\n");
  printf("        with segments of the input PSLG.  Beware:  if you are not careful,\n");
  printf("        this switch can cause the introduction of an extremely thin angle\n");
  printf("        between a PSLG segment and a convex hull segment, which can cause\n");
  printf("        overrefinement or failure if Triangle runs out of precision.  If\n");
  printf("        you are refining a mesh, the -c switch works differently; it\n");
  printf("        generates the set of boundary edges of the mesh, rather than the\n");
  printf("        convex hull.\n");
  printf("    -e  Outputs (to an .edge file) a list of edges of the triangulation.\n");
  printf("    -v  Outputs the Voronoi diagram associated with the triangulation.\n");
  printf("        Does not attempt to detect degeneracies.\n");
  printf("    -n  Outputs (to a .neigh file) a list of triangles neighboring each\n");
  printf("        triangle.\n");
  printf("    -g  Outputs the mesh to an Object File Format (.off) file, suitable for\n");
  printf("        viewing with the Geometry Center's Geomview package.\n");
  printf("    -B  No boundary markers in the output .node, .poly, and .edge output\n");
  printf("        files.  See the detailed discussion of boundary markers below.\n");
  printf("    -P  No output .poly file.  Saves disk space, but you lose the ability\n");
  printf("        to impose segment constraints on later refinements of the mesh.\n");
  printf("    -N  No output .node file.\n");
  printf("    -E  No output .ele file.\n");
  printf("    -I  No iteration numbers.  Suppresses the output of .node and .poly\n");
  printf("        files, so your input files won't be overwritten.  (If your input is\n");
  printf("        a .poly file only, a .node file will be written.)  Cannot be used\n");
  printf("        with the -r switch, because that would overwrite your input .ele\n");
  printf("        file.  Shouldn't be used with the -s, -q, or -a switch if you are\n");
  printf("        using a .node file for input, because no .node file will be\n");
  printf("        written, so there will be no record of any added points.\n");
  printf("    -O  No holes.  Ignores the holes in the .poly file.\n");
  printf("    -X  No exact arithmetic.  Normally, Triangle uses exact floating-point\n");
  printf("        arithmetic for certain tests if it thinks the inexact tests are not\n");
  printf("        accurate enough.  Exact arithmetic ensures the robustness of the\n");
  printf("        triangulation algorithms, despite floating-point roundoff error.\n");
  printf("        Disabling exact arithmetic with the -X switch will cause a small\n");
  printf("        improvement in speed and create the possibility (albeit small) that\n");
  printf("        Triangle will fail to produce a valid mesh.  Not recommended.\n");
  printf("    -z  Numbers all items starting from zero (rather than one).  Note that\n");
  printf("        this switch is normally overrided by the value used to number the\n");
  printf("        first point of the input .node or .poly file.  However, this switch\n");
  printf("        is useful when calling Triangle from another program.\n");
  printf("    -o2 Generates second-order subparametric elements with six nodes each.\n");
  printf("    -Y  No new points on the boundary.  This switch is useful when the mesh\n");
  printf("        boundary must be preserved so that it conforms to some adjacent\n");
  printf("        mesh.  Be forewarned that you will probably sacrifice some of the\n");
  printf("        quality of the mesh; Triangle will try, but the resulting mesh may\n");
  printf("        contain triangles of poor aspect ratio.  Works well if all the\n");
  printf("        boundary points are closely spaced.  Specify this switch twice\n");
  printf("        (`-YY') to prevent all segment splitting, including internal\n");
  printf("        boundaries.\n");
  printf("    -S  Specifies the maximum number of Steiner points (points that are not\n");
  printf("        in the input, but are added to meet the constraints of minimum\n");
  printf("        angle and maximum area).  The default is to allow an unlimited\n");
  printf("        number.  If you specify this switch with no number after it,\n");
  printf("        the limit is set to zero.  Triangle always adds points at segment\n");
  printf("        intersections, even if it needs to use more points than the limit\n");
  printf("        you set.  When Triangle inserts segments by splitting (-s), it\n");
  printf("        always adds enough points to ensure that all the segments appear in\n");
  printf("        the triangulation, again ignoring the limit.  Be forewarned that\n");
  printf("        the -S switch may result in a conforming triangulation that is not\n");
  printf("        truly Delaunay, because Triangle may be forced to stop adding\n");
  printf("        points when the mesh is in a state where a segment is non-Delaunay\n");
  printf("        and needs to be split.  If so, Triangle will print a warning.\n");
  printf("    -i  Uses an incremental rather than divide-and-conquer algorithm to\n");
  printf("        form a Delaunay triangulation.  Try it if the divide-and-conquer\n");
  printf("        algorithm fails.\n");
  printf("    -F  Uses Steven Fortune's sweepline algorithm to form a Delaunay\n");
  printf("        triangulation.  Warning:  does not use exact arithmetic for all\n");
  printf("        calculations.  An exact result is not guaranteed.\n");
  printf("    -l  Uses only vertical cuts in the divide-and-conquer algorithm.  By\n");
  printf("        default, Triangle uses alternating vertical and horizontal cuts,\n");
  printf("        which usually improve the speed except with point sets that are\n");
  printf("        small or short and wide.  This switch is primarily of theoretical\n");
  printf("        interest.\n");
  printf("    -s  Specifies that segments should be forced into the triangulation by\n");
  printf("        recursively splitting them at their midpoints, rather than by\n");
  printf("        generating a constrained Delaunay triangulation.  Segment splitting\n");
  printf("        is true to Ruppert's original algorithm, but can create needlessly\n");
  printf("        small triangles near external small features.\n");
  printf("    -C  Check the consistency of the final mesh.  Uses exact arithmetic for\n");
  printf("        checking, even if the -X switch is used.  Useful if you suspect\n");
  printf("        Triangle is buggy.\n");
  printf("    -Q  Quiet: Suppresses all explanation of what Triangle is doing, unless\n");
  printf("        an error occurs.\n");
  printf("    -V  Verbose: Gives detailed information about what Triangle is doing.\n");
  printf("        Add more `V's for increasing amount of detail.  `-V' gives\n");
  printf("        information on algorithmic progress and more detailed statistics.\n");
  printf("        `-VV' gives point-by-point details, and will print so much that\n");
  printf("        Triangle will run much more slowly.  `-VVV' gives information only\n");
  printf("        a debugger could love.\n");
  printf("    -h  Help:  Displays these instructions.\n\n");

  printf("Definitions:\n\n");

  printf("  A Delaunay triangulation of a point set is a triangulation whose vertices\n");
  printf("  are the point set, having the property that no point in the point set\n");
  printf("  falls in the interior of the circumcircle (circle that passes through all\n");
  printf("  three vertices) of any triangle in the triangulation.\n\n");

  printf("  A Voronoi diagram of a point set is a subdivision of the plane into\n");
  printf("  polygonal regions (some of which may be infinite), where each region is\n");
  printf("  the set of points in the plane that are closer to some input point than\n");
  printf("  to any other input point.  (The Voronoi diagram is the geometric dual of\n");
  printf("  the Delaunay triangulation.)\n\n");

  printf("  A Planar Straight Line Graph (PSLG) is a collection of points and\n");
  printf("  segments.  Segments are simply edges, whose endpoints are points in the\n");
  printf("  PSLG.  The file format for PSLGs (.poly files) is described below.\n\n");

  printf("  A constrained Delaunay triangulation of a PSLG is similar to a Delaunay\n");
  printf("  triangulation, but each PSLG segment is present as a single edge in the\n");
  printf("  triangulation.  (A constrained Delaunay triangulation is not truly a\n");
  printf("  Delaunay triangulation.)\n\n");

  printf("  A conforming Delaunay triangulation of a PSLG is a true Delaunay\n");
  printf("  triangulation in which each PSLG segment may have been subdivided into\n");
  printf("  several edges by the insertion of additional points.  These inserted\n");
  printf("  points are necessary to allow the segments to exist in the mesh while\n");
  printf("  maintaining the Delaunay property.\n\n");

  printf("File Formats:\n\n");

  printf("  All files may contain comments prefixed by the character '#'.  Points,\n");
  printf("  triangles, edges, holes, and maximum area constraints must be numbered\n");
  printf("  consecutively, starting from either 1 or 0.  Whichever you choose, all\n");
  printf("  input files must be consistent; if the nodes are numbered from 1, so must\n");
  printf("  be all other objects.  Triangle automatically detects your choice while\n");
  printf("  reading the .node (or .poly) file.  (When calling Triangle from another\n");
  printf("  program, use the -z switch if you wish to number objects from zero.)\n");
  printf("  Examples of these file formats are given below.\n\n");

  printf("  .node files:\n");
  printf("    First line:  <# of points> <dimension (must be 2)> <# of attributes>\n");
  printf("                                           <# of boundary markers (0 or 1)>\n");
  printf("    Remaining lines:  <point #> <x> <y> [attributes] [boundary marker]\n\n");

  printf("    The attributes, which are typically floating-point values of physical\n");
  printf("    quantities (such as mass or conductivity) associated with the nodes of\n");
  printf("    a finite element mesh, are copied unchanged to the output mesh.  If -s,\n");
  printf("    -q, or -a is selected, each new Steiner point added to the mesh will\n");
  printf("    have attributes assigned to it by linear interpolation.\n\n");

  printf("    If the fourth entry of the first line is `1', the last column of the\n");
  printf("    remainder of the file is assumed to contain boundary markers.  Boundary\n");
  printf("    markers are used to identify boundary points and points resting on PSLG\n");
  printf("    segments; a complete description appears in a section below.  The .node\n");
  printf("    file produced by Triangle will contain boundary markers in the last\n");
  printf("    column unless they are suppressed by the -B switch.\n\n");

  printf("  .ele files:\n");
  printf("    First line:  <# of triangles> <points per triangle> <# of attributes>\n");
  printf("    Remaining lines:  <triangle #> <point> <point> <point> ... [attributes]\n\n");

  printf("    Points are indices into the corresponding .node file.  The first three\n");
  printf("    points are the corners, and are listed in counterclockwise order around\n");
  printf("    each triangle.  (The remaining points, if any, depend on the type of\n");
  printf("    finite element used.)  The attributes are just like those of .node\n");
  printf("    files.  Because there is no simple mapping from input to output\n");
  printf("    triangles, an attempt is made to interpolate attributes, which may\n");
  printf("    result in a good deal of diffusion of attributes among nearby triangles\n");
  printf("    as the triangulation is refined.  Diffusion does not occur across\n");
  printf("    segments, so attributes used to identify segment-bounded regions remain\n");
  printf("    intact.  In output .ele files, all triangles have three points each\n");
  printf("    unless the -o2 switch is used, in which case they have six, and the\n");
  printf("    fourth, fifth, and sixth points lie on the midpoints of the edges\n");
  printf("    opposite the first, second, and third corners.\n\n");

  printf("  .poly files:\n");
  printf("    First line:  <# of points> <dimension (must be 2)> <# of attributes>\n");
  printf("                                           <# of boundary markers (0 or 1)>\n");
  printf("    Following lines:  <point #> <x> <y> [attributes] [boundary marker]\n");
  printf("    One line:  <# of segments> <# of boundary markers (0 or 1)>\n");
  printf("    Following lines:  <segment #> <endpoint> <endpoint> [boundary marker]\n");
  printf("    One line:  <# of holes>\n");
  printf("    Following lines:  <hole #> <x> <y>\n");
  printf("    Optional line:  <# of regional attributes and/or area constraints>\n");
  printf("    Optional following lines:  <constraint #> <x> <y> <attrib> <max area>\n\n");

  printf("    A .poly file represents a PSLG, as well as some additional information.\n");
  printf("    The first section lists all the points, and is identical to the format\n");
  printf("    of .node files.  <# of points> may be set to zero to indicate that the\n");
  printf("    points are listed in a separate .node file; .poly files produced by\n");
  printf("    Triangle always have this format.  This has the advantage that a point\n");
  printf("    set may easily be triangulated with or without segments.  (The same\n");
  printf("    effect can be achieved, albeit using more disk space, by making a copy\n");
  printf("    of the .poly file with the extension .node; all sections of the file\n");
  printf("    but the first are ignored.)\n\n");

  printf("    The second section lists the segments.  Segments are edges whose\n");
  printf("    presence in the triangulation is enforced.  Each segment is specified\n");
  printf("    by listing the indices of its two endpoints.  This means that you must\n");
  printf("    include its endpoints in the point list.  If -s, -q, and -a are not\n");
  printf("    selected, Triangle will produce a constrained Delaunay triangulation,\n");
  printf("    in which each segment appears as a single edge in the triangulation.\n");
  printf("    If -q or -a is selected, Triangle will produce a conforming Delaunay\n");
  printf("    triangulation, in which segments may be subdivided into smaller edges.\n");
  printf("    Each segment, like each point, may have a boundary marker.\n\n");

  printf("    The third section lists holes (and concavities, if -c is selected) in\n");
  printf("    the triangulation.  Holes are specified by identifying a point inside\n");
  printf("    each hole.  After the triangulation is formed, Triangle creates holes\n");
  printf("    by eating triangles, spreading out from each hole point until its\n");
  printf("    progress is blocked by PSLG segments; you must be careful to enclose\n");
  printf("    each hole in segments, or your whole triangulation may be eaten away.\n");
  printf("    If the two triangles abutting a segment are eaten, the segment itself\n");
  printf("    is also eaten.  Do not place a hole directly on a segment; if you do,\n");
  printf("    Triangle will choose one side of the segment arbitrarily.\n\n");

  printf("    The optional fourth section lists regional attributes (to be assigned\n");
  printf("    to all triangles in a region) and regional constraints on the maximum\n");
  printf("    triangle area.  Triangle will read this section only if the -A switch\n");
  printf("    is used or the -a switch is used without a number following it, and the\n");
  printf("    -r switch is not used.  Regional attributes and area constraints are\n");
  printf("    propagated in the same manner as holes; you specify a point for each\n");
  printf("    attribute and/or constraint, and the attribute and/or constraint will\n");
  printf("    affect the whole region (bounded by segments) containing the point.  If\n");
  printf("    two values are written on a line after the x and y coordinate, the\n");
  printf("    former is assumed to be a regional attribute (but will only be applied\n");
  printf("    if the -A switch is selected), and the latter is assumed to be a\n");
  printf("    regional area constraint (but will only be applied if the -a switch is\n");
  printf("    selected).  You may also specify just one value after the coordinates,\n");
  printf("    which can serve as both an attribute and an area constraint, depending\n");
  printf("    on the choice of switches.  If you are using the -A and -a switches\n");
  printf("    simultaneously and wish to assign an attribute to some region without\n");
  printf("    imposing an area constraint, use a negative maximum area.\n\n");

  printf("    When a triangulation is created from a .poly file, you must either\n");
  printf("    enclose the entire region to be triangulated in PSLG segments, or\n");
  printf("    use the -c switch, which encloses the convex hull of the input point\n");
  printf("    set.  If you do not use the -c switch, Triangle will eat all triangles\n");
  printf("    on the outer boundary that are not protected by segments; if you are\n");
  printf("    not careful, your whole triangulation may be eaten away.  If you do\n");
  printf("    use the -c switch, you can still produce concavities by appropriate\n");
  printf("    placement of holes just inside the convex hull.\n\n");

  printf("    An ideal PSLG has no intersecting segments, nor any points that lie\n");
  printf("    upon segments (except, of course, the endpoints of each segment.)  You\n");
  printf("    aren't required to make your .poly files ideal, but you should be aware\n");
  printf("    of what can go wrong.  Segment intersections are relatively safe -\n");
  printf("    Triangle will calculate the intersection points for you and add them to\n");
  printf("    the triangulation - as long as your machine's floating-point precision\n");
  printf("    doesn't become a problem.  You are tempting the fates if you have three\n");
  printf("    segments that cross at the same location, and expect Triangle to figure\n");
  printf("    out where the intersection point is.  Thanks to floating-point roundoff\n");
  printf("    error, Triangle will probably decide that the three segments intersect\n");
  printf("    at three different points, and you will find a minuscule triangle in\n");
  printf("    your output - unless Triangle tries to refine the tiny triangle, uses\n");
  printf("    up the last bit of machine precision, and fails to terminate at all.\n");
  printf("    You're better off putting the intersection point in the input files,\n");
  printf("    and manually breaking up each segment into two.  Similarly, if you\n");
  printf("    place a point at the middle of a segment, and hope that Triangle will\n");
  printf("    break up the segment at that point, you might get lucky.  On the other\n");
  printf("    hand, Triangle might decide that the point doesn't lie precisely on the\n");
  printf("    line, and you'll have a needle-sharp triangle in your output - or a lot\n");
  printf("    of tiny triangles if you're generating a quality mesh.\n\n");

  printf("    When Triangle reads a .poly file, it also writes a .poly file, which\n");
  printf("    includes all edges that are part of input segments.  If the -c switch\n");
  printf("    is used, the output .poly file will also include all of the edges on\n");
  printf("    the convex hull.  Hence, the output .poly file is useful for finding\n");
  printf("    edges associated with input segments and setting boundary conditions in\n");
  printf("    finite element simulations.  More importantly, you will need it if you\n");
  printf("    plan to refine the output mesh, and don't want segments to be missing\n");
  printf("    in later triangulations.\n\n");

  printf("  .area files:\n");
  printf("    First line:  <# of triangles>\n");
  printf("    Following lines:  <triangle #> <maximum area>\n\n");

  printf("    An .area file associates with each triangle a maximum area that is used\n");
  printf("    for mesh refinement.  As with other file formats, every triangle must\n");
  printf("    be represented, and they must be numbered consecutively.  A triangle\n");
  printf("    may be left unconstrained by assigning it a negative maximum area.\n\n");

  printf("  .edge files:\n");
  printf("    First line:  <# of edges> <# of boundary markers (0 or 1)>\n");
  printf("    Following lines:  <edge #> <endpoint> <endpoint> [boundary marker]\n\n");

  printf("    Endpoints are indices into the corresponding .node file.  Triangle can\n");
  printf("    produce .edge files (use the -e switch), but cannot read them.  The\n");
  printf("    optional column of boundary markers is suppressed by the -B switch.\n\n");

  printf("    In Voronoi diagrams, one also finds a special kind of edge that is an\n");
  printf("    infinite ray with only one endpoint.  For these edges, a different\n");
  printf("    format is used:\n\n");

  printf("        <edge #> <endpoint> -1 <direction x> <direction y>\n\n");

  printf("    The `direction' is a floating-point vector that indicates the direction\n");
  printf("    of the infinite ray.\n\n");

  printf("  .neigh files:\n");
  printf("    First line:  <# of triangles> <# of neighbors per triangle (always 3)>\n");
  printf("    Following lines:  <triangle #> <neighbor> <neighbor> <neighbor>\n\n");

  printf("    Neighbors are indices into the corresponding .ele file.  An index of -1\n");
  printf("    indicates a mesh boundary, and therefore no neighbor.  Triangle can\n");
  printf("    produce .neigh files (use the -n switch), but cannot read them.\n\n");

  printf("    The first neighbor of triangle i is opposite the first corner of\n");
  printf("    triangle i, and so on.\n\n");

  printf("Boundary Markers:\n\n");

  printf("  Boundary markers are tags used mainly to identify which output points and\n");
  printf("  edges are associated with which PSLG segment, and to identify which\n");
  printf("  points and edges occur on a boundary of the triangulation.  A common use\n");
  printf("  is to determine where boundary conditions should be applied to a finite\n");
  printf("  element mesh.  You can prevent boundary markers from being written into\n");
  printf("  files produced by Triangle by using the -B switch.\n\n");

  printf("  The boundary marker associated with each segment in an output .poly file\n");
  printf("  or edge in an output .edge file is chosen as follows:\n");
  printf("    - If an output edge is part or all of a PSLG segment with a nonzero\n");
  printf("      boundary marker, then the edge is assigned the same marker.\n");
  printf("    - Otherwise, if the edge occurs on a boundary of the triangulation\n");
  printf("      (including boundaries of holes), then the edge is assigned the marker\n");
  printf("      one (1).\n");
  printf("    - Otherwise, the edge is assigned the marker zero (0).\n");
  printf("  The boundary marker associated with each point in an output .node file is\n");
  printf("  chosen as follows:\n");
  printf("    - If a point is assigned a nonzero boundary marker in the input file,\n");
  printf("      then it is assigned the same marker in the output .node file.\n");
  printf("    - Otherwise, if the point lies on a PSLG segment (including the\n");
  printf("      segment's endpoints) with a nonzero boundary marker, then the point\n");
  printf("      is assigned the same marker.  If the point lies on several such\n");
  printf("      segments, one of the markers is chosen arbitrarily.\n");
  printf("    - Otherwise, if the point occurs on a boundary of the triangulation,\n");
  printf("      then the point is assigned the marker one (1).\n");
  printf("    - Otherwise, the point is assigned the marker zero (0).\n\n");

  printf("  If you want Triangle to determine for you which points and edges are on\n");
  printf("  the boundary, assign them the boundary marker zero (or use no markers at\n");
  printf("  all) in your input files.  Alternatively, you can mark some of them and\n");
  printf("  leave others marked zero, allowing Triangle to label them.\n\n");

  printf("Triangulation Iteration Numbers:\n\n");

  printf("  Because Triangle can read and refine its own triangulations, input\n");
  printf("  and output files have iteration numbers.  For instance, Triangle might\n");
  printf("  read the files mesh.3.node, mesh.3.ele, and mesh.3.poly, refine the\n");
  printf("  triangulation, and output the files mesh.4.node, mesh.4.ele, and\n");
  printf("  mesh.4.poly.  Files with no iteration number are treated as if\n");
  printf("  their iteration number is zero; hence, Triangle might read the file\n");
  printf("  points.node, triangulate it, and produce the files points.1.node and\n");
  printf("  points.1.ele.\n\n");

  printf("  Iteration numbers allow you to create a sequence of successively finer\n");
  printf("  meshes suitable for multigrid methods.  They also allow you to produce a\n");
  printf("  sequence of meshes using error estimate-driven mesh refinement.\n\n");

  printf("  If you're not using refinement or quality meshing, and you don't like\n");
  printf("  iteration numbers, use the -I switch to disable them.  This switch will\n");
  printf("  also disable output of .node and .poly files to prevent your input files\n");
  printf("  from being overwritten.  (If the input is a .poly file that contains its\n");
  printf("  own points, a .node file will be written.)\n\n");

  printf("Examples of How to Use Triangle:\n\n");

  printf("  `triangle dots' will read points from dots.node, and write their Delaunay\n");
  printf("  triangulation to dots.1.node and dots.1.ele.  (dots.1.node will be\n");
  printf("  identical to dots.node.)  `triangle -I dots' writes the triangulation to\n");
  printf("  dots.ele instead.  (No additional .node file is needed, so none is\n");
  printf("  written.)\n\n");

  printf("  `triangle -pe object.1' will read a PSLG from object.1.poly (and possibly\n");
  printf("  object.1.node, if the points are omitted from object.1.poly) and write\n");
  printf("  their constrained Delaunay triangulation to object.2.node and\n");
  printf("  object.2.ele.  The segments will be copied to object.2.poly, and all\n");
  printf("  edges will be written to object.2.edge.\n\n");

  printf("  `triangle -pq31.5a.1 object' will read a PSLG from object.poly (and\n");
  printf("  possibly object.node), generate a mesh whose angles are all greater than\n");
  printf("  31.5 degrees and whose triangles all have area smaller than 0.1, and\n");
  printf("  write the mesh to object.1.node and object.1.ele.  Each segment may have\n");
  printf("  been broken up into multiple edges; the resulting constrained edges are\n");
  printf("  written to object.1.poly.\n\n");

  printf("  Here is a sample file `box.poly' describing a square with a square hole:\n\n");

  printf("    # A box with eight points in 2D, no attributes, one boundary marker.\n");
  printf("    8 2 0 1\n");
  printf("    # Outer box has these vertices:\n");
  printf("     1   0 0   0\n");
  printf("     2   0 3   0\n");
  printf("     3   3 0   0\n");
  printf("     4   3 3   33     # A special marker for this point.\n");
  printf("    # Inner square has these vertices:\n");
  printf("     5   1 1   0\n");
  printf("     6   1 2   0\n");
  printf("     7   2 1   0\n");
  printf("     8   2 2   0\n");
  printf("    # Five segments with boundary markers.\n");
  printf("    5 1\n");
  printf("     1   1 2   5      # Left side of outer box.\n");
  printf("     2   5 7   0      # Segments 2 through 5 enclose the hole.\n");
  printf("     3   7 8   0\n");
  printf("     4   8 6   10\n");
  printf("     5   6 5   0\n");
  printf("    # One hole in the middle of the inner square.\n");
  printf("    1\n");
  printf("     1   1.5 1.5\n\n");

  printf("  Note that some segments are missing from the outer square, so one must\n");
  printf("  use the `-c' switch.  After `triangle -pqc box.poly', here is the output\n");
  printf("  file `box.1.node', with twelve points.  The last four points were added\n");
  printf("  to meet the angle constraint.  Points 1, 2, and 9 have markers from\n");
  printf("  segment 1.  Points 6 and 8 have markers from segment 4.  All the other\n");
  printf("  points but 4 have been marked to indicate that they lie on a boundary.\n\n");

  printf("    12  2  0  1\n");
  printf("       1    0   0      5\n");
  printf("       2    0   3      5\n");
  printf("       3    3   0      1\n");
  printf("       4    3   3     33\n");
  printf("       5    1   1      1\n");
  printf("       6    1   2     10\n");
  printf("       7    2   1      1\n");
  printf("       8    2   2     10\n");
  printf("       9    0   1.5    5\n");
  printf("      10    1.5   0    1\n");
  printf("      11    3   1.5    1\n");
  printf("      12    1.5   3    1\n");
  printf("    # Generated by triangle -pqc box.poly\n\n");

  printf("  Here is the output file `box.1.ele', with twelve triangles.\n\n");

  printf("    12  3  0\n");
  printf("       1     5   6   9\n");
  printf("       2    10   3   7\n");
  printf("       3     6   8  12\n");
  printf("       4     9   1   5\n");
  printf("       5     6   2   9\n");
  printf("       6     7   3  11\n");
  printf("       7    11   4   8\n");
  printf("       8     7   5  10\n");
  printf("       9    12   2   6\n");
  printf("      10     8   7  11\n");
  printf("      11     5   1  10\n");
  printf("      12     8   4  12\n");
  printf("    # Generated by triangle -pqc box.poly\n\n");

  printf("  Here is the output file `box.1.poly'.  Note that segments have been added\n");
  printf("  to represent the convex hull, and some segments have been split by newly\n");
  printf("  added points.  Note also that <# of points> is set to zero to indicate\n");
  printf("  that the points should be read from the .node file.\n\n");

  printf("    0  2  0  1\n");
  printf("    12  1\n");
  printf("       1     1   9     5\n");
  printf("       2     5   7     1\n");
  printf("       3     8   7     1\n");
  printf("       4     6   8    10\n");
  printf("       5     5   6     1\n");
  printf("       6     3  10     1\n");
  printf("       7     4  11     1\n");
  printf("       8     2  12     1\n");
  printf("       9     9   2     5\n");
  printf("      10    10   1     1\n");
  printf("      11    11   3     1\n");
  printf("      12    12   4     1\n");
  printf("    1\n");
  printf("       1   1.5 1.5\n");
  printf("    # Generated by triangle -pqc box.poly\n\n");

  printf("Refinement and Area Constraints:\n\n");

  printf("  The -r switch causes a mesh (.node and .ele files) to be read and\n");
  printf("  refined.  If the -p switch is also used, a .poly file is read and used to\n");
  printf("  specify edges that are constrained and cannot be eliminated (although\n");
  printf("  they can be divided into smaller edges) by the refinement process.\n\n");

  printf("  When you refine a mesh, you generally want to impose tighter quality\n");
  printf("  constraints.  One way to accomplish this is to use -q with a larger\n");
  printf("  angle, or -a followed by a smaller area than you used to generate the\n");
  printf("  mesh you are refining.  Another way to do this is to create an .area\n");
  printf("  file, which specifies a maximum area for each triangle, and use the -a\n");
  printf("  switch (without a number following).  Each triangle's area constraint is\n");
  printf("  applied to that triangle.  Area constraints tend to diffuse as the mesh\n");
  printf("  is refined, so if there are large variations in area constraint between\n");
  printf("  adjacent triangles, you may not get the results you want.\n\n");

  printf("  If you are refining a mesh composed of linear (three-node) elements, the\n");
  printf("  output mesh will contain all the nodes present in the input mesh, in the\n");
  printf("  same order, with new nodes added at the end of the .node file.  However,\n");
  printf("  there is no guarantee that each output element is contained in a single\n");
  printf("  input element.  Often, output elements will overlap two input elements,\n");
  printf("  and input edges are not present in the output mesh.  Hence, a sequence of\n");
  printf("  refined meshes will form a hierarchy of nodes, but not a hierarchy of\n");
  printf("  elements.  If you a refining a mesh of higher-order elements, the\n");
  printf("  hierarchical property applies only to the nodes at the corners of an\n");
  printf("  element; other nodes may not be present in the refined mesh.\n\n");

  printf("  It is important to understand that maximum area constraints in .poly\n");
  printf("  files are handled differently from those in .area files.  A maximum area\n");
  printf("  in a .poly file applies to the whole (segment-bounded) region in which a\n");
  printf("  point falls, whereas a maximum area in an .area file applies to only one\n");
  printf("  triangle.  Area constraints in .poly files are used only when a mesh is\n");
  printf("  first generated, whereas area constraints in .area files are used only to\n");
  printf("  refine an existing mesh, and are typically based on a posteriori error\n");
  printf("  estimates resulting from a finite element simulation on that mesh.\n\n");

  printf("  `triangle -rq25 object.1' will read object.1.node and object.1.ele, then\n");
  printf("  refine the triangulation to enforce a 25 degree minimum angle, and then\n");
  printf("  write the refined triangulation to object.2.node and object.2.ele.\n\n");

  printf("  `triangle -rpaa6.2 z.3' will read z.3.node, z.3.ele, z.3.poly, and\n");
  printf("  z.3.area.  After reconstructing the mesh and its segments, Triangle will\n");
  printf("  refine the mesh so that no triangle has area greater than 6.2, and\n");
  printf("  furthermore the triangles satisfy the maximum area constraints in\n");
  printf("  z.3.area.  The output is written to z.4.node, z.4.ele, and z.4.poly.\n\n");

  printf("  The sequence `triangle -qa1 x', `triangle -rqa.3 x.1', `triangle -rqa.1\n");
  printf("  x.2' creates a sequence of successively finer meshes x.1, x.2, and x.3,\n");
  printf("  suitable for multigrid.\n\n");

  printf("Convex Hulls and Mesh Boundaries:\n\n");

  printf("  If the input is a point set (rather than a PSLG), Triangle produces its\n");
  printf("  convex hull as a by-product in the output .poly file if you use the -c\n");
  printf("  switch.  There are faster algorithms for finding a two-dimensional convex\n");
  printf("  hull than triangulation, of course, but this one comes for free.  If the\n");
  printf("  input is an unconstrained mesh (you are using the -r switch but not the\n");
  printf("  -p switch), Triangle produces a list of its boundary edges (including\n");
  printf("  hole boundaries) as a by-product if you use the -c switch.\n\n");

  printf("Voronoi Diagrams:\n\n");

  printf("  The -v switch produces a Voronoi diagram, in files suffixed .v.node and\n");
  printf("  .v.edge.  For example, `triangle -v points' will read points.node,\n");
  printf("  produce its Delaunay triangulation in points.1.node and points.1.ele,\n");
  printf("  and produce its Voronoi diagram in points.1.v.node and points.1.v.edge.\n");
  printf("  The .v.node file contains a list of all Voronoi vertices, and the .v.edge\n");
  printf("  file contains a list of all Voronoi edges, some of which may be infinite\n");
  printf("  rays.  (The choice of filenames makes it easy to run the set of Voronoi\n");
  printf("  vertices through Triangle, if so desired.)\n\n");

  printf("  This implementation does not use exact arithmetic to compute the Voronoi\n");
  printf("  vertices, and does not check whether neighboring vertices are identical.\n");
  printf("  Be forewarned that if the Delaunay triangulation is degenerate or\n");
  printf("  near-degenerate, the Voronoi diagram may have duplicate points, crossing\n");
  printf("  edges, or infinite rays whose direction vector is zero.  Also, if you\n");
  printf("  generate a constrained (as opposed to conforming) Delaunay triangulation,\n");
  printf("  or if the triangulation has holes, the corresponding Voronoi diagram is\n");
  printf("  likely to have crossing edges and unlikely to make sense.\n\n");

  printf("Mesh Topology:\n\n");

  printf("  You may wish to know which triangles are adjacent to a certain Delaunay\n");
  printf("  edge in an .edge file, which Voronoi regions are adjacent to a certain\n");
  printf("  Voronoi edge in a .v.edge file, or which Voronoi regions are adjacent to\n");
  printf("  each other.  All of this information can be found by cross-referencing\n");
  printf("  output files with the recollection that the Delaunay triangulation and\n");
  printf("  the Voronoi diagrams are planar duals.\n\n");

  printf("  Specifically, edge i of an .edge file is the dual of Voronoi edge i of\n");
  printf("  the corresponding .v.edge file, and is rotated 90 degrees counterclock-\n");
  printf("  wise from the Voronoi edge.  Triangle j of an .ele file is the dual of\n");
  printf("  vertex j of the corresponding .v.node file; and Voronoi region k is the\n");
  printf("  dual of point k of the corresponding .node file.\n\n");

  printf("  Hence, to find the triangles adjacent to a Delaunay edge, look at the\n");
  printf("  vertices of the corresponding Voronoi edge; their dual triangles are on\n");
  printf("  the left and right of the Delaunay edge, respectively.  To find the\n");
  printf("  Voronoi regions adjacent to a Voronoi edge, look at the endpoints of the\n");
  printf("  corresponding Delaunay edge; their dual regions are on the right and left\n");
  printf("  of the Voronoi edge, respectively.  To find which Voronoi regions are\n");
  printf("  adjacent to each other, just read the list of Delaunay edges.\n\n");

  printf("Statistics:\n\n");

  printf("  After generating a mesh, Triangle prints a count of the number of points,\n");
  printf("  triangles, edges, boundary edges, and segments in the output mesh.  If\n");
  printf("  you've forgotten the statistics for an existing mesh, the -rNEP switches\n");
  printf("  (or -rpNEP if you've got a .poly file for the existing mesh) will\n");
  printf("  regenerate these statistics without writing any output.\n\n");

  printf("  The -V switch produces extended statistics, including a rough estimate\n");
  printf("  of memory use and a histogram of triangle aspect ratios and angles in the\n");
  printf("  mesh.\n\n");

  printf("Exact Arithmetic:\n\n");

  printf("  Triangle uses adaptive exact arithmetic to perform what computational\n");
  printf("  geometers call the `orientation' and `incircle' tests.  If the floating-\n");
  printf("  point arithmetic of your machine conforms to the IEEE 754 standard (as\n");
  printf("  most workstations do), and does not use extended precision internal\n");
  printf("  registers, then your output is guaranteed to be an absolutely true\n");
  printf("  Delaunay or conforming Delaunay triangulation, roundoff error\n");
  printf("  notwithstanding.  The word `adaptive' implies that these arithmetic\n");
  printf("  routines compute the result only to the precision necessary to guarantee\n");
  printf("  correctness, so they are usually nearly as fast as their approximate\n");
  printf("  counterparts.  The exact tests can be disabled with the -X switch.  On\n");
  printf("  most inputs, this switch will reduce the computation time by about eight\n");
  printf("  percent - it's not worth the risk.  There are rare difficult inputs\n");
  printf("  (having many collinear and cocircular points), however, for which the\n");
  printf("  difference could be a factor of two.  These are precisely the inputs most\n");
  printf("  likely to cause errors if you use the -X switch.\n\n");

  printf("  Unfortunately, these routines don't solve every numerical problem.  Exact\n");
  printf("  arithmetic is not used to compute the positions of points, because the\n");
  printf("  bit complexity of point coordinates would grow without bound.  Hence,\n");
  printf("  segment intersections aren't computed exactly; in very unusual cases,\n");
  printf("  roundoff error in computing an intersection point might actually lead to\n");
  printf("  an inverted triangle and an invalid triangulation.  (This is one reason\n");
  printf("  to compute your own intersection points in your .poly files.)  Similarly,\n");
  printf("  exact arithmetic is not used to compute the vertices of the Voronoi\n");
  printf("  diagram.\n\n");

  printf("  Underflow and overflow can also cause difficulties; the exact arithmetic\n");
  printf("  routines do not ameliorate out-of-bounds exponents, which can arise\n");
  printf("  during the orientation and incircle tests.  As a rule of thumb, you\n");
  printf("  should ensure that your input values are within a range such that their\n");
  printf("  third powers can be taken without underflow or overflow.  Underflow can\n");
  printf("  silently prevent the tests from being performed exactly, while overflow\n");
  printf("  will typically cause a floating exception.\n\n");

  printf("Calling Triangle from Another Program:\n\n");

  printf("  Read the file triangle.h for details.\n\n");

  printf("Troubleshooting:\n\n");

  printf("  Please read this section before mailing me bugs.\n\n");

  printf("  `My output mesh has no triangles!'\n\n");

  printf("    If you're using a PSLG, you've probably failed to specify a proper set\n");
  printf("    of bounding segments, or forgotten to use the -c switch.  Or you may\n");
  printf("    have placed a hole badly.  To test these possibilities, try again with\n");
  printf("    the -c and -O switches.  Alternatively, all your input points may be\n");
  printf("    collinear, in which case you can hardly expect to triangulate them.\n\n");

  printf("  `Triangle doesn't terminate, or just crashes.'\n\n");

  printf("    Bad things can happen when triangles get so small that the distance\n");
  printf("    between their vertices isn't much larger than the precision of your\n");
  printf("    machine's arithmetic.  If you've compiled Triangle for single-precision\n");
  printf("    arithmetic, you might do better by recompiling it for double-precision.\n");
  printf("    Then again, you might just have to settle for more lenient constraints\n");
  printf("    on the minimum angle and the maximum area than you had planned.\n\n");

  printf("    You can minimize precision problems by ensuring that the origin lies\n");
  printf("    inside your point set, or even inside the densest part of your\n");
  printf("    mesh.  On the other hand, if you're triangulating an object whose x\n");
  printf("    coordinates all fall between 6247133 and 6247134, you're not leaving\n");
  printf("    much floating-point precision for Triangle to work with.\n\n");

  printf("    Precision problems can occur covertly if the input PSLG contains two\n");
  printf("    segments that meet (or intersect) at a very small angle, or if such an\n");
  printf("    angle is introduced by the -c switch, which may occur if a point lies\n");
  printf("    ever-so-slightly inside the convex hull, and is connected by a PSLG\n");
  printf("    segment to a point on the convex hull.  If you don't realize that a\n");
  printf("    small angle is being formed, you might never discover why Triangle is\n");
  printf("    crashing.  To check for this possibility, use the -S switch (with an\n");
  printf("    appropriate limit on the number of Steiner points, found by trial-and-\n");
  printf("    error) to stop Triangle early, and view the output .poly file with\n");
  printf("    Show Me (described below).  Look carefully for small angles between\n");
  printf("    segments; zoom in closely, as such segments might look like a single\n");
  printf("    segment from a distance.\n\n");

  printf("    If some of the input values are too large, Triangle may suffer a\n");
  printf("    floating exception due to overflow when attempting to perform an\n");
  printf("    orientation or incircle test.  (Read the section on exact arithmetic\n");
  printf("    above.)  Again, I recommend compiling Triangle for double (rather\n");
  printf("    than single) precision arithmetic.\n\n");

  printf("  `The numbering of the output points doesn't match the input points.'\n\n");

  printf("    You may have eaten some of your input points with a hole, or by placing\n");
  printf("    them outside the area enclosed by segments.\n\n");

  printf("  `Triangle executes without incident, but when I look at the resulting\n");
  printf("  mesh, it has overlapping triangles or other geometric inconsistencies.'\n\n");

  printf("    If you select the -X switch, Triangle's divide-and-conquer Delaunay\n");
  printf("    triangulation algorithm occasionally makes mistakes due to floating-\n");
  printf("    point roundoff error.  Although these errors are rare, don't use the -X\n");
  printf("    switch.  If you still have problems, please report the bug.\n\n");

  printf("  Strange things can happen if you've taken liberties with your PSLG.  Do\n");
  printf("  you have a point lying in the middle of a segment?  Triangle sometimes\n");
  printf("  copes poorly with that sort of thing.  Do you want to lay out a collinear\n");
  printf("  row of evenly spaced, segment-connected points?  Have you simply defined\n");
  printf("  one long segment connecting the leftmost point to the rightmost point,\n");
  printf("  and a bunch of points lying along it?  This method occasionally works,\n");
  printf("  especially with horizontal and vertical lines, but often it doesn't, and\n");
  printf("  you'll have to connect each adjacent pair of points with a separate\n");
  printf("  segment.  If you don't like it, tough.\n\n");

  printf("  Furthermore, if you have segments that intersect other than at their\n");
  printf("  endpoints, try not to let the intersections fall extremely close to PSLG\n");
  printf("  points or each other.\n\n");

  printf("  If you have problems refining a triangulation not produced by Triangle:\n");
  printf("  Are you sure the triangulation is geometrically valid?  Is it formatted\n");
  printf("  correctly for Triangle?  Are the triangles all listed so the first three\n");
  printf("  points are their corners in counterclockwise order?\n\n");

  printf("Show Me:\n\n");

  printf("  Triangle comes with a separate program named `Show Me', whose primary\n");
  printf("  purpose is to draw meshes on your screen or in PostScript.  Its secondary\n");
  printf("  purpose is to check the validity of your input files, and do so more\n");
  printf("  thoroughly than Triangle does.  Show Me requires that you have the X\n");
  printf("  Windows system.  If you didn't receive Show Me with Triangle, complain to\n");
  printf("  whomever you obtained Triangle from, then send me mail.\n\n");

  printf("Triangle on the Web:\n\n");

  printf("  To see an illustrated, updated version of these instructions, check out\n\n");

  printf("    http://www.cs.cmu.edu/~quake/triangle.html\n\n");

  printf("A Brief Plea:\n\n");

  printf("  If you use Triangle, and especially if you use it to accomplish real\n");
  printf("  work, I would like very much to hear from you.  A short letter or email\n");
  printf("  (to jrs@cs.cmu.edu) describing how you use Triangle will mean a lot to\n");
  printf("  me.  The more people I know are using this program, the more easily I can\n");
  printf("  justify spending time on improvements and on the three-dimensional\n");
  printf("  successor to Triangle, which in turn will benefit you.  Also, I can put\n");
  printf("  you on a list to receive email whenever a new version of Triangle is\n");
  printf("  available.\n\n");

  printf("  If you use a mesh generated by Triangle in a publication, please include\n");
  printf("  an acknowledgment as well.\n\n");

  printf("Research credit:\n\n");

  printf("  Of course, I can take credit for only a fraction of the ideas that made\n");
  printf("  this mesh generator possible.  Triangle owes its existence to the efforts\n");
  printf("  of many fine computational geometers and other researchers, including\n");
  printf("  Marshall Bern, L. Paul Chew, Boris Delaunay, Rex A. Dwyer, David\n");
  printf("  Eppstein, Steven Fortune, Leonidas J. Guibas, Donald E. Knuth, C. L.\n");
  printf("  Lawson, Der-Tsai Lee, Ernst P. Mucke, Douglas M. Priest, Jim Ruppert,\n");
  printf("  Isaac Saias, Bruce J. Schachter, Micha Sharir, Jorge Stolfi, Christopher\n");
  printf("  J. Van Wyk, David F. Watson, and Binhai Zhu.  See the comments at the\n");
  printf("  beginning of the source code for references.\n\n");
  exit(0);
}

#endif /* not TRILIBRARY */

/*****************************************************************************/
/*                                                                           */
/*  internalerror()   Ask the user to send me the defective product.  Exit.  */
/*                                                                           */
/*****************************************************************************/

void internalerror(void)
{
  printf("  Please report this bug to jrs@cs.cmu.edu\n");
  printf("  Include the message above, your input data set, and the exact\n");
  printf("    command line you used to run Triangle.\n");
  exit(1);
}

/*****************************************************************************/
/*                                                                           */
/*  parsecommandline()   Read the command line, identify switches, and set   */
/*                       up options and file names.                          */
/*                                                                           */
/*  The effects of this routine are felt entirely through global variables.  */
/*                                                                           */
/*****************************************************************************/

void parsecommandline(argc, argv)
int argc;
char **argv;
{
#ifdef TRILIBRARY
#define STARTINDEX 0
#else /* not TRILIBRARY */
#define STARTINDEX 1
  int increment;
  int meshnumber;
#endif /* not TRILIBRARY */
  int i, j, k;
  char workstring[FILENAMESIZE];

  poly = refine = quality = vararea = fixedarea = regionattrib = convex = 0;
  firstnumber = 1;
  edgesout = voronoi = neighbors = geomview = 0;
  nobound = nopolywritten = nonodewritten = noelewritten = noiterationnum = 0;
  noholes = noexact = 0;
  incremental = sweepline = 0;
  dwyer = 1;
  splitseg = 0;
  docheck = 0;
  nobisect = 0;
  steiner = -1;
  order = 1;
  minangle = 0.0;
  maxarea = -1.0;
  quiet = verbose = 0;
#ifndef TRILIBRARY
  innodefilename[0] = '\0';
#endif /* not TRILIBRARY */

  for (i = STARTINDEX; i < argc; i++) {
#ifndef TRILIBRARY
    if (argv[i][0] == '-') {
#endif /* not TRILIBRARY */
      for (j = STARTINDEX; argv[i][j] != '\0'; j++) {
        if (argv[i][j] == 'p') {
          poly = 1;
        }
#ifndef CDT_ONLY
        if (argv[i][j] == 'r') {
          refine = 1;
        }
        if (argv[i][j] == 'q') {
          quality = 1;
          if (((argv[i][j + 1] >= '0') && (argv[i][j + 1] <= '9')) ||
              (argv[i][j + 1] == '.')) {
            k = 0;
            while (((argv[i][j + 1] >= '0') && (argv[i][j + 1] <= '9')) ||
                   (argv[i][j + 1] == '.')) {
              j++;
              workstring[k] = argv[i][j];
              k++;
            }
            workstring[k] = '\0';
            minangle = (REAL) strtod(workstring, (char **) NULL);
          } else {
            minangle = 20.0;
          }
        }
        if (argv[i][j] == 'a') {
          quality = 1;
          if (((argv[i][j + 1] >= '0') && (argv[i][j + 1] <= '9')) ||
              (argv[i][j + 1] == '.')) {
            fixedarea = 1;
            k = 0;
            while (((argv[i][j + 1] >= '0') && (argv[i][j + 1] <= '9')) ||
                   (argv[i][j + 1] == '.')) {
              j++;
              workstring[k] = argv[i][j];
              k++;
            }
            workstring[k] = '\0';
            maxarea = (REAL) strtod(workstring, (char **) NULL);
            if (maxarea <= 0.0) {
              printf("Error:  Maximum area must be greater than zero.\n");
              exit(1);
            }
          } else {
            vararea = 1;
          }
        }
#endif /* not CDT_ONLY */
        if (argv[i][j] == 'A') {
          regionattrib = 1;
        }
        if (argv[i][j] == 'c') {
          convex = 1;
        }
        if (argv[i][j] == 'z') {
          firstnumber = 0;
        }
        if (argv[i][j] == 'e') {
          edgesout = 1;
        }
        if (argv[i][j] == 'v') {
          voronoi = 1;
        }
        if (argv[i][j] == 'n') {
          neighbors = 1;
        }
        if (argv[i][j] == 'g') {
          geomview = 1;
        }
        if (argv[i][j] == 'B') {
          nobound = 1;
        }
        if (argv[i][j] == 'P') {
          nopolywritten = 1;
        }
        if (argv[i][j] == 'N') {
          nonodewritten = 1;
        }
        if (argv[i][j] == 'E') {
          noelewritten = 1;
        }
#ifndef TRILIBRARY
        if (argv[i][j] == 'I') {
          noiterationnum = 1;
        }
#endif /* not TRILIBRARY */
        if (argv[i][j] == 'O') {
          noholes = 1;
        }
        if (argv[i][j] == 'X') {
          noexact = 1;
        }
        if (argv[i][j] == 'o') {
          if (argv[i][j + 1] == '2') {
            j++;
            order = 2;
          }
        }
#ifndef CDT_ONLY
        if (argv[i][j] == 'Y') {
          nobisect++;
        }
        if (argv[i][j] == 'S') {
          steiner = 0;
          while ((argv[i][j + 1] >= '0') && (argv[i][j + 1] <= '9')) {
            j++;
            steiner = steiner * 10 + (int) (argv[i][j] - '0');
          }
        }
#endif /* not CDT_ONLY */
#ifndef REDUCED
        if (argv[i][j] == 'i') {
          incremental = 1;
        }
        if (argv[i][j] == 'F') {
          sweepline = 1;
        }
#endif /* not REDUCED */
        if (argv[i][j] == 'l') {
          dwyer = 0;
        }
#ifndef REDUCED
#ifndef CDT_ONLY
        if (argv[i][j] == 's') {
          splitseg = 1;
        }
#endif /* not CDT_ONLY */
        if (argv[i][j] == 'C') {
          docheck = 1;
        }
#endif /* not REDUCED */
        if (argv[i][j] == 'Q') {
          quiet = 1;
        }
        if (argv[i][j] == 'V') {
          verbose++;
        }
#ifndef TRILIBRARY
        if ((argv[i][j] == 'h') || (argv[i][j] == 'H') || (argv[i][j] == '?')) {
          info();
        }
#endif /* not TRILIBRARY */
      }
#ifndef TRILIBRARY
    } else {
      strncpy(innodefilename, argv[i], FILENAMESIZE - 1);
      innodefilename[FILENAMESIZE - 1] = '\0';
    }
#endif /* not TRILIBRARY */
  }
#ifndef TRILIBRARY
  if (innodefilename[0] == '\0') {
    syntax();
  }
  if (!strcmp(&innodefilename[strlen(innodefilename) - 5], ".node")) {
    innodefilename[strlen(innodefilename) - 5] = '\0';
  }
  if (!strcmp(&innodefilename[strlen(innodefilename) - 5], ".poly")) {
    innodefilename[strlen(innodefilename) - 5] = '\0';
    poly = 1;
  }
#ifndef CDT_ONLY
  if (!strcmp(&innodefilename[strlen(innodefilename) - 4], ".ele")) {
    innodefilename[strlen(innodefilename) - 4] = '\0';
    refine = 1;
  }
  if (!strcmp(&innodefilename[strlen(innodefilename) - 5], ".area")) {
    innodefilename[strlen(innodefilename) - 5] = '\0';
    refine = 1;
    quality = 1;
    vararea = 1;
  }
#endif /* not CDT_ONLY */
#endif /* not TRILIBRARY */
  steinerleft = steiner;
  useshelles = poly || refine || quality || convex;
  goodangle = cos(minangle * PI / 180.0);
  goodangle *= goodangle;
  if (refine && noiterationnum) {
    printf(
      "Error:  You cannot use the -I switch when refining a triangulation.\n");
    exit(1);
  }
  /* Be careful not to allocate space for element area constraints that */
  /*   will never be assigned any value (other than the default -1.0).  */
  if (!refine && !poly) {
    vararea = 0;
  }
  /* Be careful not to add an extra attribute to each element unless the */
  /*   input supports it (PSLG in, but not refining a preexisting mesh). */
  if (refine || !poly) {
    regionattrib = 0;
  }

#ifndef TRILIBRARY
  strcpy(inpolyfilename, innodefilename);
  strcpy(inelefilename, innodefilename);
  strcpy(areafilename, innodefilename);
  increment = 0;
  strcpy(workstring, innodefilename);
  j = 1;
  while (workstring[j] != '\0') {
    if ((workstring[j] == '.') && (workstring[j + 1] != '\0')) {
      increment = j + 1;
    }
    j++;
  }
  meshnumber = 0;
  if (increment > 0) {
    j = increment;
    do {
      if ((workstring[j] >= '0') && (workstring[j] <= '9')) {
        meshnumber = meshnumber * 10 + (int) (workstring[j] - '0');
      } else {
        increment = 0;
      }
      j++;
    } while (workstring[j] != '\0');
  }
  if (noiterationnum) {
    strcpy(outnodefilename, innodefilename);
    strcpy(outelefilename, innodefilename);
    strcpy(edgefilename, innodefilename);
    strcpy(vnodefilename, innodefilename);
    strcpy(vedgefilename, innodefilename);
    strcpy(neighborfilename, innodefilename);
    strcpy(offfilename, innodefilename);
    strcat(outnodefilename, ".node");
    strcat(outelefilename, ".ele");
    strcat(edgefilename, ".edge");
    strcat(vnodefilename, ".v.node");
    strcat(vedgefilename, ".v.edge");
    strcat(neighborfilename, ".neigh");
    strcat(offfilename, ".off");
  } else if (increment == 0) {
    strcpy(outnodefilename, innodefilename);
    strcpy(outpolyfilename, innodefilename);
    strcpy(outelefilename, innodefilename);
    strcpy(edgefilename, innodefilename);
    strcpy(vnodefilename, innodefilename);
    strcpy(vedgefilename, innodefilename);
    strcpy(neighborfilename, innodefilename);
    strcpy(offfilename, innodefilename);
    strcat(outnodefilename, ".1.node");
    strcat(outpolyfilename, ".1.poly");
    strcat(outelefilename, ".1.ele");
    strcat(edgefilename, ".1.edge");
    strcat(vnodefilename, ".1.v.node");
    strcat(vedgefilename, ".1.v.edge");
    strcat(neighborfilename, ".1.neigh");
    strcat(offfilename, ".1.off");
  } else {
    workstring[increment] = '%';
    workstring[increment + 1] = 'd';
    workstring[increment + 2] = '\0';
    sprintf(outnodefilename, workstring, meshnumber + 1);
    strcpy(outpolyfilename, outnodefilename);
    strcpy(outelefilename, outnodefilename);
    strcpy(edgefilename, outnodefilename);
    strcpy(vnodefilename, outnodefilename);
    strcpy(vedgefilename, outnodefilename);
    strcpy(neighborfilename, outnodefilename);
    strcpy(offfilename, outnodefilename);
    strcat(outnodefilename, ".node");
    strcat(outpolyfilename, ".poly");
    strcat(outelefilename, ".ele");
    strcat(edgefilename, ".edge");
    strcat(vnodefilename, ".v.node");
    strcat(vedgefilename, ".v.edge");
    strcat(neighborfilename, ".neigh");
    strcat(offfilename, ".off");
  }
  strcat(innodefilename, ".node");
  strcat(inpolyfilename, ".poly");
  strcat(inelefilename, ".ele");
  strcat(areafilename, ".area");
#endif /* not TRILIBRARY */
}

/**                                                                         **/
/**                                                                         **/
/********* User interaction routines begin here                      *********/

/********* Debugging routines begin here                             *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/* print_cast()  Convert a pointer to an unsigned long integer for display   */
/*                                                                           */
/* This is used to avoid compiler warnings for those compilers that check    */
/* and warn about potential 64-bit problems.                                 */
/*****************************************************************************/

#if defined(_MSC_VER) && (_MSC_VER >= 1300)
#  pragma warning( push )
   /* warning C4311: 'type cast' : pointer truncation from 'void *' to 'unsigned long' */
#  pragma warning( disable: 4311 )
#endif

static
unsigned long print_cast( void* p )
{
  return (unsigned long)p;
}

#if defined(_MSC_VER) && (_MSC_VER >= 1300)
#  pragma warning( pop )
#endif

/*****************************************************************************/
/*                                                                           */
/*  printtriangle()   Print out the details of a triangle/edge handle.       */
/*                                                                           */
/*  I originally wrote this procedure to simplify debugging; it can be       */
/*  called directly from the debugger, and presents information about a      */
/*  triangle/edge handle in digestible form.  It's also used when the        */
/*  highest level of verbosity (`-VVV') is specified.                        */
/*                                                                           */
/*****************************************************************************/

void printtriangle(t)
struct triedge *t;
{
  struct triedge printtri;
  struct edge printsh;
  point printpoint;

  printf("triangle x%lx with orientation %d:\n", print_cast( t->tri ),
         t->orient);
  decode(t->tri[0], printtri);
  if (printtri.tri == dummytri) {
    printf("    [0] = Outer space\n");
  } else {
    printf("    [0] = x%lx  %d\n", print_cast( printtri.tri ),
           printtri.orient);
  }
  decode(t->tri[1], printtri);
  if (printtri.tri == dummytri) {
    printf("    [1] = Outer space\n");
  } else {
    printf("    [1] = x%lx  %d\n", print_cast( printtri.tri ),
           printtri.orient);
  }
  decode(t->tri[2], printtri);
  if (printtri.tri == dummytri) {
    printf("    [2] = Outer space\n");
  } else {
    printf("    [2] = x%lx  %d\n", print_cast( printtri.tri ),
           printtri.orient);
  }
  org(*t, printpoint);
  if (printpoint == (point) NULL)
    printf("    Origin[%d] = NULL\n", (t->orient + 1) % 3 + 3);
  else
    printf("    Origin[%d] = x%lx  (%.12g, %.12g)\n",
           (t->orient + 1) % 3 + 3, print_cast( printpoint ),
           printpoint[0], printpoint[1]);
  dest(*t, printpoint);
  if (printpoint == (point) NULL)
    printf("    Dest  [%d] = NULL\n", (t->orient + 2) % 3 + 3);
  else
    printf("    Dest  [%d] = x%lx  (%.12g, %.12g)\n",
           (t->orient + 2) % 3 + 3, print_cast( printpoint ),
           printpoint[0], printpoint[1]);
  apex(*t, printpoint);
  if (printpoint == (point) NULL)
    printf("    Apex  [%d] = NULL\n", t->orient + 3);
  else
    printf("    Apex  [%d] = x%lx  (%.12g, %.12g)\n",
           t->orient + 3, print_cast( printpoint ),
           printpoint[0], printpoint[1]);
  if (useshelles) {
    sdecode(t->tri[6], printsh);
    if (printsh.sh != dummysh) {
      printf("    [6] = x%lx  %d\n", print_cast( printsh.sh ),
             printsh.shorient);
    }
    sdecode(t->tri[7], printsh);
    if (printsh.sh != dummysh) {
      printf("    [7] = x%lx  %d\n", print_cast( printsh.sh ),
             printsh.shorient);
    }
    sdecode(t->tri[8], printsh);
    if (printsh.sh != dummysh) {
      printf("    [8] = x%lx  %d\n", print_cast( printsh.sh ),
             printsh.shorient);
    }
  }
  if (vararea) {
    printf("    Area constraint:  %.4g\n", areabound(*t));
  }
}

/*****************************************************************************/
/*                                                                           */
/*  printshelle()   Print out the details of a shell edge handle.            */
/*                                                                           */
/*  I originally wrote this procedure to simplify debugging; it can be       */
/*  called directly from the debugger, and presents information about a      */
/*  shell edge handle in digestible form.  It's also used when the highest   */
/*  level of verbosity (`-VVV') is specified.                                */
/*                                                                           */
/*****************************************************************************/

void printshelle(s)
struct edge *s;
{
  struct edge printsh;
  struct triedge printtri;
  point printpoint;

  printf("shell edge x%lx with orientation %d and mark %d:\n",
         print_cast( s->sh ), s->shorient, mark(*s));
  sdecode(s->sh[0], printsh);
  if (printsh.sh == dummysh) {
    printf("    [0] = No shell\n");
  } else {
    printf("    [0] = x%lx  %d\n", print_cast( printsh.sh ),
           printsh.shorient);
  }
  sdecode(s->sh[1], printsh);
  if (printsh.sh == dummysh) {
    printf("    [1] = No shell\n");
  } else {
    printf("    [1] = x%lx  %d\n", print_cast( printsh.sh ),
           printsh.shorient);
  }
  sorg(*s, printpoint);
  if (printpoint == (point) NULL)
    printf("    Origin[%d] = NULL\n", 2 + s->shorient);
  else
    printf("    Origin[%d] = x%lx  (%.12g, %.12g)\n",
           2 + s->shorient, print_cast( printpoint ),
           printpoint[0], printpoint[1]);
  sdest(*s, printpoint);
  if (printpoint == (point) NULL)
    printf("    Dest  [%d] = NULL\n", 3 - s->shorient);
  else
    printf("    Dest  [%d] = x%lx  (%.12g, %.12g)\n",
           3 - s->shorient, print_cast( printpoint ),
           printpoint[0], printpoint[1]);
  decode(s->sh[4], printtri);
  if (printtri.tri == dummytri) {
    printf("    [4] = Outer space\n");
  } else {
    printf("    [4] = x%lx  %d\n", print_cast( printtri.tri ),
           printtri.orient);
  }
  decode(s->sh[5], printtri);
  if (printtri.tri == dummytri) {
    printf("    [5] = Outer space\n");
  } else {
    printf("    [5] = x%lx  %d\n", print_cast( printtri.tri ),
           printtri.orient);
  }
}

/**                                                                         **/
/**                                                                         **/
/********* Debugging routines end here                               *********/

/********* Memory management routines begin here                     *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  poolinit()   Initialize a pool of memory for allocation of items.        */
/*                                                                           */
/*  This routine initializes the machinery for allocating items.  A `pool'   */
/*  is created whose records have size at least `bytecount'.  Items will be  */
/*  allocated in `itemcount'-item blocks.  Each item is assumed to be a      */
/*  collection of words, and either pointers or floating-point values are    */
/*  assumed to be the "primary" word type.  (The "primary" word type is used */
/*  to determine alignment of items.)  If `alignment' isn't zero, all items  */
/*  will be `alignment'-byte aligned in memory.  `alignment' must be either  */
/*  a multiple or a factor of the primary word size; powers of two are safe. */
/*  `alignment' is normally used to create a few unused bits at the bottom   */
/*  of each item's pointer, in which information may be stored.              */
/*                                                                           */
/*  Don't change this routine unless you understand it.                      */
/*                                                                           */
/*****************************************************************************/

void poolinit(pool, bytecount, itemcount, wtype, alignment)
struct memorypool *pool;
int bytecount;
int itemcount;
enum wordtype wtype;
int alignment;
{
  int wordsize;

  /* Initialize values in the pool. */
  pool->itemwordtype = wtype;
  wordsize = (pool->itemwordtype == POINTER) ? sizeof(VOID *) : sizeof(REAL);
  /* Find the proper alignment, which must be at least as large as:   */
  /*   - The parameter `alignment'.                                   */
  /*   - The primary word type, to avoid unaligned accesses.          */
  /*   - sizeof(VOID *), so the stack of dead items can be maintained */
  /*       without unaligned accesses.                                */
  if (alignment > wordsize) {
    pool->alignbytes = alignment;
  } else {
    pool->alignbytes = wordsize;
  }
  if (sizeof(VOID *) > pool->alignbytes) {
    pool->alignbytes = sizeof(VOID *);
  }
  pool->itemwords = ((bytecount + pool->alignbytes - 1) / pool->alignbytes)
                  * (pool->alignbytes / wordsize);
  pool->itembytes = pool->itemwords * wordsize;
  pool->itemsperblock = itemcount;

  /* Allocate a block of items.  Space for `itemsperblock' items and one    */
  /*   pointer (to point to the next block) are allocated, as well as space */
  /*   to ensure alignment of the items.                                    */
  pool->firstblock = (VOID **) malloc(pool->itemsperblock * pool->itembytes
                                      + sizeof(VOID *) + pool->alignbytes);
  if (pool->firstblock == (VOID **) NULL) {
    printf("Error:  Out of memory.\n");
    exit(1);
  }
  /* Set the next block pointer to NULL. */
  *(pool->firstblock) = (VOID *) NULL;
  poolrestart(pool);
}

/*****************************************************************************/
/*                                                                           */
/*  poolrestart()   Deallocate all items in a pool.                          */
/*                                                                           */
/*  The pool is returned to its starting state, except that no memory is     */
/*  freed to the operating system.  Rather, the previously allocated blocks  */
/*  are ready to be reused.                                                  */
/*                                                                           */
/*****************************************************************************/

void poolrestart(pool)
struct memorypool *pool;
{
  intptr_t alignptr;

  pool->items = 0;
  pool->maxitems = 0;

  /* Set the currently active block. */
  pool->nowblock = pool->firstblock;
  /* Find the first item in the pool.  Increment by the size of (VOID *). */
  alignptr = (intptr_t) (pool->nowblock + 1);
  /* Align the item on an `alignbytes'-byte boundary. */
  pool->nextitem = (VOID *)
    (alignptr + (intptr_t) pool->alignbytes
     - (alignptr % (intptr_t) pool->alignbytes));
  /* There are lots of unallocated items left in this block. */
  pool->unallocateditems = pool->itemsperblock;
  /* The stack of deallocated items is empty. */
  pool->deaditemstack = (VOID *) NULL;
}

/*****************************************************************************/
/*                                                                           */
/*  pooldeinit()   Free to the operating system all memory taken by a pool.  */
/*                                                                           */
/*****************************************************************************/

void pooldeinit(pool)
struct memorypool *pool;
{
  while (pool->firstblock != (VOID **) NULL) {
    pool->nowblock = (VOID **) *(pool->firstblock);
    free(pool->firstblock);
    pool->firstblock = pool->nowblock;
  }
}

/*****************************************************************************/
/*                                                                           */
/*  poolalloc()   Allocate space for an item.                                */
/*                                                                           */
/*****************************************************************************/

VOID *poolalloc(pool)
struct memorypool *pool;
{
  VOID *newitem;
  VOID **newblock;
  intptr_t alignptr;

  /* First check the linked list of dead items.  If the list is not   */
  /*   empty, allocate an item from the list rather than a fresh one. */
  if (pool->deaditemstack != (VOID *) NULL) {
    newitem = pool->deaditemstack;               /* Take first item in list. */
    pool->deaditemstack = * (VOID **) pool->deaditemstack;
  } else {
    /* Check if there are any free items left in the current block. */
    if (pool->unallocateditems == 0) {
      /* Check if another block must be allocated. */
      if (*(pool->nowblock) == (VOID *) NULL) {
        /* Allocate a new block of items, pointed to by the previous block. */
        newblock = (VOID **) malloc(pool->itemsperblock * pool->itembytes
                                    + sizeof(VOID *) + pool->alignbytes);
        if (newblock == (VOID **) NULL) {
          printf("Error:  Out of memory.\n");
          exit(1);
        }
        *(pool->nowblock) = (VOID *) newblock;
        /* The next block pointer is NULL. */
        *newblock = (VOID *) NULL;
      }
      /* Move to the new block. */
      pool->nowblock = (VOID **) *(pool->nowblock);
      /* Find the first item in the block.    */
      /*   Increment by the size of (VOID *). */
      alignptr = (intptr_t) (pool->nowblock + 1);
      /* Align the item on an `alignbytes'-byte boundary. */
      pool->nextitem = (VOID *)
        (alignptr + (intptr_t) pool->alignbytes
         - (alignptr % (intptr_t) pool->alignbytes));
      /* There are lots of unallocated items left in this block. */
      pool->unallocateditems = pool->itemsperblock;
    }
    /* Allocate a new item. */
    newitem = pool->nextitem;
    /* Advance `nextitem' pointer to next free item in block. */
    if (pool->itemwordtype == POINTER) {
      pool->nextitem = (VOID *) ((VOID **) pool->nextitem + pool->itemwords);
    } else {
      pool->nextitem = (VOID *) ((REAL *) pool->nextitem + pool->itemwords);
    }
    pool->unallocateditems--;
    pool->maxitems++;
  }
  pool->items++;
  return newitem;
}

/*****************************************************************************/
/*                                                                           */
/*  pooldealloc()   Deallocate space for an item.                            */
/*                                                                           */
/*  The deallocated space is stored in a queue for later reuse.              */
/*                                                                           */
/*****************************************************************************/

void pooldealloc(pool, dyingitem)
struct memorypool *pool;
VOID *dyingitem;
{
  /* Push freshly killed item onto stack. */
  *((VOID **) dyingitem) = pool->deaditemstack;
  pool->deaditemstack = dyingitem;
  pool->items--;
}

/*****************************************************************************/
/*                                                                           */
/*  traversalinit()   Prepare to traverse the entire list of items.          */
/*                                                                           */
/*  This routine is used in conjunction with traverse().                     */
/*                                                                           */
/*****************************************************************************/

void traversalinit(pool)
struct memorypool *pool;
{
  intptr_t alignptr;

  /* Begin the traversal in the first block. */
  pool->pathblock = pool->firstblock;
  /* Find the first item in the block.  Increment by the size of (VOID *). */
  alignptr = (intptr_t) (pool->pathblock + 1);
  /* Align with item on an `alignbytes'-byte boundary. */
  pool->pathitem = (VOID *)
    (alignptr + (intptr_t) pool->alignbytes
     - (alignptr % (intptr_t) pool->alignbytes));
  /* Set the number of items left in the current block. */
  pool->pathitemsleft = pool->itemsperblock;
}

/*****************************************************************************/
/*                                                                           */
/*  traverse()   Find the next item in the list.                             */
/*                                                                           */
/*  This routine is used in conjunction with traversalinit().  Be forewarned */
/*  that this routine successively returns all items in the list, including  */
/*  deallocated ones on the deaditemqueue.  It's up to you to figure out     */
/*  which ones are actually dead.  Why?  I don't want to allocate extra      */
/*  space just to demarcate dead items.  It can usually be done more         */
/*  space-efficiently by a routine that knows something about the structure  */
/*  of the item.                                                             */
/*                                                                           */
/*****************************************************************************/

VOID *traverse(pool)
struct memorypool *pool;
{
  VOID *newitem;
  intptr_t alignptr;

  /* Stop upon exhausting the list of items. */
  if (pool->pathitem == pool->nextitem) {
    return (VOID *) NULL;
  }
  /* Check whether any untraversed items remain in the current block. */
  if (pool->pathitemsleft == 0) {
    /* Find the next block. */
    pool->pathblock = (VOID **) *(pool->pathblock);
    /* Find the first item in the block.  Increment by the size of (VOID *). */
    alignptr = (intptr_t) (pool->pathblock + 1);
    /* Align with item on an `alignbytes'-byte boundary. */
    pool->pathitem = (VOID *)
      (alignptr + (intptr_t) pool->alignbytes
       - (alignptr % (intptr_t) pool->alignbytes));
    /* Set the number of items left in the current block. */
    pool->pathitemsleft = pool->itemsperblock;
  }
  newitem = pool->pathitem;
  /* Find the next item in the block. */
  if (pool->itemwordtype == POINTER) {
    pool->pathitem = (VOID *) ((VOID **) pool->pathitem + pool->itemwords);
  } else {
    pool->pathitem = (VOID *) ((REAL *) pool->pathitem + pool->itemwords);
  }
  pool->pathitemsleft--;
  return newitem;
}

/*****************************************************************************/
/*                                                                           */
/*  dummyinit()   Initialize the triangle that fills "outer space" and the   */
/*                omnipresent shell edge.                                    */
/*                                                                           */
/*  The triangle that fills "outer space", called `dummytri', is pointed to  */
/*  by every triangle and shell edge on a boundary (be it outer or inner) of */
/*  the triangulation.  Also, `dummytri' points to one of the triangles on   */
/*  the convex hull (until the holes and concavities are carved), making it  */
/*  possible to find a starting triangle for point location.                 */
/*                                                                           */
/*  The omnipresent shell edge, `dummysh', is pointed to by every triangle   */
/*  or shell edge that doesn't have a full complement of real shell edges    */
/*  to point to.                                                             */
/*                                                                           */
/*****************************************************************************/

void dummyinit(trianglewords, shellewords)
int trianglewords;
int shellewords;
{
  intptr_t alignptr;

  /* `triwords' and `shwords' are used by the mesh manipulation primitives */
  /*   to extract orientations of triangles and shell edges from pointers. */
  triwords = trianglewords;       /* Initialize `triwords' once and for all. */
  shwords = shellewords;           /* Initialize `shwords' once and for all. */

  /* Set up `dummytri', the `triangle' that occupies "outer space". */
  dummytribase = (triangle *) malloc(triwords * sizeof(triangle)
                                     + triangles.alignbytes);
  if (dummytribase == (triangle *) NULL) {
    printf("Error:  Out of memory.\n");
    exit(1);
  }
  /* Align `dummytri' on a `triangles.alignbytes'-byte boundary. */
  alignptr = (intptr_t) dummytribase;
  dummytri = (triangle *)
    (alignptr + (intptr_t) triangles.alignbytes
     - (alignptr % (intptr_t) triangles.alignbytes));
  /* Initialize the three adjoining triangles to be "outer space".  These  */
  /*   will eventually be changed by various bonding operations, but their */
  /*   values don't really matter, as long as they can legally be          */
  /*   dereferenced.                                                       */
  dummytri[0] = (triangle) dummytri;
  dummytri[1] = (triangle) dummytri;
  dummytri[2] = (triangle) dummytri;
  /* Three NULL vertex points. */
  dummytri[3] = (triangle) NULL;
  dummytri[4] = (triangle) NULL;
  dummytri[5] = (triangle) NULL;

  if (useshelles) {
    /* Set up `dummysh', the omnipresent "shell edge" pointed to by any      */
    /*   triangle side or shell edge end that isn't attached to a real shell */
    /*   edge.                                                               */
    dummyshbase = (shelle *) malloc(shwords * sizeof(shelle)
                                    + shelles.alignbytes);
    if (dummyshbase == (shelle *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
    /* Align `dummysh' on a `shelles.alignbytes'-byte boundary. */
    alignptr = (intptr_t) dummyshbase;
    dummysh = (shelle *)
      (alignptr + (intptr_t) shelles.alignbytes
       - (alignptr % (intptr_t) shelles.alignbytes));
    /* Initialize the two adjoining shell edges to be the omnipresent shell */
    /*   edge.  These will eventually be changed by various bonding         */
    /*   operations, but their values don't really matter, as long as they  */
    /*   can legally be dereferenced.                                       */
    dummysh[0] = (shelle) dummysh;
    dummysh[1] = (shelle) dummysh;
    /* Two NULL vertex points. */
    dummysh[2] = (shelle) NULL;
    dummysh[3] = (shelle) NULL;
    /* Initialize the two adjoining triangles to be "outer space". */
    dummysh[4] = (shelle) dummytri;
    dummysh[5] = (shelle) dummytri;
    /* Set the boundary marker to zero. */
    * (int *) (dummysh + 6) = 0;

    /* Initialize the three adjoining shell edges of `dummytri' to be */
    /*   the omnipresent shell edge.                                  */
    dummytri[6] = (triangle) dummysh;
    dummytri[7] = (triangle) dummysh;
    dummytri[8] = (triangle) dummysh;
  }
}

/*****************************************************************************/
/*                                                                           */
/*  initializepointpool()   Calculate the size of the point data structure   */
/*                          and initialize its memory pool.                  */
/*                                                                           */
/*  This routine also computes the `pointmarkindex' and `point2triindex'     */
/*  indices used to find values within each point.                           */
/*                                                                           */
/*****************************************************************************/

void initializepointpool()
{
  int pointsize;

  /* The index within each point at which the boundary marker is found.  */
  /*   Ensure the point marker is aligned to a sizeof(int)-byte address. */
  pointmarkindex = ((mesh_dim + nextras) * sizeof(REAL) + sizeof(int) - 1)
                 / sizeof(int);
  pointsize = (pointmarkindex + 1) * sizeof(int);
  if (poly) {
    /* The index within each point at which a triangle pointer is found.   */
    /*   Ensure the pointer is aligned to a sizeof(triangle)-byte address. */
    point2triindex = (pointsize + sizeof(triangle) - 1) / sizeof(triangle);
    pointsize = (point2triindex + 1) * sizeof(triangle);
  }
  /* Initialize the pool of points. */
  poolinit(&points, pointsize, POINTPERBLOCK,
           (sizeof(REAL) >= sizeof(triangle)) ? FLOATINGPOINT : POINTER, 0);
}

/*****************************************************************************/
/*                                                                           */
/*  initializetrisegpools()   Calculate the sizes of the triangle and shell  */
/*                            edge data structures and initialize their      */
/*                            memory pools.                                  */
/*                                                                           */
/*  This routine also computes the `highorderindex', `elemattribindex', and  */
/*  `areaboundindex' indices used to find values within each triangle.       */
/*                                                                           */
/*****************************************************************************/

void initializetrisegpools()
{
  int trisize;

  /* The index within each triangle at which the extra nodes (above three)  */
  /*   associated with high order elements are found.  There are three      */
  /*   pointers to other triangles, three pointers to corners, and possibly */
  /*   three pointers to shell edges before the extra nodes.                */
  highorderindex = 6 + (useshelles * 3);
  /* The number of bytes occupied by a triangle. */
  trisize = ((order + 1) * (order + 2) / 2 + (highorderindex - 3)) *
            sizeof(triangle);
  /* The index within each triangle at which its attributes are found, */
  /*   where the index is measured in REALs.                           */
  elemattribindex = (trisize + sizeof(REAL) - 1) / sizeof(REAL);
  /* The index within each triangle at which the maximum area constraint  */
  /*   is found, where the index is measured in REALs.  Note that if the  */
  /*   `regionattrib' flag is set, an additional attribute will be added. */
  areaboundindex = elemattribindex + eextras + regionattrib;
  /* If triangle attributes or an area bound are needed, increase the number */
  /*   of bytes occupied by a triangle.                                      */
  if (vararea) {
    trisize = (areaboundindex + 1) * sizeof(REAL);
  } else if (eextras + regionattrib > 0) {
    trisize = areaboundindex * sizeof(REAL);
  }
  /* If a Voronoi diagram or triangle neighbor graph is requested, make    */
  /*   sure there's room to store an integer index in each triangle.  This */
  /*   integer index can occupy the same space as the shell edges or       */
  /*   attributes or area constraint or extra nodes.                       */
  if ((voronoi || neighbors) &&
      (trisize < 6 * sizeof(triangle) + sizeof(int))) {
    trisize = 6 * sizeof(triangle) + sizeof(int);
  }
  /* Having determined the memory size of a triangle, initialize the pool. */
  poolinit(&triangles, trisize, TRIPERBLOCK, POINTER, 4);

  if (useshelles) {
    /* Initialize the pool of shell edges. */
    poolinit(&shelles, 6 * sizeof(triangle) + sizeof(int), SHELLEPERBLOCK,
             POINTER, 4);

    /* Initialize the "outer space" triangle and omnipresent shell edge. */
    dummyinit(triangles.itemwords, shelles.itemwords);
  } else {
    /* Initialize the "outer space" triangle. */
    dummyinit(triangles.itemwords, 0);
  }
}

/*****************************************************************************/
/*                                                                           */
/*  triangledealloc()   Deallocate space for a triangle, marking it dead.    */
/*                                                                           */
/*****************************************************************************/

void triangledealloc(dyingtriangle)
triangle *dyingtriangle;
{
  /* Set triangle's vertices to NULL.  This makes it possible to        */
  /*   detect dead triangles when traversing the list of all triangles. */
  dyingtriangle[3] = (triangle) NULL;
  dyingtriangle[4] = (triangle) NULL;
  dyingtriangle[5] = (triangle) NULL;
  pooldealloc(&triangles, (VOID *) dyingtriangle);
}

/*****************************************************************************/
/*                                                                           */
/*  triangletraverse()   Traverse the triangles, skipping dead ones.         */
/*                                                                           */
/*****************************************************************************/

triangle *triangletraverse()
{
  triangle *newtriangle;

  do {
    newtriangle = (triangle *) traverse(&triangles);
    if (newtriangle == (triangle *) NULL) {
      return (triangle *) NULL;
    }
  } while (newtriangle[3] == (triangle) NULL);            /* Skip dead ones. */
  return newtriangle;
}

/*****************************************************************************/
/*                                                                           */
/*  shelledealloc()   Deallocate space for a shell edge, marking it dead.    */
/*                                                                           */
/*****************************************************************************/

void shelledealloc(dyingshelle)
shelle *dyingshelle;
{
  /* Set shell edge's vertices to NULL.  This makes it possible to */
  /*   detect dead shells when traversing the list of all shells.  */
  dyingshelle[2] = (shelle) NULL;
  dyingshelle[3] = (shelle) NULL;
  pooldealloc(&shelles, (VOID *) dyingshelle);
}

/*****************************************************************************/
/*                                                                           */
/*  shelletraverse()   Traverse the shell edges, skipping dead ones.         */
/*                                                                           */
/*****************************************************************************/

shelle *shelletraverse()
{
  shelle *newshelle;

  do {
    newshelle = (shelle *) traverse(&shelles);
    if (newshelle == (shelle *) NULL) {
      return (shelle *) NULL;
    }
  } while (newshelle[2] == (shelle) NULL);                /* Skip dead ones. */
  return newshelle;
}

/*****************************************************************************/
/*                                                                           */
/*  pointdealloc()   Deallocate space for a point, marking it dead.          */
/*                                                                           */
/*****************************************************************************/

void pointdealloc(dyingpoint)
point dyingpoint;
{
  /* Mark the point as dead.  This makes it possible to detect dead points */
  /*   when traversing the list of all points.                             */
  setpointmark(dyingpoint, DEADPOINT);
  pooldealloc(&points, (VOID *) dyingpoint);
}

/*****************************************************************************/
/*                                                                           */
/*  pointtraverse()   Traverse the points, skipping dead ones.               */
/*                                                                           */
/*****************************************************************************/

point pointtraverse()
{
  point newpoint;

  do {
    newpoint = (point) traverse(&points);
    if (newpoint == (point) NULL) {
      return (point) NULL;
    }
  } while (pointmark(newpoint) == DEADPOINT);             /* Skip dead ones. */
  return newpoint;
}

/*****************************************************************************/
/*                                                                           */
/*  badsegmentdealloc()   Deallocate space for a bad segment, marking it     */
/*                        dead.                                              */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void badsegmentdealloc(dyingseg)
struct edge *dyingseg;
{
  /* Set segment's orientation to -1.  This makes it possible to      */
  /*   detect dead segments when traversing the list of all segments. */
  dyingseg->shorient = -1;
  pooldealloc(&badsegments, (VOID *) dyingseg);
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  badsegmenttraverse()   Traverse the bad segments, skipping dead ones.    */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

struct edge *badsegmenttraverse()
{
  struct edge *newseg;

  do {
    newseg = (struct edge *) traverse(&badsegments);
    if (newseg == (struct edge *) NULL) {
      return (struct edge *) NULL;
    }
  } while (newseg->shorient == -1);                       /* Skip dead ones. */
  return newseg;
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  getpoint()   Get a specific point, by number, from the list.             */
/*                                                                           */
/*  The first point is number 'firstnumber'.                                 */
/*                                                                           */
/*  Note that this takes O(n) time (with a small constant, if POINTPERBLOCK  */
/*  is large).  I don't care to take the trouble to make it work in constant */
/*  time.                                                                    */
/*                                                                           */
/*****************************************************************************/

point getpoint(number)
int number;
{
  VOID **getblock;
  point foundpoint;
  intptr_t alignptr;
  int current;

  getblock = points.firstblock;
  current = firstnumber;
  /* Find the right block. */
  while (current + points.itemsperblock <= number) {
    getblock = (VOID **) *getblock;
    current += points.itemsperblock;
  }
  /* Now find the right point. */
  alignptr = (intptr_t) (getblock + 1);
  foundpoint = (point) (alignptr + (intptr_t) points.alignbytes
                        - (alignptr % (intptr_t) points.alignbytes));
  while (current < number) {
    foundpoint += points.itemwords;
    current++;
  }
  return foundpoint;
}

/*****************************************************************************/
/*                                                                           */
/*  triangledeinit()   Free all remaining allocated memory.                  */
/*                                                                           */
/*****************************************************************************/

void triangledeinit()
{
  pooldeinit(&triangles);
  free(dummytribase);
  if (useshelles) {
    pooldeinit(&shelles);
    free(dummyshbase);
  }
  pooldeinit(&points);
#ifndef CDT_ONLY
  if (quality) {
    pooldeinit(&badsegments);
    if ((minangle > 0.0) || vararea || fixedarea) {
      pooldeinit(&badtriangles);
    }
  }
#endif /* not CDT_ONLY */
}

/**                                                                         **/
/**                                                                         **/
/********* Memory management routines end here                       *********/

/********* Constructors begin here                                   *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  maketriangle()   Create a new triangle with orientation zero.            */
/*                                                                           */
/*****************************************************************************/

void maketriangle(newtriedge)
struct triedge *newtriedge;
{
  int i;

  newtriedge->tri = (triangle *) poolalloc(&triangles);
  /* Initialize the three adjoining triangles to be "outer space". */
  newtriedge->tri[0] = (triangle) dummytri;
  newtriedge->tri[1] = (triangle) dummytri;
  newtriedge->tri[2] = (triangle) dummytri;
  /* Three NULL vertex points. */
  newtriedge->tri[3] = (triangle) NULL;
  newtriedge->tri[4] = (triangle) NULL;
  newtriedge->tri[5] = (triangle) NULL;
  /* Initialize the three adjoining shell edges to be the omnipresent */
  /*   shell edge.                                                    */
  if (useshelles) {
    newtriedge->tri[6] = (triangle) dummysh;
    newtriedge->tri[7] = (triangle) dummysh;
    newtriedge->tri[8] = (triangle) dummysh;
  }
  for (i = 0; i < eextras; i++) {
    setelemattribute(*newtriedge, i, 0.0);
  }
  if (vararea) {
    setareabound(*newtriedge, -1.0);
  }

  newtriedge->orient = 0;
}

/*****************************************************************************/
/*                                                                           */
/*  makeshelle()   Create a new shell edge with orientation zero.            */
/*                                                                           */
/*****************************************************************************/

void makeshelle(newedge)
struct edge *newedge;
{
  newedge->sh = (shelle *) poolalloc(&shelles);
  /* Initialize the two adjoining shell edges to be the omnipresent */
  /*   shell edge.                                                  */
  newedge->sh[0] = (shelle) dummysh;
  newedge->sh[1] = (shelle) dummysh;
  /* Two NULL vertex points. */
  newedge->sh[2] = (shelle) NULL;
  newedge->sh[3] = (shelle) NULL;
  /* Initialize the two adjoining triangles to be "outer space". */
  newedge->sh[4] = (shelle) dummytri;
  newedge->sh[5] = (shelle) dummytri;
  /* Set the boundary marker to zero. */
  setmark(*newedge, 0);

  newedge->shorient = 0;
}

/**                                                                         **/
/**                                                                         **/
/********* Constructors end here                                     *********/

/********* Determinant evaluation routines begin here                *********/
/**                                                                         **/
/**                                                                         **/

/* The adaptive exact arithmetic geometric predicates implemented herein are */
/*   described in detail in my Technical Report CMU-CS-96-140.  The complete */
/*   reference is given in the header.                                       */

/* Which of the following two methods of finding the absolute values is      */
/*   fastest is compiler-dependent.  A few compilers can inline and optimize */
/*   the fabs() call; but most will incur the overhead of a function call,   */
/*   which is disastrously slow.  A faster way on IEEE machines might be to  */
/*   mask the appropriate bit, but that's difficult to do in C.              */

#define Absolute(a)  ((a) >= 0.0 ? (a) : -(a))
/* #define Absolute(a)  fabs(a) */

/* Many of the operations are broken up into two pieces, a main part that    */
/*   performs an approximate operation, and a "tail" that computes the       */
/*   roundoff error of that operation.                                       */
/*                                                                           */
/* The operations Fast_Two_Sum(), Fast_Two_Diff(), Two_Sum(), Two_Diff(),    */
/*   Split(), and Two_Product() are all implemented as described in the      */
/*   reference.  Each of these macros requires certain variables to be       */
/*   defined in the calling routine.  The variables `bvirt', `c', `abig',    */
/*   `_i', `_j', `_k', `_l', `_m', and `_n' are declared `INEXACT' because   */
/*   they store the result of an operation that may incur roundoff error.    */
/*   The input parameter `x' (or the highest numbered `x_' parameter) must   */
/*   also be declared `INEXACT'.                                             */

#define Fast_Two_Sum_Tail(a, b, x, y) \
  bvirt = x - a; \
  y = b - bvirt

#define Fast_Two_Sum(a, b, x, y) \
  x = (REAL) (a + b); \
  Fast_Two_Sum_Tail(a, b, x, y)

#define Two_Sum_Tail(a, b, x, y) \
  bvirt = (REAL) (x - a); \
  avirt = x - bvirt; \
  bround = b - bvirt; \
  around = a - avirt; \
  y = around + bround

#define Two_Sum(a, b, x, y) \
  x = (REAL) (a + b); \
  Two_Sum_Tail(a, b, x, y)

#define Two_Diff_Tail(a, b, x, y) \
  bvirt = (REAL) (a - x); \
  avirt = x + bvirt; \
  bround = bvirt - b; \
  around = a - avirt; \
  y = around + bround

#define Two_Diff(a, b, x, y) \
  x = (REAL) (a - b); \
  Two_Diff_Tail(a, b, x, y)

#define Split(a, ahi, alo) \
  c = (REAL) (splitter * a); \
  abig = (REAL) (c - a); \
  ahi = c - abig; \
  alo = a - ahi

#define Two_Product_Tail(a, b, x, y) \
  Split(a, ahi, alo); \
  Split(b, bhi, blo); \
  err1 = x - (ahi * bhi); \
  err2 = err1 - (alo * bhi); \
  err3 = err2 - (ahi * blo); \
  y = (alo * blo) - err3

#define Two_Product(a, b, x, y) \
  x = (REAL) (a * b); \
  Two_Product_Tail(a, b, x, y)

/* Two_Product_Presplit() is Two_Product() where one of the inputs has       */
/*   already been split.  Avoids redundant splitting.                        */

#define Two_Product_Presplit(a, b, bhi, blo, x, y) \
  x = (REAL) (a * b); \
  Split(a, ahi, alo); \
  err1 = x - (ahi * bhi); \
  err2 = err1 - (alo * bhi); \
  err3 = err2 - (ahi * blo); \
  y = (alo * blo) - err3

/* Square() can be done more quickly than Two_Product().                     */

#define Square_Tail(a, x, y) \
  Split(a, ahi, alo); \
  err1 = x - (ahi * ahi); \
  err3 = err1 - ((ahi + ahi) * alo); \
  y = (alo * alo) - err3

#define Square(a, x, y) \
  x = (REAL) (a * a); \
  Square_Tail(a, x, y)

/* Macros for summing expansions of various fixed lengths.  These are all    */
/*   unrolled versions of Expansion_Sum().                                   */

#define Two_One_Sum(a1, a0, b, x2, x1, x0) \
  Two_Sum(a0, b , _i, x0); \
  Two_Sum(a1, _i, x2, x1)

#define Two_One_Diff(a1, a0, b, x2, x1, x0) \
  Two_Diff(a0, b , _i, x0); \
  Two_Sum( a1, _i, x2, x1)

#define Two_Two_Sum(a1, a0, b1, b0, x3, x2, x1, x0) \
  Two_One_Sum(a1, a0, b0, _j, _0, x0); \
  Two_One_Sum(_j, _0, b1, x3, x2, x1)

#define Two_Two_Diff(a1, a0, b1, b0, x3, x2, x1, x0) \
  Two_One_Diff(a1, a0, b0, _j, _0, x0); \
  Two_One_Diff(_j, _0, b1, x3, x2, x1)

/*****************************************************************************/
/*                                                                           */
/*  exactinit()   Initialize the variables used for exact arithmetic.        */
/*                                                                           */
/*  `epsilon' is the largest power of two such that 1.0 + epsilon = 1.0 in   */
/*  floating-point arithmetic.  `epsilon' bounds the relative roundoff       */
/*  error.  It is used for floating-point error analysis.                    */
/*                                                                           */
/*  `splitter' is used to split floating-point numbers into two half-        */
/*  length significands for exact multiplication.                            */
/*                                                                           */
/*  I imagine that a highly optimizing compiler might be too smart for its   */
/*  own good, and somehow cause this routine to fail, if it pretends that    */
/*  floating-point arithmetic is too much like real arithmetic.              */
/*                                                                           */
/*  Don't change this routine unless you fully understand it.                */
/*                                                                           */
/*****************************************************************************/

void exactinit()
{
  REAL half;
  REAL check, lastcheck;
  int every_other;

  every_other = 1;
  half = 0.5;
  epsilon = 1.0;
  splitter = 1.0;
  check = 1.0;
  /* Repeatedly divide `epsilon' by two until it is too small to add to      */
  /*   one without causing roundoff.  (Also check if the sum is equal to     */
  /*   the previous sum, for machines that round up instead of using exact   */
  /*   rounding.  Not that these routines will work on such machines anyway. */
  do {
    lastcheck = check;
    epsilon *= half;
    if (every_other) {
      splitter *= 2.0;
    }
    every_other = !every_other;
    check = 1.0 + epsilon;
  } while ((check != 1.0) && (check != lastcheck));
  splitter += 1.0;
  if (verbose > 1) {
    printf("Floating point roundoff is of magnitude %.17g\n", epsilon);
    printf("Floating point splitter is %.17g\n", splitter);
  }
  /* Error bounds for orientation and incircle tests. */
  resulterrbound = (3.0 + 8.0 * epsilon) * epsilon;
  ccwerrboundA = (3.0 + 16.0 * epsilon) * epsilon;
  ccwerrboundB = (2.0 + 12.0 * epsilon) * epsilon;
  ccwerrboundC = (9.0 + 64.0 * epsilon) * epsilon * epsilon;
  iccerrboundA = (10.0 + 96.0 * epsilon) * epsilon;
  iccerrboundB = (4.0 + 48.0 * epsilon) * epsilon;
  iccerrboundC = (44.0 + 576.0 * epsilon) * epsilon * epsilon;
}

/*****************************************************************************/
/*                                                                           */
/*  fast_expansion_sum_zeroelim()   Sum two expansions, eliminating zero     */
/*                                  components from the output expansion.    */
/*                                                                           */
/*  Sets h = e + f.  See my Robust Predicates paper for details.             */
/*                                                                           */
/*  If round-to-even is used (as with IEEE 754), maintains the strongly      */
/*  nonoverlapping property.  (That is, if e is strongly nonoverlapping, h   */
/*  will be also.)  Does NOT maintain the nonoverlapping or nonadjacent      */
/*  properties.                                                              */
/*                                                                           */
/*****************************************************************************/

int fast_expansion_sum_zeroelim(elen, e, flen, f, h)  /* h cannot be e or f. */
int elen;
REAL *e;
int flen;
REAL *f;
REAL *h;
{
  REAL Q;
  INEXACT REAL Qnew;
  INEXACT REAL hh;
  INEXACT REAL bvirt;
  REAL avirt, bround, around;
  int eindex, findex, hindex;
  REAL enow, fnow;

  enow = e[0];
  fnow = f[0];
  eindex = findex = 0;
  if ((fnow > enow) == (fnow > -enow)) {
    Q = enow;
    enow = e[++eindex];
  } else {
    Q = fnow;
    fnow = f[++findex];
  }
  hindex = 0;
  if ((eindex < elen) && (findex < flen)) {
    if ((fnow > enow) == (fnow > -enow)) {
      Fast_Two_Sum(enow, Q, Qnew, hh);
      enow = e[++eindex];
    } else {
      Fast_Two_Sum(fnow, Q, Qnew, hh);
      fnow = f[++findex];
    }
    Q = Qnew;
    if (hh != 0.0) {
      h[hindex++] = hh;
    }
    while ((eindex < elen) && (findex < flen)) {
      if ((fnow > enow) == (fnow > -enow)) {
        Two_Sum(Q, enow, Qnew, hh);
        enow = e[++eindex];
      } else {
        Two_Sum(Q, fnow, Qnew, hh);
        fnow = f[++findex];
      }
      Q = Qnew;
      if (hh != 0.0) {
        h[hindex++] = hh;
      }
    }
  }
  while (eindex < elen) {
    Two_Sum(Q, enow, Qnew, hh);
    enow = e[++eindex];
    Q = Qnew;
    if (hh != 0.0) {
      h[hindex++] = hh;
    }
  }
  while (findex < flen) {
    Two_Sum(Q, fnow, Qnew, hh);
    fnow = f[++findex];
    Q = Qnew;
    if (hh != 0.0) {
      h[hindex++] = hh;
    }
  }
  if ((Q != 0.0) || (hindex == 0)) {
    h[hindex++] = Q;
  }
  return hindex;
}

/*****************************************************************************/
/*                                                                           */
/*  scale_expansion_zeroelim()   Multiply an expansion by a scalar,          */
/*                               eliminating zero components from the        */
/*                               output expansion.                           */
/*                                                                           */
/*  Sets h = be.  See my Robust Predicates paper for details.                */
/*                                                                           */
/*  Maintains the nonoverlapping property.  If round-to-even is used (as     */
/*  with IEEE 754), maintains the strongly nonoverlapping and nonadjacent    */
/*  properties as well.  (That is, if e has one of these properties, so      */
/*  will h.)                                                                 */
/*                                                                           */
/*****************************************************************************/

int scale_expansion_zeroelim(elen, e, b, h)   /* e and h cannot be the same. */
int elen;
REAL *e;
REAL b;
REAL *h;
{
  INEXACT REAL Q, sum;
  REAL hh;
  INEXACT REAL product1;
  REAL product0;
  int eindex, hindex;
  REAL enow;
  INEXACT REAL bvirt;
  REAL avirt, bround, around;
  INEXACT REAL c;
  INEXACT REAL abig;
  REAL ahi, alo, bhi, blo;
  REAL err1, err2, err3;

  Split(b, bhi, blo);
  Two_Product_Presplit(e[0], b, bhi, blo, Q, hh);
  hindex = 0;
  if (hh != 0) {
    h[hindex++] = hh;
  }
  for (eindex = 1; eindex < elen; eindex++) {
    enow = e[eindex];
    Two_Product_Presplit(enow, b, bhi, blo, product1, product0);
    Two_Sum(Q, product0, sum, hh);
    if (hh != 0) {
      h[hindex++] = hh;
    }
    Fast_Two_Sum(product1, sum, Q, hh);
    if (hh != 0) {
      h[hindex++] = hh;
    }
  }
  if ((Q != 0.0) || (hindex == 0)) {
    h[hindex++] = Q;
  }
  return hindex;
}

/*****************************************************************************/
/*                                                                           */
/*  estimate()   Produce a one-word estimate of an expansion's value.        */
/*                                                                           */
/*  See my Robust Predicates paper for details.                              */
/*                                                                           */
/*****************************************************************************/

REAL estimate(elen, e)
int elen;
REAL *e;
{
  REAL Q;
  int eindex;

  Q = e[0];
  for (eindex = 1; eindex < elen; eindex++) {
    Q += e[eindex];
  }
  return Q;
}

/*****************************************************************************/
/*                                                                           */
/*  counterclockwise()   Return a positive value if the points pa, pb, and   */
/*                       pc occur in counterclockwise order; a negative      */
/*                       value if they occur in clockwise order; and zero    */
/*                       if they are collinear.  The result is also a rough  */
/*                       approximation of twice the signed area of the       */
/*                       triangle defined by the three points.               */
/*                                                                           */
/*  Uses exact arithmetic if necessary to ensure a correct answer.  The      */
/*  result returned is the determinant of a matrix.  This determinant is     */
/*  computed adaptively, in the sense that exact arithmetic is used only to  */
/*  the degree it is needed to ensure that the returned value has the        */
/*  correct sign.  Hence, this function is usually quite fast, but will run  */
/*  more slowly when the input points are collinear or nearly so.            */
/*                                                                           */
/*  See my Robust Predicates paper for details.                              */
/*                                                                           */
/*****************************************************************************/

REAL counterclockwiseadapt(pa, pb, pc, detsum)
point pa;
point pb;
point pc;
REAL detsum;
{
  INEXACT REAL acx, acy, bcx, bcy;
  REAL acxtail, acytail, bcxtail, bcytail;
  INEXACT REAL detleft, detright;
  REAL detlefttail, detrighttail;
  REAL det, errbound;
  REAL B[4], C1[8], C2[12], D[16];
  INEXACT REAL B3;
  int C1length, C2length, Dlength;
  REAL u[4];
  INEXACT REAL u3;
  INEXACT REAL s1, t1;
  REAL s0, t0;

  INEXACT REAL bvirt;
  REAL avirt, bround, around;
  INEXACT REAL c;
  INEXACT REAL abig;
  REAL ahi, alo, bhi, blo;
  REAL err1, err2, err3;
  INEXACT REAL _i, _j;
  REAL _0;

  acx = (REAL) (pa[0] - pc[0]);
  bcx = (REAL) (pb[0] - pc[0]);
  acy = (REAL) (pa[1] - pc[1]);
  bcy = (REAL) (pb[1] - pc[1]);

  Two_Product(acx, bcy, detleft, detlefttail);
  Two_Product(acy, bcx, detright, detrighttail);

  Two_Two_Diff(detleft, detlefttail, detright, detrighttail,
               B3, B[2], B[1], B[0]);
  B[3] = B3;

  det = estimate(4, B);
  errbound = ccwerrboundB * detsum;
  if ((det >= errbound) || (-det >= errbound)) {
    return det;
  }

  Two_Diff_Tail(pa[0], pc[0], acx, acxtail);
  Two_Diff_Tail(pb[0], pc[0], bcx, bcxtail);
  Two_Diff_Tail(pa[1], pc[1], acy, acytail);
  Two_Diff_Tail(pb[1], pc[1], bcy, bcytail);

  if ((acxtail == 0.0) && (acytail == 0.0)
      && (bcxtail == 0.0) && (bcytail == 0.0)) {
    return det;
  }

  errbound = ccwerrboundC * detsum + resulterrbound * Absolute(det);
  det += (acx * bcytail + bcy * acxtail)
       - (acy * bcxtail + bcx * acytail);
  if ((det >= errbound) || (-det >= errbound)) {
    return det;
  }

  Two_Product(acxtail, bcy, s1, s0);
  Two_Product(acytail, bcx, t1, t0);
  Two_Two_Diff(s1, s0, t1, t0, u3, u[2], u[1], u[0]);
  u[3] = u3;
  C1length = fast_expansion_sum_zeroelim(4, B, 4, u, C1);

  Two_Product(acx, bcytail, s1, s0);
  Two_Product(acy, bcxtail, t1, t0);
  Two_Two_Diff(s1, s0, t1, t0, u3, u[2], u[1], u[0]);
  u[3] = u3;
  C2length = fast_expansion_sum_zeroelim(C1length, C1, 4, u, C2);

  Two_Product(acxtail, bcytail, s1, s0);
  Two_Product(acytail, bcxtail, t1, t0);
  Two_Two_Diff(s1, s0, t1, t0, u3, u[2], u[1], u[0]);
  u[3] = u3;
  Dlength = fast_expansion_sum_zeroelim(C2length, C2, 4, u, D);

  return D[Dlength - 1];
}

REAL counterclockwise(pa, pb, pc)
point pa;
point pb;
point pc;
{
  REAL detleft, detright, det;
  REAL detsum, errbound;

  counterclockcount++;

  detleft = (pa[0] - pc[0]) * (pb[1] - pc[1]);
  detright = (pa[1] - pc[1]) * (pb[0] - pc[0]);
  det = detleft - detright;

  if (noexact) {
    return det;
  }

  if (detleft > 0.0) {
    if (detright <= 0.0) {
      return det;
    } else {
      detsum = detleft + detright;
    }
  } else if (detleft < 0.0) {
    if (detright >= 0.0) {
      return det;
    } else {
      detsum = -detleft - detright;
    }
  } else {
    return det;
  }

  errbound = ccwerrboundA * detsum;
  if ((det >= errbound) || (-det >= errbound)) {
    return det;
  }

  return counterclockwiseadapt(pa, pb, pc, detsum);
}

/*****************************************************************************/
/*                                                                           */
/*  incircle()   Return a positive value if the point pd lies inside the     */
/*               circle passing through pa, pb, and pc; a negative value if  */
/*               it lies outside; and zero if the four points are cocircular.*/
/*               The points pa, pb, and pc must be in counterclockwise       */
/*               order, or the sign of the result will be reversed.          */
/*                                                                           */
/*  Uses exact arithmetic if necessary to ensure a correct answer.  The      */
/*  result returned is the determinant of a matrix.  This determinant is     */
/*  computed adaptively, in the sense that exact arithmetic is used only to  */
/*  the degree it is needed to ensure that the returned value has the        */
/*  correct sign.  Hence, this function is usually quite fast, but will run  */
/*  more slowly when the input points are cocircular or nearly so.           */
/*                                                                           */
/*  See my Robust Predicates paper for details.                              */
/*                                                                           */
/*****************************************************************************/

REAL incircleadapt(pa, pb, pc, pd, permanent)
point pa;
point pb;
point pc;
point pd;
REAL permanent;
{
  INEXACT REAL adx, bdx, cdx, ady, bdy, cdy;
  REAL det, errbound;

  INEXACT REAL bdxcdy1, cdxbdy1, cdxady1, adxcdy1, adxbdy1, bdxady1;
  REAL bdxcdy0, cdxbdy0, cdxady0, adxcdy0, adxbdy0, bdxady0;
  REAL bc[4], ca[4], ab[4];
  INEXACT REAL bc3, ca3, ab3;
  REAL axbc[8], axxbc[16], aybc[8], ayybc[16], adet[32];
  int axbclen, axxbclen, aybclen, ayybclen, alen;
  REAL bxca[8], bxxca[16], byca[8], byyca[16], bdet[32];
  int bxcalen, bxxcalen, bycalen, byycalen, blen;
  REAL cxab[8], cxxab[16], cyab[8], cyyab[16], cdet[32];
  int cxablen, cxxablen, cyablen, cyyablen, clen;
  REAL abdet[64];
  int ablen;
  REAL fin1[1152], fin2[1152];
  REAL *finnow, *finother, *finswap;
  int finlength;

  REAL adxtail, bdxtail, cdxtail, adytail, bdytail, cdytail;
  INEXACT REAL adxadx1, adyady1, bdxbdx1, bdybdy1, cdxcdx1, cdycdy1;
  REAL adxadx0, adyady0, bdxbdx0, bdybdy0, cdxcdx0, cdycdy0;
  REAL aa[4], bb[4], cc[4];
  INEXACT REAL aa3, bb3, cc3;
  INEXACT REAL ti1, tj1;
  REAL ti0, tj0;
  REAL u[4], v[4];
  INEXACT REAL u3, v3;
  REAL temp8[8], temp16a[16], temp16b[16], temp16c[16];
  REAL temp32a[32], temp32b[32], temp48[48], temp64[64];
  int temp8len, temp16alen, temp16blen, temp16clen;
  int temp32alen, temp32blen, temp48len, temp64len;
  REAL axtbb[8], axtcc[8], aytbb[8], aytcc[8];
  int axtbblen, axtcclen, aytbblen, aytcclen;
  REAL bxtaa[8], bxtcc[8], bytaa[8], bytcc[8];
  int bxtaalen, bxtcclen, bytaalen, bytcclen;
  REAL cxtaa[8], cxtbb[8], cytaa[8], cytbb[8];
  int cxtaalen, cxtbblen, cytaalen, cytbblen;
  REAL axtbc[8], aytbc[8], bxtca[8], bytca[8], cxtab[8], cytab[8];
  int axtbclen=0, aytbclen=0, bxtcalen=0, bytcalen=0, cxtablen=0, cytablen=0;
  REAL axtbct[16], aytbct[16], bxtcat[16], bytcat[16], cxtabt[16], cytabt[16];
  int axtbctlen, aytbctlen, bxtcatlen, bytcatlen, cxtabtlen, cytabtlen;
  REAL axtbctt[8], aytbctt[8], bxtcatt[8];
  REAL bytcatt[8], cxtabtt[8], cytabtt[8];
  int axtbcttlen, aytbcttlen, bxtcattlen, bytcattlen, cxtabttlen, cytabttlen;
  REAL abt[8], bct[8], cat[8];
  int abtlen, bctlen, catlen;
  REAL abtt[4], bctt[4], catt[4];
  int abttlen, bcttlen, cattlen;
  INEXACT REAL abtt3, bctt3, catt3;
  REAL negate;

  INEXACT REAL bvirt;
  REAL avirt, bround, around;
  INEXACT REAL c;
  INEXACT REAL abig;
  REAL ahi, alo, bhi, blo;
  REAL err1, err2, err3;
  INEXACT REAL _i, _j;
  REAL _0;

  adx = (REAL) (pa[0] - pd[0]);
  bdx = (REAL) (pb[0] - pd[0]);
  cdx = (REAL) (pc[0] - pd[0]);
  ady = (REAL) (pa[1] - pd[1]);
  bdy = (REAL) (pb[1] - pd[1]);
  cdy = (REAL) (pc[1] - pd[1]);

  Two_Product(bdx, cdy, bdxcdy1, bdxcdy0);
  Two_Product(cdx, bdy, cdxbdy1, cdxbdy0);
  Two_Two_Diff(bdxcdy1, bdxcdy0, cdxbdy1, cdxbdy0, bc3, bc[2], bc[1], bc[0]);
  bc[3] = bc3;
  axbclen = scale_expansion_zeroelim(4, bc, adx, axbc);
  axxbclen = scale_expansion_zeroelim(axbclen, axbc, adx, axxbc);
  aybclen = scale_expansion_zeroelim(4, bc, ady, aybc);
  ayybclen = scale_expansion_zeroelim(aybclen, aybc, ady, ayybc);
  alen = fast_expansion_sum_zeroelim(axxbclen, axxbc, ayybclen, ayybc, adet);

  Two_Product(cdx, ady, cdxady1, cdxady0);
  Two_Product(adx, cdy, adxcdy1, adxcdy0);
  Two_Two_Diff(cdxady1, cdxady0, adxcdy1, adxcdy0, ca3, ca[2], ca[1], ca[0]);
  ca[3] = ca3;
  bxcalen = scale_expansion_zeroelim(4, ca, bdx, bxca);
  bxxcalen = scale_expansion_zeroelim(bxcalen, bxca, bdx, bxxca);
  bycalen = scale_expansion_zeroelim(4, ca, bdy, byca);
  byycalen = scale_expansion_zeroelim(bycalen, byca, bdy, byyca);
  blen = fast_expansion_sum_zeroelim(bxxcalen, bxxca, byycalen, byyca, bdet);

  Two_Product(adx, bdy, adxbdy1, adxbdy0);
  Two_Product(bdx, ady, bdxady1, bdxady0);
  Two_Two_Diff(adxbdy1, adxbdy0, bdxady1, bdxady0, ab3, ab[2], ab[1], ab[0]);
  ab[3] = ab3;
  cxablen = scale_expansion_zeroelim(4, ab, cdx, cxab);
  cxxablen = scale_expansion_zeroelim(cxablen, cxab, cdx, cxxab);
  cyablen = scale_expansion_zeroelim(4, ab, cdy, cyab);
  cyyablen = scale_expansion_zeroelim(cyablen, cyab, cdy, cyyab);
  clen = fast_expansion_sum_zeroelim(cxxablen, cxxab, cyyablen, cyyab, cdet);

  ablen = fast_expansion_sum_zeroelim(alen, adet, blen, bdet, abdet);
  finlength = fast_expansion_sum_zeroelim(ablen, abdet, clen, cdet, fin1);

  det = estimate(finlength, fin1);
  errbound = iccerrboundB * permanent;
  if ((det >= errbound) || (-det >= errbound)) {
    return det;
  }

  Two_Diff_Tail(pa[0], pd[0], adx, adxtail);
  Two_Diff_Tail(pa[1], pd[1], ady, adytail);
  Two_Diff_Tail(pb[0], pd[0], bdx, bdxtail);
  Two_Diff_Tail(pb[1], pd[1], bdy, bdytail);
  Two_Diff_Tail(pc[0], pd[0], cdx, cdxtail);
  Two_Diff_Tail(pc[1], pd[1], cdy, cdytail);
  if ((adxtail == 0.0) && (bdxtail == 0.0) && (cdxtail == 0.0)
      && (adytail == 0.0) && (bdytail == 0.0) && (cdytail == 0.0)) {
    return det;
  }

  errbound = iccerrboundC * permanent + resulterrbound * Absolute(det);
  det += ((adx * adx + ady * ady) * ((bdx * cdytail + cdy * bdxtail)
                                     - (bdy * cdxtail + cdx * bdytail))
          + 2.0 * (adx * adxtail + ady * adytail) * (bdx * cdy - bdy * cdx))
       + ((bdx * bdx + bdy * bdy) * ((cdx * adytail + ady * cdxtail)
                                     - (cdy * adxtail + adx * cdytail))
          + 2.0 * (bdx * bdxtail + bdy * bdytail) * (cdx * ady - cdy * adx))
       + ((cdx * cdx + cdy * cdy) * ((adx * bdytail + bdy * adxtail)
                                     - (ady * bdxtail + bdx * adytail))
          + 2.0 * (cdx * cdxtail + cdy * cdytail) * (adx * bdy - ady * bdx));
  if ((det >= errbound) || (-det >= errbound)) {
    return det;
  }

  finnow = fin1;
  finother = fin2;

  if ((bdxtail != 0.0) || (bdytail != 0.0)
      || (cdxtail != 0.0) || (cdytail != 0.0)) {
    Square(adx, adxadx1, adxadx0);
    Square(ady, adyady1, adyady0);
    Two_Two_Sum(adxadx1, adxadx0, adyady1, adyady0, aa3, aa[2], aa[1], aa[0]);
    aa[3] = aa3;
  }
  if ((cdxtail != 0.0) || (cdytail != 0.0)
      || (adxtail != 0.0) || (adytail != 0.0)) {
    Square(bdx, bdxbdx1, bdxbdx0);
    Square(bdy, bdybdy1, bdybdy0);
    Two_Two_Sum(bdxbdx1, bdxbdx0, bdybdy1, bdybdy0, bb3, bb[2], bb[1], bb[0]);
    bb[3] = bb3;
  }
  if ((adxtail != 0.0) || (adytail != 0.0)
      || (bdxtail != 0.0) || (bdytail != 0.0)) {
    Square(cdx, cdxcdx1, cdxcdx0);
    Square(cdy, cdycdy1, cdycdy0);
    Two_Two_Sum(cdxcdx1, cdxcdx0, cdycdy1, cdycdy0, cc3, cc[2], cc[1], cc[0]);
    cc[3] = cc3;
  }

  if (adxtail != 0.0) {
    axtbclen = scale_expansion_zeroelim(4, bc, adxtail, axtbc);
    temp16alen = scale_expansion_zeroelim(axtbclen, axtbc, 2.0 * adx, temp16a);

    axtcclen = scale_expansion_zeroelim(4, cc, adxtail, axtcc);
    temp16blen = scale_expansion_zeroelim(axtcclen, axtcc, bdy, temp16b);

    axtbblen = scale_expansion_zeroelim(4, bb, adxtail, axtbb);
    temp16clen = scale_expansion_zeroelim(axtbblen, axtbb, -cdy, temp16c);

    temp32alen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                             temp16blen, temp16b, temp32a);
    temp48len = fast_expansion_sum_zeroelim(temp16clen, temp16c,
                                            temp32alen, temp32a, temp48);
    finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                            temp48, finother);
    finswap = finnow; finnow = finother; finother = finswap;
  }
  if (adytail != 0.0) {
    aytbclen = scale_expansion_zeroelim(4, bc, adytail, aytbc);
    temp16alen = scale_expansion_zeroelim(aytbclen, aytbc, 2.0 * ady, temp16a);

    aytbblen = scale_expansion_zeroelim(4, bb, adytail, aytbb);
    temp16blen = scale_expansion_zeroelim(aytbblen, aytbb, cdx, temp16b);

    aytcclen = scale_expansion_zeroelim(4, cc, adytail, aytcc);
    temp16clen = scale_expansion_zeroelim(aytcclen, aytcc, -bdx, temp16c);

    temp32alen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                             temp16blen, temp16b, temp32a);
    temp48len = fast_expansion_sum_zeroelim(temp16clen, temp16c,
                                            temp32alen, temp32a, temp48);
    finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                            temp48, finother);
    finswap = finnow; finnow = finother; finother = finswap;
  }
  if (bdxtail != 0.0) {
    bxtcalen = scale_expansion_zeroelim(4, ca, bdxtail, bxtca);
    temp16alen = scale_expansion_zeroelim(bxtcalen, bxtca, 2.0 * bdx, temp16a);

    bxtaalen = scale_expansion_zeroelim(4, aa, bdxtail, bxtaa);
    temp16blen = scale_expansion_zeroelim(bxtaalen, bxtaa, cdy, temp16b);

    bxtcclen = scale_expansion_zeroelim(4, cc, bdxtail, bxtcc);
    temp16clen = scale_expansion_zeroelim(bxtcclen, bxtcc, -ady, temp16c);

    temp32alen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                             temp16blen, temp16b, temp32a);
    temp48len = fast_expansion_sum_zeroelim(temp16clen, temp16c,
                                            temp32alen, temp32a, temp48);
    finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                            temp48, finother);
    finswap = finnow; finnow = finother; finother = finswap;
  }
  if (bdytail != 0.0) {
    bytcalen = scale_expansion_zeroelim(4, ca, bdytail, bytca);
    temp16alen = scale_expansion_zeroelim(bytcalen, bytca, 2.0 * bdy, temp16a);

    bytcclen = scale_expansion_zeroelim(4, cc, bdytail, bytcc);
    temp16blen = scale_expansion_zeroelim(bytcclen, bytcc, adx, temp16b);

    bytaalen = scale_expansion_zeroelim(4, aa, bdytail, bytaa);
    temp16clen = scale_expansion_zeroelim(bytaalen, bytaa, -cdx, temp16c);

    temp32alen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                             temp16blen, temp16b, temp32a);
    temp48len = fast_expansion_sum_zeroelim(temp16clen, temp16c,
                                            temp32alen, temp32a, temp48);
    finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                            temp48, finother);
    finswap = finnow; finnow = finother; finother = finswap;
  }
  if (cdxtail != 0.0) {
    cxtablen = scale_expansion_zeroelim(4, ab, cdxtail, cxtab);
    temp16alen = scale_expansion_zeroelim(cxtablen, cxtab, 2.0 * cdx, temp16a);

    cxtbblen = scale_expansion_zeroelim(4, bb, cdxtail, cxtbb);
    temp16blen = scale_expansion_zeroelim(cxtbblen, cxtbb, ady, temp16b);

    cxtaalen = scale_expansion_zeroelim(4, aa, cdxtail, cxtaa);
    temp16clen = scale_expansion_zeroelim(cxtaalen, cxtaa, -bdy, temp16c);

    temp32alen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                             temp16blen, temp16b, temp32a);
    temp48len = fast_expansion_sum_zeroelim(temp16clen, temp16c,
                                            temp32alen, temp32a, temp48);
    finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                            temp48, finother);
    finswap = finnow; finnow = finother; finother = finswap;
  }
  if (cdytail != 0.0) {
    cytablen = scale_expansion_zeroelim(4, ab, cdytail, cytab);
    temp16alen = scale_expansion_zeroelim(cytablen, cytab, 2.0 * cdy, temp16a);

    cytaalen = scale_expansion_zeroelim(4, aa, cdytail, cytaa);
    temp16blen = scale_expansion_zeroelim(cytaalen, cytaa, bdx, temp16b);

    cytbblen = scale_expansion_zeroelim(4, bb, cdytail, cytbb);
    temp16clen = scale_expansion_zeroelim(cytbblen, cytbb, -adx, temp16c);

    temp32alen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                             temp16blen, temp16b, temp32a);
    temp48len = fast_expansion_sum_zeroelim(temp16clen, temp16c,
                                            temp32alen, temp32a, temp48);
    finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                            temp48, finother);
    finswap = finnow; finnow = finother; finother = finswap;
  }

  if ((adxtail != 0.0) || (adytail != 0.0)) {
    if ((bdxtail != 0.0) || (bdytail != 0.0)
        || (cdxtail != 0.0) || (cdytail != 0.0)) {
      Two_Product(bdxtail, cdy, ti1, ti0);
      Two_Product(bdx, cdytail, tj1, tj0);
      Two_Two_Sum(ti1, ti0, tj1, tj0, u3, u[2], u[1], u[0]);
      u[3] = u3;
      negate = -bdy;
      Two_Product(cdxtail, negate, ti1, ti0);
      negate = -bdytail;
      Two_Product(cdx, negate, tj1, tj0);
      Two_Two_Sum(ti1, ti0, tj1, tj0, v3, v[2], v[1], v[0]);
      v[3] = v3;
      bctlen = fast_expansion_sum_zeroelim(4, u, 4, v, bct);

      Two_Product(bdxtail, cdytail, ti1, ti0);
      Two_Product(cdxtail, bdytail, tj1, tj0);
      Two_Two_Diff(ti1, ti0, tj1, tj0, bctt3, bctt[2], bctt[1], bctt[0]);
      bctt[3] = bctt3;
      bcttlen = 4;
    } else {
      bct[0] = 0.0;
      bctlen = 1;
      bctt[0] = 0.0;
      bcttlen = 1;
    }

    if (adxtail != 0.0) {
      temp16alen = scale_expansion_zeroelim(axtbclen, axtbc, adxtail, temp16a);
      axtbctlen = scale_expansion_zeroelim(bctlen, bct, adxtail, axtbct);
      temp32alen = scale_expansion_zeroelim(axtbctlen, axtbct, 2.0 * adx, temp32a);
      temp48len = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                              temp32alen, temp32a, temp48);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                              temp48, finother);
      finswap = finnow; finnow = finother; finother = finswap;
      if (bdytail != 0.0) {
        temp8len = scale_expansion_zeroelim(4, cc, adxtail, temp8);
        temp16alen = scale_expansion_zeroelim(temp8len, temp8, bdytail, temp16a);
        finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp16alen,
                                                temp16a, finother);
        finswap = finnow; finnow = finother; finother = finswap;
      }
      if (cdytail != 0.0) {
        temp8len = scale_expansion_zeroelim(4, bb, -adxtail, temp8);
        temp16alen = scale_expansion_zeroelim(temp8len, temp8, cdytail, temp16a);
        finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp16alen,
                                                temp16a, finother);
        finswap = finnow; finnow = finother; finother = finswap;
      }

      temp32alen = scale_expansion_zeroelim(axtbctlen, axtbct, adxtail, temp32a);
      axtbcttlen = scale_expansion_zeroelim(bcttlen, bctt, adxtail, axtbctt);
      temp16alen = scale_expansion_zeroelim(axtbcttlen, axtbctt, 2.0 * adx, temp16a);
      temp16blen = scale_expansion_zeroelim(axtbcttlen, axtbctt, adxtail, temp16b);
      temp32blen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                               temp16blen, temp16b, temp32b);
      temp64len = fast_expansion_sum_zeroelim(temp32alen, temp32a,
                                              temp32blen, temp32b, temp64);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp64len,
                                              temp64, finother);
      finswap = finnow; finnow = finother; finother = finswap;
    }
    if (adytail != 0.0) {
      temp16alen = scale_expansion_zeroelim(aytbclen, aytbc, adytail, temp16a);
      aytbctlen = scale_expansion_zeroelim(bctlen, bct, adytail, aytbct);
      temp32alen = scale_expansion_zeroelim(aytbctlen, aytbct, 2.0 * ady, temp32a);
      temp48len = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                              temp32alen, temp32a, temp48);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                              temp48, finother);
      finswap = finnow; finnow = finother; finother = finswap;


      temp32alen = scale_expansion_zeroelim(aytbctlen, aytbct, adytail, temp32a);
      aytbcttlen = scale_expansion_zeroelim(bcttlen, bctt, adytail, aytbctt);
      temp16alen = scale_expansion_zeroelim(aytbcttlen, aytbctt, 2.0 * ady, temp16a);
      temp16blen = scale_expansion_zeroelim(aytbcttlen, aytbctt, adytail, temp16b);
      temp32blen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                               temp16blen, temp16b, temp32b);
      temp64len = fast_expansion_sum_zeroelim(temp32alen, temp32a,
                                              temp32blen, temp32b, temp64);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp64len,
                                              temp64, finother);
      finswap = finnow; finnow = finother; finother = finswap;
    }
  }
  if ((bdxtail != 0.0) || (bdytail != 0.0)) {
    if ((cdxtail != 0.0) || (cdytail != 0.0)
        || (adxtail != 0.0) || (adytail != 0.0)) {
      Two_Product(cdxtail, ady, ti1, ti0);
      Two_Product(cdx, adytail, tj1, tj0);
      Two_Two_Sum(ti1, ti0, tj1, tj0, u3, u[2], u[1], u[0]);
      u[3] = u3;
      negate = -cdy;
      Two_Product(adxtail, negate, ti1, ti0);
      negate = -cdytail;
      Two_Product(adx, negate, tj1, tj0);
      Two_Two_Sum(ti1, ti0, tj1, tj0, v3, v[2], v[1], v[0]);
      v[3] = v3;
      catlen = fast_expansion_sum_zeroelim(4, u, 4, v, cat);

      Two_Product(cdxtail, adytail, ti1, ti0);
      Two_Product(adxtail, cdytail, tj1, tj0);
      Two_Two_Diff(ti1, ti0, tj1, tj0, catt3, catt[2], catt[1], catt[0]);
      catt[3] = catt3;
      cattlen = 4;
    } else {
      cat[0] = 0.0;
      catlen = 1;
      catt[0] = 0.0;
      cattlen = 1;
    }

    if (bdxtail != 0.0) {
      temp16alen = scale_expansion_zeroelim(bxtcalen, bxtca, bdxtail, temp16a);
      bxtcatlen = scale_expansion_zeroelim(catlen, cat, bdxtail, bxtcat);
      temp32alen = scale_expansion_zeroelim(bxtcatlen, bxtcat, 2.0 * bdx, temp32a);
      temp48len = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                              temp32alen, temp32a, temp48);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                              temp48, finother);
      finswap = finnow; finnow = finother; finother = finswap;
      if (cdytail != 0.0) {
        temp8len = scale_expansion_zeroelim(4, aa, bdxtail, temp8);
        temp16alen = scale_expansion_zeroelim(temp8len, temp8, cdytail, temp16a);
        finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp16alen,
                                                temp16a, finother);
        finswap = finnow; finnow = finother; finother = finswap;
      }
      if (adytail != 0.0) {
        temp8len = scale_expansion_zeroelim(4, cc, -bdxtail, temp8);
        temp16alen = scale_expansion_zeroelim(temp8len, temp8, adytail, temp16a);
        finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp16alen,
                                                temp16a, finother);
        finswap = finnow; finnow = finother; finother = finswap;
      }

      temp32alen = scale_expansion_zeroelim(bxtcatlen, bxtcat, bdxtail, temp32a);
      bxtcattlen = scale_expansion_zeroelim(cattlen, catt, bdxtail, bxtcatt);
      temp16alen = scale_expansion_zeroelim(bxtcattlen, bxtcatt, 2.0 * bdx, temp16a);
      temp16blen = scale_expansion_zeroelim(bxtcattlen, bxtcatt, bdxtail, temp16b);
      temp32blen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                               temp16blen, temp16b, temp32b);
      temp64len = fast_expansion_sum_zeroelim(temp32alen, temp32a,
                                              temp32blen, temp32b, temp64);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp64len,
                                              temp64, finother);
      finswap = finnow; finnow = finother; finother = finswap;
    }
    if (bdytail != 0.0) {
      temp16alen = scale_expansion_zeroelim(bytcalen, bytca, bdytail, temp16a);
      bytcatlen = scale_expansion_zeroelim(catlen, cat, bdytail, bytcat);
      temp32alen = scale_expansion_zeroelim(bytcatlen, bytcat, 2.0 * bdy, temp32a);
      temp48len = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                              temp32alen, temp32a, temp48);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                              temp48, finother);
      finswap = finnow; finnow = finother; finother = finswap;


      temp32alen = scale_expansion_zeroelim(bytcatlen, bytcat, bdytail, temp32a);
      bytcattlen = scale_expansion_zeroelim(cattlen, catt, bdytail, bytcatt);
      temp16alen = scale_expansion_zeroelim(bytcattlen, bytcatt, 2.0 * bdy, temp16a);
      temp16blen = scale_expansion_zeroelim(bytcattlen, bytcatt, bdytail, temp16b);
      temp32blen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                               temp16blen, temp16b, temp32b);
      temp64len = fast_expansion_sum_zeroelim(temp32alen, temp32a,
                                              temp32blen, temp32b, temp64);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp64len,
                                              temp64, finother);
      finswap = finnow; finnow = finother; finother = finswap;
    }
  }
  if ((cdxtail != 0.0) || (cdytail != 0.0)) {
    if ((adxtail != 0.0) || (adytail != 0.0)
        || (bdxtail != 0.0) || (bdytail != 0.0)) {
      Two_Product(adxtail, bdy, ti1, ti0);
      Two_Product(adx, bdytail, tj1, tj0);
      Two_Two_Sum(ti1, ti0, tj1, tj0, u3, u[2], u[1], u[0]);
      u[3] = u3;
      negate = -ady;
      Two_Product(bdxtail, negate, ti1, ti0);
      negate = -adytail;
      Two_Product(bdx, negate, tj1, tj0);
      Two_Two_Sum(ti1, ti0, tj1, tj0, v3, v[2], v[1], v[0]);
      v[3] = v3;
      abtlen = fast_expansion_sum_zeroelim(4, u, 4, v, abt);

      Two_Product(adxtail, bdytail, ti1, ti0);
      Two_Product(bdxtail, adytail, tj1, tj0);
      Two_Two_Diff(ti1, ti0, tj1, tj0, abtt3, abtt[2], abtt[1], abtt[0]);
      abtt[3] = abtt3;
      abttlen = 4;
    } else {
      abt[0] = 0.0;
      abtlen = 1;
      abtt[0] = 0.0;
      abttlen = 1;
    }

    if (cdxtail != 0.0) {
      temp16alen = scale_expansion_zeroelim(cxtablen, cxtab, cdxtail, temp16a);
      cxtabtlen = scale_expansion_zeroelim(abtlen, abt, cdxtail, cxtabt);
      temp32alen = scale_expansion_zeroelim(cxtabtlen, cxtabt, 2.0 * cdx, temp32a);
      temp48len = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                              temp32alen, temp32a, temp48);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                              temp48, finother);
      finswap = finnow; finnow = finother; finother = finswap;
      if (adytail != 0.0) {
        temp8len = scale_expansion_zeroelim(4, bb, cdxtail, temp8);
        temp16alen = scale_expansion_zeroelim(temp8len, temp8, adytail, temp16a);
        finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp16alen,
                                                temp16a, finother);
        finswap = finnow; finnow = finother; finother = finswap;
      }
      if (bdytail != 0.0) {
        temp8len = scale_expansion_zeroelim(4, aa, -cdxtail, temp8);
        temp16alen = scale_expansion_zeroelim(temp8len, temp8, bdytail, temp16a);
        finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp16alen,
                                                temp16a, finother);
        finswap = finnow; finnow = finother; finother = finswap;
      }

      temp32alen = scale_expansion_zeroelim(cxtabtlen, cxtabt, cdxtail, temp32a);
      cxtabttlen = scale_expansion_zeroelim(abttlen, abtt, cdxtail, cxtabtt);
      temp16alen = scale_expansion_zeroelim(cxtabttlen, cxtabtt, 2.0 * cdx, temp16a);
      temp16blen = scale_expansion_zeroelim(cxtabttlen, cxtabtt, cdxtail, temp16b);
      temp32blen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                               temp16blen, temp16b, temp32b);
      temp64len = fast_expansion_sum_zeroelim(temp32alen, temp32a,
                                              temp32blen, temp32b, temp64);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp64len,
                                              temp64, finother);
      finswap = finnow; finnow = finother; finother = finswap;
    }
    if (cdytail != 0.0) {
      temp16alen = scale_expansion_zeroelim(cytablen, cytab, cdytail, temp16a);
      cytabtlen = scale_expansion_zeroelim(abtlen, abt, cdytail, cytabt);
      temp32alen = scale_expansion_zeroelim(cytabtlen, cytabt, 2.0 * cdy, temp32a);
      temp48len = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                              temp32alen, temp32a, temp48);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp48len,
                                              temp48, finother);
      finswap = finnow; finnow = finother; finother = finswap;


      temp32alen = scale_expansion_zeroelim(cytabtlen, cytabt, cdytail, temp32a);
      cytabttlen = scale_expansion_zeroelim(abttlen, abtt, cdytail, cytabtt);
      temp16alen = scale_expansion_zeroelim(cytabttlen, cytabtt, 2.0 * cdy, temp16a);
      temp16blen = scale_expansion_zeroelim(cytabttlen, cytabtt, cdytail, temp16b);
      temp32blen = fast_expansion_sum_zeroelim(temp16alen, temp16a,
                                               temp16blen, temp16b, temp32b);
      temp64len = fast_expansion_sum_zeroelim(temp32alen, temp32a,
                                              temp32blen, temp32b, temp64);
      finlength = fast_expansion_sum_zeroelim(finlength, finnow, temp64len,
                                              temp64, finother);
      finswap = finnow; finnow = finother; /* finother = finswap; */
    }
  }

  return finnow[finlength - 1];
}

REAL incircle(pa, pb, pc, pd)
point pa;
point pb;
point pc;
point pd;
{
  REAL adx, bdx, cdx, ady, bdy, cdy;
  REAL bdxcdy, cdxbdy, cdxady, adxcdy, adxbdy, bdxady;
  REAL alift, blift, clift;
  REAL det;
  REAL permanent, errbound;

  incirclecount++;

  adx = pa[0] - pd[0];
  bdx = pb[0] - pd[0];
  cdx = pc[0] - pd[0];
  ady = pa[1] - pd[1];
  bdy = pb[1] - pd[1];
  cdy = pc[1] - pd[1];

  bdxcdy = bdx * cdy;
  cdxbdy = cdx * bdy;
  alift = adx * adx + ady * ady;

  cdxady = cdx * ady;
  adxcdy = adx * cdy;
  blift = bdx * bdx + bdy * bdy;

  adxbdy = adx * bdy;
  bdxady = bdx * ady;
  clift = cdx * cdx + cdy * cdy;

  det = alift * (bdxcdy - cdxbdy)
      + blift * (cdxady - adxcdy)
      + clift * (adxbdy - bdxady);

  if (noexact) {
    return det;
  }

  permanent = (Absolute(bdxcdy) + Absolute(cdxbdy)) * alift
            + (Absolute(cdxady) + Absolute(adxcdy)) * blift
            + (Absolute(adxbdy) + Absolute(bdxady)) * clift;
  errbound = iccerrboundA * permanent;
  if ((det > errbound) || (-det > errbound)) {
    return det;
  }

  return incircleadapt(pa, pb, pc, pd, permanent);
}

/**                                                                         **/
/**                                                                         **/
/********* Determinant evaluation routines end here                  *********/

/*****************************************************************************/
/*                                                                           */
/*  triangleinit()   Initialize some variables.                              */
/*                                                                           */
/*****************************************************************************/

void triangleinit()
{
  points.maxitems = triangles.maxitems = shelles.maxitems = viri.maxitems =
    badsegments.maxitems = badtriangles.maxitems = splaynodes.maxitems = 0l;
  points.itembytes = triangles.itembytes = shelles.itembytes = viri.itembytes =
    badsegments.itembytes = badtriangles.itembytes = splaynodes.itembytes = 0;
  recenttri.tri = (triangle *) NULL;    /* No triangle has been visited yet. */
  samples = 1;            /* Point location should take at least one sample. */
  checksegments = 0;      /* There are no segments in the triangulation yet. */
  incirclecount = counterclockcount = hyperbolacount = 0;
  circumcentercount = circletopcount = 0;
  randomseed = 1;

  exactinit();                     /* Initialize exact arithmetic constants. */
}

/*****************************************************************************/
/*                                                                           */
/*  randomnation()   Generate a random number between 0 and `choices' - 1.   */
/*                                                                           */
/*  This is a simple linear congruential random number generator.  Hence, it */
/*  is a bad random number generator, but good enough for most randomized    */
/*  geometric algorithms.                                                    */
/*                                                                           */
/*****************************************************************************/

unsigned long randomnation(choices)
unsigned int choices;
{
  randomseed = (randomseed * 1366l + 150889l) % 714025l;
  return randomseed / (714025l / choices + 1);
}

/********* Mesh quality testing routines begin here                  *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  checkmesh()   Test the mesh for topological consistency.                 */
/*                                                                           */
/*****************************************************************************/

#ifndef REDUCED

void checkmesh()
{
  struct triedge triangleloop;
  struct triedge oppotri, oppooppotri;
  point triorg, tridest, triapex;
  point oppoorg, oppodest;
  int horrors;
  int saveexact;
  triangle ptr;                         /* Temporary variable used by sym(). */

  /* Temporarily turn on exact arithmetic if it's off. */
  saveexact = noexact;
  noexact = 0;
  if (!quiet) {
    printf("  Checking consistency of mesh...\n");
  }
  horrors = 0;
  /* Run through the list of triangles, checking each one. */
  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  while (triangleloop.tri != (triangle *) NULL) {
    /* Check all three edges of the triangle. */
    for (triangleloop.orient = 0; triangleloop.orient < 3; triangleloop.orient++) {
      org(triangleloop, triorg);
      dest(triangleloop, tridest);
      if (triangleloop.orient == 0) {       /* Only test for inversion once. */
        /* Test if the triangle is flat or inverted. */
        apex(triangleloop, triapex);
        if (counterclockwise(triorg, tridest, triapex) <= 0.0) {
          printf("  !! !! Inverted ");
          printtriangle(&triangleloop);
          horrors++;
        }
      }
      /* Find the neighboring triangle on this edge. */
      sym(triangleloop, oppotri);
      if (oppotri.tri != dummytri) {
        /* Check that the triangle's neighbor knows it's a neighbor. */
        sym(oppotri, oppooppotri);
        if ((triangleloop.tri != oppooppotri.tri)
            || (triangleloop.orient != oppooppotri.orient)) {
          printf("  !! !! Asymmetric triangle-triangle bond:\n");
          if (triangleloop.tri == oppooppotri.tri) {
            printf("   (Right triangle, wrong orientation)\n");
          }
          printf("    First ");
          printtriangle(&triangleloop);
          printf("    Second (nonreciprocating) ");
          printtriangle(&oppotri);
          horrors++;
        }
        /* Check that both triangles agree on the identities */
        /*   of their shared vertices.                       */
        org(oppotri, oppoorg);
        dest(oppotri, oppodest);
        if ((triorg != oppodest) || (tridest != oppoorg)) {
          printf("  !! !! Mismatched edge coordinates between two triangles:\n");
          printf("    First mismatched ");
          printtriangle(&triangleloop);
          printf("    Second mismatched ");
          printtriangle(&oppotri);
          horrors++;
        }
      }
    }
    triangleloop.tri = triangletraverse();
  }
  if (horrors == 0) {
    if (!quiet) {
      printf("  In my studied opinion, the mesh appears to be consistent.\n");
    }
  } else if (horrors == 1) {
    printf("  !! !! !! !! Precisely one festering wound discovered.\n");
  } else {
    printf("  !! !! !! !! %d abominations witnessed.\n", horrors);
  }
  /* Restore the status of exact arithmetic. */
  noexact = saveexact;
}

#endif /* not REDUCED */

/*****************************************************************************/
/*                                                                           */
/*  checkdelaunay()   Ensure that the mesh is (constrained) Delaunay.        */
/*                                                                           */
/*****************************************************************************/

#ifndef REDUCED

void checkdelaunay()
{
  struct triedge triangleloop;
  struct triedge oppotri;
  struct edge opposhelle;
  point triorg, tridest, triapex;
  point oppoapex;
  int shouldbedelaunay;
  int horrors;
  int saveexact;
  triangle ptr;                         /* Temporary variable used by sym(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  /* Temporarily turn on exact arithmetic if it's off. */
  saveexact = noexact;
  noexact = 0;
  if (!quiet) {
    printf("  Checking Delaunay property of mesh...\n");
  }
  horrors = 0;
  /* Run through the list of triangles, checking each one. */
  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  while (triangleloop.tri != (triangle *) NULL) {
    /* Check all three edges of the triangle. */
    for (triangleloop.orient = 0; triangleloop.orient < 3; triangleloop.orient++) {
      org(triangleloop, triorg);
      dest(triangleloop, tridest);
      apex(triangleloop, triapex);
      sym(triangleloop, oppotri);
      apex(oppotri, oppoapex);
      /* Only test that the edge is locally Delaunay if there is an   */
      /*   adjoining triangle whose pointer is larger (to ensure that */
      /*   each pair isn't tested twice).                             */
      shouldbedelaunay = (oppotri.tri != dummytri)
            && (triapex != (point) NULL) && (oppoapex != (point) NULL)
            && (triangleloop.tri < oppotri.tri);
      if (checksegments && shouldbedelaunay) {
        /* If a shell edge separates the triangles, then the edge is */
        /*   constrained, so no local Delaunay test should be done.  */
        tspivot(triangleloop, opposhelle);
        if (opposhelle.sh != dummysh){
          shouldbedelaunay = 0;
        }
      }
      if (shouldbedelaunay) {
        if (incircle(triorg, tridest, triapex, oppoapex) > 0.0) {
          printf("  !! !! Non-Delaunay pair of triangles:\n");
          printf("    First non-Delaunay ");
          printtriangle(&triangleloop);
          printf("    Second non-Delaunay ");
          printtriangle(&oppotri);
          horrors++;
        }
      }
    }
    triangleloop.tri = triangletraverse();
  }
  if (horrors == 0) {
    if (!quiet) {
      printf("  By virtue of my perceptive intelligence, I declare the mesh Delaunay.\n");
    }
  } else if (horrors == 1) {
    printf("  !! !! !! !! Precisely one terrifying transgression identified.\n");
  } else {
    printf("  !! !! !! !! %d obscenities viewed with horror.\n", horrors);
  }
  /* Restore the status of exact arithmetic. */
  noexact = saveexact;
}

#endif /* not REDUCED */

/*****************************************************************************/
/*                                                                           */
/*  enqueuebadtri()   Add a bad triangle to the end of a queue.              */
/*                                                                           */
/*  The queue is actually a set of 64 queues.  I use multiple queues to give */
/*  priority to smaller angles.  I originally implemented a heap, but the    */
/*  queues are (to my surprise) much faster.                                 */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void enqueuebadtri(instri, angle, insapex, insorg, insdest)
struct triedge *instri;
REAL angle;
point insapex;
point insorg;
point insdest;
{
  struct badface *newface;
  int queuenumber;

  if (verbose > 2) {
    printf("  Queueing bad triangle:\n");
    printf("    (%.12g, %.12g) (%.12g, %.12g) (%.12g, %.12g)\n", insorg[0],
           insorg[1], insdest[0], insdest[1], insapex[0], insapex[1]);
  }
  /* Allocate space for the bad triangle. */
  newface = (struct badface *) poolalloc(&badtriangles);
  triedgecopy(*instri, newface->badfacetri);
  newface->key = angle;
  newface->faceapex = insapex;
  newface->faceorg = insorg;
  newface->facedest = insdest;
  newface->nextface = (struct badface *) NULL;
  /* Determine the appropriate queue to put the bad triangle into. */
  if (angle > 0.6) {
    queuenumber = (int) (160.0 * (angle - 0.6));
    if (queuenumber > 63) {
      queuenumber = 63;
    }
  } else {
    /* It's not a bad angle; put the triangle in the lowest-priority queue. */
    queuenumber = 0;
  }
  /* Add the triangle to the end of a queue. */
  *queuetail[queuenumber] = newface;
  /* Maintain a pointer to the NULL pointer at the end of the queue. */
  queuetail[queuenumber] = &newface->nextface;
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  dequeuebadtri()   Remove a triangle from the front of the queue.         */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

struct badface *dequeuebadtri()
{
  struct badface *result;
  int queuenumber;

  /* Look for a nonempty queue. */
  for (queuenumber = 63; queuenumber >= 0; queuenumber--) {
    result = queuefront[queuenumber];
    if (result != (struct badface *) NULL) {
      /* Remove the triangle from the queue. */
      queuefront[queuenumber] = result->nextface;
      /* Maintain a pointer to the NULL pointer at the end of the queue. */
      if (queuefront[queuenumber] == (struct badface *) NULL) {
        queuetail[queuenumber] = &queuefront[queuenumber];
      }
      return result;
    }
  }
  return (struct badface *) NULL;
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  checkedge4encroach()   Check a segment to see if it is encroached; add   */
/*                         it to the list if it is.                          */
/*                                                                           */
/*  An encroached segment is an unflippable edge that has a point in its     */
/*  diametral circle (that is, it faces an angle greater than 90 degrees).   */
/*  This definition is due to Ruppert.                                       */
/*                                                                           */
/*  Returns a nonzero value if the edge is encroached.                       */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

int checkedge4encroach(testedge)
struct edge *testedge;
{
  struct triedge neighbortri;
  struct edge testsym;
  struct edge *badedge;
  int addtolist;
  int sides;
  point eorg, edest, eapex;
  triangle ptr;                     /* Temporary variable used by stpivot(). */

  addtolist = 0;
  sides = 0;

  sorg(*testedge, eorg);
  sdest(*testedge, edest);
  /* Check one neighbor of the shell edge. */
  stpivot(*testedge, neighbortri);
  /* Does the neighbor exist, or is this a boundary edge? */
  if (neighbortri.tri != dummytri) {
    sides++;
    /* Find a vertex opposite this edge. */
    apex(neighbortri, eapex);
    /* Check whether the vertex is inside the diametral circle of the  */
    /*   shell edge.  Pythagoras' Theorem is used to check whether the */
    /*   angle at the vertex is greater than 90 degrees.               */
    if (eapex[0] * (eorg[0] + edest[0]) + eapex[1] * (eorg[1] + edest[1]) >
        eapex[0] * eapex[0] + eorg[0] * edest[0] +
        eapex[1] * eapex[1] + eorg[1] * edest[1]) {
      addtolist = 1;
    }
  }
  /* Check the other neighbor of the shell edge. */
  ssym(*testedge, testsym);
  stpivot(testsym, neighbortri);
  /* Does the neighbor exist, or is this a boundary edge? */
  if (neighbortri.tri != dummytri) {
    sides++;
    /* Find the other vertex opposite this edge. */
    apex(neighbortri, eapex);
    /* Check whether the vertex is inside the diametral circle of the  */
    /*   shell edge.  Pythagoras' Theorem is used to check whether the */
    /*   angle at the vertex is greater than 90 degrees.               */
    if (eapex[0] * (eorg[0] + edest[0]) +
        eapex[1] * (eorg[1] + edest[1]) >
        eapex[0] * eapex[0] + eorg[0] * edest[0] +
        eapex[1] * eapex[1] + eorg[1] * edest[1]) {
      addtolist += 2;
    }
  }

  if (addtolist && (!nobisect || ((nobisect == 1) && (sides == 2)))) {
    if (verbose > 2) {
      printf("  Queueing encroached segment (%.12g, %.12g) (%.12g, %.12g).\n",
             eorg[0], eorg[1], edest[0], edest[1]);
    }
    /* Add the shell edge to the list of encroached segments. */
    /*   Be sure to get the orientation right.                */
    badedge = (struct edge *) poolalloc(&badsegments);
    if (addtolist == 1) {
      shellecopy(*testedge, *badedge);
    } else {
      shellecopy(testsym, *badedge);
    }
  }
  return addtolist;
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  testtriangle()   Test a face for quality measures.                       */
/*                                                                           */
/*  Tests a triangle to see if it satisfies the minimum angle condition and  */
/*  the maximum area condition.  Triangles that aren't up to spec are added  */
/*  to the bad triangle queue.                                               */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void testtriangle(testtri)
struct triedge *testtri;
{
  struct triedge sametesttri;
  struct edge edge1, edge2;
  point torg, tdest, tapex;
  point anglevertex;
  REAL dxod, dyod, dxda, dyda, dxao, dyao;
  REAL dxod2, dyod2, dxda2, dyda2, dxao2, dyao2;
  REAL apexlen, orglen, destlen;
  REAL angle;
  REAL area;
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  org(*testtri, torg);
  dest(*testtri, tdest);
  apex(*testtri, tapex);
  dxod = torg[0] - tdest[0];
  dyod = torg[1] - tdest[1];
  dxda = tdest[0] - tapex[0];
  dyda = tdest[1] - tapex[1];
  dxao = tapex[0] - torg[0];
  dyao = tapex[1] - torg[1];
  dxod2 = dxod * dxod;
  dyod2 = dyod * dyod;
  dxda2 = dxda * dxda;
  dyda2 = dyda * dyda;
  dxao2 = dxao * dxao;
  dyao2 = dyao * dyao;
  /* Find the lengths of the triangle's three edges. */
  apexlen = dxod2 + dyod2;
  orglen = dxda2 + dyda2;
  destlen = dxao2 + dyao2;
  if ((apexlen < orglen) && (apexlen < destlen)) {
    /* The edge opposite the apex is shortest. */
    /* Find the square of the cosine of the angle at the apex. */
    angle = dxda * dxao + dyda * dyao;
    angle = angle * angle / (orglen * destlen);
    anglevertex = tapex;
    lnext(*testtri, sametesttri);
    tspivot(sametesttri, edge1);
    lnextself(sametesttri);
    tspivot(sametesttri, edge2);
  } else if (orglen < destlen) {
    /* The edge opposite the origin is shortest. */
    /* Find the square of the cosine of the angle at the origin. */
    angle = dxod * dxao + dyod * dyao;
    angle = angle * angle / (apexlen * destlen);
    anglevertex = torg;
    tspivot(*testtri, edge1);
    lprev(*testtri, sametesttri);
    tspivot(sametesttri, edge2);
  } else {
    /* The edge opposite the destination is shortest. */
    /* Find the square of the cosine of the angle at the destination. */
    angle = dxod * dxda + dyod * dyda;
    angle = angle * angle / (apexlen * orglen);
    anglevertex = tdest;
    tspivot(*testtri, edge1);
    lnext(*testtri, sametesttri);
    tspivot(sametesttri, edge2);
  }
  /* Check if both edges that form the angle are segments. */
  if ((edge1.sh != dummysh) && (edge2.sh != dummysh)) {
    /* The angle is a segment intersection. */
    if ((angle > 0.9924) && !quiet) {                  /* Roughly 5 degrees. */
      if (angle > 1.0) {
        /* Beware of a floating exception in acos(). */
        angle = 1.0;
      }
      /* Find the actual angle in degrees, for printing. */
      angle = acos(sqrt(angle)) * (180.0 / PI);
      printf("Warning:  Small angle (%.4g degrees) between segments at point\n", angle);
      printf("  (%.12g, %.12g)\n", anglevertex[0], anglevertex[1]);
    }
    /* Don't add this bad triangle to the list; there's nothing that */
    /*   can be done about a small angle between two segments.       */
    angle = 0.0;
  }
  /* Check whether the angle is smaller than permitted. */
  if (angle > goodangle) {
    /* Add this triangle to the list of bad triangles. */
    enqueuebadtri(testtri, angle, tapex, torg, tdest);
    return;
  }
  if (vararea || fixedarea) {
    /* Check whether the area is larger than permitted. */
    area = 0.5 * (dxod * dyda - dyod * dxda);
    if (fixedarea && (area > maxarea)) {
      /* Add this triangle to the list of bad triangles. */
      enqueuebadtri(testtri, angle, tapex, torg, tdest);
    } else if (vararea) {
      /* Nonpositive area constraints are treated as unconstrained. */
      if ((area > areabound(*testtri)) && (areabound(*testtri) > 0.0)) {
        /* Add this triangle to the list of bad triangles. */
        enqueuebadtri(testtri, angle, tapex, torg, tdest);
      }
    }
  }
}

#endif /* not CDT_ONLY */

/**                                                                         **/
/**                                                                         **/
/********* Mesh quality testing routines end here                    *********/

/********* Point location routines begin here                        *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  makepointmap()   Construct a mapping from points to triangles to improve  */
/*                  the speed of point location for segment insertion.       */
/*                                                                           */
/*  Traverses all the triangles, and provides each corner of each triangle   */
/*  with a pointer to that triangle.  Of course, pointers will be            */
/*  overwritten by other pointers because (almost) each point is a corner    */
/*  of several triangles, but in the end every point will point to some      */
/*  triangle that contains it.                                               */
/*                                                                           */
/*****************************************************************************/

void makepointmap()
{
  struct triedge triangleloop;
  point triorg;

  if (verbose) {
    printf("    Constructing mapping from points to triangles.\n");
  }
  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  while (triangleloop.tri != (triangle *) NULL) {
    /* Check all three points of the triangle. */
    for (triangleloop.orient = 0; triangleloop.orient < 3; triangleloop.orient++) {
      org(triangleloop, triorg);
      setpoint2tri(triorg, encode(triangleloop));
    }
    triangleloop.tri = triangletraverse();
  }
}

/*****************************************************************************/
/*                                                                           */
/*  preciselocate()   Find a triangle or edge containing a given point.      */
/*                                                                           */
/*  Begins its search from `searchtri'.  It is important that `searchtri'    */
/*  be a handle with the property that `searchpoint' is strictly to the left */
/*  of the edge denoted by `searchtri', or is collinear with that edge and   */
/*  does not intersect that edge.  (In particular, `searchpoint' should not  */
/*  be the origin or destination of that edge.)                              */
/*                                                                           */
/*  These conditions are imposed because preciselocate() is normally used in */
/*  one of two situations:                                                   */
/*                                                                           */
/*  (1)  To try to find the location to insert a new point.  Normally, we    */
/*       know an edge that the point is strictly to the left of.  In the     */
/*       incremental Delaunay algorithm, that edge is a bounding box edge.   */
/*       In Ruppert's Delaunay refinement algorithm for quality meshing,     */
/*       that edge is the shortest edge of the triangle whose circumcenter   */
/*       is being inserted.                                                  */
/*                                                                           */
/*  (2)  To try to find an existing point.  In this case, any edge on the    */
/*       convex hull is a good starting edge.  The possibility that the      */
/*       vertex one seeks is an endpoint of the starting edge must be        */
/*       screened out before preciselocate() is called.                      */
/*                                                                           */
/*  On completion, `searchtri' is a triangle that contains `searchpoint'.    */
/*                                                                           */
/*  This implementation differs from that given by Guibas and Stolfi.  It    */
/*  walks from triangle to triangle, crossing an edge only if `searchpoint'  */
/*  is on the other side of the line containing that edge.  After entering   */
/*  a triangle, there are two edges by which one can leave that triangle.    */
/*  If both edges are valid (`searchpoint' is on the other side of both      */
/*  edges), one of the two is chosen by drawing a line perpendicular to      */
/*  the entry edge (whose endpoints are `forg' and `fdest') passing through  */
/*  `fapex'.  Depending on which side of this perpendicular `searchpoint'    */
/*  falls on, an exit edge is chosen.                                        */
/*                                                                           */
/*  This implementation is empirically faster than the Guibas and Stolfi     */
/*  point location routine (which I originally used), which tends to spiral  */
/*  in toward its target.                                                    */
/*                                                                           */
/*  Returns ONVERTEX if the point lies on an existing vertex.  `searchtri'   */
/*  is a handle whose origin is the existing vertex.                         */
/*                                                                           */
/*  Returns ONEDGE if the point lies on a mesh edge.  `searchtri' is a       */
/*  handle whose primary edge is the edge on which the point lies.           */
/*                                                                           */
/*  Returns INTRIANGLE if the point lies strictly within a triangle.         */
/*  `searchtri' is a handle on the triangle that contains the point.         */
/*                                                                           */
/*  Returns OUTSIDE if the point lies outside the mesh.  `searchtri' is a    */
/*  handle whose primary edge the point is to the right of.  This might      */
/*  occur when the circumcenter of a triangle falls just slightly outside    */
/*  the mesh due to floating-point roundoff error.  It also occurs when      */
/*  seeking a hole or region point that a foolish user has placed outside    */
/*  the mesh.                                                                */
/*                                                                           */
/*  WARNING:  This routine is designed for convex triangulations, and will   */
/*  not generally work after the holes and concavities have been carved.     */
/*  However, it can still be used to find the circumcenter of a triangle, as */
/*  long as the search is begun from the triangle in question.               */
/*                                                                           */
/*****************************************************************************/

enum locateresult preciselocate(searchpoint, searchtri)
point searchpoint;
struct triedge *searchtri;
{
  struct triedge backtracktri;
  point forg, fdest, fapex;
  point swappoint;
  REAL orgorient, destorient;
  int moveleft;
  triangle ptr;                         /* Temporary variable used by sym(). */

  if (verbose > 2) {
    printf("  Searching for point (%.12g, %.12g).\n",
           searchpoint[0], searchpoint[1]);
  }
  /* Where are we? */
  org(*searchtri, forg);
  dest(*searchtri, fdest);
  apex(*searchtri, fapex);
  while (1) {
    if (verbose > 2) {
      printf("    At (%.12g, %.12g) (%.12g, %.12g) (%.12g, %.12g)\n",
             forg[0], forg[1], fdest[0], fdest[1], fapex[0], fapex[1]);
    }
    /* Check whether the apex is the point we seek. */
    if ((fapex[0] == searchpoint[0]) && (fapex[1] == searchpoint[1])) {
      lprevself(*searchtri);
      return ONVERTEX;
    }
    /* Does the point lie on the other side of the line defined by the */
    /*   triangle edge opposite the triangle's destination?            */
    destorient = counterclockwise(forg, fapex, searchpoint);
    /* Does the point lie on the other side of the line defined by the */
    /*   triangle edge opposite the triangle's origin?                 */
    orgorient = counterclockwise(fapex, fdest, searchpoint);
    if (destorient > 0.0) {
      if (orgorient > 0.0) {
        /* Move left if the inner product of (fapex - searchpoint) and  */
        /*   (fdest - forg) is positive.  This is equivalent to drawing */
        /*   a line perpendicular to the line (forg, fdest) passing     */
        /*   through `fapex', and determining which side of this line   */
        /*   `searchpoint' falls on.                                    */
        moveleft = (fapex[0] - searchpoint[0]) * (fdest[0] - forg[0]) +
                   (fapex[1] - searchpoint[1]) * (fdest[1] - forg[1]) > 0.0;
      } else {
        moveleft = 1;
      }
    } else {
      if (orgorient > 0.0) {
        moveleft = 0;
      } else {
        /* The point we seek must be on the boundary of or inside this */
        /*   triangle.                                                 */
        if (destorient == 0.0) {
          lprevself(*searchtri);
          return ONEDGE;
        }
        if (orgorient == 0.0) {
          lnextself(*searchtri);
          return ONEDGE;
        }
        return INTRIANGLE;
      }
    }

    /* Move to another triangle.  Leave a trace `backtracktri' in case */
    /*   floating-point roundoff or some such bogey causes us to walk  */
    /*   off a boundary of the triangulation.  We can just bounce off  */
    /*   the boundary as if it were an elastic band.                   */
    if (moveleft) {
      lprev(*searchtri, backtracktri);
      fdest = fapex;
    } else {
      lnext(*searchtri, backtracktri);
      forg = fapex;
    }
    sym(backtracktri, *searchtri);

    /* Check for walking off the edge. */
    if (searchtri->tri == dummytri) {
      /* Turn around. */
      triedgecopy(backtracktri, *searchtri);
      swappoint = forg;
      forg = fdest;
      fdest = swappoint;
      apex(*searchtri, fapex);
      /* Check if the point really is beyond the triangulation boundary. */
      destorient = counterclockwise(forg, fapex, searchpoint);
      orgorient = counterclockwise(fapex, fdest, searchpoint);
      if ((orgorient < 0.0) && (destorient < 0.0)) {
        return OUTSIDE;
      }
    } else {
      apex(*searchtri, fapex);
    }
  }
}

/*****************************************************************************/
/*                                                                           */
/*  locate()   Find a triangle or edge containing a given point.             */
/*                                                                           */
/*  Searching begins from one of:  the input `searchtri', a recently         */
/*  encountered triangle `recenttri', or from a triangle chosen from a       */
/*  random sample.  The choice is made by determining which triangle's       */
/*  origin is closest to the point we are searcing for.  Normally,           */
/*  `searchtri' should be a handle on the convex hull of the triangulation.  */
/*                                                                           */
/*  Details on the random sampling method can be found in the Mucke, Saias,  */
/*  and Zhu paper cited in the header of this code.                          */
/*                                                                           */
/*  On completion, `searchtri' is a triangle that contains `searchpoint'.    */
/*                                                                           */
/*  Returns ONVERTEX if the point lies on an existing vertex.  `searchtri'   */
/*  is a handle whose origin is the existing vertex.                         */
/*                                                                           */
/*  Returns ONEDGE if the point lies on a mesh edge.  `searchtri' is a       */
/*  handle whose primary edge is the edge on which the point lies.           */
/*                                                                           */
/*  Returns INTRIANGLE if the point lies strictly within a triangle.         */
/*  `searchtri' is a handle on the triangle that contains the point.         */
/*                                                                           */
/*  Returns OUTSIDE if the point lies outside the mesh.  `searchtri' is a    */
/*  handle whose primary edge the point is to the right of.  This might      */
/*  occur when the circumcenter of a triangle falls just slightly outside    */
/*  the mesh due to floating-point roundoff error.  It also occurs when      */
/*  seeking a hole or region point that a foolish user has placed outside    */
/*  the mesh.                                                                */
/*                                                                           */
/*  WARNING:  This routine is designed for convex triangulations, and will   */
/*  not generally work after the holes and concavities have been carved.     */
/*                                                                           */
/*****************************************************************************/

enum locateresult locate(searchpoint, searchtri)
point searchpoint;
struct triedge *searchtri;
{
  VOID **sampleblock;
  triangle *firsttri;
  struct triedge sampletri;
  point torg, tdest;
  intptr_t alignptr;
  REAL searchdist, dist;
  REAL ahead;
  long sampleblocks, samplesperblock, samplenum;
  long triblocks;
  long i, j;
  triangle ptr;                         /* Temporary variable used by sym(). */

  if (verbose > 2) {
    printf("  Randomly sampling for a triangle near point (%.12g, %.12g).\n",
           searchpoint[0], searchpoint[1]);
  }
  /* Record the distance from the suggested starting triangle to the */
  /*   point we seek.                                                */
  org(*searchtri, torg);
  searchdist = (searchpoint[0] - torg[0]) * (searchpoint[0] - torg[0])
             + (searchpoint[1] - torg[1]) * (searchpoint[1] - torg[1]);
  if (verbose > 2) {
    printf("    Boundary triangle has origin (%.12g, %.12g).\n",
           torg[0], torg[1]);
  }

  /* If a recently encountered triangle has been recorded and has not been */
  /*   deallocated, test it as a good starting point.                      */
  if (recenttri.tri != (triangle *) NULL) {
    if (recenttri.tri[3] != (triangle) NULL) {
      org(recenttri, torg);
      if ((torg[0] == searchpoint[0]) && (torg[1] == searchpoint[1])) {
        triedgecopy(recenttri, *searchtri);
        return ONVERTEX;
      }
      dist = (searchpoint[0] - torg[0]) * (searchpoint[0] - torg[0])
           + (searchpoint[1] - torg[1]) * (searchpoint[1] - torg[1]);
      if (dist < searchdist) {
        triedgecopy(recenttri, *searchtri);
        searchdist = dist;
        if (verbose > 2) {
          printf("    Choosing recent triangle with origin (%.12g, %.12g).\n",
                 torg[0], torg[1]);
        }
      }
    }
  }

  /* The number of random samples taken is proportional to the cube root of */
  /*   the number of triangles in the mesh.  The next bit of code assumes   */
  /*   that the number of triangles increases monotonically.                */
  while (SAMPLEFACTOR * samples * samples * samples < triangles.items) {
    samples++;
  }
  triblocks = (triangles.maxitems + TRIPERBLOCK - 1) / TRIPERBLOCK;
  samplesperblock = 1 + (samples / triblocks);
  sampleblocks = samples / samplesperblock;
  sampleblock = triangles.firstblock;
  sampletri.orient = 0;
  for (i = 0; i < sampleblocks; i++) {
    alignptr = (intptr_t) (sampleblock + 1);
    firsttri = (triangle *) (alignptr + (intptr_t) triangles.alignbytes
                          - (alignptr % (intptr_t) triangles.alignbytes));
    for (j = 0; j < samplesperblock; j++) {
      if (i == triblocks - 1) {
        samplenum = randomnation((int)
                                 (triangles.maxitems - (i * TRIPERBLOCK)));
      } else {
        samplenum = randomnation(TRIPERBLOCK);
      }
      sampletri.tri = (triangle *)
                      (firsttri + (samplenum * triangles.itemwords));
      if (sampletri.tri[3] != (triangle) NULL) {
        org(sampletri, torg);
        dist = (searchpoint[0] - torg[0]) * (searchpoint[0] - torg[0])
             + (searchpoint[1] - torg[1]) * (searchpoint[1] - torg[1]);
        if (dist < searchdist) {
          triedgecopy(sampletri, *searchtri);
          searchdist = dist;
          if (verbose > 2) {
            printf("    Choosing triangle with origin (%.12g, %.12g).\n",
                   torg[0], torg[1]);
          }
        }
      }
    }
    sampleblock = (VOID **) *sampleblock;
  }
  /* Where are we? */
  org(*searchtri, torg);
  dest(*searchtri, tdest);
  /* Check the starting triangle's vertices. */
  if ((torg[0] == searchpoint[0]) && (torg[1] == searchpoint[1])) {
    return ONVERTEX;
  }
  if ((tdest[0] == searchpoint[0]) && (tdest[1] == searchpoint[1])) {
    lnextself(*searchtri);
    return ONVERTEX;
  }
  /* Orient `searchtri' to fit the preconditions of calling preciselocate(). */
  ahead = counterclockwise(torg, tdest, searchpoint);
  if (ahead < 0.0) {
    /* Turn around so that `searchpoint' is to the left of the */
    /*   edge specified by `searchtri'.                        */
    symself(*searchtri);
  } else if (ahead == 0.0) {
    /* Check if `searchpoint' is between `torg' and `tdest'. */
    if (((torg[0] < searchpoint[0]) == (searchpoint[0] < tdest[0]))
        && ((torg[1] < searchpoint[1]) == (searchpoint[1] < tdest[1]))) {
      return ONEDGE;
    }
  }
  return preciselocate(searchpoint, searchtri);
}

/**                                                                         **/
/**                                                                         **/
/********* Point location routines end here                          *********/

/********* Mesh transformation routines begin here                   *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  insertshelle()   Create a new shell edge and insert it between two       */
/*                   triangles.                                              */
/*                                                                           */
/*  The new shell edge is inserted at the edge described by the handle       */
/*  `tri'.  Its vertices are properly initialized.  The marker `shellemark'  */
/*  is applied to the shell edge and, if appropriate, its vertices.          */
/*                                                                           */
/*****************************************************************************/

void insertshelle(tri, shellemark)
struct triedge *tri;          /* Edge at which to insert the new shell edge. */
int shellemark;                            /* Marker for the new shell edge. */
{
  struct triedge oppotri;
  struct edge newshelle;
  point triorg, tridest;
  triangle ptr;                         /* Temporary variable used by sym(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  /* Mark points if possible. */
  org(*tri, triorg);
  dest(*tri, tridest);
  if (pointmark(triorg) == 0) {
    setpointmark(triorg, shellemark);
  }
  if (pointmark(tridest) == 0) {
    setpointmark(tridest, shellemark);
  }
  /* Check if there's already a shell edge here. */
  tspivot(*tri, newshelle);
  if (newshelle.sh == dummysh) {
    /* Make new shell edge and initialize its vertices. */
    makeshelle(&newshelle);
    setsorg(newshelle, tridest);
    setsdest(newshelle, triorg);
    /* Bond new shell edge to the two triangles it is sandwiched between. */
    /*   Note that the facing triangle `oppotri' might be equal to        */
    /*   `dummytri' (outer space), but the new shell edge is bonded to it */
    /*   all the same.                                                    */
    tsbond(*tri, newshelle);
    sym(*tri, oppotri);
    ssymself(newshelle);
    tsbond(oppotri, newshelle);
    setmark(newshelle, shellemark);
    if (verbose > 2) {
      printf("  Inserting new ");
      printshelle(&newshelle);
    }
  } else {
    if (mark(newshelle) == 0) {
      setmark(newshelle, shellemark);
    }
  }
}

/*****************************************************************************/
/*                                                                           */
/*  Terminology                                                              */
/*                                                                           */
/*  A "local transformation" replaces a small set of triangles with another  */
/*  set of triangles.  This may or may not involve inserting or deleting a   */
/*  point.                                                                   */
/*                                                                           */
/*  The term "casing" is used to describe the set of triangles that are      */
/*  attached to the triangles being transformed, but are not transformed     */
/*  themselves.  Think of the casing as a fixed hollow structure inside      */
/*  which all the action happens.  A "casing" is only defined relative to    */
/*  a single transformation; each occurrence of a transformation will        */
/*  involve a different casing.                                              */
/*                                                                           */
/*  A "shell" is similar to a "casing".  The term "shell" describes the set  */
/*  of shell edges (if any) that are attached to the triangles being         */
/*  transformed.  However, I sometimes use "shell" to refer to a single      */
/*  shell edge, so don't get confused.                                       */
/*                                                                           */
/*****************************************************************************/

/*****************************************************************************/
/*                                                                           */
/*  flip()   Transform two triangles to two different triangles by flipping  */
/*           an edge within a quadrilateral.                                 */
/*                                                                           */
/*  Imagine the original triangles, abc and bad, oriented so that the        */
/*  shared edge ab lies in a horizontal plane, with the point b on the left  */
/*  and the point a on the right.  The point c lies below the edge, and the  */
/*  point d lies above the edge.  The `flipedge' handle holds the edge ab    */
/*  of triangle abc, and is directed left, from vertex a to vertex b.        */
/*                                                                           */
/*  The triangles abc and bad are deleted and replaced by the triangles cdb  */
/*  and dca.  The triangles that represent abc and bad are NOT deallocated;  */
/*  they are reused for dca and cdb, respectively.  Hence, any handles that  */
/*  may have held the original triangles are still valid, although not       */
/*  directed as they were before.                                            */
/*                                                                           */
/*  Upon completion of this routine, the `flipedge' handle holds the edge    */
/*  dc of triangle dca, and is directed down, from vertex d to vertex c.     */
/*  (Hence, the two triangles have rotated counterclockwise.)                */
/*                                                                           */
/*  WARNING:  This transformation is geometrically valid only if the         */
/*  quadrilateral adbc is convex.  Furthermore, this transformation is       */
/*  valid only if there is not a shell edge between the triangles abc and    */
/*  bad.  This routine does not check either of these preconditions, and     */
/*  it is the responsibility of the calling routine to ensure that they are  */
/*  met.  If they are not, the streets shall be filled with wailing and      */
/*  gnashing of teeth.                                                       */
/*                                                                           */
/*****************************************************************************/

void flip(flipedge)
struct triedge *flipedge;                    /* Handle for the triangle abc. */
{
  struct triedge botleft, botright;
  struct triedge topleft, topright;
  struct triedge top;
  struct triedge botlcasing, botrcasing;
  struct triedge toplcasing, toprcasing;
  struct edge botlshelle, botrshelle;
  struct edge toplshelle, toprshelle;
  point leftpoint, rightpoint, botpoint;
  point farpoint;
  triangle ptr;                         /* Temporary variable used by sym(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  /* Identify the vertices of the quadrilateral. */
  org(*flipedge, rightpoint);
  dest(*flipedge, leftpoint);
  apex(*flipedge, botpoint);
  sym(*flipedge, top);
#ifdef SELF_CHECK
  if (top.tri == dummytri) {
    printf("Internal error in flip():  Attempt to flip on boundary.\n");
    lnextself(*flipedge);
    return;
  }
  if (checksegments) {
    tspivot(*flipedge, toplshelle);
    if (toplshelle.sh != dummysh) {
      printf("Internal error in flip():  Attempt to flip a segment.\n");
      lnextself(*flipedge);
      return;
    }
  }
#endif /* SELF_CHECK */
  apex(top, farpoint);

  /* Identify the casing of the quadrilateral. */
  lprev(top, topleft);
  sym(topleft, toplcasing);
  lnext(top, topright);
  sym(topright, toprcasing);
  lnext(*flipedge, botleft);
  sym(botleft, botlcasing);
  lprev(*flipedge, botright);
  sym(botright, botrcasing);
  /* Rotate the quadrilateral one-quarter turn counterclockwise. */
  bond(topleft, botlcasing);
  bond(botleft, botrcasing);
  bond(botright, toprcasing);
  bond(topright, toplcasing);

  if (checksegments) {
    /* Check for shell edges and rebond them to the quadrilateral. */
    tspivot(topleft, toplshelle);
    tspivot(botleft, botlshelle);
    tspivot(botright, botrshelle);
    tspivot(topright, toprshelle);
    if (toplshelle.sh == dummysh) {
      tsdissolve(topright);
    } else {
      tsbond(topright, toplshelle);
    }
    if (botlshelle.sh == dummysh) {
      tsdissolve(topleft);
    } else {
      tsbond(topleft, botlshelle);
    }
    if (botrshelle.sh == dummysh) {
      tsdissolve(botleft);
    } else {
      tsbond(botleft, botrshelle);
    }
    if (toprshelle.sh == dummysh) {
      tsdissolve(botright);
    } else {
      tsbond(botright, toprshelle);
    }
  }

  /* New point assignments for the rotated quadrilateral. */
  setorg(*flipedge, farpoint);
  setdest(*flipedge, botpoint);
  setapex(*flipedge, rightpoint);
  setorg(top, botpoint);
  setdest(top, farpoint);
  setapex(top, leftpoint);
  if (verbose > 2) {
    printf("  Edge flip results in left ");
    lnextself(topleft);
    printtriangle(&topleft);
    printf("  and right ");
    printtriangle(flipedge);
  }
}

/*****************************************************************************/
/*                                                                           */
/*  insertsite()   Insert a vertex into a Delaunay triangulation,            */
/*                 performing flips as necessary to maintain the Delaunay    */
/*                 property.                                                 */
/*                                                                           */
/*  The point `insertpoint' is located.  If `searchtri.tri' is not NULL,     */
/*  the search for the containing triangle begins from `searchtri'.  If      */
/*  `searchtri.tri' is NULL, a full point location procedure is called.      */
/*  If `insertpoint' is found inside a triangle, the triangle is split into  */
/*  three; if `insertpoint' lies on an edge, the edge is split in two,       */
/*  thereby splitting the two adjacent triangles into four.  Edge flips are  */
/*  used to restore the Delaunay property.  If `insertpoint' lies on an      */
/*  existing vertex, no action is taken, and the value DUPLICATEPOINT is     */
/*  returned.  On return, `searchtri' is set to a handle whose origin is the */
/*  existing vertex.                                                         */
/*                                                                           */
/*  Normally, the parameter `splitedge' is set to NULL, implying that no     */
/*  segment should be split.  In this case, if `insertpoint' is found to     */
/*  lie on a segment, no action is taken, and the value VIOLATINGPOINT is    */
/*  returned.  On return, `searchtri' is set to a handle whose primary edge  */
/*  is the violated segment.                                                 */
/*                                                                           */
/*  If the calling routine wishes to split a segment by inserting a point in */
/*  it, the parameter `splitedge' should be that segment.  In this case,     */
/*  `searchtri' MUST be the triangle handle reached by pivoting from that    */
/*  segment; no point location is done.                                      */
/*                                                                           */
/*  `segmentflaws' and `triflaws' are flags that indicate whether or not     */
/*  there should be checks for the creation of encroached segments or bad    */
/*  quality faces.  If a newly inserted point encroaches upon segments,      */
/*  these segments are added to the list of segments to be split if          */
/*  `segmentflaws' is set.  If bad triangles are created, these are added    */
/*  to the queue if `triflaws' is set.                                       */
/*                                                                           */
/*  If a duplicate point or violated segment does not prevent the point      */
/*  from being inserted, the return value will be ENCROACHINGPOINT if the    */
/*  point encroaches upon a segment (and checking is enabled), or            */
/*  SUCCESSFULPOINT otherwise.  In either case, `searchtri' is set to a      */
/*  handle whose origin is the newly inserted vertex.                        */
/*                                                                           */
/*  insertsite() does not use flip() for reasons of speed; some              */
/*  information can be reused from edge flip to edge flip, like the          */
/*  locations of shell edges.                                                */
/*                                                                           */
/*****************************************************************************/

enum insertsiteresult insertsite(insertpoint, searchtri, splitedge,
                                 segmentflaws, triflaws)
point insertpoint;
struct triedge *searchtri;
struct edge *splitedge;
int segmentflaws;
int triflaws;
{
  struct triedge horiz;
  struct triedge top;
  struct triedge botleft, botright;
  struct triedge topleft, topright;
  struct triedge newbotleft, newbotright;
  struct triedge newtopright;
  struct triedge botlcasing, botrcasing;
  struct triedge toplcasing, toprcasing;
  struct triedge testtri;
  struct edge botlshelle, botrshelle;
  struct edge toplshelle, toprshelle;
  struct edge brokenshelle;
  struct edge checkshelle;
  struct edge rightedge;
  struct edge newedge;
  struct edge *encroached;
  point first;
  point leftpoint, rightpoint, botpoint, toppoint, farpoint;
  REAL attrib;
  REAL area;
  enum insertsiteresult success;
  enum locateresult intersect;
  int doflip;
  int mirrorflag;
  int i;
  triangle ptr;                         /* Temporary variable used by sym(). */
  shelle sptr;         /* Temporary variable used by spivot() and tspivot(). */

  if (verbose > 1) {
    printf("  Inserting (%.12g, %.12g).\n", insertpoint[0], insertpoint[1]);
  }
  if (splitedge == (struct edge *) NULL) {
    /* Find the location of the point to be inserted.  Check if a good */
    /*   starting triangle has already been provided by the caller.    */
    if (searchtri->tri == (triangle *) NULL) {
      /* Find a boundary triangle. */
      horiz.tri = dummytri;
      horiz.orient = 0;
      symself(horiz);
      /* Search for a triangle containing `insertpoint'. */
      intersect = locate(insertpoint, &horiz);
    } else {
      /* Start searching from the triangle provided by the caller. */
      triedgecopy(*searchtri, horiz);
      intersect = preciselocate(insertpoint, &horiz);
    }
  } else {
    /* The calling routine provides the edge in which the point is inserted. */
    triedgecopy(*searchtri, horiz);
    intersect = ONEDGE;
  }
  if (intersect == ONVERTEX) {
    /* There's already a vertex there.  Return in `searchtri' a triangle */
    /*   whose origin is the existing vertex.                            */
    triedgecopy(horiz, *searchtri);
    triedgecopy(horiz, recenttri);
    return DUPLICATEPOINT;
  }
  if ((intersect == ONEDGE) || (intersect == OUTSIDE)) {
    /* The vertex falls on an edge or boundary. */
    if (checksegments && (splitedge == (struct edge *) NULL)) {
      /* Check whether the vertex falls on a shell edge. */
      tspivot(horiz, brokenshelle);
      if (brokenshelle.sh != dummysh) {
        /* The vertex falls on a shell edge. */
        if (segmentflaws) {
          if (nobisect == 0) {
            /* Add the shell edge to the list of encroached segments. */
            encroached = (struct edge *) poolalloc(&badsegments);
            shellecopy(brokenshelle, *encroached);
          } else if ((nobisect == 1) && (intersect == ONEDGE)) {
            /* This segment may be split only if it is an internal boundary. */
            sym(horiz, testtri);
            if (testtri.tri != dummytri) {
              /* Add the shell edge to the list of encroached segments. */
              encroached = (struct edge *) poolalloc(&badsegments);
              shellecopy(brokenshelle, *encroached);
            }
          }
        }
        /* Return a handle whose primary edge contains the point, */
        /*   which has not been inserted.                         */
        triedgecopy(horiz, *searchtri);
        triedgecopy(horiz, recenttri);
        return VIOLATINGPOINT;
      }
    }
    /* Insert the point on an edge, dividing one triangle into two (if */
    /*   the edge lies on a boundary) or two triangles into four.      */
    lprev(horiz, botright);
    sym(botright, botrcasing);
    sym(horiz, topright);
    /* Is there a second triangle?  (Or does this edge lie on a boundary?) */
    mirrorflag = topright.tri != dummytri;
    if (mirrorflag) {
      lnextself(topright);
      sym(topright, toprcasing);
      maketriangle(&newtopright);
    } else {
      /* Splitting the boundary edge increases the number of boundary edges. */
      hullsize++;
    }
    maketriangle(&newbotright);

    /* Set the vertices of changed and new triangles. */
    org(horiz, rightpoint);
    dest(horiz, leftpoint);
    apex(horiz, botpoint);
    setorg(newbotright, botpoint);
    setdest(newbotright, rightpoint);
    setapex(newbotright, insertpoint);
    setorg(horiz, insertpoint);
    for (i = 0; i < eextras; i++) {
      /* Set the element attributes of a new triangle. */
      setelemattribute(newbotright, i, elemattribute(botright, i));
    }
    if (vararea) {
      /* Set the area constraint of a new triangle. */
      setareabound(newbotright, areabound(botright));
    }
    if (mirrorflag) {
      dest(topright, toppoint);
      setorg(newtopright, rightpoint);
      setdest(newtopright, toppoint);
      setapex(newtopright, insertpoint);
      setorg(topright, insertpoint);
      for (i = 0; i < eextras; i++) {
        /* Set the element attributes of another new triangle. */
        setelemattribute(newtopright, i, elemattribute(topright, i));
      }
      if (vararea) {
        /* Set the area constraint of another new triangle. */
        setareabound(newtopright, areabound(topright));
      }
    }

    /* There may be shell edges that need to be bonded */
    /*   to the new triangle(s).                       */
    if (checksegments) {
      tspivot(botright, botrshelle);
      if (botrshelle.sh != dummysh) {
        tsdissolve(botright);
        tsbond(newbotright, botrshelle);
      }
      if (mirrorflag) {
        tspivot(topright, toprshelle);
        if (toprshelle.sh != dummysh) {
          tsdissolve(topright);
          tsbond(newtopright, toprshelle);
        }
      }
    }

    /* Bond the new triangle(s) to the surrounding triangles. */
    bond(newbotright, botrcasing);
    lprevself(newbotright);
    bond(newbotright, botright);
    lprevself(newbotright);
    if (mirrorflag) {
      bond(newtopright, toprcasing);
      lnextself(newtopright);
      bond(newtopright, topright);
      lnextself(newtopright);
      bond(newtopright, newbotright);
    }

    if (splitedge != (struct edge *) NULL) {
      /* Split the shell edge into two. */
      setsdest(*splitedge, insertpoint);
      ssymself(*splitedge);
      spivot(*splitedge, rightedge);
      insertshelle(&newbotright, mark(*splitedge));
      tspivot(newbotright, newedge);
      sbond(*splitedge, newedge);
      ssymself(newedge);
      sbond(newedge, rightedge);
      ssymself(*splitedge);
    }

#ifdef SELF_CHECK
    if (counterclockwise(rightpoint, leftpoint, botpoint) < 0.0) {
      printf("Internal error in insertsite():\n");
      printf("  Clockwise triangle prior to edge point insertion (bottom).\n");
    }
    if (mirrorflag) {
      if (counterclockwise(leftpoint, rightpoint, toppoint) < 0.0) {
        printf("Internal error in insertsite():\n");
        printf("  Clockwise triangle prior to edge point insertion (top).\n");
      }
      if (counterclockwise(rightpoint, toppoint, insertpoint) < 0.0) {
        printf("Internal error in insertsite():\n");
        printf("  Clockwise triangle after edge point insertion (top right).\n"
               );
      }
      if (counterclockwise(toppoint, leftpoint, insertpoint) < 0.0) {
        printf("Internal error in insertsite():\n");
        printf("  Clockwise triangle after edge point insertion (top left).\n"
               );
      }
    }
    if (counterclockwise(leftpoint, botpoint, insertpoint) < 0.0) {
      printf("Internal error in insertsite():\n");
      printf("  Clockwise triangle after edge point insertion (bottom left).\n"
             );
    }
    if (counterclockwise(botpoint, rightpoint, insertpoint) < 0.0) {
      printf("Internal error in insertsite():\n");
      printf("  Clockwise triangle after edge point insertion (bottom right).\n");
    }
#endif /* SELF_CHECK */
    if (verbose > 2) {
      printf("  Updating bottom left ");
      printtriangle(&botright);
      if (mirrorflag) {
        printf("  Updating top left ");
        printtriangle(&topright);
        printf("  Creating top right ");
        printtriangle(&newtopright);
      }
      printf("  Creating bottom right ");
      printtriangle(&newbotright);
    }

    /* Position `horiz' on the first edge to check for */
    /*   the Delaunay property.                        */
    lnextself(horiz);
  } else {
    /* Insert the point in a triangle, splitting it into three. */
    lnext(horiz, botleft);
    lprev(horiz, botright);
    sym(botleft, botlcasing);
    sym(botright, botrcasing);
    maketriangle(&newbotleft);
    maketriangle(&newbotright);

    /* Set the vertices of changed and new triangles. */
    org(horiz, rightpoint);
    dest(horiz, leftpoint);
    apex(horiz, botpoint);
    setorg(newbotleft, leftpoint);
    setdest(newbotleft, botpoint);
    setapex(newbotleft, insertpoint);
    setorg(newbotright, botpoint);
    setdest(newbotright, rightpoint);
    setapex(newbotright, insertpoint);
    setapex(horiz, insertpoint);
    for (i = 0; i < eextras; i++) {
      /* Set the element attributes of the new triangles. */
      attrib = elemattribute(horiz, i);
      setelemattribute(newbotleft, i, attrib);
      setelemattribute(newbotright, i, attrib);
    }
    if (vararea) {
      /* Set the area constraint of the new triangles. */
      area = areabound(horiz);
      setareabound(newbotleft, area);
      setareabound(newbotright, area);
    }

    /* There may be shell edges that need to be bonded */
    /*   to the new triangles.                         */
    if (checksegments) {
      tspivot(botleft, botlshelle);
      if (botlshelle.sh != dummysh) {
        tsdissolve(botleft);
        tsbond(newbotleft, botlshelle);
      }
      tspivot(botright, botrshelle);
      if (botrshelle.sh != dummysh) {
        tsdissolve(botright);
        tsbond(newbotright, botrshelle);
      }
    }

    /* Bond the new triangles to the surrounding triangles. */
    bond(newbotleft, botlcasing);
    bond(newbotright, botrcasing);
    lnextself(newbotleft);
    lprevself(newbotright);
    bond(newbotleft, newbotright);
    lnextself(newbotleft);
    bond(botleft, newbotleft);
    lprevself(newbotright);
    bond(botright, newbotright);

#ifdef SELF_CHECK
    if (counterclockwise(rightpoint, leftpoint, botpoint) < 0.0) {
      printf("Internal error in insertsite():\n");
      printf("  Clockwise triangle prior to point insertion.\n");
    }
    if (counterclockwise(rightpoint, leftpoint, insertpoint) < 0.0) {
      printf("Internal error in insertsite():\n");
      printf("  Clockwise triangle after point insertion (top).\n");
    }
    if (counterclockwise(leftpoint, botpoint, insertpoint) < 0.0) {
      printf("Internal error in insertsite():\n");
      printf("  Clockwise triangle after point insertion (left).\n");
    }
    if (counterclockwise(botpoint, rightpoint, insertpoint) < 0.0) {
      printf("Internal error in insertsite():\n");
      printf("  Clockwise triangle after point insertion (right).\n");
    }
#endif /* SELF_CHECK */
    if (verbose > 2) {
      printf("  Updating top ");
      printtriangle(&horiz);
      printf("  Creating left ");
      printtriangle(&newbotleft);
      printf("  Creating right ");
      printtriangle(&newbotright);
    }
  }

  /* The insertion is successful by default, unless an encroached */
  /*   edge is found.                                             */
  success = SUCCESSFULPOINT;
  /* Circle around the newly inserted vertex, checking each edge opposite */
  /*   it for the Delaunay property.  Non-Delaunay edges are flipped.     */
  /*   `horiz' is always the edge being checked.  `first' marks where to  */
  /*   stop circling.                                                     */
  org(horiz, first);
  rightpoint = first;
  dest(horiz, leftpoint);
  /* Circle until finished. */
  while (1) {
    /* By default, the edge will be flipped. */
    doflip = 1;
    if (checksegments) {
      /* Check for a segment, which cannot be flipped. */
      tspivot(horiz, checkshelle);
      if (checkshelle.sh != dummysh) {
        /* The edge is a segment and cannot be flipped. */
        doflip = 0;
#ifndef CDT_ONLY
        if (segmentflaws) {
          /* Does the new point encroach upon this segment? */
          if (checkedge4encroach(&checkshelle)) {
            success = ENCROACHINGPOINT;
          }
        }
#endif /* not CDT_ONLY */
      }
    }
    if (doflip) {
      /* Check if the edge is a boundary edge. */
      sym(horiz, top);
      if (top.tri == dummytri) {
        /* The edge is a boundary edge and cannot be flipped. */
        doflip = 0;
      } else {
        /* Find the point on the other side of the edge. */
        apex(top, farpoint);
        /* In the incremental Delaunay triangulation algorithm, any of    */
        /*   `leftpoint', `rightpoint', and `farpoint' could be vertices  */
        /*   of the triangular bounding box.  These vertices must be      */
        /*   treated as if they are infinitely distant, even though their */
        /*   "coordinates" are not.                                       */
        if ((leftpoint == infpoint1) ||
            (leftpoint == infpoint2) ||
            (leftpoint == infpoint3)) {
          /* `leftpoint' is infinitely distant.  Check the convexity of */
          /*   the boundary of the triangulation.  'farpoint' might be  */
          /*   infinite as well, but trust me, this same condition      */
          /*   should be applied.                                       */
          doflip = counterclockwise(insertpoint, rightpoint, farpoint) > 0.0;
        } else if ((rightpoint == infpoint1) ||
                   (rightpoint == infpoint2) ||
                   (rightpoint == infpoint3)) {
          /* `rightpoint' is infinitely distant.  Check the convexity of */
          /*   the boundary of the triangulation.  'farpoint' might be  */
          /*   infinite as well, but trust me, this same condition      */
          /*   should be applied.                                       */
          doflip = counterclockwise(farpoint, leftpoint, insertpoint) > 0.0;
        } else if ((farpoint == infpoint1) ||
                   (farpoint == infpoint2) ||
                   (farpoint == infpoint3)) {
          /* `farpoint' is infinitely distant and cannot be inside */
          /*   the circumcircle of the triangle `horiz'.           */
          doflip = 0;
        } else {
          /* Test whether the edge is locally Delaunay. */
          doflip = incircle(leftpoint, insertpoint, rightpoint, farpoint)
                   > 0.0;
        }
        if (doflip) {
          /* We made it!  Flip the edge `horiz' by rotating its containing */
          /*   quadrilateral (the two triangles adjacent to `horiz').      */
          /* Identify the casing of the quadrilateral. */
          lprev(top, topleft);
          sym(topleft, toplcasing);
          lnext(top, topright);
          sym(topright, toprcasing);
          lnext(horiz, botleft);
          sym(botleft, botlcasing);
          lprev(horiz, botright);
          sym(botright, botrcasing);
          /* Rotate the quadrilateral one-quarter turn counterclockwise. */
          bond(topleft, botlcasing);
          bond(botleft, botrcasing);
          bond(botright, toprcasing);
          bond(topright, toplcasing);
          if (checksegments) {
            /* Check for shell edges and rebond them to the quadrilateral. */
            tspivot(topleft, toplshelle);
            tspivot(botleft, botlshelle);
            tspivot(botright, botrshelle);
            tspivot(topright, toprshelle);
            if (toplshelle.sh == dummysh) {
              tsdissolve(topright);
            } else {
              tsbond(topright, toplshelle);
            }
            if (botlshelle.sh == dummysh) {
              tsdissolve(topleft);
            } else {
              tsbond(topleft, botlshelle);
            }
            if (botrshelle.sh == dummysh) {
              tsdissolve(botleft);
            } else {
              tsbond(botleft, botrshelle);
            }
            if (toprshelle.sh == dummysh) {
              tsdissolve(botright);
            } else {
              tsbond(botright, toprshelle);
            }
          }
          /* New point assignments for the rotated quadrilateral. */
          setorg(horiz, farpoint);
          setdest(horiz, insertpoint);
          setapex(horiz, rightpoint);
          setorg(top, insertpoint);
          setdest(top, farpoint);
          setapex(top, leftpoint);
          for (i = 0; i < eextras; i++) {
            /* Take the average of the two triangles' attributes. */
            attrib = 0.5 * (elemattribute(top, i) + elemattribute(horiz, i));
            setelemattribute(top, i, attrib);
            setelemattribute(horiz, i, attrib);
          }
          if (vararea) {
            if ((areabound(top) <= 0.0) || (areabound(horiz) <= 0.0)) {
              area = -1.0;
            } else {
              /* Take the average of the two triangles' area constraints.    */
              /*   This prevents small area constraints from migrating a     */
              /*   long, long way from their original location due to flips. */
              area = 0.5 * (areabound(top) + areabound(horiz));
            }
            setareabound(top, area);
            setareabound(horiz, area);
          }
#ifdef SELF_CHECK
          if (insertpoint != (point) NULL) {
            if (counterclockwise(leftpoint, insertpoint, rightpoint) < 0.0) {
              printf("Internal error in insertsite():\n");
              printf("  Clockwise triangle prior to edge flip (bottom).\n");
            }
            /* The following test has been removed because constrainededge() */
            /*   sometimes generates inverted triangles that insertsite()    */
            /*   removes.                                                    */
/*
            if (counterclockwise(rightpoint, farpoint, leftpoint) < 0.0) {
              printf("Internal error in insertsite():\n");
              printf("  Clockwise triangle prior to edge flip (top).\n");
            }
*/
            if (counterclockwise(farpoint, leftpoint, insertpoint) < 0.0) {
              printf("Internal error in insertsite():\n");
              printf("  Clockwise triangle after edge flip (left).\n");
            }
            if (counterclockwise(insertpoint, rightpoint, farpoint) < 0.0) {
              printf("Internal error in insertsite():\n");
              printf("  Clockwise triangle after edge flip (right).\n");
            }
          }
#endif /* SELF_CHECK */
          if (verbose > 2) {
            printf("  Edge flip results in left ");
            lnextself(topleft);
            printtriangle(&topleft);
            printf("  and right ");
            printtriangle(&horiz);
          }
          /* On the next iterations, consider the two edges that were  */
          /*   exposed (this is, are now visible to the newly inserted */
          /*   point) by the edge flip.                                */
          lprevself(horiz);
          leftpoint = farpoint;
        }
      }
    }
    if (!doflip) {
      /* The handle `horiz' is accepted as locally Delaunay. */
#ifndef CDT_ONLY
      if (triflaws) {
        /* Check the triangle `horiz' for quality. */
        testtriangle(&horiz);
      }
#endif /* not CDT_ONLY */
      /* Look for the next edge around the newly inserted point. */
      lnextself(horiz);
      sym(horiz, testtri);
      /* Check for finishing a complete revolution about the new point, or */
      /*   falling off the edge of the triangulation.  The latter will     */
      /*   happen when a point is inserted at a boundary.                  */
      if ((leftpoint == first) || (testtri.tri == dummytri)) {
        /* We're done.  Return a triangle whose origin is the new point. */
        lnext(horiz, *searchtri);
        lnext(horiz, recenttri);
        return success;
      }
      /* Finish finding the next edge around the newly inserted point. */
      lnext(testtri, horiz);
      rightpoint = leftpoint;
      dest(horiz, leftpoint);
    }
  }
}

/*****************************************************************************/
/*                                                                           */
/*  triangulatepolygon()   Find the Delaunay triangulation of a polygon that */
/*                         has a certain "nice" shape.  This includes the    */
/*                         polygons that result from deletion of a point or  */
/*                         insertion of a segment.                           */
/*                                                                           */
/*  This is a conceptually difficult routine.  The starting assumption is    */
/*  that we have a polygon with n sides.  n - 1 of these sides are currently */
/*  represented as edges in the mesh.  One side, called the "base", need not */
/*  be.                                                                      */
/*                                                                           */
/*  Inside the polygon is a structure I call a "fan", consisting of n - 1    */
/*  triangles that share a common origin.  For each of these triangles, the  */
/*  edge opposite the origin is one of the sides of the polygon.  The        */
/*  primary edge of each triangle is the edge directed from the origin to    */
/*  the destination; note that this is not the same edge that is a side of   */
/*  the polygon.  `firstedge' is the primary edge of the first triangle.     */
/*  From there, the triangles follow in counterclockwise order about the     */
/*  polygon, until `lastedge', the primary edge of the last triangle.        */
/*  `firstedge' and `lastedge' are probably connected to other triangles     */
/*  beyond the extremes of the fan, but their identity is not important, as  */
/*  long as the fan remains connected to them.                               */
/*                                                                           */
/*  Imagine the polygon oriented so that its base is at the bottom.  This    */
/*  puts `firstedge' on the far right, and `lastedge' on the far left.       */
/*  The right vertex of the base is the destination of `firstedge', and the  */
/*  left vertex of the base is the apex of `lastedge'.                       */
/*                                                                           */
/*  The challenge now is to find the right sequence of edge flips to         */
/*  transform the fan into a Delaunay triangulation of the polygon.  Each    */
/*  edge flip effectively removes one triangle from the fan, committing it   */
/*  to the polygon.  The resulting polygon has one fewer edge.  If `doflip'  */
/*  is set, the final flip will be performed, resulting in a fan of one      */
/*  (useless?) triangle.  If `doflip' is not set, the final flip is not      */
/*  performed, resulting in a fan of two triangles, and an unfinished        */
/*  triangular polygon that is not yet filled out with a single triangle.    */
/*  On completion of the routine, `lastedge' is the last remaining triangle, */
/*  or the leftmost of the last two.                                         */
/*                                                                           */
/*  Although the flips are performed in the order described above, the       */
/*  decisions about what flips to perform are made in precisely the reverse  */
/*  order.  The recursive triangulatepolygon() procedure makes a decision,   */
/*  uses up to two recursive calls to triangulate the "subproblems"          */
/*  (polygons with fewer edges), and then performs an edge flip.             */
/*                                                                           */
/*  The "decision" it makes is which vertex of the polygon should be         */
/*  connected to the base.  This decision is made by testing every possible  */
/*  vertex.  Once the best vertex is found, the two edges that connect this  */
/*  vertex to the base become the bases for two smaller polygons.  These     */
/*  are triangulated recursively.  Unfortunately, this approach can take     */
/*  O(n^2) time not only in the worst case, but in many common cases.  It's  */
/*  rarely a big deal for point deletion, where n is rarely larger than ten, */
/*  but it could be a big deal for segment insertion, especially if there's  */
/*  a lot of long segments that each cut many triangles.  I ought to code    */
/*  a faster algorithm some time.                                            */
/*                                                                           */
/*  The `edgecount' parameter is the number of sides of the polygon,         */
/*  including its base.  `triflaws' is a flag that determines whether the    */
/*  new triangles should be tested for quality, and enqueued if they are     */
/*  bad.                                                                     */
/*                                                                           */
/*****************************************************************************/

void triangulatepolygon(firstedge, lastedge, edgecount, doflip, triflaws)
struct triedge *firstedge;
struct triedge *lastedge;
int edgecount;
int doflip;
int triflaws;
{
  struct triedge testtri;
  struct triedge besttri;
  struct triedge tempedge;
  point leftbasepoint, rightbasepoint;
  point testpoint;
  point bestpoint;
  int bestnumber;
  int i;
  triangle ptr;   /* Temporary variable used by sym(), onext(), and oprev(). */

  /* Identify the base vertices. */
  apex(*lastedge, leftbasepoint);
  dest(*firstedge, rightbasepoint);
  if (verbose > 2) {
    printf("  Triangulating interior polygon at edge\n");
    printf("    (%.12g, %.12g) (%.12g, %.12g)\n", leftbasepoint[0],
           leftbasepoint[1], rightbasepoint[0], rightbasepoint[1]);
  }
  /* Find the best vertex to connect the base to. */
  onext(*firstedge, besttri);
  dest(besttri, bestpoint);
  triedgecopy(besttri, testtri);
  bestnumber = 1;
  for (i = 2; i <= edgecount - 2; i++) {
    onextself(testtri);
    dest(testtri, testpoint);
    /* Is this a better vertex? */
    if (incircle(leftbasepoint, rightbasepoint, bestpoint, testpoint) > 0.0) {
      triedgecopy(testtri, besttri);
      bestpoint = testpoint;
      bestnumber = i;
    }
  }
  if (verbose > 2) {
    printf("    Connecting edge to (%.12g, %.12g)\n", bestpoint[0],
           bestpoint[1]);
  }
  if (bestnumber > 1) {
    /* Recursively triangulate the smaller polygon on the right. */
    oprev(besttri, tempedge);
    triangulatepolygon(firstedge, &tempedge, bestnumber + 1, 1, triflaws);
  }
  if (bestnumber < edgecount - 2) {
    /* Recursively triangulate the smaller polygon on the left. */
    sym(besttri, tempedge);
    triangulatepolygon(&besttri, lastedge, edgecount - bestnumber, 1, triflaws);
    /* Find `besttri' again; it may have been lost to edge flips. */
    sym(tempedge, besttri);
  }
  if (doflip) {
    /* Do one final edge flip. */
    flip(&besttri);
#ifndef CDT_ONLY
    if (triflaws) {
      /* Check the quality of the newly committed triangle. */
      sym(besttri, testtri);
      testtriangle(&testtri);
    }
#endif /* not CDT_ONLY */
  }
  /* Return the base triangle. */
  triedgecopy(besttri, *lastedge);
}

/*****************************************************************************/
/*                                                                           */
/*  deletesite()   Delete a vertex from a Delaunay triangulation, ensuring   */
/*                 that the triangulation remains Delaunay.                  */
/*                                                                           */
/*  The origin of `deltri' is deleted.  The union of the triangles adjacent  */
/*  to this point is a polygon, for which the Delaunay triangulation is      */
/*  found.  Two triangles are removed from the mesh.                         */
/*                                                                           */
/*  Only interior points that do not lie on segments (shell edges) or        */
/*  boundaries may be deleted.                                               */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void deletesite(deltri)
struct triedge *deltri;
{
  struct triedge countingtri;
  struct triedge firstedge, lastedge;
  struct triedge deltriright;
  struct triedge lefttri, righttri;
  struct triedge leftcasing, rightcasing;
  struct edge leftshelle, rightshelle;
  point delpoint;
  point neworg;
  int edgecount;
  triangle ptr;   /* Temporary variable used by sym(), onext(), and oprev(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  org(*deltri, delpoint);
  if (verbose > 1) {
    printf("  Deleting (%.12g, %.12g).\n", delpoint[0], delpoint[1]);
  }
  pointdealloc(delpoint);

  /* Count the degree of the point being deleted. */
  onext(*deltri, countingtri);
  edgecount = 1;
  while (!triedgeequal(*deltri, countingtri)) {
#ifdef SELF_CHECK
    if (countingtri.tri == dummytri) {
      printf("Internal error in deletesite():\n");
      printf("  Attempt to delete boundary point.\n");
      internalerror();
    }
#endif /* SELF_CHECK */
    edgecount++;
    onextself(countingtri);
  }

#ifdef SELF_CHECK
  if (edgecount < 3) {
    printf("Internal error in deletesite():\n  Point has degree %d.\n",
           edgecount);
    internalerror();
  }
#endif /* SELF_CHECK */
  if (edgecount > 3) {
    /* Triangulate the polygon defined by the union of all triangles */
    /*   adjacent to the point being deleted.  Check the quality of  */
    /*   the resulting triangles.                                    */
    onext(*deltri, firstedge);
    oprev(*deltri, lastedge);
    triangulatepolygon(&firstedge, &lastedge, edgecount, 0, !nobisect);
  }
  /* Splice out two triangles. */
  lprev(*deltri, deltriright);
  dnext(*deltri, lefttri);
  sym(lefttri, leftcasing);
  oprev(deltriright, righttri);
  sym(righttri, rightcasing);
  bond(*deltri, leftcasing);
  bond(deltriright, rightcasing);
  tspivot(lefttri, leftshelle);
  if (leftshelle.sh != dummysh) {
    tsbond(*deltri, leftshelle);
  }
  tspivot(righttri, rightshelle);
  if (rightshelle.sh != dummysh) {
    tsbond(deltriright, rightshelle);
  }

  /* Set the new origin of `deltri' and check its quality. */
  org(lefttri, neworg);
  setorg(*deltri, neworg);
  if (!nobisect) {
    testtriangle(deltri);
  }

  /* Delete the two spliced-out triangles. */
  triangledealloc(lefttri.tri);
  triangledealloc(righttri.tri);
}

#endif /* not CDT_ONLY */

/**                                                                         **/
/**                                                                         **/
/********* Mesh transformation routines end here                     *********/

/********* Divide-and-conquer Delaunay triangulation begins here     *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  The divide-and-conquer bounding box                                      */
/*                                                                           */
/*  I originally implemented the divide-and-conquer and incremental Delaunay */
/*  triangulations using the edge-based data structure presented by Guibas   */
/*  and Stolfi.  Switching to a triangle-based data structure doubled the    */
/*  speed.  However, I had to think of a few extra tricks to maintain the    */
/*  elegance of the original algorithms.                                     */
/*                                                                           */
/*  The "bounding box" used by my variant of the divide-and-conquer          */
/*  algorithm uses one triangle for each edge of the convex hull of the      */
/*  triangulation.  These bounding triangles all share a common apical       */
/*  vertex, which is represented by NULL and which represents nothing.       */
/*  The bounding triangles are linked in a circular fan about this NULL      */
/*  vertex, and the edges on the convex hull of the triangulation appear     */
/*  opposite the NULL vertex.  You might find it easiest to imagine that     */
/*  the NULL vertex is a point in 3D space behind the center of the          */
/*  triangulation, and that the bounding triangles form a sort of cone.      */
/*                                                                           */
/*  This bounding box makes it easy to represent degenerate cases.  For      */
/*  instance, the triangulation of two vertices is a single edge.  This edge */
/*  is represented by two bounding box triangles, one on each "side" of the  */
/*  edge.  These triangles are also linked together in a fan about the NULL  */
/*  vertex.                                                                  */
/*                                                                           */
/*  The bounding box also makes it easy to traverse the convex hull, as the  */
/*  divide-and-conquer algorithm needs to do.                                */
/*                                                                           */
/*****************************************************************************/

/*****************************************************************************/
/*                                                                           */
/*  pointsort()   Sort an array of points by x-coordinate, using the         */
/*                y-coordinate as a secondary key.                           */
/*                                                                           */
/*  Uses quicksort.  Randomized O(n log n) time.  No, I did not make any of  */
/*  the usual quicksort mistakes.                                            */
/*                                                                           */
/*****************************************************************************/

void pointsort(sortarray, arraysize)
point *sortarray;
int arraysize;
{
  int left, right;
  int pivot;
  REAL pivotx, pivoty;
  point temp;

  if (arraysize == 2) {
    /* Recursive base case. */
    if ((sortarray[0][0] > sortarray[1][0]) ||
        ((sortarray[0][0] == sortarray[1][0]) &&
         (sortarray[0][1] > sortarray[1][1]))) {
      temp = sortarray[1];
      sortarray[1] = sortarray[0];
      sortarray[0] = temp;
    }
    return;
  }
  /* Choose a random pivot to split the array. */
  pivot = (int) randomnation(arraysize);
  pivotx = sortarray[pivot][0];
  pivoty = sortarray[pivot][1];
  /* Split the array. */
  left = -1;
  right = arraysize;
  while (left < right) {
    /* Search for a point whose x-coordinate is too large for the left. */
    do {
      left++;
    } while ((left <= right) && ((sortarray[left][0] < pivotx) ||
                                 ((sortarray[left][0] == pivotx) &&
                                  (sortarray[left][1] < pivoty))));
    /* Search for a point whose x-coordinate is too small for the right. */
    do {
      right--;
    } while ((left <= right) && ((sortarray[right][0] > pivotx) ||
                                 ((sortarray[right][0] == pivotx) &&
                                  (sortarray[right][1] > pivoty))));
    if (left < right) {
      /* Swap the left and right points. */
      temp = sortarray[left];
      sortarray[left] = sortarray[right];
      sortarray[right] = temp;
    }
  }
  if (left > 1) {
    /* Recursively sort the left subset. */
    pointsort(sortarray, left);
  }
  if (right < arraysize - 2) {
    /* Recursively sort the right subset. */
    pointsort(&sortarray[right + 1], arraysize - right - 1);
  }
}

/*****************************************************************************/
/*                                                                           */
/*  pointmedian()   An order statistic algorithm, almost.  Shuffles an array */
/*                  of points so that the first `median' points occur        */
/*                  lexicographically before the remaining points.           */
/*                                                                           */
/*  Uses the x-coordinate as the primary key if axis == 0; the y-coordinate  */
/*  if axis == 1.  Very similar to the pointsort() procedure, but runs in    */
/*  randomized linear time.                                                  */
/*                                                                           */
/*****************************************************************************/

void pointmedian(sortarray, arraysize, median, axis)
point *sortarray;
int arraysize;
int median;
int axis;
{
  int left, right;
  int pivot;
  REAL pivot1, pivot2;
  point temp;

  if (arraysize == 2) {
    /* Recursive base case. */
    if ((sortarray[0][axis] > sortarray[1][axis]) ||
        ((sortarray[0][axis] == sortarray[1][axis]) &&
         (sortarray[0][1 - axis] > sortarray[1][1 - axis]))) {
      temp = sortarray[1];
      sortarray[1] = sortarray[0];
      sortarray[0] = temp;
    }
    return;
  }
  /* Choose a random pivot to split the array. */
  pivot = (int) randomnation(arraysize);
  pivot1 = sortarray[pivot][axis];
  pivot2 = sortarray[pivot][1 - axis];
  /* Split the array. */
  left = -1;
  right = arraysize;
  while (left < right) {
    /* Search for a point whose x-coordinate is too large for the left. */
    do {
      left++;
    } while ((left <= right) && ((sortarray[left][axis] < pivot1) ||
                                 ((sortarray[left][axis] == pivot1) &&
                                  (sortarray[left][1 - axis] < pivot2))));
    /* Search for a point whose x-coordinate is too small for the right. */
    do {
      right--;
    } while ((left <= right) && ((sortarray[right][axis] > pivot1) ||
                                 ((sortarray[right][axis] == pivot1) &&
                                  (sortarray[right][1 - axis] > pivot2))));
    if (left < right) {
      /* Swap the left and right points. */
      temp = sortarray[left];
      sortarray[left] = sortarray[right];
      sortarray[right] = temp;
    }
  }
  /* Unlike in pointsort(), at most one of the following */
  /*   conditionals is true.                             */
  if (left > median) {
    /* Recursively shuffle the left subset. */
    pointmedian(sortarray, left, median, axis);
  }
  if (right < median - 1) {
    /* Recursively shuffle the right subset. */
    pointmedian(&sortarray[right + 1], arraysize - right - 1,
                median - right - 1, axis);
  }
}

/*****************************************************************************/
/*                                                                           */
/*  alternateaxes()   Sorts the points as appropriate for the divide-and-    */
/*                    conquer algorithm with alternating cuts.               */
/*                                                                           */
/*  Partitions by x-coordinate if axis == 0; by y-coordinate if axis == 1.   */
/*  For the base case, subsets containing only two or three points are       */
/*  always sorted by x-coordinate.                                           */
/*                                                                           */
/*****************************************************************************/

void alternateaxes(sortarray, arraysize, axis)
point *sortarray;
int arraysize;
int axis;
{
  int divider;

  divider = arraysize >> 1;
  if (arraysize <= 3) {
    /* Recursive base case:  subsets of two or three points will be      */
    /*   handled specially, and should always be sorted by x-coordinate. */
    axis = 0;
  }
  /* Partition with a horizontal or vertical cut. */
  pointmedian(sortarray, arraysize, divider, axis);
  /* Recursively partition the subsets with a cross cut. */
  if (arraysize - divider >= 2) {
    if (divider >= 2) {
      alternateaxes(sortarray, divider, 1 - axis);
    }
    alternateaxes(&sortarray[divider], arraysize - divider, 1 - axis);
  }
}

/*****************************************************************************/
/*                                                                           */
/*  mergehulls()   Merge two adjacent Delaunay triangulations into a         */
/*                 single Delaunay triangulation.                            */
/*                                                                           */
/*  This is similar to the algorithm given by Guibas and Stolfi, but uses    */
/*  a triangle-based, rather than edge-based, data structure.                */
/*                                                                           */
/*  The algorithm walks up the gap between the two triangulations, knitting  */
/*  them together.  As they are merged, some of their bounding triangles     */
/*  are converted into real triangles of the triangulation.  The procedure   */
/*  pulls each hull's bounding triangles apart, then knits them together     */
/*  like the teeth of two gears.  The Delaunay property determines, at each  */
/*  step, whether the next "tooth" is a bounding triangle of the left hull   */
/*  or the right.  When a bounding triangle becomes real, its apex is        */
/*  changed from NULL to a real point.                                       */
/*                                                                           */
/*  Only two new triangles need to be allocated.  These become new bounding  */
/*  triangles at the top and bottom of the seam.  They are used to connect   */
/*  the remaining bounding triangles (those that have not been converted     */
/*  into real triangles) into a single fan.                                  */
/*                                                                           */
/*  On entry, `farleft' and `innerleft' are bounding triangles of the left   */
/*  triangulation.  The origin of `farleft' is the leftmost vertex, and      */
/*  the destination of `innerleft' is the rightmost vertex of the            */
/*  triangulation.  Similarly, `innerright' and `farright' are bounding      */
/*  triangles of the right triangulation.  The origin of `innerright' and    */
/*  destination of `farright' are the leftmost and rightmost vertices.       */
/*                                                                           */
/*  On completion, the origin of `farleft' is the leftmost vertex of the     */
/*  merged triangulation, and the destination of `farright' is the rightmost */
/*  vertex.                                                                  */
/*                                                                           */
/*****************************************************************************/

void mergehulls(farleft, innerleft, innerright, farright, axis)
struct triedge *farleft;
struct triedge *innerleft;
struct triedge *innerright;
struct triedge *farright;
int axis;
{
  struct triedge leftcand, rightcand;
  struct triedge baseedge;
  struct triedge nextedge;
  struct triedge sidecasing, topcasing, outercasing;
  struct triedge checkedge;
  point innerleftdest;
  point innerrightorg;
  point innerleftapex, innerrightapex;
  point farleftpt, farrightpt;
  point farleftapex, farrightapex;
  point lowerleft, lowerright;
  point upperleft, upperright;
  point nextapex;
  point checkvertex;
  int changemade;
  int badedge;
  int leftfinished, rightfinished;
  triangle ptr;                         /* Temporary variable used by sym(). */

  dest(*innerleft, innerleftdest);
  apex(*innerleft, innerleftapex);
  org(*innerright, innerrightorg);
  apex(*innerright, innerrightapex);
  /* Special treatment for horizontal cuts. */
  if (dwyer && (axis == 1)) {
    org(*farleft, farleftpt);
    apex(*farleft, farleftapex);
    dest(*farright, farrightpt);
    apex(*farright, farrightapex);
    /* The pointers to the extremal points are shifted to point to the */
    /*   topmost and bottommost point of each hull, rather than the    */
    /*   leftmost and rightmost points.                                */
    while (farleftapex[1] < farleftpt[1]) {
      lnextself(*farleft);
      symself(*farleft);
      farleftpt = farleftapex;
      apex(*farleft, farleftapex);
    }
    sym(*innerleft, checkedge);
    apex(checkedge, checkvertex);
    while (checkvertex[1] > innerleftdest[1]) {
      lnext(checkedge, *innerleft);
      innerleftapex = innerleftdest;
      innerleftdest = checkvertex;
      sym(*innerleft, checkedge);
      apex(checkedge, checkvertex);
    }
    while (innerrightapex[1] < innerrightorg[1]) {
      lnextself(*innerright);
      symself(*innerright);
      innerrightorg = innerrightapex;
      apex(*innerright, innerrightapex);
    }
    sym(*farright, checkedge);
    apex(checkedge, checkvertex);
    while (checkvertex[1] > farrightpt[1]) {
      lnext(checkedge, *farright);
      /*farrightapex = farrightpt;*/
      farrightpt = checkvertex;
      sym(*farright, checkedge);
      apex(checkedge, checkvertex);
    }
  }
  /* Find a line tangent to and below both hulls. */
  do {
    changemade = 0;
    /* Make innerleftdest the "bottommost" point of the left hull. */
    if (counterclockwise(innerleftdest, innerleftapex, innerrightorg) > 0.0) {
      lprevself(*innerleft);
      symself(*innerleft);
      innerleftdest = innerleftapex;
      apex(*innerleft, innerleftapex);
      changemade = 1;
    }
    /* Make innerrightorg the "bottommost" point of the right hull. */
    if (counterclockwise(innerrightapex, innerrightorg, innerleftdest) > 0.0) {
      lnextself(*innerright);
      symself(*innerright);
      innerrightorg = innerrightapex;
      apex(*innerright, innerrightapex);
      changemade = 1;
    }
  } while (changemade);
  /* Find the two candidates to be the next "gear tooth". */
  sym(*innerleft, leftcand);
  sym(*innerright, rightcand);
  /* Create the bottom new bounding triangle. */
  maketriangle(&baseedge);
  /* Connect it to the bounding boxes of the left and right triangulations. */
  bond(baseedge, *innerleft);
  lnextself(baseedge);
  bond(baseedge, *innerright);
  lnextself(baseedge);
  setorg(baseedge, innerrightorg);
  setdest(baseedge, innerleftdest);
  /* Apex is intentionally left NULL. */
  if (verbose > 2) {
    printf("  Creating base bounding ");
    printtriangle(&baseedge);
  }
  /* Fix the extreme triangles if necessary. */
  org(*farleft, farleftpt);
  if (innerleftdest == farleftpt) {
    lnext(baseedge, *farleft);
  }
  dest(*farright, farrightpt);
  if (innerrightorg == farrightpt) {
    lprev(baseedge, *farright);
  }
  /* The vertices of the current knitting edge. */
  lowerleft = innerleftdest;
  lowerright = innerrightorg;
  /* The candidate vertices for knitting. */
  apex(leftcand, upperleft);
  apex(rightcand, upperright);
  /* Walk up the gap between the two triangulations, knitting them together. */
  while (1) {
    /* Have we reached the top?  (This isn't quite the right question,       */
    /*   because even though the left triangulation might seem finished now, */
    /*   moving up on the right triangulation might reveal a new point of    */
    /*   the left triangulation.  And vice-versa.)                           */
    leftfinished = counterclockwise(upperleft, lowerleft, lowerright) <= 0.0;
    rightfinished = counterclockwise(upperright, lowerleft, lowerright) <= 0.0;
    if (leftfinished && rightfinished) {
      /* Create the top new bounding triangle. */
      maketriangle(&nextedge);
      setorg(nextedge, lowerleft);
      setdest(nextedge, lowerright);
      /* Apex is intentionally left NULL. */
      /* Connect it to the bounding boxes of the two triangulations. */
      bond(nextedge, baseedge);
      lnextself(nextedge);
      bond(nextedge, rightcand);
      lnextself(nextedge);
      bond(nextedge, leftcand);
      if (verbose > 2) {
        printf("  Creating top bounding ");
        printtriangle(&baseedge);
      }
      /* Special treatment for horizontal cuts. */
      if (dwyer && (axis == 1)) {
        org(*farleft, farleftpt);
        apex(*farleft, farleftapex);
        dest(*farright, farrightpt);
        apex(*farright, farrightapex);
        sym(*farleft, checkedge);
        apex(checkedge, checkvertex);
        /* The pointers to the extremal points are restored to the leftmost */
        /*   and rightmost points (rather than topmost and bottommost).     */
        while (checkvertex[0] < farleftpt[0]) {
          lprev(checkedge, *farleft);
          /*farleftapex = farleftpt;*/
          farleftpt = checkvertex;
          sym(*farleft, checkedge);
          apex(checkedge, checkvertex);
        }
        while (farrightapex[0] > farrightpt[0]) {
          lprevself(*farright);
          symself(*farright);
          farrightpt = farrightapex;
          apex(*farright, farrightapex);
        }
      }
      return;
    }
    /* Consider eliminating edges from the left triangulation. */
    if (!leftfinished) {
      /* What vertex would be exposed if an edge were deleted? */
      lprev(leftcand, nextedge);
      symself(nextedge);
      apex(nextedge, nextapex);
      /* If nextapex is NULL, then no vertex would be exposed; the */
      /*   triangulation would have been eaten right through.      */
      if (nextapex != (point) NULL) {
        /* Check whether the edge is Delaunay. */
        badedge = incircle(lowerleft, lowerright, upperleft, nextapex) > 0.0;
        while (badedge) {
          /* Eliminate the edge with an edge flip.  As a result, the    */
          /*   left triangulation will have one more boundary triangle. */
          lnextself(nextedge);
          sym(nextedge, topcasing);
          lnextself(nextedge);
          sym(nextedge, sidecasing);
          bond(nextedge, topcasing);
          bond(leftcand, sidecasing);
          lnextself(leftcand);
          sym(leftcand, outercasing);
          lprevself(nextedge);
          bond(nextedge, outercasing);
          /* Correct the vertices to reflect the edge flip. */
          setorg(leftcand, lowerleft);
          setdest(leftcand, NULL);
          setapex(leftcand, nextapex);
          setorg(nextedge, NULL);
          setdest(nextedge, upperleft);
          setapex(nextedge, nextapex);
          /* Consider the newly exposed vertex. */
          upperleft = nextapex;
          /* What vertex would be exposed if another edge were deleted? */
          triedgecopy(sidecasing, nextedge);
          apex(nextedge, nextapex);
          if (nextapex != (point) NULL) {
            /* Check whether the edge is Delaunay. */
            badedge = incircle(lowerleft, lowerright, upperleft, nextapex)
                      > 0.0;
          } else {
            /* Avoid eating right through the triangulation. */
            badedge = 0;
          }
        }
      }
    }
    /* Consider eliminating edges from the right triangulation. */
    if (!rightfinished) {
      /* What vertex would be exposed if an edge were deleted? */
      lnext(rightcand, nextedge);
      symself(nextedge);
      apex(nextedge, nextapex);
      /* If nextapex is NULL, then no vertex would be exposed; the */
      /*   triangulation would have been eaten right through.      */
      if (nextapex != (point) NULL) {
        /* Check whether the edge is Delaunay. */
        badedge = incircle(lowerleft, lowerright, upperright, nextapex) > 0.0;
        while (badedge) {
          /* Eliminate the edge with an edge flip.  As a result, the     */
          /*   right triangulation will have one more boundary triangle. */
          lprevself(nextedge);
          sym(nextedge, topcasing);
          lprevself(nextedge);
          sym(nextedge, sidecasing);
          bond(nextedge, topcasing);
          bond(rightcand, sidecasing);
          lprevself(rightcand);
          sym(rightcand, outercasing);
          lnextself(nextedge);
          bond(nextedge, outercasing);
          /* Correct the vertices to reflect the edge flip. */
          setorg(rightcand, NULL);
          setdest(rightcand, lowerright);
          setapex(rightcand, nextapex);
          setorg(nextedge, upperright);
          setdest(nextedge, NULL);
          setapex(nextedge, nextapex);
          /* Consider the newly exposed vertex. */
          upperright = nextapex;
          /* What vertex would be exposed if another edge were deleted? */
          triedgecopy(sidecasing, nextedge);
          apex(nextedge, nextapex);
          if (nextapex != (point) NULL) {
            /* Check whether the edge is Delaunay. */
            badedge = incircle(lowerleft, lowerright, upperright, nextapex)
                      > 0.0;
          } else {
            /* Avoid eating right through the triangulation. */
            badedge = 0;
          }
        }
      }
    }
    if (leftfinished || (!rightfinished &&
           (incircle(upperleft, lowerleft, lowerright, upperright) > 0.0))) {
      /* Knit the triangulations, adding an edge from `lowerleft' */
      /*   to `upperright'.                                       */
      bond(baseedge, rightcand);
      lprev(rightcand, baseedge);
      setdest(baseedge, lowerleft);
      lowerright = upperright;
      sym(baseedge, rightcand);
      apex(rightcand, upperright);
    } else {
      /* Knit the triangulations, adding an edge from `upperleft' */
      /*   to `lowerright'.                                       */
      bond(baseedge, leftcand);
      lnext(leftcand, baseedge);
      setorg(baseedge, lowerright);
      lowerleft = upperleft;
      sym(baseedge, leftcand);
      apex(leftcand, upperleft);
    }
    if (verbose > 2) {
      printf("  Connecting ");
      printtriangle(&baseedge);
    }
  }
}

/*****************************************************************************/
/*                                                                           */
/*  divconqrecurse()   Recursively form a Delaunay triangulation by the      */
/*                     divide-and-conquer method.                            */
/*                                                                           */
/*  Recursively breaks down the problem into smaller pieces, which are       */
/*  knitted together by mergehulls().  The base cases (problems of two or    */
/*  three points) are handled specially here.                                */
/*                                                                           */
/*  On completion, `farleft' and `farright' are bounding triangles such that */
/*  the origin of `farleft' is the leftmost vertex (breaking ties by         */
/*  choosing the highest leftmost vertex), and the destination of            */
/*  `farright' is the rightmost vertex (breaking ties by choosing the        */
/*  lowest rightmost vertex).                                                */
/*                                                                           */
/*****************************************************************************/

void divconqrecurse(sortarray, vertices, axis, farleft, farright)
point *sortarray;
int vertices;
int axis;
struct triedge *farleft;
struct triedge *farright;
{
  struct triedge midtri, tri1, tri2, tri3;
  struct triedge innerleft, innerright;
  REAL area;
  int divider;

  if (verbose > 2) {
    printf("  Triangulating %d points.\n", vertices);
  }
  if (vertices == 2) {
    /* The triangulation of two vertices is an edge.  An edge is */
    /*   represented by two bounding triangles.                  */
    maketriangle(farleft);
    setorg(*farleft, sortarray[0]);
    setdest(*farleft, sortarray[1]);
    /* The apex is intentionally left NULL. */
    maketriangle(farright);
    setorg(*farright, sortarray[1]);
    setdest(*farright, sortarray[0]);
    /* The apex is intentionally left NULL. */
    bond(*farleft, *farright);
    lprevself(*farleft);
    lnextself(*farright);
    bond(*farleft, *farright);
    lprevself(*farleft);
    lnextself(*farright);
    bond(*farleft, *farright);
    if (verbose > 2) {
      printf("  Creating ");
      printtriangle(farleft);
      printf("  Creating ");
      printtriangle(farright);
    }
    /* Ensure that the origin of `farleft' is sortarray[0]. */
    lprev(*farright, *farleft);
    return;
  } else if (vertices == 3) {
    /* The triangulation of three vertices is either a triangle (with */
    /*   three bounding triangles) or two edges (with four bounding   */
    /*   triangles).  In either case, four triangles are created.     */
    maketriangle(&midtri);
    maketriangle(&tri1);
    maketriangle(&tri2);
    maketriangle(&tri3);
    area = counterclockwise(sortarray[0], sortarray[1], sortarray[2]);
    if (area == 0.0) {
      /* Three collinear points; the triangulation is two edges. */
      setorg(midtri, sortarray[0]);
      setdest(midtri, sortarray[1]);
      setorg(tri1, sortarray[1]);
      setdest(tri1, sortarray[0]);
      setorg(tri2, sortarray[2]);
      setdest(tri2, sortarray[1]);
      setorg(tri3, sortarray[1]);
      setdest(tri3, sortarray[2]);
      /* All apices are intentionally left NULL. */
      bond(midtri, tri1);
      bond(tri2, tri3);
      lnextself(midtri);
      lprevself(tri1);
      lnextself(tri2);
      lprevself(tri3);
      bond(midtri, tri3);
      bond(tri1, tri2);
      lnextself(midtri);
      lprevself(tri1);
      lnextself(tri2);
      lprevself(tri3);
      bond(midtri, tri1);
      bond(tri2, tri3);
      /* Ensure that the origin of `farleft' is sortarray[0]. */
      triedgecopy(tri1, *farleft);
      /* Ensure that the destination of `farright' is sortarray[2]. */
      triedgecopy(tri2, *farright);
    } else {
      /* The three points are not collinear; the triangulation is one */
      /*   triangle, namely `midtri'.                                 */
      setorg(midtri, sortarray[0]);
      setdest(tri1, sortarray[0]);
      setorg(tri3, sortarray[0]);
      /* Apices of tri1, tri2, and tri3 are left NULL. */
      if (area > 0.0) {
        /* The vertices are in counterclockwise order. */
        setdest(midtri, sortarray[1]);
        setorg(tri1, sortarray[1]);
        setdest(tri2, sortarray[1]);
        setapex(midtri, sortarray[2]);
        setorg(tri2, sortarray[2]);
        setdest(tri3, sortarray[2]);
      } else {
        /* The vertices are in clockwise order. */
        setdest(midtri, sortarray[2]);
        setorg(tri1, sortarray[2]);
        setdest(tri2, sortarray[2]);
        setapex(midtri, sortarray[1]);
        setorg(tri2, sortarray[1]);
        setdest(tri3, sortarray[1]);
      }
      /* The topology does not depend on how the vertices are ordered. */
      bond(midtri, tri1);
      lnextself(midtri);
      bond(midtri, tri2);
      lnextself(midtri);
      bond(midtri, tri3);
      lprevself(tri1);
      lnextself(tri2);
      bond(tri1, tri2);
      lprevself(tri1);
      lprevself(tri3);
      bond(tri1, tri3);
      lnextself(tri2);
      lprevself(tri3);
      bond(tri2, tri3);
      /* Ensure that the origin of `farleft' is sortarray[0]. */
      triedgecopy(tri1, *farleft);
      /* Ensure that the destination of `farright' is sortarray[2]. */
      if (area > 0.0) {
        triedgecopy(tri2, *farright);
      } else {
        lnext(*farleft, *farright);
      }
    }
    if (verbose > 2) {
      printf("  Creating ");
      printtriangle(&midtri);
      printf("  Creating ");
      printtriangle(&tri1);
      printf("  Creating ");
      printtriangle(&tri2);
      printf("  Creating ");
      printtriangle(&tri3);
    }
    return;
  } else {
    /* Split the vertices in half. */
    divider = vertices >> 1;
    /* Recursively triangulate each half. */
    divconqrecurse(sortarray, divider, 1 - axis, farleft, &innerleft);
    divconqrecurse(&sortarray[divider], vertices - divider, 1 - axis,
                   &innerright, farright);
    if (verbose > 1) {
      printf("  Joining triangulations with %d and %d vertices.\n", divider,
             vertices - divider);
    }
    /* Merge the two triangulations into one. */
    mergehulls(farleft, &innerleft, &innerright, farright, axis);
  }
}

long removeghosts(startghost)
struct triedge *startghost;
{
  struct triedge searchedge;
  struct triedge dissolveedge;
  struct triedge deadtri;
  point markorg;
  long hullsize;
  triangle ptr;                         /* Temporary variable used by sym(). */

  if (verbose) {
    printf("  Removing ghost triangles.\n");
  }
  /* Find an edge on the convex hull to start point location from. */
  lprev(*startghost, searchedge);
  symself(searchedge);
  dummytri[0] = encode(searchedge);
  /* Remove the bounding box and count the convex hull edges. */
  triedgecopy(*startghost, dissolveedge);
  hullsize = 0;
  do {
    hullsize++;
    lnext(dissolveedge, deadtri);
    lprevself(dissolveedge);
    symself(dissolveedge);
    /* If no PSLG is involved, set the boundary markers of all the points */
    /*   on the convex hull.  If a PSLG is used, this step is done later. */
    if (!poly) {
      /* Watch out for the case where all the input points are collinear. */
      if (dissolveedge.tri != dummytri) {
        org(dissolveedge, markorg);
        if (pointmark(markorg) == 0) {
          setpointmark(markorg, 1);
        }
      }
    }
    /* Remove a bounding triangle from a convex hull triangle. */
    dissolve(dissolveedge);
    /* Find the next bounding triangle. */
    sym(deadtri, dissolveedge);
    /* Delete the bounding triangle. */
    triangledealloc(deadtri.tri);
  } while (!triedgeequal(dissolveedge, *startghost));
  return hullsize;
}

/*****************************************************************************/
/*                                                                           */
/*  divconqdelaunay()   Form a Delaunay triangulation by the divide-and-     */
/*                      conquer method.                                      */
/*                                                                           */
/*  Sorts the points, calls a recursive procedure to triangulate them, and   */
/*  removes the bounding box, setting boundary markers as appropriate.       */
/*                                                                           */
/*****************************************************************************/

long divconqdelaunay()
{
  point *sortarray;
  struct triedge hullleft, hullright;
  int divider;
  int i, j;

  /* Allocate an array of pointers to points for sorting. */
  sortarray = (point *) malloc(inpoints * sizeof(point));
  if (sortarray == (point *) NULL) {
    printf("Error:  Out of memory.\n");
    exit(1);
  }
  traversalinit(&points);
  for (i = 0; i < inpoints; i++) {
    sortarray[i] = pointtraverse();
  }
  if (verbose) {
    printf("  Sorting points.\n");
  }
  /* Sort the points. */
  pointsort(sortarray, inpoints);
  /* Discard duplicate points, which can really mess up the algorithm. */
  i = 0;
  for (j = 1; j < inpoints; j++) {
    if ((sortarray[i][0] == sortarray[j][0])
        && (sortarray[i][1] == sortarray[j][1])) {
      if (!quiet) {
        printf("Warning:  A duplicate point at (%.12g, %.12g) appeared and was ignored.\n",
               sortarray[j][0], sortarray[j][1]);
      }
/*  Commented out - would eliminate point from output .node file, but causes
    a failure if some segment has this point as an endpoint.
      setpointmark(sortarray[j], DEADPOINT);
*/
    } else {
      i++;
      sortarray[i] = sortarray[j];
    }
  }
  i++;
  if (dwyer) {
    /* Re-sort the array of points to accommodate alternating cuts. */
    divider = i >> 1;
    if (i - divider >= 2) {
      if (divider >= 2) {
        alternateaxes(sortarray, divider, 1);
      }
      alternateaxes(&sortarray[divider], i - divider, 1);
    }
  }
  if (verbose) {
    printf("  Forming triangulation.\n");
  }
  /* Form the Delaunay triangulation. */
  divconqrecurse(sortarray, i, 0, &hullleft, &hullright);
  free(sortarray);

  return removeghosts(&hullleft);
}

/**                                                                         **/
/**                                                                         **/
/********* Divide-and-conquer Delaunay triangulation ends here       *********/

/********* Incremental Delaunay triangulation begins here            *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  boundingbox()   Form an "infinite" bounding triangle to insert points    */
/*                  into.                                                    */
/*                                                                           */
/*  The points at "infinity" are assigned finite coordinates, which are used */
/*  by the point location routines, but (mostly) ignored by the Delaunay     */
/*  edge flip routines.                                                      */
/*                                                                           */
/*****************************************************************************/

#ifndef REDUCED

void boundingbox()
{
  struct triedge inftri;          /* Handle for the triangular bounding box. */
  REAL width;

  if (verbose) {
    printf("  Creating triangular bounding box.\n");
  }
  /* Find the width (or height, whichever is larger) of the triangulation. */
  width = xmax - xmin;
  if (ymax - ymin > width) {
    width = ymax - ymin;
  }
  if (width == 0.0) {
    width = 1.0;
  }
  /* Create the vertices of the bounding box. */
  infpoint1 = (point) malloc(points.itembytes);
  infpoint2 = (point) malloc(points.itembytes);
  infpoint3 = (point) malloc(points.itembytes);
  if ((infpoint1 == (point) NULL) || (infpoint2 == (point) NULL)
      || (infpoint3 == (point) NULL)) {
    printf("Error:  Out of memory.\n");
    exit(1);
  }
  infpoint1[0] = xmin - 50.0 * width;
  infpoint1[1] = ymin - 40.0 * width;
  infpoint2[0] = xmax + 50.0 * width;
  infpoint2[1] = ymin - 40.0 * width;
  infpoint3[0] = 0.5 * (xmin + xmax);
  infpoint3[1] = ymax + 60.0 * width;

  /* Create the bounding box. */
  maketriangle(&inftri);
  setorg(inftri, infpoint1);
  setdest(inftri, infpoint2);
  setapex(inftri, infpoint3);
  /* Link dummytri to the bounding box so we can always find an */
  /*   edge to begin searching (point location) from.           */
  dummytri[0] = (triangle) inftri.tri;
  if (verbose > 2) {
    printf("  Creating ");
    printtriangle(&inftri);
  }
}

#endif /* not REDUCED */

/*****************************************************************************/
/*                                                                           */
/*  removebox()   Remove the "infinite" bounding triangle, setting boundary  */
/*                markers as appropriate.                                    */
/*                                                                           */
/*  The triangular bounding box has three boundary triangles (one for each   */
/*  side of the bounding box), and a bunch of triangles fanning out from     */
/*  the three bounding box vertices (one triangle for each edge of the       */
/*  convex hull of the inner mesh).  This routine removes these triangles.   */
/*                                                                           */
/*****************************************************************************/

#ifndef REDUCED

long removebox()
{
  struct triedge deadtri;
  struct triedge searchedge;
  struct triedge checkedge;
  struct triedge nextedge, finaledge, dissolveedge;
  point markorg;
  long hullsize;
  triangle ptr;                         /* Temporary variable used by sym(). */

  if (verbose) {
    printf("  Removing triangular bounding box.\n");
  }
  /* Find a boundary triangle. */
  nextedge.tri = dummytri;
  nextedge.orient = 0;
  symself(nextedge);
  /* Mark a place to stop. */
  lprev(nextedge, finaledge);
  lnextself(nextedge);
  symself(nextedge);
  /* Find a triangle (on the boundary of the point set) that isn't */
  /*   a bounding box triangle.                                    */
  lprev(nextedge, searchedge);
  symself(searchedge);
  /* Check whether nextedge is another boundary triangle */
  /*   adjacent to the first one.                        */
  lnext(nextedge, checkedge);
  symself(checkedge);
  if (checkedge.tri == dummytri) {
    /* Go on to the next triangle.  There are only three boundary   */
    /*   triangles, and this next triangle cannot be the third one, */
    /*   so it's safe to stop here.                                 */
    lprevself(searchedge);
    symself(searchedge);
  }
  /* Find a new boundary edge to search from, as the current search */
  /*   edge lies on a bounding box triangle and will be deleted.    */
  dummytri[0] = encode(searchedge);
  hullsize = -2l;
  while (!triedgeequal(nextedge, finaledge)) {
    hullsize++;
    lprev(nextedge, dissolveedge);
    symself(dissolveedge);
    /* If not using a PSLG, the vertices should be marked now. */
    /*   (If using a PSLG, markhull() will do the job.)        */
    if (!poly) {
      /* Be careful!  One must check for the case where all the input   */
      /*   points are collinear, and thus all the triangles are part of */
      /*   the bounding box.  Otherwise, the setpointmark() call below  */
      /*   will cause a bad pointer reference.                          */
      if (dissolveedge.tri != dummytri) {
        org(dissolveedge, markorg);
        if (pointmark(markorg) == 0) {
          setpointmark(markorg, 1);
        }
      }
    }
    /* Disconnect the bounding box triangle from the mesh triangle. */
    dissolve(dissolveedge);
    lnext(nextedge, deadtri);
    sym(deadtri, nextedge);
    /* Get rid of the bounding box triangle. */
    triangledealloc(deadtri.tri);
    /* Do we need to turn the corner? */
    if (nextedge.tri == dummytri) {
      /* Turn the corner. */
      triedgecopy(dissolveedge, nextedge);
    }
  }
  triangledealloc(finaledge.tri);

  free(infpoint1);                  /* Deallocate the bounding box vertices. */
  free(infpoint2);
  free(infpoint3);

  return hullsize;
}

#endif /* not REDUCED */

/*****************************************************************************/
/*                                                                           */
/*  incrementaldelaunay()   Form a Delaunay triangulation by incrementally   */
/*                          adding vertices.                                 */
/*                                                                           */
/*****************************************************************************/

#ifndef REDUCED

long incrementaldelaunay()
{
  struct triedge starttri;
  point pointloop;

  /* Create a triangular bounding box. */
  boundingbox();
  if (verbose) {
    printf("  Incrementally inserting points.\n");
  }
  traversalinit(&points);
  pointloop = pointtraverse();
  while (pointloop != (point) NULL) {
    /* Find a boundary triangle to search from. */
    starttri.tri = (triangle *) NULL;
    if (insertsite(pointloop, &starttri, (struct edge *) NULL, 0, 0) ==
        DUPLICATEPOINT) {
      if (!quiet) {
        printf("Warning:  A duplicate point at (%.12g, %.12g) appeared and was ignored.\n",
               pointloop[0], pointloop[1]);
      }
/*  Commented out - would eliminate point from output .node file.
      setpointmark(pointloop, DEADPOINT);
*/
    }
    pointloop = pointtraverse();
  }
  /* Remove the bounding box. */
  return removebox();
}

#endif /* not REDUCED */

/**                                                                         **/
/**                                                                         **/
/********* Incremental Delaunay triangulation ends here              *********/

/********* Sweepline Delaunay triangulation begins here              *********/
/**                                                                         **/
/**                                                                         **/

#ifndef REDUCED

void eventheapinsert(heap, heapsize, newevent)
struct event **heap;
int heapsize;
struct event *newevent;
{
  REAL eventx, eventy;
  int eventnum;
  int parent;
  int notdone;

  eventx = newevent->xkey;
  eventy = newevent->ykey;
  eventnum = heapsize;
  notdone = eventnum > 0;
  while (notdone) {
    parent = (eventnum - 1) >> 1;
    if ((heap[parent]->ykey < eventy) ||
        ((heap[parent]->ykey == eventy)
         && (heap[parent]->xkey <= eventx))) {
      notdone = 0;
    } else {
      heap[eventnum] = heap[parent];
      heap[eventnum]->heapposition = eventnum;

      eventnum = parent;
      notdone = eventnum > 0;
    }
  }
  heap[eventnum] = newevent;
  newevent->heapposition = eventnum;
}

#endif /* not REDUCED */

#ifndef REDUCED

void eventheapify(heap, heapsize, eventnum)
struct event **heap;
int heapsize;
int eventnum;
{
  struct event *thisevent;
  REAL eventx, eventy;
  int leftchild, rightchild;
  int smallest;
  int notdone;

  thisevent = heap[eventnum];
  eventx = thisevent->xkey;
  eventy = thisevent->ykey;
  leftchild = 2 * eventnum + 1;
  notdone = leftchild < heapsize;
  while (notdone) {
    if ((heap[leftchild]->ykey < eventy) ||
        ((heap[leftchild]->ykey == eventy)
         && (heap[leftchild]->xkey < eventx))) {
      smallest = leftchild;
    } else {
      smallest = eventnum;
    }
    rightchild = leftchild + 1;
    if (rightchild < heapsize) {
      if ((heap[rightchild]->ykey < heap[smallest]->ykey) ||
          ((heap[rightchild]->ykey == heap[smallest]->ykey)
           && (heap[rightchild]->xkey < heap[smallest]->xkey))) {
        smallest = rightchild;
      }
    }
    if (smallest == eventnum) {
      notdone = 0;
    } else {
      heap[eventnum] = heap[smallest];
      heap[eventnum]->heapposition = eventnum;
      heap[smallest] = thisevent;
      thisevent->heapposition = smallest;

      eventnum = smallest;
      leftchild = 2 * eventnum + 1;
      notdone = leftchild < heapsize;
    }
  }
}

#endif /* not REDUCED */

#ifndef REDUCED

void eventheapdelete(heap, heapsize, eventnum)
struct event **heap;
int heapsize;
int eventnum;
{
  struct event *moveevent;
  REAL eventx, eventy;
  int parent;
  int notdone;

  moveevent = heap[heapsize - 1];
  if (eventnum > 0) {
    eventx = moveevent->xkey;
    eventy = moveevent->ykey;
    do {
      parent = (eventnum - 1) >> 1;
      if ((heap[parent]->ykey < eventy) ||
          ((heap[parent]->ykey == eventy)
           && (heap[parent]->xkey <= eventx))) {
        notdone = 0;
      } else {
        heap[eventnum] = heap[parent];
        heap[eventnum]->heapposition = eventnum;

        eventnum = parent;
        notdone = eventnum > 0;
      }
    } while (notdone);
  }
  heap[eventnum] = moveevent;
  moveevent->heapposition = eventnum;
  eventheapify(heap, heapsize - 1, eventnum);
}

#endif /* not REDUCED */

#ifndef REDUCED

void createeventheap(eventheap, events, freeevents)
struct event ***eventheap;
struct event **events;
struct event **freeevents;
{
  point thispoint;
  int maxevents;
  int i;

  maxevents = (3 * inpoints) / 2;
  *eventheap = (struct event **) malloc(maxevents * sizeof(struct event *));
  if (*eventheap == (struct event **) NULL) {
    printf("Error:  Out of memory.\n");
    exit(1);
  }
  *events = (struct event *) malloc(maxevents * sizeof(struct event));
  if (*events == (struct event *) NULL) {
    printf("Error:  Out of memory.\n");
    exit(1);
  }
  traversalinit(&points);
  for (i = 0; i < inpoints; i++) {
    thispoint = pointtraverse();
    (*events)[i].eventptr = (VOID *) thispoint;
    (*events)[i].xkey = thispoint[0];
    (*events)[i].ykey = thispoint[1];
    eventheapinsert(*eventheap, i, *events + i);
  }
  *freeevents = (struct event *) NULL;
  for (i = maxevents - 1; i >= inpoints; i--) {
    (*events)[i].eventptr = (VOID *) *freeevents;
    *freeevents = *events + i;
  }
}

#endif /* not REDUCED */

#ifndef REDUCED

int rightofhyperbola(fronttri, newsite)
struct triedge *fronttri;
point newsite;
{
  point leftpoint, rightpoint;
  REAL dxa, dya, dxb, dyb;

  hyperbolacount++;

  dest(*fronttri, leftpoint);
  apex(*fronttri, rightpoint);
  if ((leftpoint[1] < rightpoint[1])
      || ((leftpoint[1] == rightpoint[1]) && (leftpoint[0] < rightpoint[0]))) {
    if (newsite[0] >= rightpoint[0]) {
      return 1;
    }
  } else {
    if (newsite[0] <= leftpoint[0]) {
      return 0;
    }
  }
  dxa = leftpoint[0] - newsite[0];
  dya = leftpoint[1] - newsite[1];
  dxb = rightpoint[0] - newsite[0];
  dyb = rightpoint[1] - newsite[1];
  return dya * (dxb * dxb + dyb * dyb) > dyb * (dxa * dxa + dya * dya);
}

#endif /* not REDUCED */

#ifndef REDUCED

REAL circletop(pa, pb, pc, ccwabc)
point pa;
point pb;
point pc;
REAL ccwabc;
{
  REAL xac, yac, xbc, ybc, xab, yab;
  REAL aclen2, bclen2, ablen2;

  circletopcount++;

  xac = pa[0] - pc[0];
  yac = pa[1] - pc[1];
  xbc = pb[0] - pc[0];
  ybc = pb[1] - pc[1];
  xab = pa[0] - pb[0];
  yab = pa[1] - pb[1];
  aclen2 = xac * xac + yac * yac;
  bclen2 = xbc * xbc + ybc * ybc;
  ablen2 = xab * xab + yab * yab;
  return pc[1] + (xac * bclen2 - xbc * aclen2 + sqrt(aclen2 * bclen2 * ablen2))
               / (2.0 * ccwabc);
}

#endif /* not REDUCED */

#ifndef REDUCED

void check4deadevent(checktri, freeevents, eventheap, heapsize)
struct triedge *checktri;
struct event **freeevents;
struct event **eventheap;
int *heapsize;
{
  struct event *deadevent;
  point eventpoint;
  int eventnum;

  org(*checktri, eventpoint);
  if (eventpoint != (point) NULL) {
    deadevent = (struct event *) eventpoint;
    eventnum = deadevent->heapposition;
    deadevent->eventptr = (VOID *) *freeevents;
    *freeevents = deadevent;
    eventheapdelete(eventheap, *heapsize, eventnum);
    (*heapsize)--;
    setorg(*checktri, NULL);
  }
}

#endif /* not REDUCED */

#ifndef REDUCED

struct splaynode *splay(splaytree, searchpoint, searchtri)
struct splaynode *splaytree;
point searchpoint;
struct triedge *searchtri;
{
  struct splaynode *child, *grandchild;
  struct splaynode *lefttree, *righttree;
  struct splaynode *leftright;
  point checkpoint;
  int rightofroot, rightofchild;

  if (splaytree == (struct splaynode *) NULL) {
    return (struct splaynode *) NULL;
  }
  dest(splaytree->keyedge, checkpoint);
  if (checkpoint == splaytree->keydest) {
    rightofroot = rightofhyperbola(&splaytree->keyedge, searchpoint);
    if (rightofroot) {
      triedgecopy(splaytree->keyedge, *searchtri);
      child = splaytree->rchild;
    } else {
      child = splaytree->lchild;
    }
    if (child == (struct splaynode *) NULL) {
      return splaytree;
    }
    dest(child->keyedge, checkpoint);
    if (checkpoint != child->keydest) {
      child = splay(child, searchpoint, searchtri);
      if (child == (struct splaynode *) NULL) {
        if (rightofroot) {
          splaytree->rchild = (struct splaynode *) NULL;
        } else {
          splaytree->lchild = (struct splaynode *) NULL;
        }
        return splaytree;
      }
    }
    rightofchild = rightofhyperbola(&child->keyedge, searchpoint);
    if (rightofchild) {
      triedgecopy(child->keyedge, *searchtri);
      grandchild = splay(child->rchild, searchpoint, searchtri);
      child->rchild = grandchild;
    } else {
      grandchild = splay(child->lchild, searchpoint, searchtri);
      child->lchild = grandchild;
    }
    if (grandchild == (struct splaynode *) NULL) {
      if (rightofroot) {
        splaytree->rchild = child->lchild;
        child->lchild = splaytree;
      } else {
        splaytree->lchild = child->rchild;
        child->rchild = splaytree;
      }
      return child;
    }
    if (rightofchild) {
      if (rightofroot) {
        splaytree->rchild = child->lchild;
        child->lchild = splaytree;
      } else {
        splaytree->lchild = grandchild->rchild;
        grandchild->rchild = splaytree;
      }
      child->rchild = grandchild->lchild;
      grandchild->lchild = child;
    } else {
      if (rightofroot) {
        splaytree->rchild = grandchild->lchild;
        grandchild->lchild = splaytree;
      } else {
        splaytree->lchild = child->rchild;
        child->rchild = splaytree;
      }
      child->lchild = grandchild->rchild;
      grandchild->rchild = child;
    }
    return grandchild;
  } else {
    lefttree = splay(splaytree->lchild, searchpoint, searchtri);
    righttree = splay(splaytree->rchild, searchpoint, searchtri);

    pooldealloc(&splaynodes, (VOID *) splaytree);
    if (lefttree == (struct splaynode *) NULL) {
      return righttree;
    } else if (righttree == (struct splaynode *) NULL) {
      return lefttree;
    } else if (lefttree->rchild == (struct splaynode *) NULL) {
      lefttree->rchild = righttree->lchild;
      righttree->lchild = lefttree;
      return righttree;
    } else if (righttree->lchild == (struct splaynode *) NULL) {
      righttree->lchild = lefttree->rchild;
      lefttree->rchild = righttree;
      return lefttree;
    } else {
/*      printf("Holy Toledo!!!\n"); */
      leftright = lefttree->rchild;
      while (leftright->rchild != (struct splaynode *) NULL) {
        leftright = leftright->rchild;
      }
      leftright->rchild = righttree;
      return lefttree;
    }
  }
}

#endif /* not REDUCED */

#ifndef REDUCED

struct splaynode *splayinsert(splayroot, newkey, searchpoint)
struct splaynode *splayroot;
struct triedge *newkey;
point searchpoint;
{
  struct splaynode *newsplaynode;

  newsplaynode = (struct splaynode *) poolalloc(&splaynodes);
  triedgecopy(*newkey, newsplaynode->keyedge);
  dest(*newkey, newsplaynode->keydest);
  if (splayroot == (struct splaynode *) NULL) {
    newsplaynode->lchild = (struct splaynode *) NULL;
    newsplaynode->rchild = (struct splaynode *) NULL;
  } else if (rightofhyperbola(&splayroot->keyedge, searchpoint)) {
    newsplaynode->lchild = splayroot;
    newsplaynode->rchild = splayroot->rchild;
    splayroot->rchild = (struct splaynode *) NULL;
  } else {
    newsplaynode->lchild = splayroot->lchild;
    newsplaynode->rchild = splayroot;
    splayroot->lchild = (struct splaynode *) NULL;
  }
  return newsplaynode;
}

#endif /* not REDUCED */

#ifndef REDUCED

struct splaynode *circletopinsert(splayroot, newkey, pa, pb, pc, topy)
struct splaynode *splayroot;
struct triedge *newkey;
point pa;
point pb;
point pc;
REAL topy;
{
  REAL ccwabc;
  REAL xac, yac, xbc, ybc;
  REAL aclen2, bclen2;
  REAL searchpoint[2];
  struct triedge dummytri;

  ccwabc = counterclockwise(pa, pb, pc);
  xac = pa[0] - pc[0];
  yac = pa[1] - pc[1];
  xbc = pb[0] - pc[0];
  ybc = pb[1] - pc[1];
  aclen2 = xac * xac + yac * yac;
  bclen2 = xbc * xbc + ybc * ybc;
  searchpoint[0] = pc[0] - (yac * bclen2 - ybc * aclen2) / (2.0 * ccwabc);
  searchpoint[1] = topy;
  return splayinsert(splay(splayroot, (point) searchpoint, &dummytri), newkey,
                     (point) searchpoint);
}

#endif /* not REDUCED */

#ifndef REDUCED

struct splaynode *frontlocate(splayroot, bottommost, searchpoint, searchtri, farright)
struct splaynode *splayroot;
struct triedge *bottommost;
point searchpoint;
struct triedge *searchtri;
int *farright;
{
  int farrightflag;
  triangle ptr;                       /* Temporary variable used by onext(). */

  triedgecopy(*bottommost, *searchtri);
  splayroot = splay(splayroot, searchpoint, searchtri);

  farrightflag = 0;
  while (!farrightflag && rightofhyperbola(searchtri, searchpoint)) {
    onextself(*searchtri);
    farrightflag = triedgeequal(*searchtri, *bottommost);
  }
  *farright = farrightflag;
  return splayroot;
}

#endif /* not REDUCED */

#ifndef REDUCED

long sweeplinedelaunay()
{
  struct event **eventheap;
  struct event *events;
  struct event *freeevents;
  struct event *nextevent;
  struct event *newevent;
  struct splaynode *splayroot;
  struct triedge bottommost;
  struct triedge searchtri;
  struct triedge fliptri;
  struct triedge lefttri, righttri, farlefttri, farrighttri;
  struct triedge inserttri;
  point firstpoint, secondpoint;
  point nextpoint, lastpoint;
  point connectpoint;
  point leftpoint, midpoint, rightpoint;
  REAL lefttest, righttest;
  int heapsize;
  int check4events, farrightflag;
  triangle ptr;   /* Temporary variable used by sym(), onext(), and oprev(). */

  poolinit(&splaynodes, sizeof(struct splaynode), SPLAYNODEPERBLOCK, POINTER,
           0);
  splayroot = (struct splaynode *) NULL;

  if (verbose) {
    printf("  Placing points in event heap.\n");
  }
  createeventheap(&eventheap, &events, &freeevents);
  heapsize = inpoints;

  if (verbose) {
    printf("  Forming triangulation.\n");
  }
  maketriangle(&lefttri);
  maketriangle(&righttri);
  bond(lefttri, righttri);
  lnextself(lefttri);
  lprevself(righttri);
  bond(lefttri, righttri);
  lnextself(lefttri);
  lprevself(righttri);
  bond(lefttri, righttri);
  firstpoint = (point) eventheap[0]->eventptr;
  eventheap[0]->eventptr = (VOID *) freeevents;
  freeevents = eventheap[0];
  eventheapdelete(eventheap, heapsize, 0);
  heapsize--;
  do {
    if (heapsize == 0) {
      printf("Error:  Input points are all identical.\n");
      exit(1);
    }
    secondpoint = (point) eventheap[0]->eventptr;
    eventheap[0]->eventptr = (VOID *) freeevents;
    freeevents = eventheap[0];
    eventheapdelete(eventheap, heapsize, 0);
    heapsize--;
    if ((firstpoint[0] == secondpoint[0])
        && (firstpoint[1] == secondpoint[1])) {
      printf("Warning:  A duplicate point at (%.12g, %.12g) appeared and was ignored.\n",
             secondpoint[0], secondpoint[1]);
/*  Commented out - would eliminate point from output .node file.
      setpointmark(secondpoint, DEADPOINT);
*/
    }
  } while ((firstpoint[0] == secondpoint[0])
           && (firstpoint[1] == secondpoint[1]));
  setorg(lefttri, firstpoint);
  setdest(lefttri, secondpoint);
  setorg(righttri, secondpoint);
  setdest(righttri, firstpoint);
  lprev(lefttri, bottommost);
  lastpoint = secondpoint;
  while (heapsize > 0) {
    nextevent = eventheap[0];
    eventheapdelete(eventheap, heapsize, 0);
    heapsize--;
    check4events = 1;
    if (nextevent->xkey < xmin) {
      decode(nextevent->eventptr, fliptri);
      oprev(fliptri, farlefttri);
      check4deadevent(&farlefttri, &freeevents, eventheap, &heapsize);
      onext(fliptri, farrighttri);
      check4deadevent(&farrighttri, &freeevents, eventheap, &heapsize);

      if (triedgeequal(farlefttri, bottommost)) {
        lprev(fliptri, bottommost);
      }
      flip(&fliptri);
      setapex(fliptri, NULL);
      lprev(fliptri, lefttri);
      lnext(fliptri, righttri);
      sym(lefttri, farlefttri);

      if (randomnation(SAMPLERATE) == 0) {
        symself(fliptri);
        dest(fliptri, leftpoint);
        apex(fliptri, midpoint);
        org(fliptri, rightpoint);
        splayroot = circletopinsert(splayroot, &lefttri, leftpoint, midpoint,
                                    rightpoint, nextevent->ykey);
      }
    } else {
      nextpoint = (point) nextevent->eventptr;
      if ((nextpoint[0] == lastpoint[0]) && (nextpoint[1] == lastpoint[1])) {
        printf("Warning:  A duplicate point at (%.12g, %.12g) appeared and was ignored.\n",
               nextpoint[0], nextpoint[1]);
/*  Commented out - would eliminate point from output .node file.
        setpointmark(nextpoint, DEADPOINT);
*/
        check4events = 0;
      } else {
        lastpoint = nextpoint;

        splayroot = frontlocate(splayroot, &bottommost, nextpoint, &searchtri,
                                &farrightflag);
/*
        triedgecopy(bottommost, searchtri);
        farrightflag = 0;
        while (!farrightflag && rightofhyperbola(&searchtri, nextpoint)) {
          onextself(searchtri);
          farrightflag = triedgeequal(searchtri, bottommost);
        }
*/

        check4deadevent(&searchtri, &freeevents, eventheap, &heapsize);

        triedgecopy(searchtri, farrighttri);
        sym(searchtri, farlefttri);
        maketriangle(&lefttri);
        maketriangle(&righttri);
        dest(farrighttri, connectpoint);
        setorg(lefttri, connectpoint);
        setdest(lefttri, nextpoint);
        setorg(righttri, nextpoint);
        setdest(righttri, connectpoint);
        bond(lefttri, righttri);
        lnextself(lefttri);
        lprevself(righttri);
        bond(lefttri, righttri);
        lnextself(lefttri);
        lprevself(righttri);
        bond(lefttri, farlefttri);
        bond(righttri, farrighttri);
        if (!farrightflag && triedgeequal(farrighttri, bottommost)) {
          triedgecopy(lefttri, bottommost);
        }

        if (randomnation(SAMPLERATE) == 0) {
          splayroot = splayinsert(splayroot, &lefttri, nextpoint);
        } else if (randomnation(SAMPLERATE) == 0) {
          lnext(righttri, inserttri);
          splayroot = splayinsert(splayroot, &inserttri, nextpoint);
        }
      }
    }
    nextevent->eventptr = (VOID *) freeevents;
    freeevents = nextevent;

    if (check4events) {
      apex(farlefttri, leftpoint);
      dest(lefttri, midpoint);
      apex(lefttri, rightpoint);
      lefttest = counterclockwise(leftpoint, midpoint, rightpoint);
      if (lefttest > 0.0) {
        newevent = freeevents;
        freeevents = (struct event *) freeevents->eventptr;
        newevent->xkey = xminextreme;
        newevent->ykey = circletop(leftpoint, midpoint, rightpoint, lefttest);
        newevent->eventptr = (VOID *) encode(lefttri);
        eventheapinsert(eventheap, heapsize, newevent);
        heapsize++;
        setorg(lefttri, newevent);
      }
      apex(righttri, leftpoint);
      org(righttri, midpoint);
      apex(farrighttri, rightpoint);
      righttest = counterclockwise(leftpoint, midpoint, rightpoint);
      if (righttest > 0.0) {
        newevent = freeevents;
        freeevents = (struct event *) freeevents->eventptr;
        newevent->xkey = xminextreme;
        newevent->ykey = circletop(leftpoint, midpoint, rightpoint, righttest);
        newevent->eventptr = (VOID *) encode(farrighttri);
        eventheapinsert(eventheap, heapsize, newevent);
        heapsize++;
        setorg(farrighttri, newevent);
      }
    }
  }

  pooldeinit(&splaynodes);
  lprevself(bottommost);
  return removeghosts(&bottommost);
}

#endif /* not REDUCED */

/**                                                                         **/
/**                                                                         **/
/********* Sweepline Delaunay triangulation ends here                *********/

/********* General mesh construction routines begin here             *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  delaunay()   Form a Delaunay triangulation.                              */
/*                                                                           */
/*****************************************************************************/

long delaunay()
{
  eextras = 0;
  initializetrisegpools();

#ifdef REDUCED
  if (!quiet) {
    printf("Constructing Delaunay triangulation by divide-and-conquer method.\n");
  }
  return divconqdelaunay();
#else /* not REDUCED */
  if (!quiet) {
    printf("Constructing Delaunay triangulation ");
    if (incremental) {
      printf("by incremental method.\n");
    } else if (sweepline) {
      printf("by sweepline method.\n");
    } else {
      printf("by divide-and-conquer method.\n");
    }
  }
  if (incremental) {
    return incrementaldelaunay();
  } else if (sweepline) {
    return sweeplinedelaunay();
  } else {
    return divconqdelaunay();
  }
#endif /* not REDUCED */
}

/*****************************************************************************/
/*                                                                           */
/*  reconstruct()   Reconstruct a triangulation from its .ele (and possibly  */
/*                  .poly) file.  Used when the -r switch is used.           */
/*                                                                           */
/*  Reads an .ele file and reconstructs the original mesh.  If the -p switch */
/*  is used, this procedure will also read a .poly file and reconstruct the  */
/*  shell edges of the original mesh.  If the -a switch is used, this        */
/*  procedure will also read an .area file and set a maximum area constraint */
/*  on each triangle.                                                        */
/*                                                                           */
/*  Points that are not corners of triangles, such as nodes on edges of      */
/*  subparametric elements, are discarded.                                   */
/*                                                                           */
/*  This routine finds the adjacencies between triangles (and shell edges)   */
/*  by forming one stack of triangles for each vertex.  Each triangle is on  */
/*  three different stacks simultaneously.  Each triangle's shell edge       */
/*  pointers are used to link the items in each stack.  This memory-saving   */
/*  feature makes the code harder to read.  The most important thing to keep */
/*  in mind is that each triangle is removed from a stack precisely when     */
/*  the corresponding pointer is adjusted to refer to a shell edge rather    */
/*  than the next triangle of the stack.                                     */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

#ifdef TRILIBRARY

int reconstruct(trianglelist, triangleattriblist, trianglearealist, elements,
                corners, attribs, segmentlist, segmentmarkerlist,
                numberofsegments)
int *trianglelist;
REAL *triangleattriblist;
REAL *trianglearealist;
int elements;
int corners;
int attribs;
int *segmentlist;
int *segmentmarkerlist;
int numberofsegments;

#else /* not TRILIBRARY */

long reconstruct(elefilename, areafilename, polyfilename, polyfile)
char *elefilename;
char *areafilename;
char *polyfilename;
FILE *polyfile;

#endif /* not TRILIBRARY */

{
#ifdef TRILIBRARY
  int pointindex;
  int attribindex;
#else /* not TRILIBRARY */
  FILE *elefile;
  FILE *areafile;
  char inputline[INPUTLINESIZE];
  char *stringptr;
  int areaelements;
#endif /* not TRILIBRARY */
  struct triedge triangleloop;
  struct triedge triangleleft;
  struct triedge checktri;
  struct triedge checkleft;
  struct triedge checkneighbor;
  struct edge shelleloop;
  triangle *vertexarray;
  triangle *prevlink;
  triangle nexttri;
  point tdest, tapex;
  point checkdest, checkapex;
  point shorg;
  point killpoint;
  REAL area;
  int corner[3];
  int end[2];
  int killpointindex;
  int incorners;
  int segmentmarkers=0;
  int boundmarker;
  int aroundpoint;
  long hullsize;
  int notfound;
  int elementnumber, segmentnumber;
  int i, j;
  triangle ptr;                         /* Temporary variable used by sym(). */

#ifdef TRILIBRARY
  inelements = elements;
  incorners = corners;
  if (incorners < 3) {
    printf("Error:  Triangles must have at least 3 points.\n");
    exit(1);
  }
  eextras = attribs;
#else /* not TRILIBRARY */
  /* Read the triangles from an .ele file. */
  if (!quiet) {
    printf("Opening %s.\n", elefilename);
  }
  elefile = fopen(elefilename, "r");
  if (elefile == (FILE *) NULL) {
    printf("  Error:  Cannot access file %s.\n", elefilename);
    exit(1);
  }
  /* Read number of triangles, number of points per triangle, and */
  /*   number of triangle attributes from .ele file.              */
  stringptr = readline(inputline, elefile, elefilename);
  inelements = (int) strtol (stringptr, &stringptr, 0);
  stringptr = findfield(stringptr);
  if (*stringptr == '\0') {
    incorners = 3;
  } else {
    incorners = (int) strtol (stringptr, &stringptr, 0);
    if (incorners < 3) {
      printf("Error:  Triangles in %s must have at least 3 points.\n",
             elefilename);
      exit(1);
    }
  }
  stringptr = findfield(stringptr);
  if (*stringptr == '\0') {
    eextras = 0;
  } else {
    eextras = (int) strtol (stringptr, &stringptr, 0);
  }
#endif /* not TRILIBRARY */

  initializetrisegpools();

  /* Create the triangles. */
  for (elementnumber = 1; elementnumber <= inelements; elementnumber++) {
    maketriangle(&triangleloop);
    /* Mark the triangle as living. */
    triangleloop.tri[3] = (triangle) triangleloop.tri;
  }

  if (poly) {
#ifdef TRILIBRARY
    insegments = numberofsegments;
    segmentmarkers = segmentmarkerlist != (int *) NULL;
#else /* not TRILIBRARY */
    /* Read number of segments and number of segment */
    /*   boundary markers from .poly file.           */
    stringptr = readline(inputline, polyfile, inpolyfilename);
    insegments = (int) strtol (stringptr, &stringptr, 0);
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      segmentmarkers = 0;
    } else {
      segmentmarkers = (int) strtol (stringptr, &stringptr, 0);
    }
#endif /* not TRILIBRARY */

    /* Create the shell edges. */
    for (segmentnumber = 1; segmentnumber <= insegments; segmentnumber++) {
      makeshelle(&shelleloop);
      /* Mark the shell edge as living. */
      shelleloop.sh[2] = (shelle) shelleloop.sh;
    }
  }

#ifdef TRILIBRARY
  pointindex = 0;
  attribindex = 0;
#else /* not TRILIBRARY */
  if (vararea) {
    /* Open an .area file, check for consistency with the .ele file. */
    if (!quiet) {
      printf("Opening %s.\n", areafilename);
    }
    areafile = fopen(areafilename, "r");
    if (areafile == (FILE *) NULL) {
      printf("  Error:  Cannot access file %s.\n", areafilename);
      exit(1);
    }
    stringptr = readline(inputline, areafile, areafilename);
    areaelements = (int) strtol (stringptr, &stringptr, 0);
    if (areaelements != inelements) {
      printf("Error:  %s and %s disagree on number of triangles.\n",
             elefilename, areafilename);
      exit(1);
    }
  }
#endif /* not TRILIBRARY */

  if (!quiet) {
    printf("Reconstructing mesh.\n");
  }
  /* Allocate a temporary array that maps each point to some adjacent  */
  /*   triangle.  I took care to allocate all the permanent memory for */
  /*   triangles and shell edges first.                                */
  vertexarray = (triangle *) malloc(points.items * sizeof(triangle));
  if (vertexarray == (triangle *) NULL) {
    printf("Error:  Out of memory.\n");
    exit(1);
  }
  /* Each point is initially unrepresented. */
  for (i = 0; i < points.items; i++) {
    vertexarray[i] = (triangle) dummytri;
  }

  if (verbose) {
    printf("  Assembling triangles.\n");
  }
  /* Read the triangles from the .ele file, and link */
  /*   together those that share an edge.            */
  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  elementnumber = firstnumber;
  while (triangleloop.tri != (triangle *) NULL) {
#ifdef TRILIBRARY
    /* Copy the triangle's three corners. */
    for (j = 0; j < 3; j++) {
      corner[j] = trianglelist[pointindex++];
      if ((corner[j] < firstnumber) || (corner[j] >= firstnumber + inpoints)) {
        printf("Error:  Triangle %d has an invalid vertex index.\n",
               elementnumber);
        exit(1);
      }
    }
#else /* not TRILIBRARY */
    /* Read triangle number and the triangle's three corners. */
    stringptr = readline(inputline, elefile, elefilename);
    for (j = 0; j < 3; j++) {
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        printf("Error:  Triangle %d is missing point %d in %s.\n",
               elementnumber, j + 1, elefilename);
        exit(1);
      } else {
        corner[j] = (int) strtol (stringptr, &stringptr, 0);
        if ((corner[j] < firstnumber) ||
            (corner[j] >= firstnumber + inpoints)) {
          printf("Error:  Triangle %d has an invalid vertex index.\n",
                 elementnumber);
          exit(1);
        }
      }
    }
#endif /* not TRILIBRARY */

    /* Find out about (and throw away) extra nodes. */
    for (j = 3; j < incorners; j++) {
#ifdef TRILIBRARY
      killpointindex = trianglelist[pointindex++];
#else /* not TRILIBRARY */
      stringptr = findfield(stringptr);
      if (*stringptr != '\0') {
        killpointindex = (int) strtol (stringptr, &stringptr, 0);
#endif /* not TRILIBRARY */
        if ((killpointindex >= firstnumber) &&
            (killpointindex < firstnumber + inpoints)) {
          /* Delete the non-corner point if it's not already deleted. */
          killpoint = getpoint(killpointindex);
          if (pointmark(killpoint) != DEADPOINT) {
            pointdealloc(killpoint);
          }
        }
#ifndef TRILIBRARY
      }
#endif /* not TRILIBRARY */
    }

    /* Read the triangle's attributes. */
    for (j = 0; j < eextras; j++) {
#ifdef TRILIBRARY
      setelemattribute(triangleloop, j, triangleattriblist[attribindex++]);
#else /* not TRILIBRARY */
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        setelemattribute(triangleloop, j, 0);
      } else {
        setelemattribute(triangleloop, j,
                         (REAL) strtod (stringptr, &stringptr));
      }
#endif /* not TRILIBRARY */
    }

    if (vararea) {
#ifdef TRILIBRARY
      area = trianglearealist[elementnumber - firstnumber];
#else /* not TRILIBRARY */
      /* Read an area constraint from the .area file. */
      stringptr = readline(inputline, areafile, areafilename);
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        area = -1.0;                      /* No constraint on this triangle. */
      } else {
        area = (REAL) strtod(stringptr, &stringptr);
      }
#endif /* not TRILIBRARY */
      setareabound(triangleloop, area);
    }

    /* Set the triangle's vertices. */
    triangleloop.orient = 0;
    setorg(triangleloop, getpoint(corner[0]));
    setdest(triangleloop, getpoint(corner[1]));
    setapex(triangleloop, getpoint(corner[2]));
    /* Try linking the triangle to others that share these vertices. */
    for (triangleloop.orient = 0; triangleloop.orient < 3; triangleloop.orient++) {
      /* Take the number for the origin of triangleloop. */
      aroundpoint = corner[triangleloop.orient];
      /* Look for other triangles having this vertex. */
      nexttri = vertexarray[aroundpoint - firstnumber];
      /* Link the current triangle to the next one in the stack. */
      triangleloop.tri[6 + triangleloop.orient] = nexttri;
      /* Push the current triangle onto the stack. */
      vertexarray[aroundpoint - firstnumber] = encode(triangleloop);
      decode(nexttri, checktri);
      if (checktri.tri != dummytri) {
        dest(triangleloop, tdest);
        apex(triangleloop, tapex);
        /* Look for other triangles that share an edge. */
        do {
          dest(checktri, checkdest);
          apex(checktri, checkapex);
          if (tapex == checkdest) {
            /* The two triangles share an edge; bond them together. */
            lprev(triangleloop, triangleleft);
            bond(triangleleft, checktri);
          }
          if (tdest == checkapex) {
            /* The two triangles share an edge; bond them together. */
            lprev(checktri, checkleft);
            bond(triangleloop, checkleft);
          }
          /* Find the next triangle in the stack. */
          nexttri = checktri.tri[6 + checktri.orient];
          decode(nexttri, checktri);
        } while (checktri.tri != dummytri);
      }
    }
    triangleloop.tri = triangletraverse();
    elementnumber++;
  }

#ifdef TRILIBRARY
  pointindex = 0;
#else /* not TRILIBRARY */
  fclose(elefile);
  if (vararea) {
    fclose(areafile);
  }
#endif /* not TRILIBRARY */

  hullsize = 0;                      /* Prepare to count the boundary edges. */
  if (poly) {
    if (verbose) {
      printf("  Marking segments in triangulation.\n");
    }
    /* Read the segments from the .poly file, and link them */
    /*   to their neighboring triangles.                    */
    boundmarker = 0;
    traversalinit(&shelles);
    shelleloop.sh = shelletraverse();
    segmentnumber = firstnumber;
    while (shelleloop.sh != (shelle *) NULL) {
#ifdef TRILIBRARY
      end[0] = segmentlist[pointindex++];
      end[1] = segmentlist[pointindex++];
      if (segmentmarkers) {
        boundmarker = segmentmarkerlist[segmentnumber - firstnumber];
      }
#else /* not TRILIBRARY */
      /* Read the endpoints of each segment, and possibly a boundary marker. */
      stringptr = readline(inputline, polyfile, inpolyfilename);
      /* Skip the first (segment number) field. */
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        printf("Error:  Segment %d has no endpoints in %s.\n", segmentnumber,
               polyfilename);
        exit(1);
      } else {
        end[0] = (int) strtol (stringptr, &stringptr, 0);
      }
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        printf("Error:  Segment %d is missing its second endpoint in %s.\n",
               segmentnumber, polyfilename);
        exit(1);
      } else {
        end[1] = (int) strtol (stringptr, &stringptr, 0);
      }
      if (segmentmarkers) {
        stringptr = findfield(stringptr);
        if (*stringptr == '\0') {
          boundmarker = 0;
        } else {
          boundmarker = (int) strtol (stringptr, &stringptr, 0);
        }
      }
#endif /* not TRILIBRARY */
      for (j = 0; j < 2; j++) {
        if ((end[j] < firstnumber) || (end[j] >= firstnumber + inpoints)) {
          printf("Error:  Segment %d has an invalid vertex index.\n",
                 segmentnumber);
          exit(1);
        }
      }

      /* set the shell edge's vertices. */
      shelleloop.shorient = 0;
      setsorg(shelleloop, getpoint(end[0]));
      setsdest(shelleloop, getpoint(end[1]));
      setmark(shelleloop, boundmarker);
      /* Try linking the shell edge to triangles that share these vertices. */
      for (shelleloop.shorient = 0; shelleloop.shorient < 2;
           shelleloop.shorient++) {
        /* Take the number for the destination of shelleloop. */
        aroundpoint = end[1 - shelleloop.shorient];
        /* Look for triangles having this vertex. */
        prevlink = &vertexarray[aroundpoint - firstnumber];
        nexttri = vertexarray[aroundpoint - firstnumber];
        decode(nexttri, checktri);
        sorg(shelleloop, shorg);
        notfound = 1;
        /* Look for triangles having this edge.  Note that I'm only       */
        /*   comparing each triangle's destination with the shell edge;   */
        /*   each triangle's apex is handled through a different vertex.  */
        /*   Because each triangle appears on three vertices' lists, each */
        /*   occurrence of a triangle on a list can (and does) represent  */
        /*   an edge.  In this way, most edges are represented twice, and */
        /*   every triangle-segment bond is represented once.             */
        while (notfound && (checktri.tri != dummytri)) {
          dest(checktri, checkdest);
          if (shorg == checkdest) {
            /* We have a match.  Remove this triangle from the list. */
            *prevlink = checktri.tri[6 + checktri.orient];
            /* Bond the shell edge to the triangle. */
            tsbond(checktri, shelleloop);
            /* Check if this is a boundary edge. */
            sym(checktri, checkneighbor);
            if (checkneighbor.tri == dummytri) {
              /* The next line doesn't insert a shell edge (because there's */
              /*   already one there), but it sets the boundary markers of  */
              /*   the existing shell edge and its vertices.                */
              insertshelle(&checktri, 1);
              hullsize++;
            }
            notfound = 0;
          }
          /* Find the next triangle in the stack. */
          prevlink = &checktri.tri[6 + checktri.orient];
          nexttri = checktri.tri[6 + checktri.orient];
          decode(nexttri, checktri);
        }
      }
      shelleloop.sh = shelletraverse();
      segmentnumber++;
    }
  }

  /* Mark the remaining edges as not being attached to any shell edge. */
  /* Also, count the (yet uncounted) boundary edges.                   */
  for (i = 0; i < points.items; i++) {
    /* Search the stack of triangles adjacent to a point. */
    nexttri = vertexarray[i];
    decode(nexttri, checktri);
    while (checktri.tri != dummytri) {
      /* Find the next triangle in the stack before this */
      /*   information gets overwritten.                 */
      nexttri = checktri.tri[6 + checktri.orient];
      /* No adjacent shell edge.  (This overwrites the stack info.) */
      tsdissolve(checktri);
      sym(checktri, checkneighbor);
      if (checkneighbor.tri == dummytri) {
        insertshelle(&checktri, 1);
        hullsize++;
      }
      decode(nexttri, checktri);
    }
  }

  free(vertexarray);
  return hullsize;
}

#endif /* not CDT_ONLY */

/**                                                                         **/
/**                                                                         **/
/********* General mesh construction routines end here               *********/

/********* Segment (shell edge) insertion begins here                *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  finddirection()   Find the first triangle on the path from one point     */
/*                    to another.                                            */
/*                                                                           */
/*  Finds the triangle that intersects a line segment drawn from the         */
/*  origin of `searchtri' to the point `endpoint', and returns the result    */
/*  in `searchtri'.  The origin of `searchtri' does not change, even though  */
/*  the triangle returned may differ from the one passed in.  This routine   */
/*  is used to find the direction to move in to get from one point to        */
/*  another.                                                                 */
/*                                                                           */
/*  The return value notes whether the destination or apex of the found      */
/*  triangle is collinear with the two points in question.                   */
/*                                                                           */
/*****************************************************************************/

enum finddirectionresult finddirection(searchtri, endpoint)
struct triedge *searchtri;
point endpoint;
{
  struct triedge checktri;
  point startpoint;
  point leftpoint, rightpoint;
  REAL leftccw, rightccw;
  int leftflag, rightflag;
  triangle ptr;           /* Temporary variable used by onext() and oprev(). */

  org(*searchtri, startpoint);
  dest(*searchtri, rightpoint);
  apex(*searchtri, leftpoint);
  /* Is `endpoint' to the left? */
  leftccw = counterclockwise(endpoint, startpoint, leftpoint);
  leftflag = leftccw > 0.0;
  /* Is `endpoint' to the right? */
  rightccw = counterclockwise(startpoint, endpoint, rightpoint);
  rightflag = rightccw > 0.0;
  if (leftflag && rightflag) {
    /* `searchtri' faces directly away from `endpoint'.  We could go */
    /*   left or right.  Ask whether it's a triangle or a boundary   */
    /*   on the left.                                                */
    onext(*searchtri, checktri);
    if (checktri.tri == dummytri) {
      leftflag = 0;
    } else {
      rightflag = 0;
    }
  }
  while (leftflag) {
    /* Turn left until satisfied. */
    onextself(*searchtri);
    if (searchtri->tri == dummytri) {
      printf("Internal error in finddirection():  Unable to find a\n");
      printf("  triangle leading from (%.12g, %.12g) to", startpoint[0],
             startpoint[1]);
      printf("  (%.12g, %.12g).\n", endpoint[0], endpoint[1]);
      internalerror();
    }
    apex(*searchtri, leftpoint);
    rightccw = leftccw;
    leftccw = counterclockwise(endpoint, startpoint, leftpoint);
    leftflag = leftccw > 0.0;
  }
  while (rightflag) {
    /* Turn right until satisfied. */
    oprevself(*searchtri);
    if (searchtri->tri == dummytri) {
      printf("Internal error in finddirection():  Unable to find a\n");
      printf("  triangle leading from (%.12g, %.12g) to", startpoint[0],
             startpoint[1]);
      printf("  (%.12g, %.12g).\n", endpoint[0], endpoint[1]);
      internalerror();
    }
    dest(*searchtri, rightpoint);
    leftccw = rightccw;
    rightccw = counterclockwise(startpoint, endpoint, rightpoint);
    rightflag = rightccw > 0.0;
  }
  if (leftccw == 0.0) {
    return LEFTCOLLINEAR;
  } else if (rightccw == 0.0) {
    return RIGHTCOLLINEAR;
  } else {
    return WITHIN;
  }
}

/*****************************************************************************/
/*                                                                           */
/*  segmentintersection()   Find the intersection of an existing segment     */
/*                          and a segment that is being inserted.  Insert    */
/*                          a point at the intersection, splitting an        */
/*                          existing shell edge.                             */
/*                                                                           */
/*  The segment being inserted connects the apex of splittri to endpoint2.   */
/*  splitshelle is the shell edge being split, and MUST be opposite          */
/*  splittri.  Hence, the edge being split connects the origin and           */
/*  destination of splittri.                                                 */
/*                                                                           */
/*  On completion, splittri is a handle having the newly inserted            */
/*  intersection point as its origin, and endpoint1 as its destination.      */
/*                                                                           */
/*****************************************************************************/

void segmentintersection(splittri, splitshelle, endpoint2)
struct triedge *splittri;
struct edge *splitshelle;
point endpoint2;
{
  point endpoint1;
  point torg, tdest;
  point leftpoint, rightpoint;
  point newpoint;
  enum insertsiteresult success;
  /*enum finddirectionresult collinear;*/
  REAL ex, ey;
  REAL tx, ty;
  REAL etx, ety;
  REAL split, denom;
  int i;
  triangle ptr;                       /* Temporary variable used by onext(). */

  /* Find the other three segment endpoints. */
  apex(*splittri, endpoint1);
  org(*splittri, torg);
  dest(*splittri, tdest);
  /* Segment intersection formulae; see the Antonio reference. */
  tx = tdest[0] - torg[0];
  ty = tdest[1] - torg[1];
  ex = endpoint2[0] - endpoint1[0];
  ey = endpoint2[1] - endpoint1[1];
  etx = torg[0] - endpoint2[0];
  ety = torg[1] - endpoint2[1];
  denom = ty * ex - tx * ey;
  if (denom == 0.0) {
    printf("Internal error in segmentintersection():");
    printf("  Attempt to find intersection of parallel segments.\n");
    internalerror();
  }
  split = (ey * etx - ex * ety) / denom;
  /* Create the new point. */
  newpoint = (point) poolalloc(&points);
  /* Interpolate its coordinate and attributes. */
  for (i = 0; i < 2 + nextras; i++) {
    newpoint[i] = torg[i] + split * (tdest[i] - torg[i]);
  }
  setpointmark(newpoint, mark(*splitshelle));
  if (verbose > 1) {
    printf("  Splitting edge (%.12g, %.12g) (%.12g, %.12g) at (%.12g, %.12g).\n",
           torg[0], torg[1], tdest[0], tdest[1], newpoint[0], newpoint[1]);
  }
  /* Insert the intersection point.  This should always succeed. */
  success = insertsite(newpoint, splittri, splitshelle, 0, 0);
  if (success != SUCCESSFULPOINT) {
    printf("Internal error in segmentintersection():\n");
    printf("  Failure to split a segment.\n");
    internalerror();
  }
  if (steinerleft > 0) {
    steinerleft--;
  }
  /* Inserting the point may have caused edge flips.  We wish to rediscover */
  /*   the edge connecting endpoint1 to the new intersection point.         */
  /*collinear =*/ finddirection(splittri, endpoint1);
  dest(*splittri, rightpoint);
  apex(*splittri, leftpoint);
  if ((leftpoint[0] == endpoint1[0]) && (leftpoint[1] == endpoint1[1])) {
    onextself(*splittri);
  } else if ((rightpoint[0] != endpoint1[0]) ||
             (rightpoint[1] != endpoint1[1])) {
    printf("Internal error in segmentintersection():\n");
    printf("  Topological inconsistency after splitting a segment.\n");
    internalerror();
  }
  /* `splittri' should have destination endpoint1. */
}

/*****************************************************************************/
/*                                                                           */
/*  scoutsegment()   Scout the first triangle on the path from one endpoint  */
/*                   to another, and check for completion (reaching the      */
/*                   second endpoint), a collinear point, and the            */
/*                   intersection of two segments.                           */
/*                                                                           */
/*  Returns one if the entire segment is successfully inserted, and zero if  */
/*  the job must be finished by conformingedge() or constrainededge().       */
/*                                                                           */
/*  If the first triangle on the path has the second endpoint as its         */
/*  destination or apex, a shell edge is inserted and the job is done.       */
/*                                                                           */
/*  If the first triangle on the path has a destination or apex that lies on */
/*  the segment, a shell edge is inserted connecting the first endpoint to   */
/*  the collinear point, and the search is continued from the collinear      */
/*  point.                                                                   */
/*                                                                           */
/*  If the first triangle on the path has a shell edge opposite its origin,  */
/*  then there is a segment that intersects the segment being inserted.      */
/*  Their intersection point is inserted, splitting the shell edge.          */
/*                                                                           */
/*  Otherwise, return zero.                                                  */
/*                                                                           */
/*****************************************************************************/

int scoutsegment(searchtri, endpoint2, newmark)
struct triedge *searchtri;
point endpoint2;
int newmark;
{
  struct triedge crosstri;
  struct edge crossedge;
  point leftpoint, rightpoint;
  /*point endpoint1;*/
  enum finddirectionresult collinear;
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  collinear = finddirection(searchtri, endpoint2);
  dest(*searchtri, rightpoint);
  apex(*searchtri, leftpoint);
  if (((leftpoint[0] == endpoint2[0]) && (leftpoint[1] == endpoint2[1])) ||
      ((rightpoint[0] == endpoint2[0]) && (rightpoint[1] == endpoint2[1]))) {
    /* The segment is already an edge in the mesh. */
    if ((leftpoint[0] == endpoint2[0]) && (leftpoint[1] == endpoint2[1])) {
      lprevself(*searchtri);
    }
    /* Insert a shell edge, if there isn't already one there. */
    insertshelle(searchtri, newmark);
    return 1;
  } else if (collinear == LEFTCOLLINEAR) {
    /* We've collided with a point between the segment's endpoints. */
    /* Make the collinear point be the triangle's origin. */
    lprevself(*searchtri);
    insertshelle(searchtri, newmark);
    /* Insert the remainder of the segment. */
    return scoutsegment(searchtri, endpoint2, newmark);
  } else if (collinear == RIGHTCOLLINEAR) {
    /* We've collided with a point between the segment's endpoints. */
    insertshelle(searchtri, newmark);
    /* Make the collinear point be the triangle's origin. */
    lnextself(*searchtri);
    /* Insert the remainder of the segment. */
    return scoutsegment(searchtri, endpoint2, newmark);
  } else {
    lnext(*searchtri, crosstri);
    tspivot(crosstri, crossedge);
    /* Check for a crossing segment. */
    if (crossedge.sh == dummysh) {
      return 0;
    } else {
      /*org(*searchtri, endpoint1);*/
      /* Insert a point at the intersection. */
      segmentintersection(&crosstri, &crossedge, endpoint2);
      triedgecopy(crosstri, *searchtri);
      insertshelle(searchtri, newmark);
      /* Insert the remainder of the segment. */
      return scoutsegment(searchtri, endpoint2, newmark);
    }
  }
}

/*****************************************************************************/
/*                                                                           */
/*  conformingedge()   Force a segment into a conforming Delaunay            */
/*                     triangulation by inserting a point at its midpoint,   */
/*                     and recursively forcing in the two half-segments if   */
/*                     necessary.                                            */
/*                                                                           */
/*  Generates a sequence of edges connecting `endpoint1' to `endpoint2'.     */
/*  `newmark' is the boundary marker of the segment, assigned to each new    */
/*  splitting point and shell edge.                                          */
/*                                                                           */
/*  Note that conformingedge() does not always maintain the conforming       */
/*  Delaunay property.  Once inserted, segments are locked into place;       */
/*  points inserted later (to force other segments in) may render these      */
/*  fixed segments non-Delaunay.  The conforming Delaunay property will be   */
/*  restored by enforcequality() by splitting encroached segments.           */
/*                                                                           */
/*****************************************************************************/

#ifndef REDUCED
#ifndef CDT_ONLY

void conformingedge(endpoint1, endpoint2, newmark)
point endpoint1;
point endpoint2;
int newmark;
{
  struct triedge searchtri1, searchtri2;
  struct edge brokenshelle;
  point newpoint;
  point midpoint1, midpoint2;
  enum insertsiteresult success;
  int result1, result2;
  int i;
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  if (verbose > 2) {
    printf("Forcing segment into triangulation by recursive splitting:\n");
    printf("  (%.12g, %.12g) (%.12g, %.12g)\n", endpoint1[0], endpoint1[1],
           endpoint2[0], endpoint2[1]);
  }
  /* Create a new point to insert in the middle of the segment. */
  newpoint = (point) poolalloc(&points);
  /* Interpolate coordinates and attributes. */
  for (i = 0; i < 2 + nextras; i++) {
    newpoint[i] = 0.5 * (endpoint1[i] + endpoint2[i]);
  }
  setpointmark(newpoint, newmark);
  /* Find a boundary triangle to search from. */
  searchtri1.tri = (triangle *) NULL;
  /* Attempt to insert the new point. */
  success = insertsite(newpoint, &searchtri1, (struct edge *) NULL, 0, 0);
  if (success == DUPLICATEPOINT) {
    if (verbose > 2) {
      printf("  Segment intersects existing point (%.12g, %.12g).\n",
             newpoint[0], newpoint[1]);
    }
    /* Use the point that's already there. */
    pointdealloc(newpoint);
    /*org(searchtri1, newpoint);*/
  } else {
    if (success == VIOLATINGPOINT) {
      if (verbose > 2) {
        printf("  Two segments intersect at (%.12g, %.12g).\n",
               newpoint[0], newpoint[1]);
      }
      /* By fluke, we've landed right on another segment.  Split it. */
      tspivot(searchtri1, brokenshelle);
      success = insertsite(newpoint, &searchtri1, &brokenshelle, 0, 0);
      if (success != SUCCESSFULPOINT) {
        printf("Internal error in conformingedge():\n");
        printf("  Failure to split a segment.\n");
        internalerror();
      }
    }
    /* The point has been inserted successfully. */
    if (steinerleft > 0) {
      steinerleft--;
    }
  }
  triedgecopy(searchtri1, searchtri2);
  result1 = scoutsegment(&searchtri1, endpoint1, newmark);
  result2 = scoutsegment(&searchtri2, endpoint2, newmark);
  if (!result1) {
    /* The origin of searchtri1 may have changed if a collision with an */
    /*   intervening vertex on the segment occurred.                    */
    org(searchtri1, midpoint1);
    conformingedge(midpoint1, endpoint1, newmark);
  }
  if (!result2) {
    /* The origin of searchtri2 may have changed if a collision with an */
    /*   intervening vertex on the segment occurred.                    */
    org(searchtri2, midpoint2);
    conformingedge(midpoint2, endpoint2, newmark);
  }
}

#endif /* not CDT_ONLY */
#endif /* not REDUCED */

/*****************************************************************************/
/*                                                                           */
/*  delaunayfixup()   Enforce the Delaunay condition at an edge, fanning out */
/*                    recursively from an existing point.  Pay special       */
/*                    attention to stacking inverted triangles.              */
/*                                                                           */
/*  This is a support routine for inserting segments into a constrained      */
/*  Delaunay triangulation.                                                  */
/*                                                                           */
/*  The origin of fixuptri is treated as if it has just been inserted, and   */
/*  the local Delaunay condition needs to be enforced.  It is only enforced  */
/*  in one sector, however, that being the angular range defined by          */
/*  fixuptri.                                                                */
/*                                                                           */
/*  This routine also needs to make decisions regarding the "stacking" of    */
/*  triangles.  (Read the description of constrainededge() below before      */
/*  reading on here, so you understand the algorithm.)  If the position of   */
/*  the new point (the origin of fixuptri) indicates that the vertex before  */
/*  it on the polygon is a reflex vertex, then "stack" the triangle by       */
/*  doing nothing.  (fixuptri is an inverted triangle, which is how stacked  */
/*  triangles are identified.)                                               */
/*                                                                           */
/*  Otherwise, check whether the vertex before that was a reflex vertex.     */
/*  If so, perform an edge flip, thereby eliminating an inverted triangle    */
/*  (popping it off the stack).  The edge flip may result in the creation    */
/*  of a new inverted triangle, depending on whether or not the new vertex   */
/*  is visible to the vertex three edges behind on the polygon.              */
/*                                                                           */
/*  If neither of the two vertices behind the new vertex are reflex          */
/*  vertices, fixuptri and fartri, the triangle opposite it, are not         */
/*  inverted; hence, ensure that the edge between them is locally Delaunay.  */
/*                                                                           */
/*  `leftside' indicates whether or not fixuptri is to the left of the       */
/*  segment being inserted.  (Imagine that the segment is pointing up from   */
/*  endpoint1 to endpoint2.)                                                 */
/*                                                                           */
/*****************************************************************************/

void delaunayfixup(fixuptri, leftside)
struct triedge *fixuptri;
int leftside;
{
  struct triedge neartri;
  struct triedge fartri;
  struct edge faredge;
  point nearpoint, leftpoint, rightpoint, farpoint;
  triangle ptr;                         /* Temporary variable used by sym(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  lnext(*fixuptri, neartri);
  sym(neartri, fartri);
  /* Check if the edge opposite the origin of fixuptri can be flipped. */
  if (fartri.tri == dummytri) {
    return;
  }
  tspivot(neartri, faredge);
  if (faredge.sh != dummysh) {
    return;
  }
  /* Find all the relevant vertices. */
  apex(neartri, nearpoint);
  org(neartri, leftpoint);
  dest(neartri, rightpoint);
  apex(fartri, farpoint);
  /* Check whether the previous polygon vertex is a reflex vertex. */
  if (leftside) {
    if (counterclockwise(nearpoint, leftpoint, farpoint) <= 0.0) {
      /* leftpoint is a reflex vertex too.  Nothing can */
      /*   be done until a convex section is found.     */
      return;
    }
  } else {
    if (counterclockwise(farpoint, rightpoint, nearpoint) <= 0.0) {
      /* rightpoint is a reflex vertex too.  Nothing can */
      /*   be done until a convex section is found.      */
      return;
    }
  }
  if (counterclockwise(rightpoint, leftpoint, farpoint) > 0.0) {
    /* fartri is not an inverted triangle, and farpoint is not a reflex */
    /*   vertex.  As there are no reflex vertices, fixuptri isn't an    */
    /*   inverted triangle, either.  Hence, test the edge between the   */
    /*   triangles to ensure it is locally Delaunay.                    */
    if (incircle(leftpoint, farpoint, rightpoint, nearpoint) <= 0.0) {
      return;
    }
    /* Not locally Delaunay; go on to an edge flip. */
  }        /* else fartri is inverted; remove it from the stack by flipping. */
  flip(&neartri);
  lprevself(*fixuptri);    /* Restore the origin of fixuptri after the flip. */
  /* Recursively process the two triangles that result from the flip. */
  delaunayfixup(fixuptri, leftside);
  delaunayfixup(&fartri, leftside);
}

/*****************************************************************************/
/*                                                                           */
/*  constrainededge()   Force a segment into a constrained Delaunay          */
/*                      triangulation by deleting the triangles it           */
/*                      intersects, and triangulating the polygons that      */
/*                      form on each side of it.                             */
/*                                                                           */
/*  Generates a single edge connecting `endpoint1' to `endpoint2'.  The      */
/*  triangle `starttri' has `endpoint1' as its origin.  `newmark' is the     */
/*  boundary marker of the segment.                                          */
/*                                                                           */
/*  To insert a segment, every triangle whose interior intersects the        */
/*  segment is deleted.  The union of these deleted triangles is a polygon   */
/*  (which is not necessarily monotone, but is close enough), which is       */
/*  divided into two polygons by the new segment.  This routine's task is    */
/*  to generate the Delaunay triangulation of these two polygons.            */
/*                                                                           */
/*  You might think of this routine's behavior as a two-step process.  The   */
/*  first step is to walk from endpoint1 to endpoint2, flipping each edge    */
/*  encountered.  This step creates a fan of edges connected to endpoint1,   */
/*  including the desired edge to endpoint2.  The second step enforces the   */
/*  Delaunay condition on each side of the segment in an incremental manner: */
/*  proceeding along the polygon from endpoint1 to endpoint2 (this is done   */
/*  independently on each side of the segment), each vertex is "enforced"    */
/*  as if it had just been inserted, but affecting only the previous         */
/*  vertices.  The result is the same as if the vertices had been inserted   */
/*  in the order they appear on the polygon, so the result is Delaunay.      */
/*                                                                           */
/*  In truth, constrainededge() interleaves these two steps.  The procedure  */
/*  walks from endpoint1 to endpoint2, and each time an edge is encountered  */
/*  and flipped, the newly exposed vertex (at the far end of the flipped     */
/*  edge) is "enforced" upon the previously flipped edges, usually affecting */
/*  only one side of the polygon (depending upon which side of the segment   */
/*  the vertex falls on).                                                    */
/*                                                                           */
/*  The algorithm is complicated by the need to handle polygons that are not */
/*  convex.  Although the polygon is not necessarily monotone, it can be     */
/*  triangulated in a manner similar to the stack-based algorithms for       */
/*  monotone polygons.  For each reflex vertex (local concavity) of the      */
/*  polygon, there will be an inverted triangle formed by one of the edge    */
/*  flips.  (An inverted triangle is one with negative area - that is, its   */
/*  vertices are arranged in clockwise order - and is best thought of as a   */
/*  wrinkle in the fabric of the mesh.)  Each inverted triangle can be       */
/*  thought of as a reflex vertex pushed on the stack, waiting to be fixed   */
/*  later.                                                                   */
/*                                                                           */
/*  A reflex vertex is popped from the stack when a vertex is inserted that  */
/*  is visible to the reflex vertex.  (However, if the vertex behind the     */
/*  reflex vertex is not visible to the reflex vertex, a new inverted        */
/*  triangle will take its place on the stack.)  These details are handled   */
/*  by the delaunayfixup() routine above.                                    */
/*                                                                           */
/*****************************************************************************/

void constrainededge(starttri, endpoint2, newmark)
struct triedge *starttri;
point endpoint2;
int newmark;
{
  struct triedge fixuptri, fixuptri2;
  struct edge fixupedge;
  point endpoint1;
  point farpoint;
  REAL area;
  int collision;
  int done;
  triangle ptr;             /* Temporary variable used by sym() and oprev(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  org(*starttri, endpoint1);
  lnext(*starttri, fixuptri);
  flip(&fixuptri);
  /* `collision' indicates whether we have found a point directly */
  /*   between endpoint1 and endpoint2.                           */
  collision = 0;
  done = 0;
  do {
    org(fixuptri, farpoint);
    /* `farpoint' is the extreme point of the polygon we are "digging" */
    /*   to get from endpoint1 to endpoint2.                           */
    if ((farpoint[0] == endpoint2[0]) && (farpoint[1] == endpoint2[1])) {
      oprev(fixuptri, fixuptri2);
      /* Enforce the Delaunay condition around endpoint2. */
      delaunayfixup(&fixuptri, 0);
      delaunayfixup(&fixuptri2, 1);
      done = 1;
    } else {
      /* Check whether farpoint is to the left or right of the segment */
      /*   being inserted, to decide which edge of fixuptri to dig     */
      /*   through next.                                               */
      area = counterclockwise(endpoint1, endpoint2, farpoint);
      if (area == 0.0) {
        /* We've collided with a point between endpoint1 and endpoint2. */
        collision = 1;
        oprev(fixuptri, fixuptri2);
        /* Enforce the Delaunay condition around farpoint. */
        delaunayfixup(&fixuptri, 0);
        delaunayfixup(&fixuptri2, 1);
        done = 1;
      } else {
        if (area > 0.0) {         /* farpoint is to the left of the segment. */
          oprev(fixuptri, fixuptri2);
          /* Enforce the Delaunay condition around farpoint, on the */
          /*   left side of the segment only.                       */
          delaunayfixup(&fixuptri2, 1);
          /* Flip the edge that crosses the segment.  After the edge is */
          /*   flipped, one of its endpoints is the fan vertex, and the */
          /*   destination of fixuptri is the fan vertex.               */
          lprevself(fixuptri);
        } else {                 /* farpoint is to the right of the segment. */
          delaunayfixup(&fixuptri, 0);
          /* Flip the edge that crosses the segment.  After the edge is */
          /*   flipped, one of its endpoints is the fan vertex, and the */
          /*   destination of fixuptri is the fan vertex.               */
          oprevself(fixuptri);
        }
        /* Check for two intersecting segments. */
        tspivot(fixuptri, fixupedge);
        if (fixupedge.sh == dummysh) {
          flip(&fixuptri);   /* May create an inverted triangle on the left. */
        } else {
          /* We've collided with a segment between endpoint1 and endpoint2. */
          collision = 1;
          /* Insert a point at the intersection. */
          segmentintersection(&fixuptri, &fixupedge, endpoint2);
          done = 1;
        }
      }
    }
  } while (!done);
  /* Insert a shell edge to make the segment permanent. */
  insertshelle(&fixuptri, newmark);
  /* If there was a collision with an interceding vertex, install another */
  /*   segment connecting that vertex with endpoint2.                     */
  if (collision) {
    /* Insert the remainder of the segment. */
    if (!scoutsegment(&fixuptri, endpoint2, newmark)) {
      constrainededge(&fixuptri, endpoint2, newmark);
    }
  }
}

/*****************************************************************************/
/*                                                                           */
/*  insertsegment()   Insert a PSLG segment into a triangulation.            */
/*                                                                           */
/*****************************************************************************/

void insertsegment(endpoint1, endpoint2, newmark)
point endpoint1;
point endpoint2;
int newmark;
{
  struct triedge searchtri1, searchtri2;
  triangle encodedtri;
  point checkpoint;
  triangle ptr;                         /* Temporary variable used by sym(). */

  if (verbose > 1) {
    printf("  Connecting (%.12g, %.12g) to (%.12g, %.12g).\n",
           endpoint1[0], endpoint1[1], endpoint2[0], endpoint2[1]);
  }

  /* Find a triangle whose origin is the segment's first endpoint. */
  checkpoint = (point) NULL;
  encodedtri = point2tri(endpoint1);
  if (encodedtri != (triangle) NULL) {
    decode(encodedtri, searchtri1);
    org(searchtri1, checkpoint);
  }
  if (checkpoint != endpoint1) {
    /* Find a boundary triangle to search from. */
    searchtri1.tri = dummytri;
    searchtri1.orient = 0;
    symself(searchtri1);
    /* Search for the segment's first endpoint by point location. */
    if (locate(endpoint1, &searchtri1) != ONVERTEX) {
      printf("Internal error in insertsegment():  Unable to locate PSLG point\n");
      printf("  (%.12g, %.12g) in triangulation.\n",
             endpoint1[0], endpoint1[1]);
      internalerror();
    }
  }
  /* Remember this triangle to improve subsequent point location. */
  triedgecopy(searchtri1, recenttri);
  /* Scout the beginnings of a path from the first endpoint */
  /*   toward the second.                                   */
  if (scoutsegment(&searchtri1, endpoint2, newmark)) {
    /* The segment was easily inserted. */
    return;
  }
  /* The first endpoint may have changed if a collision with an intervening */
  /*   vertex on the segment occurred.                                      */
  org(searchtri1, endpoint1);

  /* Find a triangle whose origin is the segment's second endpoint. */
  checkpoint = (point) NULL;
  encodedtri = point2tri(endpoint2);
  if (encodedtri != (triangle) NULL) {
    decode(encodedtri, searchtri2);
    org(searchtri2, checkpoint);
  }
  if (checkpoint != endpoint2) {
    /* Find a boundary triangle to search from. */
    searchtri2.tri = dummytri;
    searchtri2.orient = 0;
    symself(searchtri2);
    /* Search for the segment's second endpoint by point location. */
    if (locate(endpoint2, &searchtri2) != ONVERTEX) {
      printf("Internal error in insertsegment():  Unable to locate PSLG point\n");
      printf("  (%.12g, %.12g) in triangulation.\n",
             endpoint2[0], endpoint2[1]);
      internalerror();
    }
  }
  /* Remember this triangle to improve subsequent point location. */
  triedgecopy(searchtri2, recenttri);
  /* Scout the beginnings of a path from the second endpoint */
  /*   toward the first.                                     */
  if (scoutsegment(&searchtri2, endpoint1, newmark)) {
    /* The segment was easily inserted. */
    return;
  }
  /* The second endpoint may have changed if a collision with an intervening */
  /*   vertex on the segment occurred.                                       */
  org(searchtri2, endpoint2);

#ifndef REDUCED
#ifndef CDT_ONLY
  if (splitseg) {
    /* Insert vertices to force the segment into the triangulation. */
    conformingedge(endpoint1, endpoint2, newmark);
  } else {
#endif /* not CDT_ONLY */
#endif /* not REDUCED */
    /* Insert the segment directly into the triangulation. */
    constrainededge(&searchtri1, endpoint2, newmark);
#ifndef REDUCED
#ifndef CDT_ONLY
  }
#endif /* not CDT_ONLY */
#endif /* not REDUCED */
}

/*****************************************************************************/
/*                                                                           */
/*  markhull()   Cover the convex hull of a triangulation with shell edges.  */
/*                                                                           */
/*****************************************************************************/

void markhull()
{
  struct triedge hulltri;
  struct triedge nexttri;
  struct triedge starttri;
  triangle ptr;             /* Temporary variable used by sym() and oprev(). */

  /* Find a triangle handle on the hull. */
  hulltri.tri = dummytri;
  hulltri.orient = 0;
  symself(hulltri);
  /* Remember where we started so we know when to stop. */
  triedgecopy(hulltri, starttri);
  /* Go once counterclockwise around the convex hull. */
  do {
    /* Create a shell edge if there isn't already one here. */
    insertshelle(&hulltri, 1);
    /* To find the next hull edge, go clockwise around the next vertex. */
    lnextself(hulltri);
    oprev(hulltri, nexttri);
    while (nexttri.tri != dummytri) {
      triedgecopy(nexttri, hulltri);
      oprev(hulltri, nexttri);
    }
  } while (!triedgeequal(hulltri, starttri));
}

/*****************************************************************************/
/*                                                                           */
/*  formskeleton()   Create the shell edges of a triangulation, including    */
/*                   PSLG edges and edges on the convex hull.                */
/*                                                                           */
/*  The PSLG edges are read from a .poly file.  The return value is the      */
/*  number of segments in the file.                                          */
/*                                                                           */
/*****************************************************************************/

#ifdef TRILIBRARY

int formskeleton(segmentlist, segmentmarkerlist, numberofsegments)
int *segmentlist;
int *segmentmarkerlist;
int numberofsegments;

#else /* not TRILIBRARY */

int formskeleton(polyfile, polyfilename)
FILE *polyfile;
char *polyfilename;

#endif /* not TRILIBRARY */

{
#ifdef TRILIBRARY
  char polyfilename[6];
  int index;
#else /* not TRILIBRARY */
  char inputline[INPUTLINESIZE];
  char *stringptr;
#endif /* not TRILIBRARY */
  point endpoint1, endpoint2;
  int segments;
  int segmentmarkers;
  int end1, end2;
  int boundmarker;
  int i;

  if (poly) {
    if (!quiet) {
      printf("Inserting segments into Delaunay triangulation.\n");
    }
#ifdef TRILIBRARY
    strcpy(polyfilename, "input");
    segments = numberofsegments;
    segmentmarkers = segmentmarkerlist != (int *) NULL;
    index = 0;
#else /* not TRILIBRARY */
    /* Read the segments from a .poly file. */
    /* Read number of segments and number of boundary markers. */
    stringptr = readline(inputline, polyfile, polyfilename);
    segments = (int) strtol (stringptr, &stringptr, 0);
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      segmentmarkers = 0;
    } else {
      segmentmarkers = (int) strtol (stringptr, &stringptr, 0);
    }
#endif /* not TRILIBRARY */
    /* If segments are to be inserted, compute a mapping */
    /*   from points to triangles.                       */
    if (segments > 0) {
      if (verbose) {
        printf("  Inserting PSLG segments.\n");
      }
      makepointmap();
    }

    boundmarker = 0;
    /* Read and insert the segments. */
    for (i = 1; i <= segments; i++) {
#ifdef TRILIBRARY
      end1 = segmentlist[index++];
      end2 = segmentlist[index++];
      if (segmentmarkers) {
        boundmarker = segmentmarkerlist[i - 1];
      }
#else /* not TRILIBRARY */
      stringptr = readline(inputline, polyfile, inpolyfilename);
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        printf("Error:  Segment %d has no endpoints in %s.\n", i,
               polyfilename);
        exit(1);
      } else {
        end1 = (int) strtol (stringptr, &stringptr, 0);
      }
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        printf("Error:  Segment %d is missing its second endpoint in %s.\n", i,
               polyfilename);
        exit(1);
      } else {
        end2 = (int) strtol (stringptr, &stringptr, 0);
      }
      if (segmentmarkers) {
        stringptr = findfield(stringptr);
        if (*stringptr == '\0') {
          boundmarker = 0;
        } else {
          boundmarker = (int) strtol (stringptr, &stringptr, 0);
        }
      }
#endif /* not TRILIBRARY */
      if ((end1 < firstnumber) || (end1 >= firstnumber + inpoints)) {
        if (!quiet) {
          printf("Warning:  Invalid first endpoint of segment %d in %s.\n", i,
                 polyfilename);
        }
      } else if ((end2 < firstnumber) || (end2 >= firstnumber + inpoints)) {
        if (!quiet) {
          printf("Warning:  Invalid second endpoint of segment %d in %s.\n", i,
                 polyfilename);
        }
      } else {
        endpoint1 = getpoint(end1);
        endpoint2 = getpoint(end2);
        if ((endpoint1[0] == endpoint2[0]) && (endpoint1[1] == endpoint2[1])) {
          if (!quiet) {
            printf("Warning:  Endpoints of segment %d are coincident in %s.\n",
                   i, polyfilename);
          }
        } else {
          insertsegment(endpoint1, endpoint2, boundmarker);
        }
      }
    }
  } else {
    segments = 0;
  }
  if (convex || !poly) {
    /* Enclose the convex hull with shell edges. */
    if (verbose) {
      printf("  Enclosing convex hull with segments.\n");
    }
    markhull();
  }
  return segments;
}

/**                                                                         **/
/**                                                                         **/
/********* Segment (shell edge) insertion ends here                  *********/

/********* Carving out holes and concavities begins here             *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  infecthull()   Virally infect all of the triangles of the convex hull    */
/*                 that are not protected by shell edges.  Where there are   */
/*                 shell edges, set boundary markers as appropriate.         */
/*                                                                           */
/*****************************************************************************/

void infecthull()
{
  struct triedge hulltri;
  struct triedge nexttri;
  struct triedge starttri;
  struct edge hulledge;
  triangle **deadtri;
  point horg, hdest;
  triangle ptr;                         /* Temporary variable used by sym(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  if (verbose) {
    printf("  Marking concavities (external triangles) for elimination.\n");
  }
  /* Find a triangle handle on the hull. */
  hulltri.tri = dummytri;
  hulltri.orient = 0;
  symself(hulltri);
  /* Remember where we started so we know when to stop. */
  triedgecopy(hulltri, starttri);
  /* Go once counterclockwise around the convex hull. */
  do {
    /* Ignore triangles that are already infected. */
    if (!infected(hulltri)) {
      /* Is the triangle protected by a shell edge? */
      tspivot(hulltri, hulledge);
      if (hulledge.sh == dummysh) {
        /* The triangle is not protected; infect it. */
        infect(hulltri);
        deadtri = (triangle **) poolalloc(&viri);
        *deadtri = hulltri.tri;
      } else {
        /* The triangle is protected; set boundary markers if appropriate. */
        if (mark(hulledge) == 0) {
          setmark(hulledge, 1);
          org(hulltri, horg);
          dest(hulltri, hdest);
          if (pointmark(horg) == 0) {
            setpointmark(horg, 1);
          }
          if (pointmark(hdest) == 0) {
            setpointmark(hdest, 1);
          }
        }
      }
    }
    /* To find the next hull edge, go clockwise around the next vertex. */
    lnextself(hulltri);
    oprev(hulltri, nexttri);
    while (nexttri.tri != dummytri) {
      triedgecopy(nexttri, hulltri);
      oprev(hulltri, nexttri);
    }
  } while (!triedgeequal(hulltri, starttri));
}

/*****************************************************************************/
/*                                                                           */
/*  plague()   Spread the virus from all infected triangles to any neighbors */
/*             not protected by shell edges.  Delete all infected triangles. */
/*                                                                           */
/*  This is the procedure that actually creates holes and concavities.       */
/*                                                                           */
/*  This procedure operates in two phases.  The first phase identifies all   */
/*  the triangles that will die, and marks them as infected.  They are       */
/*  marked to ensure that each triangle is added to the virus pool only      */
/*  once, so the procedure will terminate.                                   */
/*                                                                           */
/*  The second phase actually eliminates the infected triangles.  It also    */
/*  eliminates orphaned points.                                              */
/*                                                                           */
/*****************************************************************************/

void plague()
{
  struct triedge testtri;
  struct triedge neighbor;
  triangle **virusloop;
  triangle **deadtri;
  struct edge neighborshelle;
  point testpoint;
  point norg, ndest;
  point deadorg, deaddest, deadapex;
  int killorg;
  triangle ptr;             /* Temporary variable used by sym() and onext(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  if (verbose) {
    printf("  Marking neighbors of marked triangles.\n");
  }
  /* Loop through all the infected triangles, spreading the virus to */
  /*   their neighbors, then to their neighbors' neighbors.          */
  traversalinit(&viri);
  virusloop = (triangle **) traverse(&viri);
  while (virusloop != (triangle **) NULL) {
    testtri.tri = *virusloop;
    /* A triangle is marked as infected by messing with one of its shell */
    /*   edges, setting it to an illegal value.  Hence, we have to       */
    /*   temporarily uninfect this triangle so that we can examine its   */
    /*   adjacent shell edges.                                           */
    uninfect(testtri);
    if (verbose > 2) {
      /* Assign the triangle an orientation for convenience in */
      /*   checking its points.                                */
      testtri.orient = 0;
      org(testtri, deadorg);
      dest(testtri, deaddest);
      apex(testtri, deadapex);
      printf("    Checking (%.12g, %.12g) (%.12g, %.12g) (%.12g, %.12g)\n",
             deadorg[0], deadorg[1], deaddest[0], deaddest[1],
             deadapex[0], deadapex[1]);
    }
    /* Check each of the triangle's three neighbors. */
    for (testtri.orient = 0; testtri.orient < 3; testtri.orient++) {
      /* Find the neighbor. */
      sym(testtri, neighbor);
      /* Check for a shell between the triangle and its neighbor. */
      tspivot(testtri, neighborshelle);
      /* Check if the neighbor is nonexistent or already infected. */
      if ((neighbor.tri == dummytri) || infected(neighbor)) {
        if (neighborshelle.sh != dummysh) {
          /* There is a shell edge separating the triangle from its */
          /*   neighbor, but both triangles are dying, so the shell */
          /*   edge dies too.                                       */
          shelledealloc(neighborshelle.sh);
          if (neighbor.tri != dummytri) {
            /* Make sure the shell edge doesn't get deallocated again */
            /*   later when the infected neighbor is visited.         */
            uninfect(neighbor);
            tsdissolve(neighbor);
            infect(neighbor);
          }
        }
      } else {                   /* The neighbor exists and is not infected. */
        if (neighborshelle.sh == dummysh) {
          /* There is no shell edge protecting the neighbor, so */
          /*   the neighbor becomes infected.                   */
          if (verbose > 2) {
            org(neighbor, deadorg);
            dest(neighbor, deaddest);
            apex(neighbor, deadapex);
            printf("    Marking (%.12g, %.12g) (%.12g, %.12g) (%.12g, %.12g)\n",
                   deadorg[0], deadorg[1], deaddest[0], deaddest[1],
                   deadapex[0], deadapex[1]);
          }
          infect(neighbor);
          /* Ensure that the neighbor's neighbors will be infected. */
          deadtri = (triangle **) poolalloc(&viri);
          *deadtri = neighbor.tri;
        } else {               /* The neighbor is protected by a shell edge. */
          /* Remove this triangle from the shell edge. */
          stdissolve(neighborshelle);
          /* The shell edge becomes a boundary.  Set markers accordingly. */
          if (mark(neighborshelle) == 0) {
            setmark(neighborshelle, 1);
          }
          org(neighbor, norg);
          dest(neighbor, ndest);
          if (pointmark(norg) == 0) {
            setpointmark(norg, 1);
          }
          if (pointmark(ndest) == 0) {
            setpointmark(ndest, 1);
          }
        }
      }
    }
    /* Remark the triangle as infected, so it doesn't get added to the */
    /*   virus pool again.                                             */
    infect(testtri);
    virusloop = (triangle **) traverse(&viri);
  }

  if (verbose) {
    printf("  Deleting marked triangles.\n");
  }
  traversalinit(&viri);
  virusloop = (triangle **) traverse(&viri);
  while (virusloop != (triangle **) NULL) {
    testtri.tri = *virusloop;

    /* Check each of the three corners of the triangle for elimination. */
    /*   This is done by walking around each point, checking if it is   */
    /*   still connected to at least one live triangle.                 */
    for (testtri.orient = 0; testtri.orient < 3; testtri.orient++) {
      org(testtri, testpoint);
      /* Check if the point has already been tested. */
      if (testpoint != (point) NULL) {
        killorg = 1;
        /* Mark the corner of the triangle as having been tested. */
        setorg(testtri, NULL);
        /* Walk counterclockwise about the point. */
        onext(testtri, neighbor);
        /* Stop upon reaching a boundary or the starting triangle. */
        while ((neighbor.tri != dummytri)
               && (!triedgeequal(neighbor, testtri))) {
          if (infected(neighbor)) {
            /* Mark the corner of this triangle as having been tested. */
            setorg(neighbor, NULL);
          } else {
            /* A live triangle.  The point survives. */
            killorg = 0;
          }
          /* Walk counterclockwise about the point. */
          onextself(neighbor);
        }
        /* If we reached a boundary, we must walk clockwise as well. */
        if (neighbor.tri == dummytri) {
          /* Walk clockwise about the point. */
          oprev(testtri, neighbor);
          /* Stop upon reaching a boundary. */
          while (neighbor.tri != dummytri) {
            if (infected(neighbor)) {
            /* Mark the corner of this triangle as having been tested. */
              setorg(neighbor, NULL);
            } else {
              /* A live triangle.  The point survives. */
              killorg = 0;
            }
            /* Walk clockwise about the point. */
            oprevself(neighbor);
          }
        }
        if (killorg) {
          if (verbose > 1) {
            printf("    Deleting point (%.12g, %.12g)\n",
                   testpoint[0], testpoint[1]);
          }
          pointdealloc(testpoint);
        }
      }
    }

    /* Record changes in the number of boundary edges, and disconnect */
    /*   dead triangles from their neighbors.                         */
    for (testtri.orient = 0; testtri.orient < 3; testtri.orient++) {
      sym(testtri, neighbor);
      if (neighbor.tri == dummytri) {
        /* There is no neighboring triangle on this edge, so this edge    */
        /*   is a boundary edge.  This triangle is being deleted, so this */
        /*   boundary edge is deleted.                                    */
        hullsize--;
      } else {
        /* Disconnect the triangle from its neighbor. */
        dissolve(neighbor);
        /* There is a neighboring triangle on this edge, so this edge */
        /*   becomes a boundary edge when this triangle is deleted.   */
        hullsize++;
      }
    }
    /* Return the dead triangle to the pool of triangles. */
    triangledealloc(testtri.tri);
    virusloop = (triangle **) traverse(&viri);
  }
  /* Empty the virus pool. */
  poolrestart(&viri);
}

/*****************************************************************************/
/*                                                                           */
/*  regionplague()   Spread regional attributes and/or area constraints      */
/*                   (from a .poly file) throughout the mesh.                */
/*                                                                           */
/*  This procedure operates in two phases.  The first phase spreads an       */
/*  attribute and/or an area constraint through a (segment-bounded) region.  */
/*  The triangles are marked to ensure that each triangle is added to the    */
/*  virus pool only once, so the procedure will terminate.                   */
/*                                                                           */
/*  The second phase uninfects all infected triangles, returning them to     */
/*  normal.                                                                  */
/*                                                                           */
/*****************************************************************************/

void regionplague(attribute, area)
REAL attribute;
REAL area;
{
  struct triedge testtri;
  struct triedge neighbor;
  triangle **virusloop;
  triangle **regiontri;
  struct edge neighborshelle;
  point regionorg, regiondest, regionapex;
  triangle ptr;             /* Temporary variable used by sym() and onext(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  if (verbose > 1) {
    printf("  Marking neighbors of marked triangles.\n");
  }
  /* Loop through all the infected triangles, spreading the attribute      */
  /*   and/or area constraint to their neighbors, then to their neighbors' */
  /*   neighbors.                                                          */
  traversalinit(&viri);
  virusloop = (triangle **) traverse(&viri);
  while (virusloop != (triangle **) NULL) {
    testtri.tri = *virusloop;
    /* A triangle is marked as infected by messing with one of its shell */
    /*   edges, setting it to an illegal value.  Hence, we have to       */
    /*   temporarily uninfect this triangle so that we can examine its   */
    /*   adjacent shell edges.                                           */
    uninfect(testtri);
    if (regionattrib) {
      /* Set an attribute. */
      setelemattribute(testtri, eextras, attribute);
    }
    if (vararea) {
      /* Set an area constraint. */
      setareabound(testtri, area);
    }
    if (verbose > 2) {
      /* Assign the triangle an orientation for convenience in */
      /*   checking its points.                                */
      testtri.orient = 0;
      org(testtri, regionorg);
      dest(testtri, regiondest);
      apex(testtri, regionapex);
      printf("    Checking (%.12g, %.12g) (%.12g, %.12g) (%.12g, %.12g)\n",
             regionorg[0], regionorg[1], regiondest[0], regiondest[1],
             regionapex[0], regionapex[1]);
    }
    /* Check each of the triangle's three neighbors. */
    for (testtri.orient = 0; testtri.orient < 3; testtri.orient++) {
      /* Find the neighbor. */
      sym(testtri, neighbor);
      /* Check for a shell between the triangle and its neighbor. */
      tspivot(testtri, neighborshelle);
      /* Make sure the neighbor exists, is not already infected, and */
      /*   isn't protected by a shell edge.                          */
      if ((neighbor.tri != dummytri) && !infected(neighbor)
          && (neighborshelle.sh == dummysh)) {
        if (verbose > 2) {
          org(neighbor, regionorg);
          dest(neighbor, regiondest);
          apex(neighbor, regionapex);
          printf("    Marking (%.12g, %.12g) (%.12g, %.12g) (%.12g, %.12g)\n",
                 regionorg[0], regionorg[1], regiondest[0], regiondest[1],
                 regionapex[0], regionapex[1]);
        }
        /* Infect the neighbor. */
        infect(neighbor);
        /* Ensure that the neighbor's neighbors will be infected. */
        regiontri = (triangle **) poolalloc(&viri);
        *regiontri = neighbor.tri;
      }
    }
    /* Remark the triangle as infected, so it doesn't get added to the */
    /*   virus pool again.                                             */
    infect(testtri);
    virusloop = (triangle **) traverse(&viri);
  }

  /* Uninfect all triangles. */
  if (verbose > 1) {
    printf("  Unmarking marked triangles.\n");
  }
  traversalinit(&viri);
  virusloop = (triangle **) traverse(&viri);
  while (virusloop != (triangle **) NULL) {
    testtri.tri = *virusloop;
    uninfect(testtri);
    virusloop = (triangle **) traverse(&viri);
  }
  /* Empty the virus pool. */
  poolrestart(&viri);
}

/*****************************************************************************/
/*                                                                           */
/*  carveholes()   Find the holes and infect them.  Find the area            */
/*                 constraints and infect them.  Infect the convex hull.     */
/*                 Spread the infection and kill triangles.  Spread the      */
/*                 area constraints.                                         */
/*                                                                           */
/*  This routine mainly calls other routines to carry out all these          */
/*  functions.                                                               */
/*                                                                           */
/*****************************************************************************/

void carveholes(holelist, holes, regionlist, regions)
REAL *holelist;
int holes;
REAL *regionlist;
int regions;
{
  struct triedge searchtri;
  struct triedge triangleloop;
  struct triedge *regiontris=0;
  triangle **holetri;
  triangle **regiontri;
  point searchorg, searchdest;
  enum locateresult intersect;
  int i;
  triangle ptr;                         /* Temporary variable used by sym(). */

  if (!(quiet || (noholes && convex))) {
    printf("Removing unwanted triangles.\n");
    if (verbose && (holes > 0)) {
      printf("  Marking holes for elimination.\n");
    }
  }

  if (regions > 0) {
    /* Allocate storage for the triangles in which region points fall. */
    regiontris = (struct triedge *) malloc(regions * sizeof(struct triedge));
    if (regiontris == (struct triedge *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }

  if (((holes > 0) && !noholes) || !convex || (regions > 0)) {
    /* Initialize a pool of viri to be used for holes, concavities, */
    /*   regional attributes, and/or regional area constraints.     */
    poolinit(&viri, sizeof(triangle *), VIRUSPERBLOCK, POINTER, 0);
  }

  if (!convex) {
    /* Mark as infected any unprotected triangles on the boundary. */
    /*   This is one way by which concavities are created.         */
    infecthull();
  }

  if ((holes > 0) && !noholes) {
    /* Infect each triangle in which a hole lies. */
    for (i = 0; i < 2 * holes; i += 2) {
      /* Ignore holes that aren't within the bounds of the mesh. */
      if ((holelist[i] >= xmin) && (holelist[i] <= xmax)
          && (holelist[i + 1] >= ymin) && (holelist[i + 1] <= ymax)) {
        /* Start searching from some triangle on the outer boundary. */
        searchtri.tri = dummytri;
        searchtri.orient = 0;
        symself(searchtri);
        /* Ensure that the hole is to the left of this boundary edge; */
        /*   otherwise, locate() will falsely report that the hole    */
        /*   falls within the starting triangle.                      */
        org(searchtri, searchorg);
        dest(searchtri, searchdest);
        if (counterclockwise(searchorg, searchdest, &holelist[i]) > 0.0) {
          /* Find a triangle that contains the hole. */
          intersect = locate(&holelist[i], &searchtri);
          if ((intersect != OUTSIDE) && (!infected(searchtri))) {
            /* Infect the triangle.  This is done by marking the triangle */
            /*   as infect and including the triangle in the virus pool.  */
            infect(searchtri);
            holetri = (triangle **) poolalloc(&viri);
            *holetri = searchtri.tri;
          }
        }
      }
    }
  }

  /* Now, we have to find all the regions BEFORE we carve the holes, because */
  /*   locate() won't work when the triangulation is no longer convex.       */
  /*   (Incidentally, this is the reason why regional attributes and area    */
  /*   constraints can't be used when refining a preexisting mesh, which     */
  /*   might not be convex; they can only be used with a freshly             */
  /*   triangulated PSLG.)                                                   */
  if (regions > 0) {
    /* Find the starting triangle for each region. */
    for (i = 0; i < regions; i++) {
      regiontris[i].tri = dummytri;
      /* Ignore region points that aren't within the bounds of the mesh. */
      if ((regionlist[4 * i] >= xmin) && (regionlist[4 * i] <= xmax) &&
          (regionlist[4 * i + 1] >= ymin) && (regionlist[4 * i + 1] <= ymax)) {
        /* Start searching from some triangle on the outer boundary. */
        searchtri.tri = dummytri;
        searchtri.orient = 0;
        symself(searchtri);
        /* Ensure that the region point is to the left of this boundary */
        /*   edge; otherwise, locate() will falsely report that the     */
        /*   region point falls within the starting triangle.           */
        org(searchtri, searchorg);
        dest(searchtri, searchdest);
        if (counterclockwise(searchorg, searchdest, &regionlist[4 * i]) >
            0.0) {
          /* Find a triangle that contains the region point. */
          intersect = locate(&regionlist[4 * i], &searchtri);
          if ((intersect != OUTSIDE) && (!infected(searchtri))) {
            /* Record the triangle for processing after the */
            /*   holes have been carved.                    */
            triedgecopy(searchtri, regiontris[i]);
          }
        }
      }
    }
  }

  if (viri.items > 0) {
    /* Carve the holes and concavities. */
    plague();
  }
  /* The virus pool should be empty now. */

  if (regions > 0) {
    if (!quiet) {
      if (regionattrib) {
        if (vararea) {
          printf("Spreading regional attributes and area constraints.\n");
        } else {
          printf("Spreading regional attributes.\n");
        }
      } else {
        printf("Spreading regional area constraints.\n");
      }
    }
    if (regionattrib && !refine) {
      /* Assign every triangle a regional attribute of zero. */
      traversalinit(&triangles);
      triangleloop.orient = 0;
      triangleloop.tri = triangletraverse();
      while (triangleloop.tri != (triangle *) NULL) {
        setelemattribute(triangleloop, eextras, 0.0);
        triangleloop.tri = triangletraverse();
      }
    }
    for (i = 0; i < regions; i++) {
      if (regiontris[i].tri != dummytri) {
        /* Make sure the triangle under consideration still exists. */
        /*   It may have been eaten by the virus.                   */
        if (regiontris[i].tri[3] != (triangle) NULL) {
          /* Put one triangle in the virus pool. */
          infect(regiontris[i]);
          regiontri = (triangle **) poolalloc(&viri);
          *regiontri = regiontris[i].tri;
          /* Apply one region's attribute and/or area constraint. */
          regionplague(regionlist[4 * i + 2], regionlist[4 * i + 3]);
          /* The virus pool should be empty now. */
        }
      }
    }
    if (regionattrib && !refine) {
      /* Note the fact that each triangle has an additional attribute. */
      eextras++;
    }
  }

  /* Free up memory. */
  if (((holes > 0) && !noholes) || !convex || (regions > 0)) {
    pooldeinit(&viri);
  }
  if (regions > 0) {
    free(regiontris);
  }
}

/**                                                                         **/
/**                                                                         **/
/********* Carving out holes and concavities ends here               *********/

/********* Mesh quality maintenance begins here                      *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  tallyencs()   Traverse the entire list of shell edges, check each edge   */
/*                to see if it is encroached.  If so, add it to the list.    */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void tallyencs()
{
  struct edge edgeloop;

  traversalinit(&shelles);
  edgeloop.shorient = 0;
  edgeloop.sh = shelletraverse();
  while (edgeloop.sh != (shelle *) NULL) {
    /* If the segment is encroached, add it to the list. */
    /* dummy = */ checkedge4encroach(&edgeloop);
    edgeloop.sh = shelletraverse();
  }
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  precisionerror()  Print an error message for precision problems.         */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void precisionerror()
{
  printf("Try increasing the area criterion and/or reducing the minimum\n");
  printf("  allowable angle so that tiny triangles are not created.\n");
#ifdef SINGLE
  printf("Alternatively, try recompiling me with double precision\n");
  printf("  arithmetic (by removing \"#define SINGLE\" from the\n");
  printf("  source file or \"-DSINGLE\" from the makefile).\n");
#endif /* SINGLE */
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  repairencs()   Find and repair all the encroached segments.              */
/*                                                                           */
/*  Encroached segments are repaired by splitting them by inserting a point  */
/*  at or near their centers.                                                */
/*                                                                           */
/*  `flaws' is a flag that specifies whether one should take note of new     */
/*  encroached segments and bad triangles that result from inserting points  */
/*  to repair existing encroached segments.                                  */
/*                                                                           */
/*  When a segment is split, the two resulting subsegments are always        */
/*  tested to see if they are encroached upon, regardless of the value       */
/*  of `flaws'.                                                              */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void repairencs(flaws)
int flaws;
{
  struct triedge enctri;
  struct triedge testtri;
  struct edge *encloop;
  struct edge testsh;
  point eorg, edest;
  point newpoint;
  enum insertsiteresult success;
  REAL segmentlength, nearestpoweroftwo;
  REAL split;
  int acuteorg, acutedest;
  int i;
  triangle ptr;                     /* Temporary variable used by stpivot(). */
  shelle sptr;                        /* Temporary variable used by snext(). */

  while ((badsegments.items > 0) && (steinerleft != 0)) {
    traversalinit(&badsegments);
    encloop = badsegmenttraverse();
    while ((encloop != (struct edge *) NULL) && (steinerleft != 0)) {
      /* To decide where to split a segment, we need to know if the  */
      /*   segment shares an endpoint with an adjacent segment.      */
      /*   The concern is that, if we simply split every encroached  */
      /*   segment in its center, two adjacent segments with a small */
      /*   angle between them might lead to an infinite loop; each   */
      /*   point added to split one segment will encroach upon the   */
      /*   other segment, which must then be split with a point that */
      /*   will encroach upon the first segment, and so on forever.  */
      /* To avoid this, imagine a set of concentric circles, whose   */
      /*   radii are powers of two, about each segment endpoint.     */
      /*   These concentric circles determine where the segment is   */
      /*   split.  (If both endpoints are shared with adjacent       */
      /*   segments, split the segment in the middle, and apply the  */
      /*   concentric shells for later splittings.)                  */

      /* Is the origin shared with another segment? */
      stpivot(*encloop, enctri);
      lnext(enctri, testtri);
      tspivot(testtri, testsh);
      acuteorg = testsh.sh != dummysh;
      /* Is the destination shared with another segment? */
      lnextself(testtri);
      tspivot(testtri, testsh);
      acutedest = testsh.sh != dummysh;
      /* Now, check the other side of the segment, if there's a triangle */
      /*   there.                                                        */
      sym(enctri, testtri);
      if (testtri.tri != dummytri) {
        /* Is the destination shared with another segment? */
        lnextself(testtri);
        tspivot(testtri, testsh);
        acutedest = acutedest || (testsh.sh != dummysh);
        /* Is the origin shared with another segment? */
        lnextself(testtri);
        tspivot(testtri, testsh);
        acuteorg = acuteorg || (testsh.sh != dummysh);
      }

      sorg(*encloop, eorg);
      sdest(*encloop, edest);
      /* Use the concentric circles if exactly one endpoint is shared */
      /*   with another adjacent segment.                             */
      if (acuteorg ^ acutedest) {
        segmentlength = sqrt((edest[0] - eorg[0]) * (edest[0] - eorg[0])
                             + (edest[1] - eorg[1]) * (edest[1] - eorg[1]));
        /* Find the power of two nearest the segment's length. */
        nearestpoweroftwo = 1.0;
        while (segmentlength > SQUAREROOTTWO * nearestpoweroftwo) {
          nearestpoweroftwo *= 2.0;
        }
        while (segmentlength < (0.5 * SQUAREROOTTWO) * nearestpoweroftwo) {
          nearestpoweroftwo *= 0.5;
        }
        /* Where do we split the segment? */
        split = 0.5 * nearestpoweroftwo / segmentlength;
        if (acutedest) {
          split = 1.0 - split;
        }
      } else {
        /* If we're not worried about adjacent segments, split */
        /*   this segment in the middle.                       */
        split = 0.5;
      }

      /* Create the new point. */
      newpoint = (point) poolalloc(&points);
      /* Interpolate its coordinate and attributes. */
      for (i = 0; i < 2 + nextras; i++) {
        newpoint[i] = (1.0 - split) * eorg[i] + split * edest[i];
      }
      setpointmark(newpoint, mark(*encloop));
      if (verbose > 1) {
        printf("  Splitting edge (%.12g, %.12g) (%.12g, %.12g) at (%.12g, %.12g).\n",
               eorg[0], eorg[1], edest[0], edest[1], newpoint[0], newpoint[1]);
      }
      /* Check whether the new point lies on an endpoint. */
      if (((newpoint[0] == eorg[0]) && (newpoint[1] == eorg[1]))
        || ((newpoint[0] == edest[0]) && (newpoint[1] == edest[1]))) {
        printf("Error:  Ran out of precision at (%.12g, %.12g).\n",
               newpoint[0], newpoint[1]);
        printf("I attempted to split a segment to a smaller size than can\n");
        printf("  be accommodated by the finite precision of floating point\n");
        printf("  arithmetic.\n");
        precisionerror();
        exit(1);
      }
      /* Insert the splitting point.  This should always succeed. */
      success = insertsite(newpoint, &enctri, encloop, flaws, flaws);
      if ((success != SUCCESSFULPOINT) && (success != ENCROACHINGPOINT)) {
        printf("Internal error in repairencs():\n");
        printf("  Failure to split a segment.\n");
        internalerror();
      }
      if (steinerleft > 0) {
        steinerleft--;
      }
      /* Check the two new subsegments to see if they're encroached. */
      /* dummy = */ checkedge4encroach(encloop);
      snextself(*encloop);
      /* dummy = */ checkedge4encroach(encloop);

      badsegmentdealloc(encloop);
      encloop = badsegmenttraverse();
    }
  }
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  tallyfaces()   Test every triangle in the mesh for quality measures.     */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void tallyfaces()
{
  struct triedge triangleloop;

  if (verbose) {
    printf("  Making a list of bad triangles.\n");
  }
  traversalinit(&triangles);
  triangleloop.orient = 0;
  triangleloop.tri = triangletraverse();
  while (triangleloop.tri != (triangle *) NULL) {
    /* If the triangle is bad, enqueue it. */
    testtriangle(&triangleloop);
    triangleloop.tri = triangletraverse();
  }
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  findcircumcenter()   Find the circumcenter of a triangle.                */
/*                                                                           */
/*  The result is returned both in terms of x-y coordinates and xi-eta       */
/*  coordinates.  The xi-eta coordinate system is defined in terms of the    */
/*  triangle:  the origin of the triangle is the origin of the coordinate    */
/*  system; the destination of the triangle is one unit along the xi axis;   */
/*  and the apex of the triangle is one unit along the eta axis.             */
/*                                                                           */
/*  The return value indicates which edge of the triangle is shortest.       */
/*                                                                           */
/*****************************************************************************/

enum circumcenterresult findcircumcenter(torg, tdest, tapex, circumcenter, xi, eta)
point torg;
point tdest;
point tapex;
point circumcenter;
REAL *xi;
REAL *eta;
{
  REAL xdo, ydo, xao, yao, xad, yad;
  REAL dodist, aodist, addist;
  REAL denominator;
  REAL dx, dy;

  circumcentercount++;

  /* Compute the circumcenter of the triangle. */
  xdo = tdest[0] - torg[0];
  ydo = tdest[1] - torg[1];
  xao = tapex[0] - torg[0];
  yao = tapex[1] - torg[1];
  dodist = xdo * xdo + ydo * ydo;
  aodist = xao * xao + yao * yao;
  if (noexact) {
    denominator = 0.5 / (xdo * yao - xao * ydo);
  } else {
    /* Use the counterclockwise() routine to ensure a positive (and */
    /*   reasonably accurate) result, avoiding any possibility of   */
    /*   division by zero.                                          */
    denominator = 0.5 / counterclockwise(tdest, tapex, torg);
    /* Don't count the above as an orientation test. */
    counterclockcount--;
  }
  circumcenter[0] = torg[0] - (ydo * aodist - yao * dodist) * denominator;
  circumcenter[1] = torg[1] + (xdo * aodist - xao * dodist) * denominator;

  /* To interpolate point attributes for the new point inserted at  */
  /*   the circumcenter, define a coordinate system with a xi-axis, */
  /*   directed from the triangle's origin to its destination, and  */
  /*   an eta-axis, directed from its origin to its apex.           */
  /*   Calculate the xi and eta coordinates of the circumcenter.    */
  dx = circumcenter[0] - torg[0];
  dy = circumcenter[1] - torg[1];
  *xi = (dx * yao - xao * dy) * (2.0 * denominator);
  *eta = (xdo * dy - dx * ydo) * (2.0 * denominator);

  xad = tapex[0] - tdest[0];
  yad = tapex[1] - tdest[1];
  addist = xad * xad + yad * yad;
  if ((addist < dodist) && (addist < aodist)) {
    return OPPOSITEORG;
  } else if (dodist < aodist) {
    return OPPOSITEAPEX;
  } else {
    return OPPOSITEDEST;
  }
}

/*****************************************************************************/
/*                                                                           */
/*  splittriangle()   Inserts a point at the circumcenter of a triangle.     */
/*                    Deletes the newly inserted point if it encroaches upon */
/*                    a segment.                                             */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void splittriangle(badtri)
struct badface *badtri;
{
  point borg, bdest, bapex;
  point newpoint;
  REAL xi, eta;
  enum insertsiteresult success;
  enum circumcenterresult shortedge;
  int errorflag;
  int i;

  org(badtri->badfacetri, borg);
  dest(badtri->badfacetri, bdest);
  apex(badtri->badfacetri, bapex);
  /* Make sure that this triangle is still the same triangle it was      */
  /*   when it was tested and determined to be of bad quality.           */
  /*   Subsequent transformations may have made it a different triangle. */
  if ((borg == badtri->faceorg) && (bdest == badtri->facedest) &&
      (bapex == badtri->faceapex)) {
    if (verbose > 1) {
      printf("  Splitting this triangle at its circumcenter:\n");
      printf("    (%.12g, %.12g) (%.12g, %.12g) (%.12g, %.12g)\n", borg[0],
             borg[1], bdest[0], bdest[1], bapex[0], bapex[1]);
    }
    errorflag = 0;
    /* Create a new point at the triangle's circumcenter. */
    newpoint = (point) poolalloc(&points);
    shortedge = findcircumcenter(borg, bdest, bapex, newpoint, &xi, &eta);
    /* Check whether the new point lies on a triangle vertex. */
    if (((newpoint[0] == borg[0]) && (newpoint[1] == borg[1]))
        || ((newpoint[0] == bdest[0]) && (newpoint[1] == bdest[1]))
        || ((newpoint[0] == bapex[0]) && (newpoint[1] == bapex[1]))) {
      if (!quiet) {
        printf("Warning:  New point (%.12g, %.12g) falls on existing vertex.\n"
               , newpoint[0], newpoint[1]);
        errorflag = 1;
      }
      pointdealloc(newpoint);
    } else {
      for (i = 2; i < 2 + nextras; i++) {
        /* Interpolate the point attributes at the circumcenter. */
        newpoint[i] = borg[i] + xi * (bdest[i] - borg[i])
                             + eta * (bapex[i] - borg[i]);
      }
      /* The new point must be in the interior, and have a marker of zero. */
      setpointmark(newpoint, 0);
      /* Ensure that the handle `badtri->badfacetri' represents the shortest */
      /*   edge of the triangle.  This ensures that the circumcenter must    */
      /*   fall to the left of this edge, so point location will work.       */
      if (shortedge == OPPOSITEORG) {
        lnextself(badtri->badfacetri);
      } else if (shortedge == OPPOSITEDEST) {
        lprevself(badtri->badfacetri);
      }
      /* Insert the circumcenter, searching from the edge of the triangle, */
      /*   and maintain the Delaunay property of the triangulation.        */
      success = insertsite(newpoint, &(badtri->badfacetri),
                           (struct edge *) NULL, 1, 1);
      if (success == SUCCESSFULPOINT) {
        if (steinerleft > 0) {
          steinerleft--;
        }
      } else if (success == ENCROACHINGPOINT) {
        /* If the newly inserted point encroaches upon a segment, delete it. */
        deletesite(&(badtri->badfacetri));
      } else if (success == VIOLATINGPOINT) {
        /* Failed to insert the new point, but some segment was */
        /*   marked as being encroached.                        */
        pointdealloc(newpoint);
      } else {                                  /* success == DUPLICATEPOINT */
        /* Failed to insert the new point because a vertex is already there. */
        if (!quiet) {
          printf("Warning:  New point (%.12g, %.12g) falls on existing vertex.\n",
                 newpoint[0], newpoint[1]);
          errorflag = 1;
        }
        pointdealloc(newpoint);
      }
    }
    if (errorflag) {
      if (verbose) {
        printf("  The new point is at the circumcenter of triangle\n");
        printf("    (%.12g, %.12g) (%.12g, %.12g) (%.12g, %.12g)\n",
               borg[0], borg[1], bdest[0], bdest[1], bapex[0], bapex[1]);
      }
      printf("This probably means that I am trying to refine triangles\n");
      printf("  to a smaller size than can be accommodated by the finite\n");
      printf("  precision of floating point arithmetic.  (You can be\n");
      printf("  sure of this if I fail to terminate.)\n");
      precisionerror();
    }
  }
  /* Return the bad triangle to the pool. */
  pooldealloc(&badtriangles, (VOID *) badtri);
}

#endif /* not CDT_ONLY */

/*****************************************************************************/
/*                                                                           */
/*  enforcequality()   Remove all the encroached edges and bad triangles     */
/*                     from the triangulation.                               */
/*                                                                           */
/*****************************************************************************/

#ifndef CDT_ONLY

void enforcequality()
{
  int i;

  if (!quiet) {
    printf("Adding Steiner points to enforce quality.\n");
  }
  /* Initialize the pool of encroached segments. */
  poolinit(&badsegments, sizeof(struct edge), BADSEGMENTPERBLOCK, POINTER, 0);
  if (verbose) {
    printf("  Looking for encroached segments.\n");
  }
  /* Test all segments to see if they're encroached. */
  tallyencs();
  if (verbose && (badsegments.items > 0)) {
    printf("  Splitting encroached segments.\n");
  }
  /* Note that steinerleft == -1 if an unlimited number */
  /*   of Steiner points is allowed.                    */
  while ((badsegments.items > 0) && (steinerleft != 0)) {
    /* Fix the segments without noting newly encroached segments or   */
    /*   bad triangles.  The reason we don't want to note newly       */
    /*   encroached segments is because some encroached segments are  */
    /*   likely to be noted multiple times, and would then be blindly */
    /*   split multiple times.  I should fix that some time.          */
    repairencs(0);
    /* Now, find all the segments that became encroached while adding */
    /*   points to split encroached segments.                         */
    tallyencs();
  }
  /* At this point, if we haven't run out of Steiner points, the */
  /*   triangulation should be (conforming) Delaunay.            */

  /* Next, we worry about enforcing triangle quality. */
  if ((minangle > 0.0) || vararea || fixedarea) {
    /* Initialize the pool of bad triangles. */
    poolinit(&badtriangles, sizeof(struct badface), BADTRIPERBLOCK, POINTER,
             0);
    /* Initialize the queues of bad triangles. */
    for (i = 0; i < 64; i++) {
      queuefront[i] = (struct badface *) NULL;
      queuetail[i] = &queuefront[i];
    }
    /* Test all triangles to see if they're bad. */
    tallyfaces();
    if (verbose) {
      printf("  Splitting bad triangles.\n");
    }
    while ((badtriangles.items > 0) && (steinerleft != 0)) {
      /* Fix one bad triangle by inserting a point at its circumcenter. */
      splittriangle(dequeuebadtri());
      /* Fix any encroached segments that may have resulted.  Record */
      /*   any new bad triangles or encroached segments that result. */
      if (badsegments.items > 0) {
        repairencs(1);
      }
    }
  }
  /* At this point, if we haven't run out of Steiner points, the */
  /*   triangulation should be (conforming) Delaunay and have no */
  /*   low-quality triangles.                                    */

  /* Might we have run out of Steiner points too soon? */
  if (!quiet && (badsegments.items > 0) && (steinerleft == 0)) {
    printf("\nWarning:  I ran out of Steiner points, but the mesh has\n");
    if (badsegments.items == 1) {
      printf("  an encroached segment, and therefore might not be truly\n");
    } else {
      printf("  %ld encroached segments, and therefore might not be truly\n",
             badsegments.items);
    }
    printf("  Delaunay.  If the Delaunay property is important to you,\n");
    printf("  try increasing the number of Steiner points (controlled by\n");
    printf("  the -S switch) slightly and try again.\n\n");
  }
}

#endif /* not CDT_ONLY */

/**                                                                         **/
/**                                                                         **/
/********* Mesh quality maintenance ends here                        *********/

/*****************************************************************************/
/*                                                                           */
/*  highorder()   Create extra nodes for quadratic subparametric elements.   */
/*                                                                           */
/*****************************************************************************/

void highorder()
{
  struct triedge triangleloop, trisym;
  struct edge checkmark;
  point newpoint;
  point torg, tdest;
  int i;
  triangle ptr;                         /* Temporary variable used by sym(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

  if (!quiet) {
    printf("Adding vertices for second-order triangles.\n");
  }
  /* The following line ensures that dead items in the pool of nodes    */
  /*   cannot be allocated for the extra nodes associated with high     */
  /*   order elements.  This ensures that the primary nodes (at the     */
  /*   corners of elements) will occur earlier in the output files, and */
  /*   have lower indices, than the extra nodes.                        */
  points.deaditemstack = (VOID *) NULL;

  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  /* To loop over the set of edges, loop over all triangles, and look at   */
  /*   the three edges of each triangle.  If there isn't another triangle  */
  /*   adjacent to the edge, operate on the edge.  If there is another     */
  /*   adjacent triangle, operate on the edge only if the current triangle */
  /*   has a smaller pointer than its neighbor.  This way, each edge is    */
  /*   considered only once.                                               */
  while (triangleloop.tri != (triangle *) NULL) {
    for (triangleloop.orient = 0; triangleloop.orient < 3; triangleloop.orient++) {
      sym(triangleloop, trisym);
      if ((triangleloop.tri < trisym.tri) || (trisym.tri == dummytri)) {
        org(triangleloop, torg);
        dest(triangleloop, tdest);
        /* Create a new node in the middle of the edge.  Interpolate */
        /*   its attributes.                                         */
        newpoint = (point) poolalloc(&points);
        for (i = 0; i < 2 + nextras; i++) {
          newpoint[i] = 0.5 * (torg[i] + tdest[i]);
        }
        /* Set the new node's marker to zero or one, depending on */
        /*   whether it lies on a boundary.                       */
        setpointmark(newpoint, trisym.tri == dummytri);
        if (useshelles) {
          tspivot(triangleloop, checkmark);
          /* If this edge is a segment, transfer the marker to the new node. */
          if (checkmark.sh != dummysh) {
            setpointmark(newpoint, mark(checkmark));
          }
        }
        if (verbose > 1) {
          printf("  Creating (%.12g, %.12g).\n", newpoint[0], newpoint[1]);
        }
        /* Record the new node in the (one or two) adjacent elements. */
        triangleloop.tri[highorderindex + triangleloop.orient] =
                (triangle) newpoint;
        if (trisym.tri != dummytri) {
          trisym.tri[highorderindex + trisym.orient] = (triangle) newpoint;
        }
      }
    }
    triangleloop.tri = triangletraverse();
  }
}

/********* File I/O routines begin here                              *********/
/**                                                                         **/
/**                                                                         **/

/*****************************************************************************/
/*                                                                           */
/*  readline()   Read a nonempty line from a file.                           */
/*                                                                           */
/*  A line is considered "nonempty" if it contains something that looks like */
/*  a number.                                                                */
/*                                                                           */
/*****************************************************************************/

#ifndef TRILIBRARY

char *readline(string, infile, infilename)
char *string;
FILE *infile;
char *infilename;
{
  char *result;

  /* Search for something that looks like a number. */
  do {
    result = fgets(string, INPUTLINESIZE, infile);
    if (result == (char *) NULL) {
      printf("  Error:  Unexpected end of file in %s.\n", infilename);
      exit(1);
    }
    /* Skip anything that doesn't look like a number, a comment, */
    /*   or the end of a line.                                   */
    while ((*result != '\0') && (*result != '#')
           && (*result != '.') && (*result != '+') && (*result != '-')
           && ((*result < '0') || (*result > '9'))) {
      result++;
    }
  /* If it's a comment or end of line, read another line and try again. */
  } while ((*result == '#') || (*result == '\0'));
  return result;
}

#endif /* not TRILIBRARY */

/*****************************************************************************/
/*                                                                           */
/*  findfield()   Find the next field of a string.                           */
/*                                                                           */
/*  Jumps past the current field by searching for whitespace, then jumps     */
/*  past the whitespace to find the next field.                              */
/*                                                                           */
/*****************************************************************************/

#ifndef TRILIBRARY

char *findfield(string)
char *string;
{
  char *result;

  result = string;
  /* Skip the current field.  Stop upon reaching whitespace. */
  while ((*result != '\0') && (*result != '#')
         && (*result != ' ') && (*result != '\t')) {
    result++;
  }
  /* Now skip the whitespace and anything else that doesn't look like a */
  /*   number, a comment, or the end of a line.                         */
  while ((*result != '\0') && (*result != '#')
         && (*result != '.') && (*result != '+') && (*result != '-')
         && ((*result < '0') || (*result > '9'))) {
    result++;
  }
  /* Check for a comment (prefixed with `#'). */
  if (*result == '#') {
    *result = '\0';
  }
  return result;
}

#endif /* not TRILIBRARY */

/*****************************************************************************/
/*                                                                           */
/*  readnodes()   Read the points from a file, which may be a .node or .poly */
/*                file.                                                      */
/*                                                                           */
/*****************************************************************************/

#ifndef TRILIBRARY

void readnodes(nodefilename, polyfilename, polyfile)
char *nodefilename;
char *polyfilename;
FILE **polyfile;
{
  FILE *infile;
  point pointloop;
  char inputline[INPUTLINESIZE];
  char *stringptr;
  char *infilename;
  REAL x, y;
  int firstnode;
  int nodemarkers;
  int currentmarker;
  int i, j;

  if (poly) {
    /* Read the points from a .poly file. */
    if (!quiet) {
      printf("Opening %s.\n", polyfilename);
    }
    *polyfile = fopen(polyfilename, "r");
    if (*polyfile == (FILE *) NULL) {
      printf("  Error:  Cannot access file %s.\n", polyfilename);
      exit(1);
    }
    /* Read number of points, number of dimensions, number of point */
    /*   attributes, and number of boundary markers.                */
    stringptr = readline(inputline, *polyfile, polyfilename);
    inpoints = (int) strtol (stringptr, &stringptr, 0);
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      mesh_dim = 2;
    } else {
      mesh_dim = (int) strtol (stringptr, &stringptr, 0);
    }
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      nextras = 0;
    } else {
      nextras = (int) strtol (stringptr, &stringptr, 0);
    }
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      nodemarkers = 0;
    } else {
      nodemarkers = (int) strtol (stringptr, &stringptr, 0);
    }
    if (inpoints > 0) {
      infile = *polyfile;
      infilename = polyfilename;
      readnodefile = 0;
    } else {
      /* If the .poly file claims there are zero points, that means that */
      /*   the points should be read from a separate .node file.         */
      readnodefile = 1;
      infilename = innodefilename;
    }
  } else {
    readnodefile = 1;
    infilename = innodefilename;
    *polyfile = (FILE *) NULL;
  }

  if (readnodefile) {
    /* Read the points from a .node file. */
    if (!quiet) {
      printf("Opening %s.\n", innodefilename);
    }
    infile = fopen(innodefilename, "r");
    if (infile == (FILE *) NULL) {
      printf("  Error:  Cannot access file %s.\n", innodefilename);
      exit(1);
    }
    /* Read number of points, number of dimensions, number of point */
    /*   attributes, and number of boundary markers.                */
    stringptr = readline(inputline, infile, innodefilename);
    inpoints = (int) strtol (stringptr, &stringptr, 0);
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      mesh_dim = 2;
    } else {
      mesh_dim = (int) strtol (stringptr, &stringptr, 0);
    }
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      nextras = 0;
    } else {
      nextras = (int) strtol (stringptr, &stringptr, 0);
    }
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      nodemarkers = 0;
    } else {
      nodemarkers = (int) strtol (stringptr, &stringptr, 0);
    }
  }

  if (inpoints < 3) {
    printf("Error:  Input must have at least three input points.\n");
    exit(1);
  }
  if (mesh_dim != 2) {
    printf("Error:  Triangle only works with two-dimensional meshes.\n");
    exit(1);
  }

  initializepointpool();

  /* Read the points. */
  for (i = 0; i < inpoints; i++) {
    pointloop = (point) poolalloc(&points);
    stringptr = readline(inputline, infile, infilename);
    if (i == 0) {
      firstnode = (int) strtol (stringptr, &stringptr, 0);
      if ((firstnode == 0) || (firstnode == 1)) {
        firstnumber = firstnode;
      }
    }
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      printf("Error:  Point %d has no x coordinate.\n", firstnumber + i);
      exit(1);
    }
    x = (REAL) strtod(stringptr, &stringptr);
    stringptr = findfield(stringptr);
    if (*stringptr == '\0') {
      printf("Error:  Point %d has no y coordinate.\n", firstnumber + i);
      exit(1);
    }
    y = (REAL) strtod(stringptr, &stringptr);
    pointloop[0] = x;
    pointloop[1] = y;
    /* Read the point attributes. */
    for (j = 2; j < 2 + nextras; j++) {
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        pointloop[j] = 0.0;
      } else {
        pointloop[j] = (REAL) strtod(stringptr, &stringptr);
      }
    }
    if (nodemarkers) {
      /* Read a point marker. */
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        setpointmark(pointloop, 0);
      } else {
        currentmarker = (int) strtol (stringptr, &stringptr, 0);
        setpointmark(pointloop, currentmarker);
      }
    } else {
      /* If no markers are specified in the file, they default to zero. */
      setpointmark(pointloop, 0);
    }
    /* Determine the smallest and largest x and y coordinates. */
    if (i == 0) {
      xmin = xmax = x;
      ymin = ymax = y;
    } else {
      xmin = (x < xmin) ? x : xmin;
      xmax = (x > xmax) ? x : xmax;
      ymin = (y < ymin) ? y : ymin;
      ymax = (y > ymax) ? y : ymax;
    }
  }
  if (readnodefile) {
    fclose(infile);
  }

  /* Nonexistent x value used as a flag to mark circle events in sweepline */
  /*   Delaunay algorithm.                                                 */
  xminextreme = 10 * xmin - 9 * xmax;
}

#endif /* not TRILIBRARY */

/*****************************************************************************/
/*                                                                           */
/*  transfernodes()   Read the points from memory.                           */
/*                                                                           */
/*****************************************************************************/

#ifdef TRILIBRARY

void transfernodes(pointlist, pointattriblist, pointmarkerlist, numberofpoints,
                   numberofpointattribs)
REAL *pointlist;
REAL *pointattriblist;
int *pointmarkerlist;
int numberofpoints;
int numberofpointattribs;
{
  point pointloop;
  REAL x, y;
  int i, j;
  int coordindex;
  int attribindex;

  inpoints = numberofpoints;
  mesh_dim = 2;
  nextras = numberofpointattribs;
  readnodefile = 0;
  if (inpoints < 3) {
    printf("Error:  Input must have at least three input points.\n");
    exit(1);
  }

  initializepointpool();

  /* Read the points. */
  coordindex = 0;
  attribindex = 0;
  for (i = 0; i < inpoints; i++) {
    pointloop = (point) poolalloc(&points);
    /* Read the point coordinates. */
    pointloop[0] = pointlist[coordindex++];
    pointloop[1] = pointlist[coordindex++];
    /* Read the point attributes. */
    for (j = 0; j < numberofpointattribs; j++) {
      pointloop[2 + j] = pointattriblist[attribindex++];
    }
    if (pointmarkerlist != (int *) NULL) {
      /* Read a point marker. */
      setpointmark(pointloop, pointmarkerlist[i]);
    } else {
      /* If no markers are specified, they default to zero. */
      setpointmark(pointloop, 0);
    }
    x = pointloop[0];
    y = pointloop[1];
    /* Determine the smallest and largest x and y coordinates. */
    if (i == 0) {
      xmin = xmax = x;
      ymin = ymax = y;
    } else {
      xmin = (x < xmin) ? x : xmin;
      xmax = (x > xmax) ? x : xmax;
      ymin = (y < ymin) ? y : ymin;
      ymax = (y > ymax) ? y : ymax;
    }
  }

  /* Nonexistent x value used as a flag to mark circle events in sweepline */
  /*   Delaunay algorithm.                                                 */
  xminextreme = 10 * xmin - 9 * xmax;
}

#endif /* TRILIBRARY */

/*****************************************************************************/
/*                                                                           */
/*  readholes()   Read the holes, and possibly regional attributes and area  */
/*                constraints, from a .poly file.                            */
/*                                                                           */
/*****************************************************************************/

#ifndef TRILIBRARY

void readholes(polyfile, polyfilename, hlist, holes, rlist, regions)
FILE *polyfile;
char *polyfilename;
REAL **hlist;
int *holes;
REAL **rlist;
int *regions;
{
  REAL *holelist;
  REAL *regionlist;
  char inputline[INPUTLINESIZE];
  char *stringptr;
  int index;
  int i;

  /* Read the holes. */
  stringptr = readline(inputline, polyfile, polyfilename);
  *holes = (int) strtol (stringptr, &stringptr, 0);
  if (*holes > 0) {
    holelist = (REAL *) malloc(2 * *holes * sizeof(REAL));
    *hlist = holelist;
    if (holelist == (REAL *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
    for (i = 0; i < 2 * *holes; i += 2) {
      stringptr = readline(inputline, polyfile, polyfilename);
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        printf("Error:  Hole %d has no x coordinate.\n", firstnumber + (i >> 1));
        exit(1);
      } else {
        holelist[i] = (REAL) strtod(stringptr, &stringptr);
      }
      stringptr = findfield(stringptr);
      if (*stringptr == '\0') {
        printf("Error:  Hole %d has no y coordinate.\n", firstnumber + (i >> 1));
        exit(1);
      } else {
        holelist[i + 1] = (REAL) strtod(stringptr, &stringptr);
      }
    }
  } else {
    *hlist = (REAL *) NULL;
  }

#ifndef CDT_ONLY
  if ((regionattrib || vararea) && !refine) {
    /* Read the area constraints. */
    stringptr = readline(inputline, polyfile, polyfilename);
    *regions = (int) strtol (stringptr, &stringptr, 0);
    if (*regions > 0) {
      regionlist = (REAL *) malloc(4 * *regions * sizeof(REAL));
      *rlist = regionlist;
      if (regionlist == (REAL *) NULL) {
        printf("Error:  Out of memory.\n");
        exit(1);
      }
      index = 0;
      for (i = 0; i < *regions; i++) {
        stringptr = readline(inputline, polyfile, polyfilename);
        stringptr = findfield(stringptr);
        if (*stringptr == '\0') {
          printf("Error:  Region %d has no x coordinate.\n", firstnumber + i);
          exit(1);
        } else {
          regionlist[index++] = (REAL) strtod(stringptr, &stringptr);
        }
        stringptr = findfield(stringptr);
        if (*stringptr == '\0') {
          printf("Error:  Region %d has no y coordinate.\n", firstnumber + i);
          exit(1);
        } else {
          regionlist[index++] = (REAL) strtod(stringptr, &stringptr);
        }
        stringptr = findfield(stringptr);
        if (*stringptr == '\0') {
          printf("Error:  Region %d has no region attribute or area constraint.\n",
                 firstnumber + i);
          exit(1);
        } else {
          regionlist[index++] = (REAL) strtod(stringptr, &stringptr);
        }
        stringptr = findfield(stringptr);
        if (*stringptr == '\0') {
          regionlist[index] = regionlist[index - 1];
        } else {
          regionlist[index] = (REAL) strtod(stringptr, &stringptr);
        }
        index++;
      }
    }
  } else {
    /* Set `*regions' to zero to avoid an accidental free() later. */
    *regions = 0;
    *rlist = (REAL *) NULL;
  }
#endif /* not CDT_ONLY */

  fclose(polyfile);
}

#endif /* not TRILIBRARY */

/*****************************************************************************/
/*                                                                           */
/*  finishfile()   Write the command line to the output file so the user     */
/*                 can remember how the file was generated.  Close the file. */
/*                                                                           */
/*****************************************************************************/

#ifndef TRILIBRARY

void finishfile(outfile, argc, argv)
FILE *outfile;
int argc;
char **argv;
{
  int i;

  fprintf(outfile, "# Generated by");
  for (i = 0; i < argc; i++) {
    fprintf(outfile, " ");
    fputs(argv[i], outfile);
  }
  fprintf(outfile, "\n");
  fclose(outfile);
}

#endif /* not TRILIBRARY */

/*****************************************************************************/
/*                                                                           */
/*  writenodes()   Number the points and write them to a .node file.         */
/*                                                                           */
/*  To save memory, the point numbers are written over the shell markers     */
/*  after the points are written to a file.                                  */
/*                                                                           */
/*****************************************************************************/

#ifdef TRILIBRARY

void writenodes(pointlist, pointattriblist, pointmarkerlist)
REAL **pointlist;
REAL **pointattriblist;
int **pointmarkerlist;

#else /* not TRILIBRARY */

void writenodes(nodefilename, argc, argv)
char *nodefilename;
int argc;
char **argv;

#endif /* not TRILIBRARY */

{
#ifdef TRILIBRARY
  REAL *plist;
  REAL *palist;
  int *pmlist;
  int coordindex;
  int attribindex;
#else /* not TRILIBRARY */
  FILE *outfile;
#endif /* not TRILIBRARY */
  point pointloop;
  int pointnumber;
  int i;

#ifdef TRILIBRARY
  if (!quiet) {
    printf("Writing points.\n");
  }
  /* Allocate memory for output points if necessary. */
  if (*pointlist == (REAL *) NULL) {
    *pointlist = (REAL *) malloc(points.items * 2 * sizeof(REAL));
    if (*pointlist == (REAL *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  /* Allocate memory for output point attributes if necessary. */
  if ((nextras > 0) && (*pointattriblist == (REAL *) NULL)) {
    *pointattriblist = (REAL *) malloc(points.items * nextras * sizeof(REAL));
    if (*pointattriblist == (REAL *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  /* Allocate memory for output point markers if necessary. */
  if (!nobound && (*pointmarkerlist == (int *) NULL)) {
    *pointmarkerlist = (int *) malloc(points.items * sizeof(int));
    if (*pointmarkerlist == (int *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  plist = *pointlist;
  palist = *pointattriblist;
  pmlist = *pointmarkerlist;
  coordindex = 0;
  attribindex = 0;
#else /* not TRILIBRARY */
  if (!quiet) {
    printf("Writing %s.\n", nodefilename);
  }
  outfile = fopen(nodefilename, "w");
  if (outfile == (FILE *) NULL) {
    printf("  Error:  Cannot create file %s.\n", nodefilename);
    exit(1);
  }
  /* Number of points, number of dimensions, number of point attributes, */
  /*   and number of boundary markers (zero or one).                     */
  fprintf(outfile, "%ld  %d  %d  %d\n", points.items, mesh_dim, nextras,
          1 - nobound);
#endif /* not TRILIBRARY */

  traversalinit(&points);
  pointloop = pointtraverse();
  pointnumber = firstnumber;
  while (pointloop != (point) NULL) {
#ifdef TRILIBRARY
    /* X and y coordinates. */
    plist[coordindex++] = pointloop[0];
    plist[coordindex++] = pointloop[1];
    /* Point attributes. */
    for (i = 0; i < nextras; i++) {
      palist[attribindex++] = pointloop[2 + i];
    }
    if (!nobound) {
      /* Copy the boundary marker. */
      pmlist[pointnumber - firstnumber] = pointmark(pointloop);
    }
#else /* not TRILIBRARY */
    /* Point number, x and y coordinates. */
    fprintf(outfile, "%4d    %.17g  %.17g", pointnumber, pointloop[0],
            pointloop[1]);
    for (i = 0; i < nextras; i++) {
      /* Write an attribute. */
      fprintf(outfile, "  %.17g", pointloop[i + 2]);
    }
    if (nobound) {
      fprintf(outfile, "\n");
    } else {
      /* Write the boundary marker. */
      fprintf(outfile, "    %d\n", pointmark(pointloop));
    }
#endif /* not TRILIBRARY */

    setpointmark(pointloop, pointnumber);
    pointloop = pointtraverse();
    pointnumber++;
  }

#ifndef TRILIBRARY
  finishfile(outfile, argc, argv);
#endif /* not TRILIBRARY */
}

/*****************************************************************************/
/*                                                                           */
/*  numbernodes()   Number the points.                                       */
/*                                                                           */
/*  Each point is assigned a marker equal to its number.                     */
/*                                                                           */
/*  Used when writenodes() is not called because no .node file is written.   */
/*                                                                           */
/*****************************************************************************/

void numbernodes()
{
  point pointloop;
  int pointnumber;

  traversalinit(&points);
  pointloop = pointtraverse();
  pointnumber = firstnumber;
  while (pointloop != (point) NULL) {
    setpointmark(pointloop, pointnumber);
    pointloop = pointtraverse();
    pointnumber++;
  }
}

/*****************************************************************************/
/*                                                                           */
/*  writeelements()   Write the triangles to an .ele file.                   */
/*                                                                           */
/*****************************************************************************/

#ifdef TRILIBRARY

void writeelements(trianglelist, triangleattriblist)
int **trianglelist;
REAL **triangleattriblist;

#else /* not TRILIBRARY */

void writeelements(elefilename, argc, argv)
char *elefilename;
int argc;
char **argv;

#endif /* not TRILIBRARY */

{
#ifdef TRILIBRARY
  int *tlist;
  REAL *talist;
  int pointindex;
  int attribindex;
#else /* not TRILIBRARY */
  FILE *outfile;
#endif /* not TRILIBRARY */
  struct triedge triangleloop;
  point p1, p2, p3;
  point mid1, mid2, mid3;
  int elementnumber;
  int i;

#ifdef TRILIBRARY
  if (!quiet) {
    printf("Writing triangles.\n");
  }
  /* Allocate memory for output triangles if necessary. */
  if (*trianglelist == (int *) NULL) {
    *trianglelist = (int *) malloc(triangles.items *
                               ((order + 1) * (order + 2) / 2) * sizeof(int));
    if (*trianglelist == (int *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  /* Allocate memory for output triangle attributes if necessary. */
  if ((eextras > 0) && (*triangleattriblist == (REAL *) NULL)) {
    *triangleattriblist = (REAL *) malloc(triangles.items * eextras *
                                          sizeof(REAL));
    if (*triangleattriblist == (REAL *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  tlist = *trianglelist;
  talist = *triangleattriblist;
  pointindex = 0;
  attribindex = 0;
#else /* not TRILIBRARY */
  if (!quiet) {
    printf("Writing %s.\n", elefilename);
  }
  outfile = fopen(elefilename, "w");
  if (outfile == (FILE *) NULL) {
    printf("  Error:  Cannot create file %s.\n", elefilename);
    exit(1);
  }
  /* Number of triangles, points per triangle, attributes per triangle. */
  fprintf(outfile, "%ld  %d  %d\n", triangles.items,
          (order + 1) * (order + 2) / 2, eextras);
#endif /* not TRILIBRARY */

  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  triangleloop.orient = 0;
  elementnumber = firstnumber;
  while (triangleloop.tri != (triangle *) NULL) {
    org(triangleloop, p1);
    dest(triangleloop, p2);
    apex(triangleloop, p3);
    if (order == 1) {
#ifdef TRILIBRARY
      tlist[pointindex++] = pointmark(p1);
      tlist[pointindex++] = pointmark(p2);
      tlist[pointindex++] = pointmark(p3);
#else /* not TRILIBRARY */
      /* Triangle number, indices for three points. */
      fprintf(outfile, "%4d    %4d  %4d  %4d", elementnumber,
              pointmark(p1), pointmark(p2), pointmark(p3));
#endif /* not TRILIBRARY */
    } else {
      mid1 = (point) triangleloop.tri[highorderindex + 1];
      mid2 = (point) triangleloop.tri[highorderindex + 2];
      mid3 = (point) triangleloop.tri[highorderindex];
#ifdef TRILIBRARY
      tlist[pointindex++] = pointmark(p1);
      tlist[pointindex++] = pointmark(p2);
      tlist[pointindex++] = pointmark(p3);
      tlist[pointindex++] = pointmark(mid1);
      tlist[pointindex++] = pointmark(mid2);
      tlist[pointindex++] = pointmark(mid3);
#else /* not TRILIBRARY */
      /* Triangle number, indices for six points. */
      fprintf(outfile, "%4d    %4d  %4d  %4d  %4d  %4d  %4d", elementnumber,
              pointmark(p1), pointmark(p2), pointmark(p3), pointmark(mid1),
              pointmark(mid2), pointmark(mid3));
#endif /* not TRILIBRARY */
    }

#ifdef TRILIBRARY
    for (i = 0; i < eextras; i++) {
      talist[attribindex++] = elemattribute(triangleloop, i);
    }
#else /* not TRILIBRARY */
    for (i = 0; i < eextras; i++) {
      fprintf(outfile, "  %.17g", elemattribute(triangleloop, i));
    }
    fprintf(outfile, "\n");
#endif /* not TRILIBRARY */

    triangleloop.tri = triangletraverse();
#ifndef TRILIBRARY
    elementnumber++;
#endif /* not TRILIBRARY */
  }

#ifndef TRILIBRARY
  finishfile(outfile, argc, argv);
#endif /* not TRILIBRARY */
}

/*****************************************************************************/
/*                                                                           */
/*  writepoly()   Write the segments and holes to a .poly file.              */
/*                                                                           */
/*****************************************************************************/

#ifdef TRILIBRARY

void writepoly(segmentlist, segmentmarkerlist)
int **segmentlist;
int **segmentmarkerlist;

#else /* not TRILIBRARY */

void writepoly(polyfilename, holelist, holes, regionlist, regions, argc, argv)
char *polyfilename;
REAL *holelist;
int holes;
REAL *regionlist;
int regions;
int argc;
char **argv;

#endif /* not TRILIBRARY */

{
#ifdef TRILIBRARY
  int *slist;
  int *smlist;
  int index;
#else /* not TRILIBRARY */
  FILE *outfile;
  int i;
#endif /* not TRILIBRARY */
  struct edge shelleloop;
  point endpoint1, endpoint2;
  int shellenumber;

#ifdef TRILIBRARY
  if (!quiet) {
    printf("Writing segments.\n");
  }
  /* Allocate memory for output segments if necessary. */
  if (*segmentlist == (int *) NULL) {
    *segmentlist = (int *) malloc(shelles.items * 2 * sizeof(int));
    if (*segmentlist == (int *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  /* Allocate memory for output segment markers if necessary. */
  if (!nobound && (*segmentmarkerlist == (int *) NULL)) {
    *segmentmarkerlist = (int *) malloc(shelles.items * sizeof(int));
    if (*segmentmarkerlist == (int *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  slist = *segmentlist;
  smlist = *segmentmarkerlist;
  index = 0;
#else /* not TRILIBRARY */
  if (!quiet) {
    printf("Writing %s.\n", polyfilename);
  }
  outfile = fopen(polyfilename, "w");
  if (outfile == (FILE *) NULL) {
    printf("  Error:  Cannot create file %s.\n", polyfilename);
    exit(1);
  }
  /* The zero indicates that the points are in a separate .node file. */
  /*   Followed by number of dimensions, number of point attributes,  */
  /*   and number of boundary markers (zero or one).                  */
  fprintf(outfile, "%d  %d  %d  %d\n", 0, mesh_dim, nextras, 1 - nobound);
  /* Number of segments, number of boundary markers (zero or one). */
  fprintf(outfile, "%ld  %d\n", shelles.items, 1 - nobound);
#endif /* not TRILIBRARY */

  traversalinit(&shelles);
  shelleloop.sh = shelletraverse();
  shelleloop.shorient = 0;
  shellenumber = firstnumber;
  while (shelleloop.sh != (shelle *) NULL) {
    sorg(shelleloop, endpoint1);
    sdest(shelleloop, endpoint2);
#ifdef TRILIBRARY
    /* Copy indices of the segment's two endpoints. */
    slist[index++] = pointmark(endpoint1);
    slist[index++] = pointmark(endpoint2);
    if (!nobound) {
      /* Copy the boundary marker. */
      smlist[shellenumber - firstnumber] = mark(shelleloop);
    }
#else /* not TRILIBRARY */
    /* Segment number, indices of its two endpoints, and possibly a marker. */
    if (nobound) {
      fprintf(outfile, "%4d    %4d  %4d\n", shellenumber,
              pointmark(endpoint1), pointmark(endpoint2));
    } else {
      fprintf(outfile, "%4d    %4d  %4d    %4d\n", shellenumber,
              pointmark(endpoint1), pointmark(endpoint2), mark(shelleloop));
    }
#endif /* not TRILIBRARY */

    shelleloop.sh = shelletraverse();
    shellenumber++;
  }

#ifndef TRILIBRARY
#ifndef CDT_ONLY
  fprintf(outfile, "%d\n", holes);
  if (holes > 0) {
    for (i = 0; i < holes; i++) {
      /* Hole number, x and y coordinates. */
      fprintf(outfile, "%4d   %.17g  %.17g\n", firstnumber + i,
              holelist[2 * i], holelist[2 * i + 1]);
    }
  }
  if (regions > 0) {
    fprintf(outfile, "%d\n", regions);
    for (i = 0; i < regions; i++) {
      /* Region number, x and y coordinates, attribute, maximum area. */
      fprintf(outfile, "%4d   %.17g  %.17g  %.17g  %.17g\n", firstnumber + i,
              regionlist[4 * i], regionlist[4 * i + 1],
              regionlist[4 * i + 2], regionlist[4 * i + 3]);
    }
  }
#endif /* not CDT_ONLY */

  finishfile(outfile, argc, argv);
#endif /* not TRILIBRARY */
}

/*****************************************************************************/
/*                                                                           */
/*  writeedges()   Write the edges to a .edge file.                          */
/*                                                                           */
/*****************************************************************************/

#ifdef TRILIBRARY

void writeedges(edgelist, edgemarkerlist)
int **edgelist;
int **edgemarkerlist;

#else /* not TRILIBRARY */

void writeedges(edgefilename, argc, argv)
char *edgefilename;
int argc;
char **argv;

#endif /* not TRILIBRARY */

{
#ifdef TRILIBRARY
  int *elist;
  int *emlist;
  int index;
#else /* not TRILIBRARY */
  FILE *outfile;
#endif /* not TRILIBRARY */
  struct triedge triangleloop, trisym;
  struct edge checkmark;
  point p1, p2;
  int edgenumber;
  triangle ptr;                         /* Temporary variable used by sym(). */
  shelle sptr;                      /* Temporary variable used by tspivot(). */

#ifdef TRILIBRARY
  if (!quiet) {
    printf("Writing edges.\n");
  }
  /* Allocate memory for edges if necessary. */
  if (*edgelist == (int *) NULL) {
    *edgelist = (int *) malloc(edges * 2 * sizeof(int));
    if (*edgelist == (int *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  /* Allocate memory for edge markers if necessary. */
  if (!nobound && (*edgemarkerlist == (int *) NULL)) {
    *edgemarkerlist = (int *) malloc(edges * sizeof(int));
    if (*edgemarkerlist == (int *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  elist = *edgelist;
  emlist = *edgemarkerlist;
  index = 0;
#else /* not TRILIBRARY */
  if (!quiet) {
    printf("Writing %s.\n", edgefilename);
  }
  outfile = fopen(edgefilename, "w");
  if (outfile == (FILE *) NULL) {
    printf("  Error:  Cannot create file %s.\n", edgefilename);
    exit(1);
  }
  /* Number of edges, number of boundary markers (zero or one). */
  fprintf(outfile, "%ld  %d\n", edges, 1 - nobound);
#endif /* not TRILIBRARY */

  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  edgenumber = firstnumber;
  /* To loop over the set of edges, loop over all triangles, and look at   */
  /*   the three edges of each triangle.  If there isn't another triangle  */
  /*   adjacent to the edge, operate on the edge.  If there is another     */
  /*   adjacent triangle, operate on the edge only if the current triangle */
  /*   has a smaller pointer than its neighbor.  This way, each edge is    */
  /*   considered only once.                                               */
  while (triangleloop.tri != (triangle *) NULL) {
    for (triangleloop.orient = 0; triangleloop.orient < 3; triangleloop.orient++) {
      sym(triangleloop, trisym);
      if ((triangleloop.tri < trisym.tri) || (trisym.tri == dummytri)) {
        org(triangleloop, p1);
        dest(triangleloop, p2);
#ifdef TRILIBRARY
        elist[index++] = pointmark(p1);
        elist[index++] = pointmark(p2);
#endif /* TRILIBRARY */
        if (nobound) {
#ifndef TRILIBRARY
          /* Edge number, indices of two endpoints. */
          fprintf(outfile, "%4d   %d  %d\n", edgenumber,
                  pointmark(p1), pointmark(p2));
#endif /* not TRILIBRARY */
        } else {
          /* Edge number, indices of two endpoints, and a boundary marker. */
          /*   If there's no shell edge, the boundary marker is zero.      */
          if (useshelles) {
            tspivot(triangleloop, checkmark);
            if (checkmark.sh == dummysh) {
#ifdef TRILIBRARY
              emlist[edgenumber - firstnumber] = 0;
#else /* not TRILIBRARY */
              fprintf(outfile, "%4d   %d  %d  %d\n", edgenumber,
                      pointmark(p1), pointmark(p2), 0);
#endif /* not TRILIBRARY */
            } else {
#ifdef TRILIBRARY
              emlist[edgenumber - firstnumber] = mark(checkmark);
#else /* not TRILIBRARY */
              fprintf(outfile, "%4d   %d  %d  %d\n", edgenumber,
                      pointmark(p1), pointmark(p2), mark(checkmark));
#endif /* not TRILIBRARY */
            }
          } else {
#ifdef TRILIBRARY
            emlist[edgenumber - firstnumber] = trisym.tri == dummytri;
#else /* not TRILIBRARY */
            fprintf(outfile, "%4d   %d  %d  %d\n", edgenumber,
                    pointmark(p1), pointmark(p2), trisym.tri == dummytri);
#endif /* not TRILIBRARY */
          }
        }
        edgenumber++;
      }
    }
    triangleloop.tri = triangletraverse();
  }

#ifndef TRILIBRARY
  finishfile(outfile, argc, argv);
#endif /* not TRILIBRARY */
}

/*****************************************************************************/
/*                                                                           */
/*  writevoronoi()   Write the Voronoi diagram to a .v.node and .v.edge      */
/*                   file.                                                   */
/*                                                                           */
/*  The Voronoi diagram is the geometric dual of the Delaunay triangulation. */
/*  Hence, the Voronoi vertices are listed by traversing the Delaunay        */
/*  triangles, and the Voronoi edges are listed by traversing the Delaunay   */
/*  edges.                                                                   */
/*                                                                           */
/*  WARNING:  In order to assign numbers to the Voronoi vertices, this       */
/*  procedure messes up the shell edges or the extra nodes of every          */
/*  element.  Hence, you should call this procedure last.                    */
/*                                                                           */
/*****************************************************************************/

#ifdef TRILIBRARY

void writevoronoi(vpointlist, vpointattriblist, vpointmarkerlist, vedgelist,
                  vedgemarkerlist, vnormlist)
REAL **vpointlist;
REAL **vpointattriblist;
int **vpointmarkerlist;
int **vedgelist;
int **vedgemarkerlist;
REAL **vnormlist;

#else /* not TRILIBRARY */

void writevoronoi(vnodefilename, vedgefilename, argc, argv)
char *vnodefilename;
char *vedgefilename;
int argc;
char **argv;

#endif /* not TRILIBRARY */

{
#ifdef TRILIBRARY
  REAL *plist;
  REAL *palist;
  int *elist;
  REAL *normlist;
  int coordindex;
  int attribindex;
#else /* not TRILIBRARY */
  FILE *outfile;
#endif /* not TRILIBRARY */
  struct triedge triangleloop, trisym;
  point torg, tdest, tapex;
  REAL circumcenter[2];
  REAL xi, eta;
  int vnodenumber, vedgenumber;
  int p1, p2;
  int i;
  triangle ptr;                         /* Temporary variable used by sym(). */

#ifdef TRILIBRARY
  if (!quiet) {
    printf("Writing Voronoi vertices.\n");
  }
  /* Allocate memory for Voronoi vertices if necessary. */
  if (*vpointlist == (REAL *) NULL) {
    *vpointlist = (REAL *) malloc(triangles.items * 2 * sizeof(REAL));
    if (*vpointlist == (REAL *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  /* Allocate memory for Voronoi vertex attributes if necessary. */
  if (*vpointattriblist == (REAL *) NULL) {
    *vpointattriblist = (REAL *) malloc(triangles.items * nextras *
                                        sizeof(REAL));
    if (*vpointattriblist == (REAL *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  *vpointmarkerlist = (int *) NULL;
  plist = *vpointlist;
  palist = *vpointattriblist;
  coordindex = 0;
  attribindex = 0;
#else /* not TRILIBRARY */
  if (!quiet) {
    printf("Writing %s.\n", vnodefilename);
  }
  outfile = fopen(vnodefilename, "w");
  if (outfile == (FILE *) NULL) {
    printf("  Error:  Cannot create file %s.\n", vnodefilename);
    exit(1);
  }
  /* Number of triangles, two dimensions, number of point attributes, */
  /*   zero markers.                                                  */
  fprintf(outfile, "%ld  %d  %d  %d\n", triangles.items, 2, nextras, 0);
#endif /* not TRILIBRARY */

  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  triangleloop.orient = 0;
  vnodenumber = firstnumber;
  while (triangleloop.tri != (triangle *) NULL) {
    org(triangleloop, torg);
    dest(triangleloop, tdest);
    apex(triangleloop, tapex);
    findcircumcenter(torg, tdest, tapex, circumcenter, &xi, &eta);
#ifdef TRILIBRARY
    /* X and y coordinates. */
    plist[coordindex++] = circumcenter[0];
    plist[coordindex++] = circumcenter[1];
    for (i = 2; i < 2 + nextras; i++) {
      /* Interpolate the point attributes at the circumcenter. */
      palist[attribindex++] = torg[i] + xi * (tdest[i] - torg[i])
                                     + eta * (tapex[i] - torg[i]);
    }
#else /* not TRILIBRARY */
    /* Voronoi vertex number, x and y coordinates. */
    fprintf(outfile, "%4d    %.17g  %.17g",
            vnodenumber, circumcenter[0], circumcenter[1]);
    for (i = 2; i < 2 + nextras; i++) {
      /* Interpolate the point attributes at the circumcenter. */
      fprintf(outfile, "  %.17g", torg[i] + xi * (tdest[i] - torg[i])
                                         + eta * (tapex[i] - torg[i]));
    }
    fprintf(outfile, "\n");
#endif /* not TRILIBRARY */

    * (int *) (triangleloop.tri + 6) = vnodenumber;
    triangleloop.tri = triangletraverse();
    vnodenumber++;
  }

#ifndef TRILIBRARY
  finishfile(outfile, argc, argv);
#endif /* not TRILIBRARY */

#ifdef TRILIBRARY
  if (!quiet) {
    printf("Writing Voronoi edges.\n");
  }
  /* Allocate memory for output Voronoi edges if necessary. */
  if (*vedgelist == (int *) NULL) {
    *vedgelist = (int *) malloc(edges * 2 * sizeof(int));
    if (*vedgelist == (int *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  *vedgemarkerlist = (int *) NULL;
  /* Allocate memory for output Voronoi norms if necessary. */
  if (*vnormlist == (REAL *) NULL) {
    *vnormlist = (REAL *) malloc(edges * 2 * sizeof(REAL));
    if (*vnormlist == (REAL *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  elist = *vedgelist;
  normlist = *vnormlist;
  coordindex = 0;
#else /* not TRILIBRARY */
  if (!quiet) {
    printf("Writing %s.\n", vedgefilename);
  }
  outfile = fopen(vedgefilename, "w");
  if (outfile == (FILE *) NULL) {
    printf("  Error:  Cannot create file %s.\n", vedgefilename);
    exit(1);
  }
  /* Number of edges, zero boundary markers. */
  fprintf(outfile, "%ld  %d\n", edges, 0);
#endif /* not TRILIBRARY */

  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  vedgenumber = firstnumber;
  /* To loop over the set of edges, loop over all triangles, and look at   */
  /*   the three edges of each triangle.  If there isn't another triangle  */
  /*   adjacent to the edge, operate on the edge.  If there is another     */
  /*   adjacent triangle, operate on the edge only if the current triangle */
  /*   has a smaller pointer than its neighbor.  This way, each edge is    */
  /*   considered only once.                                               */
  while (triangleloop.tri != (triangle *) NULL) {
    for (triangleloop.orient = 0; triangleloop.orient < 3; triangleloop.orient++) {
      sym(triangleloop, trisym);
      if ((triangleloop.tri < trisym.tri) || (trisym.tri == dummytri)) {
        /* Find the number of this triangle (and Voronoi vertex). */
        p1 = * (int *) (triangleloop.tri + 6);
        if (trisym.tri == dummytri) {
          org(triangleloop, torg);
          dest(triangleloop, tdest);
#ifdef TRILIBRARY
          /* Copy an infinite ray.  Index of one endpoint, and -1. */
          elist[coordindex] = p1;
          normlist[coordindex++] = tdest[1] - torg[1];
          elist[coordindex] = -1;
          normlist[coordindex++] = torg[0] - tdest[0];
#else /* not TRILIBRARY */
          /* Write an infinite ray.  Edge number, index of one endpoint, -1, */
          /*   and x and y coordinates of a vector representing the          */
          /*   direction of the ray.                                         */
          fprintf(outfile, "%4d   %d  %d   %.17g  %.17g\n", vedgenumber,
                  p1, -1, tdest[1] - torg[1], torg[0] - tdest[0]);
#endif /* not TRILIBRARY */
        } else {
          /* Find the number of the adjacent triangle (and Voronoi vertex). */
          p2 = * (int *) (trisym.tri + 6);
          /* Finite edge.  Write indices of two endpoints. */
#ifdef TRILIBRARY
          elist[coordindex] = p1;
          normlist[coordindex++] = 0.0;
          elist[coordindex] = p2;
          normlist[coordindex++] = 0.0;
#else /* not TRILIBRARY */
          fprintf(outfile, "%4d   %d  %d\n", vedgenumber, p1, p2);
#endif /* not TRILIBRARY */
        }
#ifndef TRILIBRARY
        vedgenumber++;
#endif /* not TRILIBRARY */
      }
    }
    triangleloop.tri = triangletraverse();
  }

#ifndef TRILIBRARY
  finishfile(outfile, argc, argv);
#endif /* not TRILIBRARY */
}

#ifdef TRILIBRARY

void writeneighbors(neighborlist)
int **neighborlist;

#else /* not TRILIBRARY */

void writeneighbors(neighborfilename, argc, argv)
char *neighborfilename;
int argc;
char **argv;

#endif /* not TRILIBRARY */

{
#ifdef TRILIBRARY
  int *nlist;
  int index;
#else /* not TRILIBRARY */
  FILE *outfile;
#endif /* not TRILIBRARY */
  struct triedge triangleloop, trisym;
  int elementnumber;
  int neighbor1, neighbor2, neighbor3;
  triangle ptr;                         /* Temporary variable used by sym(). */

#ifdef TRILIBRARY
  if (!quiet) {
    printf("Writing neighbors.\n");
  }
  /* Allocate memory for neighbors if necessary. */
  if (*neighborlist == (int *) NULL) {
    *neighborlist = (int *) malloc(triangles.items * 3 * sizeof(int));
    if (*neighborlist == (int *) NULL) {
      printf("Error:  Out of memory.\n");
      exit(1);
    }
  }
  nlist = *neighborlist;
  index = 0;
#else /* not TRILIBRARY */
  if (!quiet) {
    printf("Writing %s.\n", neighborfilename);
  }
  outfile = fopen(neighborfilename, "w");
  if (outfile == (FILE *) NULL) {
    printf("  Error:  Cannot create file %s.\n", neighborfilename);
    exit(1);
  }
  /* Number of triangles, three edges per triangle. */
  fprintf(outfile, "%ld  %d\n", triangles.items, 3);
#endif /* not TRILIBRARY */

  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  triangleloop.orient = 0;
  elementnumber = firstnumber;
  while (triangleloop.tri != (triangle *) NULL) {
    * (int *) (triangleloop.tri + 6) = elementnumber;
    triangleloop.tri = triangletraverse();
    elementnumber++;
  }
  * (int *) (dummytri + 6) = -1;

  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  elementnumber = firstnumber;
  while (triangleloop.tri != (triangle *) NULL) {
    triangleloop.orient = 1;
    sym(triangleloop, trisym);
    neighbor1 = * (int *) (trisym.tri + 6);
    triangleloop.orient = 2;
    sym(triangleloop, trisym);
    neighbor2 = * (int *) (trisym.tri + 6);
    triangleloop.orient = 0;
    sym(triangleloop, trisym);
    neighbor3 = * (int *) (trisym.tri + 6);
#ifdef TRILIBRARY
    nlist[index++] = neighbor1;
    nlist[index++] = neighbor2;
    nlist[index++] = neighbor3;
#else /* not TRILIBRARY */
    /* Triangle number, neighboring triangle numbers. */
    fprintf(outfile, "%4d    %d  %d  %d\n", elementnumber,
            neighbor1, neighbor2, neighbor3);
#endif /* not TRILIBRARY */

    triangleloop.tri = triangletraverse();
#ifndef TRILIBRARY
    elementnumber++;
#endif /* not TRILIBRARY */
  }

#ifndef TRILIBRARY
  finishfile(outfile, argc, argv);
#endif /* TRILIBRARY */
}

/*****************************************************************************/
/*                                                                           */
/*  writeoff()   Write the triangulation to an .off file.                    */
/*                                                                           */
/*  OFF stands for the Object File Format, a format used by the Geometry     */
/*  Center's Geomview package.                                               */
/*                                                                           */
/*****************************************************************************/

#ifndef TRILIBRARY

void writeoff(offfilename, argc, argv)
char *offfilename;
int argc;
char **argv;
{
  FILE *outfile;
  struct triedge triangleloop;
  point pointloop;
  point p1, p2, p3;

  if (!quiet) {
    printf("Writing %s.\n", offfilename);
  }
  outfile = fopen(offfilename, "w");
  if (outfile == (FILE *) NULL) {
    printf("  Error:  Cannot create file %s.\n", offfilename);
    exit(1);
  }
  /* Number of points, triangles, and edges. */
  fprintf(outfile, "OFF\n%ld  %ld  %ld\n", points.items, triangles.items, edges);

  /* Write the points. */
  traversalinit(&points);
  pointloop = pointtraverse();
  while (pointloop != (point) NULL) {
    /* The "0.0" is here because the OFF format uses 3D coordinates. */
    fprintf(outfile, " %.17g  %.17g  %.17g\n", pointloop[0], pointloop[1], 0.0);
    pointloop = pointtraverse();
  }

  /* Write the triangles. */
  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  triangleloop.orient = 0;
  while (triangleloop.tri != (triangle *) NULL) {
    org(triangleloop, p1);
    dest(triangleloop, p2);
    apex(triangleloop, p3);
    /* The "3" means a three-vertex polygon. */
    fprintf(outfile, " 3   %4d  %4d  %4d\n", pointmark(p1) - 1,
            pointmark(p2) - 1, pointmark(p3) - 1);
    triangleloop.tri = triangletraverse();
  }
  finishfile(outfile, argc, argv);
}

#endif /* not TRILIBRARY */

/**                                                                         **/
/**                                                                         **/
/********* File I/O routines end here                                *********/

/*****************************************************************************/
/*                                                                           */
/*  quality_statistics()   Print statistics about the quality of the mesh.   */
/*                                                                           */
/*****************************************************************************/

void quality_statistics()
{
  struct triedge triangleloop;
  point p[3];
  REAL cossquaretable[8];
  REAL ratiotable[16];
  REAL dx[3], dy[3];
  REAL edgelength[3];
  REAL dotproduct;
  REAL cossquare;
  REAL triarea;
  REAL shortest, longest;
  REAL trilongest2;
  REAL smallestarea, biggestarea;
  REAL triminaltitude2;
  REAL minaltitude;
  REAL triaspect2;
  REAL worstaspect;
  REAL smallestangle, biggestangle;
  REAL radconst, degconst;
  int angletable[18];
  int aspecttable[16];
  int aspectindex;
  int tendegree;
  int acutebiggest;
  int i, ii, j, k;

  printf("Mesh quality statistics:\n\n");
  radconst = PI / 18.0;
  degconst = 180.0 / PI;
  for (i = 0; i < 8; i++) {
    cossquaretable[i] = cos(radconst * (REAL) (i + 1));
    cossquaretable[i] = cossquaretable[i] * cossquaretable[i];
  }
  for (i = 0; i < 18; i++) {
    angletable[i] = 0;
  }

  ratiotable[0]  =      1.5;      ratiotable[1]  =     2.0;
  ratiotable[2]  =      2.5;      ratiotable[3]  =     3.0;
  ratiotable[4]  =      4.0;      ratiotable[5]  =     6.0;
  ratiotable[6]  =     10.0;      ratiotable[7]  =    15.0;
  ratiotable[8]  =     25.0;      ratiotable[9]  =    50.0;
  ratiotable[10] =    100.0;      ratiotable[11] =   300.0;
  ratiotable[12] =   1000.0;      ratiotable[13] = 10000.0;
  ratiotable[14] = 100000.0;      ratiotable[15] =     0.0;
  for (i = 0; i < 16; i++) {
    aspecttable[i] = 0;
  }

  worstaspect = 0.0;
  minaltitude = xmax - xmin + ymax - ymin;
  minaltitude = minaltitude * minaltitude;
  shortest = minaltitude;
  longest = 0.0;
  smallestarea = minaltitude;
  biggestarea = 0.0;
  smallestangle = 0.0;
  biggestangle = 2.0;
  acutebiggest = 1;

  traversalinit(&triangles);
  triangleloop.tri = triangletraverse();
  triangleloop.orient = 0;
  while (triangleloop.tri != (triangle *) NULL) {
    org(triangleloop, p[0]);
    dest(triangleloop, p[1]);
    apex(triangleloop, p[2]);
    trilongest2 = 0.0;

    for (i = 0; i < 3; i++) {
      j = plus1mod3[i];
      k = minus1mod3[i];
      dx[i] = p[j][0] - p[k][0];
      dy[i] = p[j][1] - p[k][1];
      edgelength[i] = dx[i] * dx[i] + dy[i] * dy[i];
      if (edgelength[i] > trilongest2) {
        trilongest2 = edgelength[i];
      }
      if (edgelength[i] > longest) {
        longest = edgelength[i];
      }
      if (edgelength[i] < shortest) {
        shortest = edgelength[i];
      }
    }

    triarea = counterclockwise(p[0], p[1], p[2]);
    if (triarea < smallestarea) {
      smallestarea = triarea;
    }
    if (triarea > biggestarea) {
      biggestarea = triarea;
    }
    triminaltitude2 = triarea * triarea / trilongest2;
    if (triminaltitude2 < minaltitude) {
      minaltitude = triminaltitude2;
    }
    triaspect2 = trilongest2 / triminaltitude2;
    if (triaspect2 > worstaspect) {
      worstaspect = triaspect2;
    }
    aspectindex = 0;
    while ((triaspect2 > ratiotable[aspectindex] * ratiotable[aspectindex])
           && (aspectindex < 15)) {
      aspectindex++;
    }
    aspecttable[aspectindex]++;

    for (i = 0; i < 3; i++) {
      j = plus1mod3[i];
      k = minus1mod3[i];
      dotproduct = dx[j] * dx[k] + dy[j] * dy[k];
      cossquare = dotproduct * dotproduct / (edgelength[j] * edgelength[k]);
      tendegree = 8;
      for (ii = 7; ii >= 0; ii--) {
        if (cossquare > cossquaretable[ii]) {
          tendegree = ii;
        }
      }
      if (dotproduct <= 0.0) {
        angletable[tendegree]++;
        if (cossquare > smallestangle) {
          smallestangle = cossquare;
        }
        if (acutebiggest && (cossquare < biggestangle)) {
          biggestangle = cossquare;
        }
      } else {
        angletable[17 - tendegree]++;
        if (acutebiggest || (cossquare > biggestangle)) {
          biggestangle = cossquare;
          acutebiggest = 0;
        }
      }
    }
    triangleloop.tri = triangletraverse();
  }

  shortest = sqrt(shortest);
  longest = sqrt(longest);
  minaltitude = sqrt(minaltitude);
  worstaspect = sqrt(worstaspect);
  smallestarea *= 2.0;
  biggestarea *= 2.0;
  if (smallestangle >= 1.0) {
    smallestangle = 0.0;
  } else {
    smallestangle = degconst * acos(sqrt(smallestangle));
  }
  if (biggestangle >= 1.0) {
    biggestangle = 180.0;
  } else {
    if (acutebiggest) {
      biggestangle = degconst * acos(sqrt(biggestangle));
    } else {
      biggestangle = 180.0 - degconst * acos(sqrt(biggestangle));
    }
  }

  printf("  Smallest area: %16.5g   |  Largest area: %16.5g\n", smallestarea, biggestarea);
  printf("  Shortest edge: %16.5g   |  Longest edge: %16.5g\n", shortest, longest);
  printf("  Shortest altitude: %12.5g   |  Largest aspect ratio: %8.5g\n\n", minaltitude, worstaspect);
  printf("  Aspect ratio histogram:\n");
  printf("  1.1547 - %-6.6g    :  %8d    | %6.6g - %-6.6g     :  %8d\n",
         ratiotable[0], aspecttable[0], ratiotable[7], ratiotable[8], aspecttable[8]);
  for (i = 1; i < 7; i++) {
    printf("  %6.6g - %-6.6g    :  %8d    | %6.6g - %-6.6g     :  %8d\n",
           ratiotable[i - 1], ratiotable[i], aspecttable[i], ratiotable[i + 7], ratiotable[i + 8], aspecttable[i + 8]);
  }
  printf("  %6.6g - %-6.6g    :  %8d    | %6.6g -            :  %8d\n",
         ratiotable[6], ratiotable[7], aspecttable[7], ratiotable[14], aspecttable[15]);
  printf("  (Triangle aspect ratio is longest edge divided by shortest altitude)\n\n");
  printf("  Smallest angle: %15.5g   |  Largest angle: %15.5g\n\n", smallestangle, biggestangle);
  printf("  Angle histogram:\n");
  for (i = 0; i < 9; i++) {
    printf("    %3d - %3d degrees:  %8d    |    %3d - %3d degrees:  %8d\n",
           i * 10, i * 10 + 10, angletable[i], i * 10 + 90, i * 10 + 100, angletable[i + 9]);
  }
  printf("\n");
}

/*****************************************************************************/
/*                                                                           */
/*  statistics()   Print all sorts of cool facts.                            */
/*                                                                           */
/*****************************************************************************/

void statistics()
{
  printf("\nStatistics:\n\n");
  printf("  Input points: %d\n", inpoints);
  if (refine) {
    printf("  Input triangles: %d\n", inelements);
  }
  if (poly) {
    printf("  Input segments: %d\n", insegments);
    if (!refine) {
      printf("  Input holes: %d\n", holes);
    }
  }

  printf("\n  Mesh points: %ld\n", points.items);
  printf("  Mesh triangles: %ld\n", triangles.items);
  printf("  Mesh edges: %ld\n", edges);
  if (poly || refine) {
    printf("  Mesh boundary edges: %ld\n", hullsize);
    printf("  Mesh segments: %ld\n\n", shelles.items);
  } else {
    printf("  Mesh convex hull edges: %ld\n\n", hullsize);
  }
  if (verbose) {
    quality_statistics();
    printf("Memory allocation statistics:\n\n");
    printf("  Maximum number of points: %ld\n", points.maxitems);
    printf("  Maximum number of triangles: %ld\n", triangles.maxitems);
    if (shelles.maxitems > 0) {
      printf("  Maximum number of segments: %ld\n", shelles.maxitems);
    }
    if (viri.maxitems > 0) {
      printf("  Maximum number of viri: %ld\n", viri.maxitems);
    }
    if (badsegments.maxitems > 0) {
      printf("  Maximum number of encroached segments: %ld\n",
             badsegments.maxitems);
    }
    if (badtriangles.maxitems > 0) {
      printf("  Maximum number of bad triangles: %ld\n",
             badtriangles.maxitems);
    }
    if (splaynodes.maxitems > 0) {
      printf("  Maximum number of splay tree nodes: %ld\n",
             splaynodes.maxitems);
    }
    printf("  Approximate heap memory use (bytes): %ld\n\n",
           points.maxitems * points.itembytes
           + triangles.maxitems * triangles.itembytes
           + shelles.maxitems * shelles.itembytes
           + viri.maxitems * viri.itembytes
           + badsegments.maxitems * badsegments.itembytes
           + badtriangles.maxitems * badtriangles.itembytes
           + splaynodes.maxitems * splaynodes.itembytes);

    printf("Algorithmic statistics:\n\n");
    printf("  Number of incircle tests: %ld\n", incirclecount);
    printf("  Number of orientation tests: %ld\n", counterclockcount);
    if (hyperbolacount > 0) {
      printf("  Number of right-of-hyperbola tests: %ld\n",
             hyperbolacount);
    }
    if (circumcentercount > 0) {
      printf("  Number of circumcenter computations: %ld\n",
             circumcentercount);
    }
    if (circletopcount > 0) {
      printf("  Number of circle top computations: %ld\n",
             circletopcount);
    }
    printf("\n");
  }
}

/*****************************************************************************/
/*                                                                           */
/*  main() or triangulate()   Gosh, do everything.                           */
/*                                                                           */
/*  The sequence is roughly as follows.  Many of these steps can be skipped, */
/*  depending on the command line switches.                                  */
/*                                                                           */
/*  - Initialize constants and parse the command line.                       */
/*  - Read the points from a file and either                                 */
/*    - triangulate them (no -r), or                                         */
/*    - read an old mesh from files and reconstruct it (-r).                 */
/*  - Insert the PSLG segments (-p), and possibly segments on the convex     */
/*      hull (-c).                                                           */
/*  - Read the holes (-p), regional attributes (-pA), and regional area      */
/*      constraints (-pa).  Carve the holes and concavities, and spread the  */
/*      regional attributes and area constraints.                            */
/*  - Enforce the constraints on minimum angle (-q) and maximum area (-a).   */
/*      Also enforce the conforming Delaunay property (-q and -a).           */
/*  - Compute the number of edges in the resulting mesh.                     */
/*  - Promote the mesh's linear triangles to higher order elements (-o).     */
/*  - Write the output files and print the statistics.                       */
/*  - Check the consistency and Delaunay property of the mesh (-C).          */
/*                                                                           */
/*****************************************************************************/

#ifdef TRILIBRARY

void triangulate(triswitches, in, out, vorout)
char *triswitches;
struct triangulateio *in;
struct triangulateio *out;
struct triangulateio *vorout;

#else /* not TRILIBRARY */

int main(argc, argv)
int argc;
char **argv;

#endif /* not TRILIBRARY */

{
  REAL *holearray;                                        /* Array of holes. */
  REAL *regionarray;   /* Array of regional attributes and area constraints. */
#ifndef TRILIBRARY
  FILE *polyfile;
#endif /* not TRILIBRARY */
#ifndef NO_TIMER
  /* Variables for timing the performance of Triangle.  The types are */
  /*   defined in sys/time.h.                                         */
  struct timeval tv0, tv1, tv2, tv3, tv4, tv5, tv6;
  struct timezone tz;
#endif /* NO_TIMER */

#ifndef NO_TIMER
  gettimeofday(&tv0, &tz);
#endif /* NO_TIMER */

  triangleinit();
#ifdef TRILIBRARY
  parsecommandline(1, &triswitches);
#else /* not TRILIBRARY */
  parsecommandline(argc, argv);
#endif /* not TRILIBRARY */

#ifdef TRILIBRARY
  transfernodes(in->pointlist, in->pointattributelist, in->pointmarkerlist,
                in->numberofpoints, in->numberofpointattributes);
#else /* not TRILIBRARY */
  readnodes(innodefilename, inpolyfilename, &polyfile);
#endif /* not TRILIBRARY */

#ifndef NO_TIMER
  if (!quiet) {
    gettimeofday(&tv1, &tz);
  }
#endif /* NO_TIMER */

#ifdef CDT_ONLY
  hullsize = delaunay();                          /* Triangulate the points. */
#else /* not CDT_ONLY */
  if (refine) {
    /* Read and reconstruct a mesh. */
#ifdef TRILIBRARY
    hullsize = reconstruct(in->trianglelist, in->triangleattributelist,
                           in->trianglearealist, in->numberoftriangles,
                           in->numberofcorners, in->numberoftriangleattributes,
                           in->segmentlist, in->segmentmarkerlist,
                           in->numberofsegments);
#else /* not TRILIBRARY */
    hullsize = reconstruct(inelefilename, areafilename, inpolyfilename,
                           polyfile);
#endif /* not TRILIBRARY */
  } else {
    hullsize = delaunay();                        /* Triangulate the points. */
  }
#endif /* not CDT_ONLY */

#ifndef NO_TIMER
  if (!quiet) {
    gettimeofday(&tv2, &tz);
    if (refine) {
      printf("Mesh reconstruction");
    } else {
      printf("Delaunay");
    }
    printf(" milliseconds:  %ld\n", 1000l * (tv2.tv_sec - tv1.tv_sec)
           + (tv2.tv_usec - tv1.tv_usec) / 1000l);
  }
#endif /* NO_TIMER */

  /* Ensure that no point can be mistaken for a triangular bounding */
  /*   box point in insertsite().                                   */
  infpoint1 = (point) NULL;
  infpoint2 = (point) NULL;
  infpoint3 = (point) NULL;

  if (useshelles) {
    checksegments = 1;                  /* Segments will be introduced next. */
    if (!refine) {
      /* Insert PSLG segments and/or convex hull segments. */
#ifdef TRILIBRARY
      insegments = formskeleton(in->segmentlist, in->segmentmarkerlist,
                                in->numberofsegments);
#else /* not TRILIBRARY */
      insegments = formskeleton(polyfile, inpolyfilename);
#endif /* not TRILIBRARY */
    }
  }

#ifndef NO_TIMER
  if (!quiet) {
    gettimeofday(&tv3, &tz);
    if (useshelles && !refine) {
      printf("Segment milliseconds:  %ld\n",
             1000l * (tv3.tv_sec - tv2.tv_sec) + (tv3.tv_usec - tv2.tv_usec) / 1000l);
    }
  }
#endif /* NO_TIMER */

  if (poly) {
#ifdef TRILIBRARY
    holearray = in->holelist;
    holes = in->numberofholes;
    regionarray = in->regionlist;
    regions = in->numberofregions;
#else /* not TRILIBRARY */
    readholes(polyfile, inpolyfilename, &holearray, &holes,
              &regionarray, &regions);
#endif /* not TRILIBRARY */
    if (!refine) {
      /* Carve out holes and concavities. */
      carveholes(holearray, holes, regionarray, regions);
    }
  } else {
    /* Without a PSLG, there can be no holes or regional attributes   */
    /*   or area constraints.  The following are set to zero to avoid */
    /*   an accidental free() later.                                  */
    holes = 0;
    regions = 0;
  }

#ifndef NO_TIMER
  if (!quiet) {
    gettimeofday(&tv4, &tz);
    if (poly && !refine) {
      printf("Hole milliseconds:  %ld\n", 1000l * (tv4.tv_sec - tv3.tv_sec)
             + (tv4.tv_usec - tv3.tv_usec) / 1000l);
    }
  }
#endif /* NO_TIMER */

#ifndef CDT_ONLY
  if (quality) {
    enforcequality();                 /* Enforce angle and area constraints. */
  }
#endif /* not CDT_ONLY */

#ifndef NO_TIMER
  if (!quiet) {
    gettimeofday(&tv5, &tz);
#ifndef CDT_ONLY
    if (quality) {
      printf("Quality milliseconds:  %ld\n",
             1000l * (tv5.tv_sec - tv4.tv_sec) + (tv5.tv_usec - tv4.tv_usec) / 1000l);
    }
#endif /* not CDT_ONLY */
  }
#endif /* NO_TIMER */

  /* Compute the number of edges. */
  edges = (3l * triangles.items + hullsize) / 2l;

  if (order > 1) {
    highorder();             /* Promote elements to higher polynomial order. */
  }
  if (!quiet) {
    printf("\n");
  }

#ifdef TRILIBRARY
  out->numberofpoints = points.items;
  out->numberofpointattributes = nextras;
  out->numberoftriangles = triangles.items;
  out->numberofcorners = (order + 1) * (order + 2) / 2;
  out->numberoftriangleattributes = eextras;
  out->numberofedges = edges;
  if (useshelles) {
    out->numberofsegments = shelles.items;
  } else {
    out->numberofsegments = hullsize;
  }
  if (vorout != (struct triangulateio *) NULL) {
    vorout->numberofpoints = triangles.items;
    vorout->numberofpointattributes = nextras;
    vorout->numberofedges = edges;
  }
#endif /* TRILIBRARY */
  /* If not using iteration numbers, don't write a .node file if one was */
  /*   read, because the original one would be overwritten!              */
  if (nonodewritten || (noiterationnum && readnodefile)) {
    if (!quiet) {
#ifdef TRILIBRARY
      printf("NOT writing points.\n");
#else /* not TRILIBRARY */
      printf("NOT writing a .node file.\n");
#endif /* not TRILIBRARY */
    }
    numbernodes();                 /* We must remember to number the points. */
  } else {
#ifdef TRILIBRARY
    writenodes(&out->pointlist, &out->pointattributelist,
               &out->pointmarkerlist);
#else /* not TRILIBRARY */
    writenodes(outnodefilename, argc, argv);      /* Numbers the points too. */
#endif /* TRILIBRARY */
  }
  if (noelewritten) {
    if (!quiet) {
#ifdef TRILIBRARY
      printf("NOT writing triangles.\n");
#else /* not TRILIBRARY */
      printf("NOT writing an .ele file.\n");
#endif /* not TRILIBRARY */
    }
  } else {
#ifdef TRILIBRARY
    writeelements(&out->trianglelist, &out->triangleattributelist);
#else /* not TRILIBRARY */
    writeelements(outelefilename, argc, argv);
#endif /* not TRILIBRARY */
  }
  /* The -c switch (convex switch) causes a PSLG to be written */
  /*   even if none was read.                                  */
  if (poly || convex) {
    /* If not using iteration numbers, don't overwrite the .poly file. */
    if (nopolywritten || noiterationnum) {
      if (!quiet) {
#ifdef TRILIBRARY
        printf("NOT writing segments.\n");
#else /* not TRILIBRARY */
        printf("NOT writing a .poly file.\n");
#endif /* not TRILIBRARY */
      }
    } else {
#ifdef TRILIBRARY
      writepoly(&out->segmentlist, &out->segmentmarkerlist);
      out->numberofholes = holes;
      out->numberofregions = regions;
      if (poly) {
        out->holelist = in->holelist;
        out->regionlist = in->regionlist;
      } else {
        out->holelist = (REAL *) NULL;
        out->regionlist = (REAL *) NULL;
      }
#else /* not TRILIBRARY */
      writepoly(outpolyfilename, holearray, holes, regionarray, regions, argc, argv);
#endif /* not TRILIBRARY */
    }
  }
#ifndef TRILIBRARY
#ifndef CDT_ONLY
  if (regions > 0) {
    free(regionarray);
  }
#endif /* not CDT_ONLY */
  if (holes > 0) {
    free(holearray);
  }
  if (geomview) {
    writeoff(offfilename, argc, argv);
  }
#endif /* not TRILIBRARY */
  if (edgesout) {
#ifdef TRILIBRARY
    writeedges(&out->edgelist, &out->edgemarkerlist);
#else /* not TRILIBRARY */
    writeedges(edgefilename, argc, argv);
#endif /* not TRILIBRARY */
  }
  if (voronoi) {
#ifdef TRILIBRARY
    writevoronoi(&vorout->pointlist, &vorout->pointattributelist,
                 &vorout->pointmarkerlist, &vorout->edgelist,
                 &vorout->edgemarkerlist, &vorout->normlist);
#else /* not TRILIBRARY */
    writevoronoi(vnodefilename, vedgefilename, argc, argv);
#endif /* not TRILIBRARY */
  }
  if (neighbors) {
#ifdef TRILIBRARY
    writeneighbors(&out->neighborlist);
#else /* not TRILIBRARY */
    writeneighbors(neighborfilename, argc, argv);
#endif /* not TRILIBRARY */
  }

  if (!quiet) {
#ifndef NO_TIMER
    gettimeofday(&tv6, &tz);
    printf("\nOutput milliseconds:  %ld\n",
           1000l * (tv6.tv_sec - tv5.tv_sec) + (tv6.tv_usec - tv5.tv_usec) / 1000l);
    printf("Total running milliseconds:  %ld\n",
           1000l * (tv6.tv_sec - tv0.tv_sec) + (tv6.tv_usec - tv0.tv_usec) / 1000l);
#endif /* NO_TIMER */

    statistics();
  }

#ifndef REDUCED
  if (docheck) {
    checkmesh();
    checkdelaunay();
  }
#endif /* not REDUCED */

  triangledeinit();
#ifndef TRILIBRARY
  return 0;
#endif /* not TRILIBRARY */
}
