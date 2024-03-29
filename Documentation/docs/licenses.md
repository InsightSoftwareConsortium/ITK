ITK Software Licenses
=====================

The copyright of the Insight Toolkit (ITK) is held by:

> Copyright 1999-2019    Insight Software Consortium
>
> Copyright 2020-present NumFOCUS

This software is distributed under the Apache 2.0 License.

See the [LICENSE](../../LICENSE) file for details.

Historical Notes
----------------

The Insight Toolkit (ITK) was initially developed under contract to the
National Library of Medicine at the National Institutes of Health. ITK is
partially derived from VTK and VXL, hence some code is copyrighted accordingly
(see VTKCopyright.txt and VXLCopyright.txt).

The copyright of most of the files in the "Utilities" subdirectory is held by
third parties who allow to distribute this material under a license compatible
with the one used by ITK. Please read the content of the subdirectories for
specific details on those third-party licenses. You will also find details in
the README.txt file under the "Copyright" subdirectory.

Starting with its version ITK 3.6, The Insight Toolkit is distributed under the
new and simplified BSD license approved by the [Open Source Initiative (OSI)](https://www.opensource.org/licenses/bsd-license.php).

Starting with its version ITK 4, The Insight Toolkit is distributed under the
Apache 2.0 License. The move from the BSD license to the Apache 2.0 was
discussed at the October 2009 Insight Software Consortium Board Meeting,
discussed until December 14th, 2009 and the Insight Software Consortium Board
voted and approved the transition to Apache 2.0 license during a period from
December 15, 2009 to January 14th, 2010 to move to the Apache 2.0 License.

On November 26th, 2019 the Insight Software Consortium transferred the ITK
copyright to NumFOCUS following a vote by the Insight Software Consortium
Board of Directors.

Vendored Third Party Software Licenses
--------------------------------------

The Toolkit contains code from third parties that have different copyrights.

They are:


1. DICOMParser

> Copyright by Matt Turek.
>
> Distributed under a BSD-Like license.
>
> [This work was funded by NLM so its copyright
> should probably be transfered to the ISC.]

2. AnalyzeDB.h

> The Mayo Clinic aggreed to distribute this file
> with ITK. Point of contact: Vincent Magnotta.
>
> The file was moved to Utilities/itkExtHdrs

3. PixelData

> GE Copyrights,
>
> Vincent is contacting the providers of this code too.

4. Netlib

> Netlib is a conglomerate of packages. Each
> one having different copyrights and licenses.
> Here is a listing of what is included inside
> Utilities/vxl/v3p/netlib:

>> blas:         From www.netlib.org/blas :
>> FAQ: The reference BLAS is a freely-available software package.
>> It is available from netlib via anonymous ftp and the World Wide Web.
>> Thus, it can be included in commercial software packages (and has been).
>> We only ask that proper credit be given to the authors.

>> datapac:   Written by James Filliben, National Bureau of Standars.

>> Our understanding is that the US Government cannot hold
>> copyrights, therefore we assume that this code is in the
>> public domain.

>> eispack:
>> lapack:
>> laso:
>> libf2c:
>> linalg:
>> linpack:
>> mathews:
>> minpack:
>> napack:
>> opt:
>> temperton:
>> tests:
>> toms:

5. The triangle.c/h and showme.c files have been removed.

> Their copyright/license was incompatible with
> ITK and their functionality was not needed.
> These removed the "itknetlib" library.

6. itk_hash_map

> It has copyrights by SGI and HP, but the license
> is consistent with BSD-like. So we simply need
> to acknowledge those in our general copyright.

7. itkMersenneTwisterRandomVariateGenerator

> Copyright: Makoto Matsumoto and Takuji Nishimura,
> Copyright: Richard J. Wagner
> under a BSD-Like license.

8. Code/Numerics/FEM/dsrc2c.c

> Copyright: 1977 IMSL Inc, no license available.
> Stephen Aylward removed the specific functions
> in this file that were copyrighted by IMSL and
> replaced them with other functions available in
> netlib. The file is now cleared from copyright
> concerns.

9. Code/Numerics/cvsrmvend

> Copyright:  Greg A. Woods
> "Freely distributable"
> I guess that counts as BSD...
> Easy to remove if it becomes a problem,
> This is just a script for making easier
> the CVS syncs of VXL.

10. Testing/Code/Numerics/NeuralNetworks

> Copyright: CADDLab at UNC
> Fixed now in CVS --> ISC

### Utilities:

A. JPEG

> Thomas G Lane ( BSD-Like)
>
> DCMTK : OFFIS... and many others... but BSD-Like

B. OpenJPEG

> Universite Catholique de Louvain:
> BSD license

C. NrrdIO

> Gordon Kindlmann and  University of Utah
> BSD-Like license. Clarifies that the original
> package "Teem" (not in ITK) is still under GPL license

D. NIFTI

> No copyright ?
> There is no mention of licensing nor copyright
> at the NIFTI Web site:
>
>     https://nifti.nimh.nih.gov/nifti-1
>
> However at http://niftilib.sourceforge.net it
> appears as "public domain" license, but in
> its subdirectory matlibs it has a GPL license.
> The matlibs subdirectory is not included in ITK.
>
> The subdirectory jlibs is public domain
> The subdirecotry clibs is public domain
>
> in ITK it had a subdir:
>
>    fsliolib  :  From the FSL package:
>                 University of Oxford:
>                 NO FOR COMMERCIAL USE
>
>                 This code has now been removed.

E. EXPAT

> Thai Open Source Software Center Ltd and Clark Cooper.
> with BSD-Like license

F. PNG

> Copyright: Glen Randers-Pehrson,
> Andreas Dilger,
> Guy Erc Schalnat

G. TIFF

> Copyright Sam Leffler and Silicon Graphics

H. GDCM

> Copyright: CREATIS, Mathieu Malaterre: BSD-Like License

I. ZLIB

> Copyright:  Mark Adler

J. kwsys

> Copyright: Kitware : BSD-Like License

K. VXL/lbfgsb

>       http://users.eecs.northwestern.edu/~nocedal/lbfgsb.html
>
> This software is released under the BSD License
>
> Condition for Use: This software is freely available, but we expect that
> all publications describing  work using this software , or all
> commercial products using it, quote at least one of the references given
> below.
>
> References
>
> 1. R. H. Byrd, P. Lu and J. Nocedal. A Limited Memory Algorithm for Bound Constrained Optimization, (1995), SIAM Journal on Scientific and Statistical Computing , 16, 5, pp. 1190-1208.
> 2. C. Zhu, R. H. Byrd and J. Nocedal. L-BFGS-B: Algorithm 778: L-BFGS-B, FORTRAN routines for large scale bound constrained optimization (1997), ACM Transactions on Mathematical Software, Vol 23, Num. 4, pp. 550 - 560.
> 3. J.L. Morales and J. Nocedal. L-BFGS-B: Remark on Algorithm 778: L-BFGS-B, FORTRAN routines for large scale bound constrained optimization (2011), to appear in ACM Transactions on Mathematical Software.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

VTK License Statement (VTKCopyright.txt)
----------------------------------------

Portions of the Insight code base are derived from the Visualization Toolkit
(VTK), and hence are covered under the VTK copyright. The VTK copyright is as
follows:

Copyright (c) 1993-2002 Ken Martin, Will Schroeder, Bill Lorensen
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * Neither name of Ken Martin, Will Schroeder, or Bill Lorensen nor the names
   of any contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

 * Modified source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

See also the VTK web site: https://www.vtk.org for more information.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

VXL License Statement (VXLCopyright.txt)
----------------------------------------

Portions of the Insight code base (the numerics package VNL) are derived from
the Vision-something-Libraries (VXL), and hence are covered under the VXL
copyright. The VXL copyright is as follows:


                Copyright (c) 2000 TargetJr Consortium
            GE Corporate Research and Development (GE CRD)
                          1 Research Circle
                         Niskayuna, NY 12309
                         All Rights Reserved
           Reproduction rights limited as described below.

   Permission to use, copy, modify, distribute, and sell this software
   and its documentation for any purpose is hereby granted without fee,
   provided that (i) the above copyright notice and this permission
   notice appear in all copies of the software and related documentation,
   (ii) the name TargetJr Consortium (represented by GE CRD), may not be
   used in any advertising or publicity relating to the software without
   the specific, prior written permission of GE CRD, and (iii) any
   modifications are clearly marked and summarized in a change history
   log.

   THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,
   EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
   WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
   IN NO EVENT SHALL THE TARGETJR CONSORTIUM BE LIABLE FOR ANY SPECIAL,
   INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND OR ANY
   DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
   WHETHER OR NOT ADVISED OF THE POSSIBILITY OF SUCH DAMAGES, OR ON
   ANY THEORY OF LIABILITY ARISING OUT OF OR IN CONNECTION WITH THE
   USE OR PERFORMANCE OF THIS SOFTWARE.


See also the VXL web site: http://vxl.sourceforge.net/ for more information.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


 Copyright (c)
 Biomedical Imaging Resource
 Mayo Clinic

 All rights reserved.

 Redistribution and use in source and binary forms, with or
 without modification, are permitted provided that the following
 conditions are met:

 1) Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.

 2) Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.

 Neither the name of the Mayo Clinic nor the names of its
 contributors may be used to endorse or promote products derived from
 this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

28 files covered by:

Copyright (c)
  Silicon Graphics Computer Systems, Inc.

  Permission to use, copy, modify, distribute and sell this software
  and its documentation for any purpose is hereby granted without fee,
  provided that the above copyright notice appear in all copies and
  that both that copyright notice and this permission notice appear
  in supporting documentation.  Silicon Graphics makes no
  representations about the suitability of this software for any
  purpose.  It is provided "as is" without express or implied warranty.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

29 files covered by:

Copyright (c)
  Hewlett-Packard Company

  Permission to use, copy, modify, distribute and sell this software
  and its documentation for any purpose is hereby granted without fee,
  provided that the above copyright notice appear in all copies and
  that both that copyright notice and this permission notice appear
  in supporting documentation.  Hewlett-Packard Company makes no
  representations about the suitability of this software for any
  purpose.  It is provided "as is" without express or implied warranty.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

9 files covered by:

Copyright (c)
  Mark of the Unicorn, Inc.

  Permission to use, copy, modify, distribute and sell this software
  and its documentation for any purpose is hereby granted without fee,
  provided that the above copyright notice appear in all copies and
  that both that copyright notice and this permission notice appear
  in supporting documentation.  Mark of the Unicorn makes no
  representations about the suitability of this software for any
  purpose.  It is provided "as is" without express or implied warranty.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

38 files covered by:

Copyright (c)
  Moscow Center for SPARC Technology

  Permission to use, copy, modify, distribute and sell this software
  and its documentation for any purpose is hereby granted without fee,
  provided that the above copyright notice appear in all copies and
  that both that copyright notice and this permission notice appear
  in supporting documentation.  Moscow Center for SPARC Technology makes no
  representations about the suitability of this software for any
  purpose.  It is provided "as is" without express or implied warranty.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

1 file covered by:

Copyright (C) Makoto Matsumoto and Takuji Nishimura,
  Copyright (C) Richard J. Wagner
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. The names of its contributors may not be used to endorse or promote
       products derived from this software without specific prior written
       permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  The original code included the following notice:

      When you use this, send an email to: matumoto at math dot keio dot ac dot jp
      with an appropriate reference to your work.

  It would be nice to CC:
  rjwagner at writeme dot com and Cokus at math dot washington dot edu
  when you write.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

1 file covered by:

(c) Copyright by Greg A. Woods <woods@planix.com>

  Freely redistibutable.
  All other rights reserved.
  Return all fixes/modifications to <woods@planix.com>.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

1 file covered by:

Copyright by AT&T, Lucent Technologies and Bellcore.

Permission to use, copy, modify, and distribute this software
and its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the names of AT&T, Bell Laboratories,
Lucent or Bellcore or any of their entities not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

AT&T, Lucent and Bellcore disclaim all warranties with regard to
this software, including all implied warranties of
merchantability and fitness.  In no event shall AT&T, Lucent or
Bellcore be liable for any special, indirect or consequential
damages or any damages whatsoever resulting from loss of use,
data or profits, whether in an action of contract, negligence or
other tortious action, arising out of or in connection with the
use or performance of this software.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

1 file covered by:

Copyright (C) Lucent Technologies
All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name of Lucent or any of its entities
not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

LUCENT DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL LUCENT OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

1 file covered by:

Copyright (c) TargetJr Consortium
               GE Corporate Research and Development (GE CRD)
                             1 Research Circle
                            Niskayuna, NY 12309
                            All Rights Reserved
              Reproduction rights limited as described below.

      Permission to use, copy, modify, distribute, and sell this software
      and its documentation for any purpose is hereby granted without fee,
      provided that (i) the above copyright notice and this permission
      notice appear in all copies of the software and related documentation,
      (ii) the name TargetJr Consortium (represented by GE CRD), may not be
      used in any advertising or publicity relating to the software without
      the specific, prior written permission of GE CRD, and (iii) any
      modifications are clearly marked and summarized in a change history
      log.

      THE SOFTWARE IS PROVIDED "AS IS" AND WITHOUT WARRANTY OF ANY KIND,
      EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
      WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
      IN NO EVENT SHALL THE TARGETJR CONSORTIUM BE LIABLE FOR ANY SPECIAL,
      INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND OR ANY
      DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
      WHETHER OR NOT ADVISED OF THE POSSIBILITY OF SUCH DAMAGES, OR ON
      ANY THEORY OF LIABILITY ARISING OUT OF OR IN CONNECTION WITH THE
      USE OR PERFORMANCE OF THIS SOFTWARE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


1 file covered by:

Copyright (C) General Electric Company.

 Permission is granted to any individual or institution to use, copy, modify,
 and distribute this software, provided that this complete copyright and
 permission notice is maintained, intact, in all copies and supporting
 documentation.

 General Electric Company,
 provides this software "as is" without express or implied warranty.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


5 files covered by:

Copyright (C) Texas Instruments Incorporated.

 Permission is granted to any individual or institution to use, copy, modify,
 and distribute this software, provided that this complete copyright and
 permission notice is maintained, intact, in all copies and supporting
 documentation.

 Texas Instruments Incorporated provides this software "as is" without
 express or implied warranty.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


2 files covered by:

Copyright (C) Texas Instruments Incorporated.
 Copyright (C) General Electric Company.

 Permission is granted to any individual or institution to use, copy, modify,
 and distribute this software, provided that this complete copyright and
 permission notice is maintained, intact, in all copies and supporting
 documentation.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


1 file covered by:

Copyright (C) Free Software Foundation, Inc.

 This configure script is free software; the Free Software Foundation
 gives unlimited permission to copy, distribute and modify it.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


1 file covered by:

Copyright by the Massachusetts Institute of Technology

 Permission to use, copy, modify, distribute, and sell this software and its
 documentation for any purpose is hereby granted without fee, provided that
 the above copyright notice appear in all copies and that both that
 copyright notice and this permission notice appear in supporting
 documentation, and that the name of M.I.T. not be used in advertising or
 publicity pertaining to distribution of the software without specific,
 written prior permission.  M.I.T. makes no representations about the
 suitability of this software for any purpose.  It is provided "as is"
 without express or implied warranty.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


98 files covered by:

Copyright (c) 1999-2005 CREATIS
(CREATIS = Centre de Recherche et d'Applications en Traitement de l'Image)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * Neither the name of CREATIS, nor the names of any contributor (CNRS, INSERM,
   INSA, Universite Claude-Bernard Lyon I), may be used to endorse or promote
   products derived from this software without specific prior written permission.

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


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


13 files covered by:

Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
                               and Clark Cooper

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


54 files covered by:

Copyright (C) Gordon Kindlmann
  Copyright (C) University of Utah

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


2 files covered by:

Copyright (c) David A. Clunie DBA PixelMed Publishing. All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are
 permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this list of
    conditions and the following disclaimers.

 2. Redistributions in binary form must reproduce the above copyright notice, this list of
    conditions and the following disclaimers in the documentation and/or other materials
    provided with the distribution.

 3. Neither the name of PixelMed Publishing nor the names of its contributors may
    be used to endorse or promote products derived from this software.

 This software is provided by the copyright holders and contributors "as is" and any
 express or implied warranties, including, but not limited to, the implied warranties
 of merchantability and fitness for a particular purpose are disclaimed. In no event
 shall the copyright owner or contributors be liable for any direct, indirect, incidental,
 special, exemplary, or consequential damages (including, but not limited to, procurement
 of substitute goods or services; loss of use, data or profits; or business interruption)
 however caused and on any theory of liability, whether in contract, strict liability, or
 tort (including negligence or otherwise) arising in any way out of the use of this software,
 even if advised of the possibility of such damage.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


14 files covered by:

Copyright (c) 2003 Matt Turek
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of Matt Turek nor the names of any contributors may be used to
   endorse or promote products derived from this software without specific
   prior written permission.

 * Modified source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


20 files covered by:

libpng versions 1.0.7, July 1, 2000, through  1.0.12, June 8, 2001, are
Copyright (c) 2000, 2001 Glenn Randers-Pehrson, and are
distributed according to the same disclaimer and license as libpng-1.0.6
with the following individuals added to the list of Contributing Authors

   Simon-Pierre Cadieux
   Eric S. Raymond
   Gilles Vollant

and with the following additions to the disclaimer:

   There is no warranty against interference with your enjoyment of the
   library or against infringement.  There is no warranty that our
   efforts or the library will fulfill any of your particular purposes
   or needs.  This library is provided with all faults, and the entire
   risk of satisfactory quality, performance, accuracy, and effort is with
   the user.

libpng versions 0.97, January 1998, through 1.0.6, March 20, 2000, are
Copyright (c) 1998, 1999, 2000 Glenn Randers-Pehrson
Distributed according to the same disclaimer and license as libpng-0.96,
with the following individuals added to the list of Contributing Authors:

   Tom Lane
   Glenn Randers-Pehrson
   Willem van Schaik

libpng versions 0.89, June 1996, through 0.96, May 1997, are
Copyright (c) 1996, 1997 Andreas Dilger
Distributed according to the same disclaimer and license as libpng-0.88,
with the following individuals added to the list of Contributing Authors:

   John Bowler
   Kevin Bracey
   Sam Bushell
   Magnus Holmgren
   Greg Roelofs
   Tom Tanner

libpng versions 0.5, May 1995, through 0.88, January 1996, are
Copyright (c) 1995, 1996 Guy Eric Schalnat, Group 42, Inc.

For the purposes of this copyright and license, "Contributing Authors"
is defined as the following set of individuals:

   Andreas Dilger
   Dave Martindale
   Guy Eric Schalnat
   Paul Schmidt
   Tim Wegner

The PNG Reference Library is supplied "AS IS".  The Contributing Authors
and Group 42, Inc. disclaim all warranties, expressed or implied,
including, without limitation, the warranties of merchantability and of
fitness for any purpose.  The Contributing Authors and Group 42, Inc.
assume no liability for direct, indirect, incidental, special, exemplary,
or consequential damages, which may result from the use of the PNG
Reference Library, even if advised of the possibility of such damage.

Permission is hereby granted to use, copy, modify, and distribute this
source code, or portions hereof, for any purpose, without fee, subject
to the following restrictions:

1. The origin of this source code must not be misrepresented.

2. Altered versions must be plainly marked as such and
must not be misrepresented as being the original source.

3. This Copyright notice may not be removed or altered from
   any source or altered source distribution.

The Contributing Authors and Group 42, Inc. specifically permit, without
fee, and encourage the use of this source code as a component to
supporting the PNG file format in commercial products.  If you use this
source code in a product, acknowledgment is not required but would be
appreciated.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


1 file covered by:

(c) Copyright
         Biomedical Imaging Resource
         Mayo Foundation
   Incorporation of components of dbh.h are by permission of the
   Mayo Foundation.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


2 files covered by:

Copyright (C) Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


1 file covered by:

Copyright (c) by University of Toronto.
      Written by Henry Spencer.  Not derived from licensed software.

      Permission is granted to anyone to use this software for any
      purpose on any computer system, and to redistribute it freely,
      subject to the following restrictions:

      1. The author is not responsible for the consequences of use of
              this software, no matter how awful, even if they arise
              from defects in it.

      2. The origin of this software must not be misrepresented, either
              by explicit claim or by omission.

      3. Altered versions must be plainly marked as such, and must not
              be misrepresented as being the original software.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


25 files covered by:

Copyright (c) David Janssens
 Copyright (c) Yannick Verschueren
 Copyright (c) Francois Devaux and Antonin Descampe
 Copyright (c) Hervé Drolon, FreeImage Team
 Copyright (c) Communications and remote sensing Laboratory, Universite catholique de Louvain, Belgium
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


4 files covered by:

Copyright (c) Yannick Verschueren
 Copyright (c) Hervé Drolon, FreeImage Team
 Copyright (c) Communications and remote sensing Laboratory, Universite catholique de Louvain, Belgium
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


8 files covered by:

Copyright (c) Hervé Drolon, FreeImage Team
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


2 files covered by:

Copyright (c) Francois Devaux and Antonin Descampe
 Copyright (c) Hervé Drolon, FreeImage Team
 Copyright (c) Communications and remote sensing Laboratory, Universite catholique de Louvain, Belgium
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS `AS IS'
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


78 files covered by:

This software is copyright (C) 1991-1998, Thomas G. Lane.
All Rights Reserved except as specified below.

Permission is hereby granted to use, copy, modify, and distribute this
software (or portions thereof) for any purpose, without fee, subject to these
conditions:
(1) If any part of the source code for this software is distributed, then this
README file must be included, with this copyright and no-warranty notice
unaltered; and any additions, deletions, or changes to the original files
must be clearly indicated in accompanying documentation.
(2) If only executable code is distributed, then the accompanying
documentation must state that "this software is based in part on the work of
the Independent JPEG Group".
(3) Permission for use of this software is granted only if the user accepts
full responsibility for any undesirable consequences; the authors accept
NO LIABILITY for damages of any kind.

These conditions apply to any software derived from or based on the IJG code,
not just to the unmodified library.  If you use our work, you ought to
acknowledge us.

Permission is NOT granted for the use of any IJG author's name or company name
in advertising or publicity relating to this software or products derived from
it.  This software may be referred to only as "the Independent JPEG Group's
software".

We specifically permit and encourage the use of this software as the basis of
commercial products, provided that all warranty or liability claims are
assumed by the product vendor.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


54 files covered by:

Copyright (c) Sam Leffler
 Copyright (c) Silicon Graphics, Inc.

 Permission to use, copy, modify, distribute, and sell this software and
 its documentation for any purpose is hereby granted without fee, provided
 that (i) the above copyright notices and this permission notice appear in
 all copies of the software and related documentation, and (ii) the names of
 Sam Leffler and Silicon Graphics may not be used in any advertising or
 publicity relating to the software without the specific, prior written
 permission of Sam Leffler and Silicon Graphics.

 THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

 IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
 LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 OF THIS SOFTWARE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


1 file covered by:

Copyright (c) Greg Ward Larson
 Copyright (c) Silicon Graphics, Inc.

 Permission to use, copy, modify, distribute, and sell this software and
 its documentation for any purpose is hereby granted without fee, provided
 that (i) the above copyright notices and this permission notice appear in
 all copies of the software and related documentation, and (ii) the names of
 Sam Leffler, Greg Larson and Silicon Graphics may not be used in any
 advertising or publicity relating to the software without the specific,
 prior written permission of Sam Leffler, Greg Larson and Silicon Graphics.

 THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

 IN NO EVENT SHALL SAM LEFFLER, GREG LARSON OR SILICON GRAPHICS BE LIABLE
 FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
 LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 OF THIS SOFTWARE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


1 file covered by:

Copyright (c) The Regents of the University of California.
 All rights reserved.

 This code is derived from software contributed to Berkeley by
 James A. Woods, derived from original work by Spencer Thomas
 and Joseph Orost.

 Redistribution and use in source and binary forms are permitted
 provided that the above copyright notice and this paragraph are
 duplicated in all such forms and that any documentation,
 advertising materials, and other materials related to such
 distribution and use acknowledge that the software was developed
 by the University of California, Berkeley.  The name of the
 University may not be used to endorse or promote products derived
 from this software without specific prior written permission.
 THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


3 files covered by:

Decoder support is derived, with permission, from the code
 in Frank Cringle's viewfax program;
      Copyright (C) Frank D. Cringle.


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


1 file covered by:

Copyright (c) Sam Leffler
 Copyright (c) Pixar

 Permission to use, copy, modify, distribute, and sell this software and
 its documentation for any purpose is hereby granted without fee, provided
 that (i) the above copyright notices and this permission notice appear in
 all copies of the software and related documentation, and (ii) the names of
 Pixar, Sam Leffler and Silicon Graphics may not be used in any advertising or
 publicity relating to the software without the specific, prior written
 permission of Pixar, Sam Leffler and Silicon Graphics.

 THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
 EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
 WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

 IN NO EVENT SHALL PIXAR, SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
 LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 OF THIS SOFTWARE.

============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============

646 HDF5 (Hierarchical Data Format 5) Software Library and Utilities files
covered by:


HDF5 (Hierarchical Data Format 5) Software Library and Utilities
Copyright (c) 2006-2018, The HDF Group.

NCSA HDF5 (Hierarchical Data Format 5) Software Library and Utilities
Copyright (c) 1998-2006, The Board of Trustees of the University of Illinois.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted for any purpose (including commercial purposes)
provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice,
   this list of conditions, and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions, and the following disclaimer in the documentation
   and/or materials provided with the distribution.

3. Neither the name of The HDF Group, the name of the University, nor the
   name of any Contributor may be used to endorse or promote products derived
   from this software without specific prior written permission from
   The HDF Group, the University, or the Contributor, respectively.

DISCLAIMER:
THIS SOFTWARE IS PROVIDED BY THE HDF GROUP AND THE CONTRIBUTORS
"AS IS" WITH NO WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED. IN NO EVENT SHALL THE HDF GROUP OR THE CONTRIBUTORS BE LIABLE FOR ANY DAMAGES SUFFERED BY THE USERS ARISING OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

You are under no obligation whatsoever to provide any bug fixes, patches, or upgrades to the features, functionality or performance of the source code ("Enhancements") to anyone; however, if you choose to make your Enhancements available either publicly, or directly to The HDF Group, without imposing a separate written license agreement for such Enhancements, then you hereby grant the following license: a non-exclusive, royalty-free perpetual license to install, use, modify, prepare derivative works, incorporate into other computer software, distribute, and sublicense such enhancements or derivative works thereof, in binary and source code form.

-----------------------------------------------------------------------------

Limited portions of HDF5 were developed by Lawrence Berkeley National
Laboratory (LBNL). LBNL's Copyright Notice and Licensing Terms can be
found here: COPYING_LBNL_HDF5 file in this directory or at
https://support.hdfgroup.org/ftp/HDF5/releases/COPYING_LBNL_HDF5.

-----------------------------------------------------------------------------

Contributors:   National Center for Supercomputing Applications (NCSA) at
the University of Illinois, Fortner Software, Unidata Program Center (netCDF), The Independent JPEG Group (JPEG), Jean-loup Gailly and Mark Adler (gzip), and Digital Equipment Corporation (DEC).

-----------------------------------------------------------------------------

Portions of HDF5 were developed with support from the Lawrence Berkeley
National Laboratory (LBNL) and the United States Department of Energy
under Prime Contract No. DE-AC02-05CH11231.

-----------------------------------------------------------------------------

Portions of HDF5 were developed with support from the University of
California, Lawrence Livermore National Laboratory (UC LLNL).
The following statement applies to those portions of the product and must
be retained in any redistribution of source code, binaries, documentation,
and/or accompanying materials:

   This work was partially produced at the University of California,
   Lawrence Livermore National Laboratory (UC LLNL) under contract
   no. W-7405-ENG-48 (Contract 48) between the U.S. Department of Energy
   (DOE) and The Regents of the University of California (University)
   for the operation of UC LLNL.

   DISCLAIMER:
   This work was prepared as an account of work sponsored by an agency of
   the United States Government. Neither the United States Government nor
   the University of California nor any of their employees, makes any
   warranty, express or implied, or assumes any liability or responsibility
   for the accuracy, completeness, or usefulness of any information,
   apparatus, product, or process disclosed, or represents that its use
   would not infringe privately- owned rights. Reference herein to any
   specific commercial products, process, or service by trade name,
   trademark, manufacturer, or otherwise, does not necessarily constitute
   or imply its endorsement, recommendation, or favoring by the United
   States Government or the University of California. The views and
   opinions of authors expressed herein do not necessarily state or reflect
   those of the United States Government or the University of California,
   and shall not be used for advertising or product endorsement purposes.

-----------------------------------------------------------------------------

HDF5 is available with the SZIP compression library but SZIP is not part
of HDF5 and has separate copyright and license terms. See SZIP Compression
in HDF Products (www.hdfgroup.org/doc_resource/SZIP/) for further details.

-----------------------------------------------------------------------------


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


25 files covered by:

/* zlib.h -- interface of the 'zlib' general purpose compression library
  version 1.1.4, March 11th, 2002

  Copyright (C) 1995-2002 Jean-loup Gailly and Mark Adler

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.

  Jean-loup Gailly        Mark Adler
  jloup@gzip.org          madler@alumni.caltech.edu


============== NEXT COPYRIGHT NOTICE AND LICENSE ENTRY ==============


343 Eigen files covered by MPL2:

Mozilla Public License Version 2.0
----------------------------------

1. Definitions
--------------

1.1. "Contributor"
    means each individual or legal entity that creates, contributes to
    the creation of, or owns Covered Software.

1.2. "Contributor Version"
    means the combination of the Contributions of others (if any) used
    by a Contributor and that particular Contributor's Contribution.

1.3. "Contribution"
    means Covered Software of a particular Contributor.

1.4. "Covered Software"
    means Source Code Form to which the initial Contributor has attached
    the notice in Exhibit A, the Executable Form of such Source Code
    Form, and Modifications of such Source Code Form, in each case
    including portions thereof.

1.5. "Incompatible With Secondary Licenses"
    means

    (a) that the initial Contributor has attached the notice described
        in Exhibit B to the Covered Software; or

    (b) that the Covered Software was made available under the terms of
        version 1.1 or earlier of the License, but not also under the
        terms of a Secondary License.

1.6. "Executable Form"
    means any form of the work other than Source Code Form.

1.7. "Larger Work"
    means a work that combines Covered Software with other material, in
    a separate file or files, that is not Covered Software.

1.8. "License"
    means this document.

1.9. "Licensable"
    means having the right to grant, to the maximum extent possible,
    whether at the time of the initial grant or subsequently, any and
    all of the rights conveyed by this License.

1.10. "Modifications"
    means any of the following:

    (a) any file in Source Code Form that results from an addition to,
        deletion from, or modification of the contents of Covered
        Software; or

    (b) any new file in Source Code Form that contains any Covered
        Software.

1.11. "Patent Claims" of a Contributor
    means any patent claim(s), including without limitation, method,
    process, and apparatus claims, in any patent Licensable by such
    Contributor that would be infringed, but for the grant of the
    License, by the making, using, selling, offering for sale, having
    made, import, or transfer of either its Contributions or its
    Contributor Version.

1.12. "Secondary License"
    means either the GNU General Public License, Version 2.0, the GNU
    Lesser General Public License, Version 2.1, the GNU Affero General
    Public License, Version 3.0, or any later versions of those
    licenses.

1.13. "Source Code Form"
    means the form of the work preferred for making modifications.

1.14. "You" (or "Your")
    means an individual or a legal entity exercising rights under this
    License. For legal entities, "You" includes any entity that
    controls, is controlled by, or is under common control with You. For
    purposes of this definition, "control" means (a) the power, direct
    or indirect, to cause the direction or management of such entity,
    whether by contract or otherwise, or (b) ownership of more than
    fifty percent (50%) of the outstanding shares or beneficial
    ownership of such entity.

2. License Grants and Conditions
--------------------------------

2.1. Grants

Each Contributor hereby grants You a world-wide, royalty-free,
non-exclusive license:

(a) under intellectual property rights (other than patent or trademark)
    Licensable by such Contributor to use, reproduce, make available,
    modify, display, perform, distribute, and otherwise exploit its
    Contributions, either on an unmodified basis, with Modifications, or
    as part of a Larger Work; and

(b) under Patent Claims of such Contributor to make, use, sell, offer
    for sale, have made, import, and otherwise transfer either its
    Contributions or its Contributor Version.

2.2. Effective Date

The licenses granted in Section 2.1 with respect to any Contribution
become effective for each Contribution on the date the Contributor first
distributes such Contribution.

2.3. Limitations on Grant Scope

The licenses granted in this Section 2 are the only rights granted under
this License. No additional rights or licenses will be implied from the
distribution or licensing of Covered Software under this License.
Notwithstanding Section 2.1(b) above, no patent license is granted by a
Contributor:

(a) for any code that a Contributor has removed from Covered Software;
    or

(b) for infringements caused by: (i) Your and any other third party's
    modifications of Covered Software, or (ii) the combination of its
    Contributions with other software (except as part of its Contributor
    Version); or

(c) under Patent Claims infringed by Covered Software in the absence of
    its Contributions.

This License does not grant any rights in the trademarks, service marks,
or logos of any Contributor (except as may be necessary to comply with
the notice requirements in Section 3.4).

2.4. Subsequent Licenses

No Contributor makes additional grants as a result of Your choice to
distribute the Covered Software under a subsequent version of this
License (see Section 10.2) or under the terms of a Secondary License (if
permitted under the terms of Section 3.3).

2.5. Representation

Each Contributor represents that the Contributor believes its
Contributions are its original creation(s) or it has sufficient rights
to grant the rights to its Contributions conveyed by this License.

2.6. Fair Use

This License is not intended to limit any rights You have under
applicable copyright doctrines of fair use, fair dealing, or other
equivalents.

2.7. Conditions

Sections 3.1, 3.2, 3.3, and 3.4 are conditions of the licenses granted
in Section 2.1.

3. Responsibilities
-------------------

3.1. Distribution of Source Form

All distribution of Covered Software in Source Code Form, including any
Modifications that You create or to which You contribute, must be under
the terms of this License. You must inform recipients that the Source
Code Form of the Covered Software is governed by the terms of this
License, and how they can obtain a copy of this License. You may not
attempt to alter or restrict the recipients' rights in the Source Code
Form.

3.2. Distribution of Executable Form

If You distribute Covered Software in Executable Form then:

(a) such Covered Software must also be made available in Source Code
    Form, as described in Section 3.1, and You must inform recipients of
    the Executable Form how they can obtain a copy of such Source Code
    Form by reasonable means in a timely manner, at a charge no more
    than the cost of distribution to the recipient; and

(b) You may distribute such Executable Form under the terms of this
    License, or sublicense it under different terms, provided that the
    license for the Executable Form does not attempt to limit or alter
    the recipients' rights in the Source Code Form under this License.

3.3. Distribution of a Larger Work

You may create and distribute a Larger Work under terms of Your choice,
provided that You also comply with the requirements of this License for
the Covered Software. If the Larger Work is a combination of Covered
Software with a work governed by one or more Secondary Licenses, and the
Covered Software is not Incompatible With Secondary Licenses, this
License permits You to additionally distribute such Covered Software
under the terms of such Secondary License(s), so that the recipient of
the Larger Work may, at their option, further distribute the Covered
Software under the terms of either this License or such Secondary
License(s).

3.4. Notices

You may not remove or alter the substance of any license notices
(including copyright notices, patent notices, disclaimers of warranty,
or limitations of liability) contained within the Source Code Form of
the Covered Software, except that You may alter any license notices to
the extent required to remedy known factual inaccuracies.

3.5. Application of Additional Terms

You may choose to offer, and to charge a fee for, warranty, support,
indemnity or liability obligations to one or more recipients of Covered
Software. However, You may do so only on Your own behalf, and not on
behalf of any Contributor. You must make it absolutely clear that any
such warranty, support, indemnity, or liability obligation is offered by
You alone, and You hereby agree to indemnify every Contributor for any
liability incurred by such Contributor as a result of warranty, support,
indemnity or liability terms You offer. You may include additional
disclaimers of warranty and limitations of liability specific to any
jurisdiction.

4. Inability to Comply Due to Statute or Regulation
---------------------------------------------------

If it is impossible for You to comply with any of the terms of this
License with respect to some or all of the Covered Software due to
statute, judicial order, or regulation then You must: (a) comply with
the terms of this License to the maximum extent possible; and (b)
describe the limitations and the code they affect. Such description must
be placed in a text file included with all distributions of the Covered
Software under this License. Except to the extent prohibited by statute
or regulation, such description must be sufficiently detailed for a
recipient of ordinary skill to be able to understand it.

5. Termination
--------------

5.1. The rights granted under this License will terminate automatically
if You fail to comply with any of its terms. However, if You become
compliant, then the rights granted under this License from a particular
Contributor are reinstated (a) provisionally, unless and until such
Contributor explicitly and finally terminates Your grants, and (b) on an
ongoing basis, if such Contributor fails to notify You of the
non-compliance by some reasonable means prior to 60 days after You have
come back into compliance. Moreover, Your grants from a particular
Contributor are reinstated on an ongoing basis if such Contributor
notifies You of the non-compliance by some reasonable means, this is the
first time You have received notice of non-compliance with this License
from such Contributor, and You become compliant prior to 30 days after
Your receipt of the notice.

5.2. If You initiate litigation against any entity by asserting a patent
infringement claim (excluding declaratory judgment actions,
counter-claims, and cross-claims) alleging that a Contributor Version
directly or indirectly infringes any patent, then the rights granted to
You by any and all Contributors for the Covered Software under Section
2.1 of this License shall terminate.

5.3. In the event of termination under Sections 5.1 or 5.2 above, all
end user license agreements (excluding distributors and resellers) which
have been validly granted by You or Your distributors under this License
prior to termination shall survive termination.

************************************************************************

6. Disclaimer of Warranty
-------------------------

Covered Software is provided under this License on an "as is"
basis, without warranty of any kind, either expressed, implied, or
statutory, including, without limitation, warranties that the
Covered Software is free of defects, merchantable, fit for a
particular purpose or non-infringing. The entire risk as to theg
quality and performance of the Covered Software is with You.
Should any Covered Software prove defective in any respect, Youg
(not any Contributor) assume the cost of any necessary servicing,
repair, or correction. This disclaimer of warranty constitutes an
essential part of this License. No use of any Covered Software is
Authorized under this License except under this disclaimer.

************************************************************************

7. Limitation of Liability
--------------------------

Under no circumstances and under no legal theory, whether tortg
(including negligence), contract, or otherwise, shall anyg
Contributor, or anyone who distributes Covered Software asg
permitted above, be liable to You for any direct, indirect,
special, incidental, or consequential damages of any characterg
including, without limitation, damages for lost profits, loss of
goodwill, work stoppage, computer failure or malfunction, or any
and all other commercial damages or losses, even if such partyg
shall have been informed of the possibility of such damages. This
limitation of liability shall not apply to liability for death or
personal injury resulting from such party's negligence to the
extent applicable law prohibits such limitation. Someg
jurisdictions do not allow the exclusion or limitation ofg
incidental or consequential damages, so this exclusion andg
limitation may not apply to You.

************************************************************************

8. Litigation
-------------

Any litigation relating to this License may be brought only in the
courts of a jurisdiction where the defendant maintains its principal
place of business and such litigation shall be governed by laws of that
jurisdiction, without reference to its conflict-of-law provisions.
Nothing in this Section shall prevent a party's ability to bring
cross-claims or counter-claims.

9. Miscellaneous
----------------

This License represents the complete agreement concerning the subject
matter hereof. If any provision of this License is held to be
unenforceable, such provision shall be reformed only to the extent
necessary to make it enforceable. Any law or regulation which provides
that the language of a contract shall be construed against the drafter
shall not be used to construe this License against a Contributor.

10. Versions of the License
---------------------------

10.1. New Versions

Mozilla Foundation is the license steward. Except as provided in Section
10.3, no one other than the license steward has the right to modify or
publish new versions of this License. Each version will be given a
distinguishing version number.

10.2. Effect of New Versions

You may distribute the Covered Software under the terms of the version
of the License under which You originally received the Covered Software,
or under the terms of any subsequent version published by the license
steward.

10.3. Modified Versions

If you create software not governed by this License, and you want to
create a new license for such software, you may create and use a
modified version of this License if you rename the license and remove
any references to the name of the license steward (except to note that
such modified license differs from this License).

10.4. Distributing Source Code Form that is Incompatible With Secondary
Licenses

If You choose to distribute Source Code Form that is Incompatible With
Secondary Licenses under the terms of this version of the License, the
notice described in Exhibit B of this License must be attached.

Exhibit A - Source Code Form License Notice
-------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.

If it is not possible or desirable to put the notice in a particular
file, then You may include the notice in a location (such as a LICENSE
file in a relevant directory) where a recipient would be likely to look
for such a notice.

You may add additional accurate notices of copyright ownership.

Exhibit B - "Incompatible With Secondary Licenses" Notice
---------------------------------------------------------

  This Source Code Form is "Incompatible With Secondary Licenses", as
  defined by the Mozilla Public License, v. 2.0.
