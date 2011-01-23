#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

import itk, vtk
import ItkVtkGlue

names = [name for name in dir(ItkVtkGlue) if not name.startswith("__")]
for name in names :
    setattr(itk, name, ItkVtkGlue.__dict__[name])

# some cleanup
del itk, vtk, ItkVtkGlue, names, name

# also keep ItkVtkGlue members in that namespace
from ItkVtkGlue import *

# display a deprecation warning
import sys
print >> sys.stderr, "Warning: ItkVtkGlue classes are available in itk module, without importing itkvtk."
print >> sys.stderr, "Warning: itkvtk is no more supported and will be removed soon."
del sys
