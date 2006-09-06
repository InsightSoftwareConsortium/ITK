
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
