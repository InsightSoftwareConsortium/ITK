import itk, re, sys
# itk.auto_progress(True)

from itkTemplate import itkTemplate

# sets are not in builtin with python older than 2.4
import sets
set = sets.Set

# dirty but easier: a global var to count the empty classes
count = 0

def exploreTpl(tpl):
    for cl in tpl.itervalues():
	exploreMethods(cl)
	# try to instanciate the class
	try :
	    obj = cl.New()
	    exploreMethods(obj)
	except:
	    pass
	try :
	    exploreMethods(cl())
	except:
	    pass
    
def exploreMethods(obj):
    global count
    excludeList = ['this', 'thisown']
    attrNameList = [i for i in dir(obj) if isinstance(i, str) and i[0].isupper() and i not in excludeList]
    if attrNameList == [] :
      count += 1
      print obj
	
      
excluded = set([
  "PeriodicBoundaryCondition",
  "BandNode",
  "DefaultDynamicMeshTraits",
  "DefaultStaticMeshTraits",
  "NormalBandNode",
  "ZeroFluxNeumannBoundaryCondition",
  ])

attrNameList = set([i for i in dir(itk) if i[0].isupper() and len(i) > 2]) - excluded

for name in attrNameList:
    # use it because of lazy loading
    exec "attr = itk."+name
    # print "-----------", name, "-----------"
    if isinstance(attr, itkTemplate) :
	exploreTpl(attr)
    else :
	exploreMethods(attr)
        try :
	    exploreMethods(cl.New())
	except:
	    pass
	try :
	    exploreMethods(cl())
	except:
	    pass
								
sys.exit(count)
