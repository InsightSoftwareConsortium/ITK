set(DOCUMENTATION  "This module contains the third party <a
href=\"http://www.vtk.org/Wiki/MetaIO\">MetaIO</a> library.  MetaImage is the
text-based tagged file format for medical images.  It was extended to file
formats to support a variety of objects that occur in medicine such a tubes (for
vessels, needles, etc.), blobs (for arbitrary shaped objects), cubes, spheres,
etc. The complete library is known as MetaIO.")

itk_module(ITKMetaIO
  DEPENDS
    ITKZLIB
  DESCRIPTION
    "${DOCUMENTATION}"
)
