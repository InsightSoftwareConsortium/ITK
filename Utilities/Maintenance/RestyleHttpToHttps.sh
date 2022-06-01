#!/bin/bash

#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================


# Script to automate restyling in an ITK remote module.
# `sed` is used to replace http with https links for
# known valid cases.

find . -type f | \
    fgrep -v sed-commands | fgrep -v .git/ | fgrep -v ThirdParty | \
    tr \\n \\0 | xargs -0 sed -i -r \
        -e 's#http://a11yproject.com#https://a11yproject.com#g' \
        -e 's#http://arxiv.org#https://arxiv.org#g' \
        -e 's#http://blog.alexmaccaw.com#https://blog.alexmaccaw.com#g' \
        -e 's#http://bootswatch.com#https://bootswatch.com#g' \
        -e 's#http://brainweb.bic.mni.mcgill.ca#https://brainweb.bic.mni.mcgill.ca#g' \
        -e 's#http://breathe.readthedocs.org#https://breathe.readthedocs.org#g' \
        -e 's#http://campar.in.tum.de#https://campar.in.tum.de#g' \
        -e 's#http://cdn-fastly.deb.debian.org#https://cdn-fastly.deb.debian.org#g' \
        -e 's#http://choorucode.com#https://choorucode.com#g' \
        -e 's#http://citeseerx.ist.psu.edu#https://citeseerx.ist.psu.edu#g' \
        -e 's#http://creativecommons.org#https://creativecommons.org#g' \
        -e 's#http://data.org#https://data.org#g' \
        -e 's#http://dblp.uni-trier.de#https://dblp.uni-trier.de#g' \
        -e 's#http://dicom.nema.org#https://dicom.nema.org#g' \
        -e 's#http://doc.qt.io#https://doc.qt.io#g' \
        -e 's#http://docs.mathjax.org#https://docs.mathjax.org#g' \
        -e 's#http://docs.python.org#https://docs.python.org#g' \
        -e 's#http://docs.scipy.org#https://docs.scipy.org#g' \
        -e 's#http://dx.doi.org#https://dx.doi.org#g' \
        -e 's#http://EditorConfig.org#https://EditorConfig.org#g' \
        -e 's#http://en.wikipedia.org#https://en.wikipedia.org#g' \
        -e 's#http://example.com#https://example.com#g' \
        -e 's#http://getbootstrap.com#https://getbootstrap.com#g' \
        -e 's#http://git-scm.com#https://git-scm.com#g' \
        -e 's#http://goo.gl#https://goo.gl#g' \
        -e 's#http://hdl.handle.net#https://hdl.handle.net#g' \
        -e 's#http://insight-journal.org#https://insight-journal.org#g' \
        -e 's#http://itk.org#https://itk.org#g' \
        -e 's#http://keepachangelog.com#https://keepachangelog.com#g' \
        -e 's#http://lcw.lehman.edu#https://lcw.lehman.edu#g' \
        -e 's#http://librelist.com#https://librelist.com#g' \
        -e 's#http://linkinghub.elsevier.com#https://linkinghub.elsevier.com#g' \
        -e 's#http://lists.apple.com#https://lists.apple.com#g' \
        -e 's#http://math.nist.gov#https://math.nist.gov#g' \
        -e 's#http://mathworld.wolfram.com#https://mathworld.wolfram.com#g' \
        -e 's#http://matplotlib.org#https://matplotlib.org#g' \
        -e 's#http://medical.nema.org#https://medical.nema.org#g' \
        -e 's#http://networkx.github.io#https://networkx.github.io#g' \
        -e 's#http://nicolasgallagher.com#https://nicolasgallagher.com#g' \
        -e 's#http://openmicroscopy.org#https://openmicroscopy.org#g' \
        -e 's#http://openslide.cs.cmu.edu#https://openslide.cs.cmu.edu#g' \
        -e 's#http://openslide.org#https://openslide.org#g' \
        -e 's#http://openslide.org.#https://openslide.org.#g' \
        -e 's#http://opensource.org#https://opensource.org#g' \
        -e 's#http://people.csail.mit.edu#https://people.csail.mit.edu#g' \
        -e 's#http://projecteuclid.org#https://projecteuclid.org#g' \
        -e 's#http://public.kitware.com#https://public.kitware.com#g' \
        -e 's#http://purl.org#https://purl.org#g' \
        -e 's#http://pygments.org#https://pygments.org#g' \
        -e 's#http://pypi.python.org#https://pypi.python.org#g' \
        -e 's#http://raw.githubusercontent.com#https://raw.githubusercontent.com#g' \
        -e 's#http://readthedocs.org#https://readthedocs.org#g' \
        -e 's#http://releases.llvm.org#https://releases.llvm.org#g' \
        -e 's#http://scif.io#https://scif.io#g' \
        -e 's#http://scikit-learn.org#https://scikit-learn.org#g' \
        -e 's#http://seaborn.pydata.org#https://seaborn.pydata.org#g' \
        -e 's#http://sflogo.sourceforge.net#https://sflogo.sourceforge.net#g' \
        -e 's#http://sourceforge.net#https://sourceforge.net#g' \
        -e 's#http://stackoverflow.com#https://stackoverflow.com#g' \
        -e 's#http://timkadlec.com#https://timkadlec.com#g' \
        -e 's#http://tools.ietf.org#https://tools.ietf.org#g' \
        -e 's#http://webassembly.org#https://webassembly.org#g' \
        -e 's#http://wiki.epfl.ch#https://wiki.epfl.ch#g' \
        -e 's#http://wiki.openrtk.org#https://wiki.openrtk.org#g' \
        -e 's#http://www2.imm.dtu.dk#https://www2.imm.dtu.dk#g' \
        -e 's#http://www5.informatik.uni-erlangen.de#https://www5.informatik.uni-erlangen.de#g' \
        -e 's#http://www.alleninstitute.org#https://www.alleninstitute.org#g' \
        -e 's#http://www.apache.org#https://www.apache.org#g' \
        -e 's#http://www.bio-rad.com#https://www.bio-rad.com#g' \
        -e 's#http://www.boost.org#https://www.boost.org#g' \
        -e 's#http://www.cancer.gov#https://www.cancer.gov#g' \
        -e 's#http://www.cise.ufl.edu#https://www.cise.ufl.edu#g' \
        -e 's#http://www.cmake.org#https://www.cmake.org#g' \
        -e 's#http://www.cplusplus.com#https://www.cplusplus.com#g' \
        -e 's#http://www.creatis.insa-lyon.fr#https://www.creatis.insa-lyon.fr#g' \
        -e 's#http://www.cs.bgu.ac.il#https://www.cs.bgu.ac.il#g' \
        -e 's#http://www.cs.unb.ca#https://www.cs.unb.ca#g' \
        -e 's#http://www.cs.unc.edu#https://www.cs.unc.edu#g' \
        -e 's#http://www.darpa.mil#https://www.darpa.mil#g' \
        -e 's#http://www.dbf2002.com#https://www.dbf2002.com#g' \
        -e 's#http://www.doxygen.nl#https://www.doxygen.nl#g' \
        -e 's#http://www.doxygen.org#https://www.doxygen.org#g' \
        -e 's#http://www.fftw.org#https://www.fftw.org#g' \
        -e 's#http://www.github.com#https://www.github.com#g' \
        -e 's#http://www.gnu.org#https://www.gnu.org#g' \
        -e 's#http://www.graphviz.org#https://www.graphviz.org#g' \
        -e 's#http://www.gutenberg.org#https://www.gutenberg.org#g' \
        -e 's#http://www.iba-protontherapy.com#https://www.iba-protontherapy.com#g' \
        -e 's#http://www.ibm.com#https://www.ibm.com#g' \
        -e 's#http://www.incf.org#https://www.incf.org#g' \
        -e 's#http://www.inkscape.org#https://www.inkscape.org#g' \
        -e 's#http://www.insight-journal.org#https://www.insight-journal.org#g' \
        -e 's#http://www.istb.unibe.ch#https://www.istb.unibe.ch#g' \
        -e 's#http://www.itk.org#https://www.itk.org#g' \
        -e 's#http://www.kitware.com#https://www.kitware.com#g' \
        -e 's#http://www.lorenzobettini.it#https://www.lorenzobettini.it#g' \
        -e 's#http://www.mathworks.com#https://www.mathworks.com#g' \
        -e 's#http://www.mcternan.me.uk#https://www.mcternan.me.uk#g' \
        -e 's#http://www.mgh.harvard.edu#https://www.mgh.harvard.edu#g' \
        -e 's#http://www.mitk.org#https://www.mitk.org#g' \
        -e 's#http://www.modernizr.com#https://www.modernizr.com#g' \
        -e 's#http://www.ncbi.nlm.nih.gov#https://www.ncbi.nlm.nih.gov#g' \
        -e 's#http://www.nibib.nih.gov#https://www.nibib.nih.gov#g' \
        -e 's#http://www.nigms.nih.gov#https://www.nigms.nih.gov#g' \
        -e 's#http://www.ninds.nih.gov#https://www.ninds.nih.gov#g' \
        -e 's#http://www.nmr.mgh.harvard.edu#https://www.nmr.mgh.harvard.edu#g' \
        -e 's#http://www.numpy.org#https://www.numpy.org#g' \
        -e 's#http://www.oasis-open.org#https://www.oasis-open.org#g' \
        -e 's#http://www.opengl.org#https://www.opengl.org#g' \
        -e 's#http://www.openjpeg.org#https://www.openjpeg.org#g' \
        -e 's#http://www.openmicroscopy.org#https://www.openmicroscopy.org#g' \
        -e 's#http://www.openrtk.org#https://www.openrtk.org#g' \
        -e 's#http://www.openwetware.org#https://www.openwetware.org#g' \
        -e 's#http://www.povray.org#https://www.povray.org#g' \
        -e 's#http://www.predictive-toxicology.org#https://www.predictive-toxicology.org#g' \
        -e 's#http://www.pubmedcentral.nih.gov#https://www.pubmedcentral.nih.gov#g' \
        -e 's#http://www.sat.qc.ca#https://www.sat.qc.ca#g' \
        -e 's#http://www.scanco.ch#https://www.scanco.ch#g' \
        -e 's#http://www.slaney.org#https://www.slaney.org#g' \
        -e 's#http://www.slicer.org#https://www.slicer.org#g' \
        -e 's#http://www.stack.nl#https://www.stack.nl#g' \
        -e 's#http://www.uclouvain.be#https://www.uclouvain.be#g' \
        -e 's#http://www.uspto.gov#https://www.uspto.gov#g' \
        -e 's#http://www.via.cornell.edu#https://www.via.cornell.edu#g' \
        -e 's#http://www.vtk.org#https://www.vtk.org#g' \
        -e 's#http://www.w3.org#https://www.w3.org#g' \
        -e 's#http://xquartz.macosforge.org#https://xquartz.macosforge.org#g'
