# -*- coding: utf-8 -*-
from __future__ import print_function
from os import sys

try:
    from skbuild import setup
except ImportError:
    print('scikit-build is required to build from source.', file=sys.stderr)
    print('Please run:', file=sys.stderr)
    print('', file=sys.stderr)
    print('  python -m pip install scikit-build')
    sys.exit(1)

setup(
    name='itk-montage',
    version='0.8.1',
    author='Dženan Zukić, Matt McCormick',
    author_email='itk+community@discourse.itk.org',
    packages=['itk'],
    package_dir={'itk': 'itk'},
    download_url=r'https://github.com/InsightSoftwareConsortium/ITKMontage',
    description=r'Montaging for microscopy imaging files.',
    long_description='itk-montage provides classes for montaging of '
                 'microscopy imaging files.\n'
                 'Please refer to:\n'
                 'Bican, J. "Phase Correlation Method for ITK" '
                 'Insight Journal, July-December 2006, https://hdl.handle.net/1926/396.',
    classifiers=[
        "License :: OSI Approved :: Apache Software License",
        "Programming Language :: Python",
        "Programming Language :: C++",
        "Development Status :: 4 - Beta",
        "Intended Audience :: Developers",
        "Intended Audience :: Education",
        "Intended Audience :: Healthcare Industry",
        "Intended Audience :: Science/Research",
        "Topic :: Scientific/Engineering",
        "Topic :: Scientific/Engineering :: Medical Science Apps.",
        "Topic :: Scientific/Engineering :: Information Analysis",
        "Topic :: Software Development :: Libraries",
        "Operating System :: Android",
        "Operating System :: Microsoft :: Windows",
        "Operating System :: POSIX",
        "Operating System :: Unix",
        "Operating System :: MacOS"
        ],
    license='Apache',
    keywords='ITK InsightToolkit Montage Image-stitching Image-montage',
    url=r'https://github.com/InsightSoftwareConsortium/ITKMontage',
    install_requires=[
        r'itk-core>=v5.3.0',
        r'itk-filtering>=v5.3.0',
        r'itk-io>=v5.3.0',
        r'numpy'
    ]
    )
