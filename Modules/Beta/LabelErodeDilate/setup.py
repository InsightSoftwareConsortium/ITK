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
    name='itk-labelerodedilate',
    version='1.1.1',
    author='Richard Beare',
    author_email='Richard.Beare@med.monash.edu.au',
    packages=['itk'],
    package_dir={'itk': 'itk'},
    download_url=r'https://github.com/InsightSoftwareConsortium/ITKLabelErodeDilate',
    description=r'An ITK module for erode and dilate operations on label images',
    long_description='itk-labelerodedilate provides classes for morphological math '
                     'erode and dilate operations on label images.\n'
                     'Please refer to:\n'
                     'Beare, R. and Jackway, P. '
                     '"Parallel Algorithms via Scaled Paraboloid Structuring '
                     'Functions for Spatially-Variant and Label-Set Dilations '
                     'and Erosions", '
                     '2011 International Conference on Digital Image Computing '
                     'Techniques and Applications (DICTA). 180--185. 2011. IEEE.',
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
    keywords='ITK InsightToolkit Math-morphology Label-images',
    url=r'https://github.com/InsightSoftwareConsortium/ITKLabelErodeDilate',
    install_requires=[
        r'itk>=5.2rc1'
    ]
    )
