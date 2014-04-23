#!/usr/bin/env python

description = """
Update the JRE tarballs to be bundled with the SCIFIOImageIO plugin.

The OpenJDK JRE (but no the JDK) can be redistributed.  It is downloaded at
build time from Midas and shipped with the SCIFIOImageIO plugin so that the
plugin "just works".

The Fiji fellows maintain Git repositories that tracks the OpenJDK JRE.
Here we clone that repository and create the JRE tarball from it.

Currently, the tarball needs to be uploaded manually to midas3.kitware.com.
In the future, pydas can be used for automatic upload.
"""

from __future__ import print_function

import os
import subprocess
import tarfile
import tempfile

platforms = ['linux-amd64',
    'linux',
    # Newer MacOX will eventually need a download for Java7 (once the security
    # vulnerabilities are gone.
    #'macosx',
    'win32',
    'win64']

os.chdir(tempfile.gettempdir())
for platform in platforms:
    call = ['git', 'clone',
            '--depth', '0',
            'git://fiji.sc/java/' + platform + '.git']
    subprocess.check_call(call)

for platform in platforms:
    print('Writing tarball for ' + platform + '...')
    os.chdir(platform)
    version = os.listdir(os.getcwd())[1]
    os.chdir(version)
    tarballFile = version + '.tar.bz2'
    with tarfile.open(tarballFile, 'w:bz2', dereference=True) as \
        tarball:
            tarball.add('jre')
    os.chdir(os.path.join('..', '..'))
