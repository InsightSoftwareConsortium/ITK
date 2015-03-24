#!/usr/bin/env python

from __future__ import print_function

description = """
Upload all the ExternalData files to Azure Blob storage.

The files corresponding to all the ExternalData content links
in the source tree are archived on the Azure storage container
using the local ExternalData object store.

Requires the azure package:

  https://github.com/Azure/azure-sdk-for-python

There should also be a "md5" container on the storage account.  See:

  http://azure.microsoft.com/en-us/documentation/articles/storage-python-how-to-use-blob-storage/
"""

import argparse
import fnmatch
import os
import sys

from azure.storage import BlobService

def upload_to_azure(content_link, externaldata_object_store,
                    blob_service, current_blobs):
    # get the MD5 checksum
    print('Uploading ' + content_link + ' ...')
    with open(content_link, 'r') as fp:
        md5hash = fp.readline().strip()
    print('Checksum: ' + md5hash)

    if not md5hash in current_blobs:
        object_store = os.path.join(externaldata_object_store, 'MD5', md5hash)
        if not os.path.exists(object_store):
            sys.stderr.write('Could not find the expected object store.\n')
            sys.exit(1)

        blob_service.put_block_blob_from_path('md5', md5hash, object_store)


def run(itk_source_dir, externaldata_object_store, account_name, account_key):
    blob_service = BlobService(account_name=account_name,
                               account_key=account_key)
    blobs = blob_service.list_blobs('md5')
    current_blobs = [blob.name for blob in blobs]

    md5files = []
    for root, dirnames, filenames in os.walk(itk_source_dir):
        for filename in fnmatch.filter(filenames, '*.md5'):
            md5files.append(os.path.join(root, filename))

    for content_link in md5files:
        upload_to_azure(content_link, externaldata_object_store,
                        blob_service, current_blobs)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--account-name', '-a', default="itkexternaldata",
            help="Azure Storage account name.")
    parser.add_argument('account_key', type=argparse.FileType('r'),
            help="A file that contains the Azure Storage access key.")
    parser.add_argument('itk_source_dir',
            help='Path to the ITK source tree.')
    parser.add_argument('externaldata_object_store',
            help='Path to the ExternalData object store, e.g. ' \
            + 'ExternalData/Objects/ in a build tree.')
    args = parser.parse_args()

    account_key = args.account_key.readline()
    account_key = account_key.strip()

    run(args.itk_source_dir, args.externaldata_object_store,
        account_name=args.account_name, account_key=account_key)
