#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import unicodecsv as csv
import vobject
import jieba
from pypinyin import lazy_pinyin
import codecs
import locale
import os
import sys


__author__ = 'Ken Chou <kenchou77@gmail.com>'


def we_are_frozen():
    """Returns whether we are frozen via py2exe.
    This will affect how we find out where we are located."""
    return hasattr(sys, "frozen")


def module_path():
    """ This will get us the program's directory,
    even if we are frozen using py2exe"""
    if we_are_frozen():
        return os.path.dirname(unicode(sys.executable, sys.getfilesystemencoding( )))
    return os.path.dirname(unicode(__file__, sys.getfilesystemencoding( )))


sys.stdout = codecs.getwriter(locale.getpreferredencoding())(sys.stdout)

# init dict only for py2exe
if we_are_frozen():
    jieba.set_dictionary(os.path.join(module_path(), 'dict.txt'))
    jieba.initialize()

# parse command line arguments
parser = argparse.ArgumentParser()
parser.add_argument('remaining_args', nargs=argparse.REMAINDER)
args = parser.parse_args()
file_list = args.remaining_args if args.remaining_args else [sys.stdin]

contacts = []
for filename in file_list:
    # print 'Processing', filename if isinstance(filename, basestring) else 'stdin'
    fileStream = codecs.open(filename, 'r', encoding='utf8') if isinstance(filename, basestring) else codecs.getreader('utf-8')(sys.stdin)
    items = vobject.readComponents(fileStream)
    for item in items:
        for child in item.getChildren():
            if child.name == 'TEL':
                types = [x.lower()for x in child.params['TYPE']] if 'TYPE' in child.params else []
                data = {'Name': item.fn.value,
                        'PhoneNum': child.value.replace('-', ''),
                        'Memo': ','.join(types)}
                if data not in contacts:
                    contacts.append(data)

print 'Total', len(contacts), 'contacts'
print '------'
contacts_sorted = sorted(contacts, key=lambda k: lazy_pinyin(k['Name']))

with open('contact.csv', 'wb') as f:
    fieldnames = ['Name', 'PhoneNum', 'Memo']
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(contacts_sorted)
