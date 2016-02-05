#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import csv
import fileinput
import vobject
from pypinyin import lazy_pinyin
import sys
import codecs
import locale

sys.stdout = codecs.getwriter(locale.getpreferredencoding())(sys.stdout)

# parse command line arguments
parser = argparse.ArgumentParser()
parser.add_argument('remaining_args', nargs=argparse.REMAINDER)
args = parser.parse_args()
filelist = args.remaining_args if args.remaining_args else [sys.stdin]

contacts = []
# items = vobject.readComponents(codecs.getreader('utf-8')(sys.stdin))
for filename in filelist:
    print 'Processing', filename if isinstance(filename, basestring) else 'stdin'
    fileStream = open(filename, 'r') if isinstance(filename, basestring) else codecs.getreader('utf-8')(sys.stdin)
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
    # writer.writerows(contacts_sorted)
    for row in contacts_sorted:
        writer.writerow({k:v.encode('utf8') for k,v in row.items()})
