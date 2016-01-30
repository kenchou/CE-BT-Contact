#!/usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import csv
import vobject
import sys
import codecs
import locale

sys.stdout = codecs.getwriter(locale.getpreferredencoding())(sys.stdout)

# parse command line arguments
parser = argparse.ArgumentParser()
parser.add_argument('remaining_args', nargs=argparse.REMAINDER)
args = parser.parse_args()

contacts = []
items = vobject.readComponents(codecs.getreader('utf-8')(sys.stdin))
# with codecs.open('00001.vcf', 'r', 'utf-8') as f:
#     items = vobject.readComponents(f)
#     # print items.content
for item in items:
    for child in item.getChildren():
        if child.name == 'TEL':
            types = [x.lower()for x in child.params['TYPE'] if x not in ['CELL', 'PREF', 'VOICE']]
            contacts.append({'Name': item.fn.value.encode('utf-8'),
                             'PhoneNum': child.value.encode('utf-8'),
                             'Memo': ','.join(types).encode('utf-8')})

contacts_sorted = sorted(contacts, key=lambda k: k[u'Name'])

with open('contact.csv', 'wb') as f:
    fieldnames = ['Name', 'PhoneNum', 'Memo']
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(contacts_sorted)
