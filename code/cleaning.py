'''
Cleaning Hillary Clinton email data. Note that this was written and tested
on Python 3.5 (Anaconda 2.4), and may or may not work on Python 2.X.

'''
import re
from datetime import datetime as dt

import numpy as np
import pandas as pd

# Possible issues:
#   Email body text often contains leftover reply/forward text, is there
#     a good way to deal with this?

# Read data
emails = pd.read_csv('../data/raw_data/Emails.csv')
persons = pd.read_csv('../data/raw_data/Persons.csv')
receivers = pd.read_csv('../data/raw_data/EmailReceivers.csv')

# Redacted indicator
part = emails['ExtractedReleaseInPartOrFull'] == 'RELEASE IN PART'
full = emails['ExtractedReleaseInPartOrFull'] == 'RELEASE IN FULL'
emails = emails.ix[np.logical_or(part, full)]  # Drop obs where unknown
emails['Redacted'] = part

# Drop unused variables
drop_columns = [
    'DocNumber',
    'MetadataDateReleased',
    'MetadataPdfLink',
    'MetadataCaseNumber',
    'MetadataDocumentClass',
    'ExtractedCc',
    'ExtractedCaseNumber',
    'ExtractedDocNumber',
    'ExtractedDateReleased',
    'ExtractedReleaseInPartOrFull',
    'RawText',
]
emails.drop(drop_columns, axis=1, inplace=True)

# Combine subject and body text
def comb_subj_body(row):
    subj = row['MetadataSubject']
    text = row['ExtractedBodyText']
    if pd.isnull(subj) and pd.isnull(text):
        return np.nan

    if pd.isnull(subj):
        subj = ''
    if pd.isnull(text):
        text = ''
    return subj + '\n' + text
emails['AllText'] = emails.apply(comb_subj_body, axis=1)
emails = emails.ix[pd.notnull(emails['AllText'])]

# Merge sender names
persons.columns = ['SenderPersonId', 'SenderPerson']
emails = emails.merge(persons, how='left', on='SenderPersonId')

# Merge receiver names
receivers.columns = ['drop', 'Id', 'ReceiverPersonId']
receivers = receivers[['Id', 'ReceiverPersonId']]
receivers = pd.DataFrame(
    receivers.groupby('Id').ReceiverPersonId.apply(tuple))
receivers['Id'] = receivers.index
emails = emails.merge(receivers, how='left', on='Id')

persons.index = persons['SenderPersonId']
persons_dict = persons.to_dict()['SenderPerson']
def receiver_idtoperson(row):
    ids = row['ReceiverPersonId']
    if pd.isnull(ids):
        return np.nan
    else:
        return tuple(map(persons_dict.get, ids))
emails['ReceiverPerson'] = emails.apply(receiver_idtoperson, axis=1)

# Hillary checks
def to_hrc(row):
    ids = row['ReceiverPersonId']
    if pd.isnull(ids):
        return False
    else:
        return 80 in ids
emails['ReceiverHillary'] = emails.apply(to_hrc, axis=1)
emails['SenderHillary'] = emails['SenderPersonId'] == 80

# Clean Text
pattern_email = re.compile('([\w\-\.]+@(\w[\w\-]+\.)+[\w\-]+)')
pattern_declassify = re.compile('Declassify on:\s+\d+/\d+/\d+')
pattern_confidential = re.compile('Class:\s+CONFIDENTIAL')
pattern_reason = re.compile('Reason:\s\d.\d\(\w\)')
pattern_release = re.compile('(RELEASE\s+IN\s+PART)')
pattern_date = re.compile(
    '(\w+day, \w+ \d+, \d+ \d+:\d+ [AP]M)')
pattern_misc = re.compile('([rR][eE]:)|([fF][wW]([Dd])?:)')
patterns = (
    pattern_email, pattern_declassify,
    pattern_confidential, pattern_reason, pattern_release,
    pattern_date, pattern_misc
)
pattern_plsprint = re.compile('[Pp][iI]s ([Pp]rint)')
pattern_plsrespond = re.compile('[Pp][iI]s ([Rr]espond)')
pattern_plsadd = re.compile('[Pp][iI]s ([Aa]dd)')
def clean_text(row):
    text = row['AllText']
    for pattern in patterns:
        text = re.sub(pattern, ' ', text)
    text = re.sub(pattern_plsprint, 'Pls print', text)
    text = re.sub(pattern_plsrespond, 'Pls respond', text)
    text = re.sub(pattern_plsadd, 'Pls add', text)
    return text
emails['AllText'] = emails.apply(clean_text, axis=1)

# Extract Dates
def get_date(row):
    datestr = row['MetadataDateSent']
    if isinstance(datestr, str):
        return dt.strptime(datestr[:10], '%Y-%m-%d').strftime('%Y-%m-%d')
    else:
        return np.nan
def get_year(row):
    d = row['DateSent']
    if isinstance(d, str):
        return int(d[:4])
    else:
        return np.nan
def get_month(row):
    d = row['DateSent']
    if isinstance(d, str):
        return int(d[5:7])
    else:
        return np.nan
emails['DateSent'] = emails.apply(get_date, axis=1)
emails['YearSent'] = emails.apply(get_year, axis=1)
emails['MonthSent'] = emails.apply(get_month, axis=1)

# Save
emails = emails[[
    'Id', 'MetadataSubject', 'MetadataTo', 'MetadataFrom', 'MetadataDateSent',
    'ExtractedSubject', 'ExtractedTo', 'ExtractedFrom', 'ExtractedDateSent',
    'ExtractedBodyText', 'AllText',
    'SenderPersonId', 'SenderPerson', 'ReceiverPersonId', 'ReceiverPerson',
    'Redacted', 'ReceiverHillary', 'SenderHillary',
    'DateSent', 'YearSent', 'MonthSent',
]]

emails_hill = emails.ix[emails['SenderHillary']]
emails_hill = emails_hill.reset_index(drop=True)

emails_hill.to_csv('../data/emails_hill.csv', index=False)
emails.to_csv('../data/emails.csv', index=False)
