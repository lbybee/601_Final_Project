'''
Cleaning Hillary Clinton email data. Note that this was written and tested
on Python 3.5 (Anaconda 2.4), and may or may not work on Python 2.X.

'''
import numpy as np
import pandas as pd

# Possible issues:
#   Email body text often contains leftover reply/forward text, is there
#     a good way to deal with this?

# Read data
emails = pd.read_csv('./data/raw_data/emails.csv')
persons = pd.read_csv('./data/raw_data/persons.csv')
receivers = pd.read_csv('./data/raw_data/EmailReceivers.csv')

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

# Save
emails = emails[[
    'Id', 'MetadataSubject', 'MetadataTo', 'MetadataFrom', 'MetadataDateSent',
    'ExtractedSubject', 'ExtractedTo', 'ExtractedFrom', 'ExtractedDateSent',
    'ExtractedBodyText', 'AllText',
    'SenderPersonId', 'SenderPerson', 'ReceiverPersonId', 'ReceiverPerson',
    'Redacted', 'ReceiverHillary', 'SenderHillary',
]]

emails_hill = emails.ix[emails['SenderHillary']]
emails_hill.to_csv('./data/emails_hill.csv', index=False)
emails.to_csv('./data/emails.csv', index=False)

